param([string]$Revision = '4176c1642', [string]$Label = 'current', [switch]$WorkingTree)
$ErrorActionPreference = 'Stop'
$root = (Get-Location).Path
$dest = Join-Path $root "target/runtime-investigation/$Label"
New-Item -ItemType Directory -Force -Path $dest | Out-Null
git archive --format=zip "--output=$dest/source.zip" $Revision src/main/scala
if ($LASTEXITCODE -ne 0) { throw 'git archive failed' }
Expand-Archive -LiteralPath "$dest/source.zip" -DestinationPath $dest -Force
if ($WorkingTree) { Copy-Item -Path "$root/src/main/scala/*" -Destination "$dest/src/main/scala" -Recurse -Force }
$process = "$dest/src/main/scala/scalation/simulation/process"
$utf8 = New-Object System.Text.UTF8Encoding($false)
function Rewrite([string]$path, [string]$before, [string]$after) {
    $body = [IO.File]::ReadAllText($path)
    if (!$body.Contains($before)) { throw "Missing instrumentation anchor: $path : $before" }
    [IO.File]::WriteAllText($path, $body.Replace($before, $after), $utf8)
}
$modelPath = "$process/model/CalRoute101_3.scala"
if ([IO.File]::ReadAllText($modelPath).Contains('animating: Boolean = true')) {
    Rewrite $modelPath 'animating: Boolean = true' 'animating: Boolean = false'
}
Rewrite "$process/Model.scala" '        while simulating && !agenda.isEmpty do' "        RuntimeProbe.start ()`n        while simulating && !agenda.isEmpty do"
Rewrite "$process/Model.scala" '            _theActor = agenda.dequeue()' "            RuntimeProbe.event (clock, numActors)`n            _theActor = agenda.dequeue()"
Rewrite "$process/Model.scala" '        hasFinished()' "        RuntimeProbe.finish (clock, numActors)`n        hasFinished()"
Rewrite "$process/Sink.scala" '        val ctime = director.clock' "        RuntimeProbe.exited += 1`n        val ctime = director.clock"
Rewrite "$process/Sink.scala" '        tally (ctime - actor.arrivalT)' "        RuntimeProbe.departure (actor, ctime)`n        tally (ctime - actor.arrivalT)"
Rewrite "$process/Junction.scala" '        val ctime = director.clock' "        RuntimeProbe.detector (name, actor, director.clock)`n        val ctime = director.clock"
Rewrite "$process/VTransport.scala" '            motion.updateV(actor, length)' "            val probeBefore = actor.disp`n            motion.updateV(actor, length)"
Rewrite "$process/VTransport.scala" '            val cp = calcPoint2(actor.disp)' "            RuntimeProbe.moves += 1`n            RuntimeProbe.motion (actor)`n            if actor.disp <= probeBefore then RuntimeProbe.stationary += 1`n            val cp = calcPoint2(actor.disp)"
$probe = @'
package scalation.simulation.process

/** Identical measurement overlay for historical snapshots; never changes traffic state. */
object RuntimeProbe:
    var exited = 0L
    var events = 0L
    var moves = 0L
    var stationary = 0L
    private var motionHash = 1L
    private var exitHash = 1L
    private var detectorHash = 1L
    private var detectorCount = 0L
    var liveSize: () => Int = () => 0
    private var peakLiveSize = 0
    private def bits (x: Double): Long = java.lang.Double.doubleToLongBits (x)
    def motion (car: Vehicle): Unit =
        motionHash = java.lang.Long.rotateLeft (motionHash, 7) ^ bits (car.disp) ^ bits (car.velocity)
        motionHash = motionHash * 31 + car.displayLabel.hashCode + car.laneID * 7 + car.segId
    def departure (actor: SimActor, clock: Double): Unit =
        exitHash = java.lang.Long.rotateLeft (exitHash, 7) ^ bits (clock) ^ bits (clock - actor.arrivalT)
        exitHash = exitHash * 31 + actor.asInstanceOf[Vehicle].displayLabel.hashCode
    def detector (name: String, car: Vehicle, clock: Double): Unit =
        detectorCount += 1
        detectorHash = java.lang.Long.rotateLeft (detectorHash, 7) ^ bits (clock) ^ bits (car.velocity)
        detectorHash = detectorHash * 31 + name.hashCode + car.laneID
    private var started = 0L
    private var last = 0L
    def start (): Unit =
        started = System.nanoTime ()
        last = started
    def finish (clock: Double, created: Int): Unit =
        val elapsed = (System.nanoTime () - started) / 1e9
        println (s"[probe-complete] wall=$elapsed clock=$clock created=$created exited=$exited active=${created-exited} events=$events moves=$moves stationary=$stationary")
        println (s"[traffic-signature] motion=$motionHash exits=$exitHash detectors=$detectorHash detectorCount=$detectorCount")
        println (s"[visual-state] retained=${liveSize()} peak=$peakLiveSize")
        if java.lang.Boolean.getBoolean ("bench.exitAfterSimulation") then System.exit (0)
    def event (clock: Double, created: Int): Unit =
        events += 1
        if events % 10000 == 0 then
            peakLiveSize = math.max (peakLiveSize, liveSize())
            val now = System.nanoTime ()
            val elapsed = (now - started) / 1e9
            if now - last >= 10000000000L then
                println (s"[probe] wall=$elapsed clock=$clock created=$created exited=$exited active=${created-exited} events=$events moves=$moves stationary=$stationary")
                val runtime = Runtime.getRuntime
                println (s"[memory] usedMiB=${(runtime.totalMemory-runtime.freeMemory)/1048576} live=${liveSize()}")
                last = now
            if elapsed >= java.lang.Double.parseDouble (System.getProperty ("bench.seconds", "180")) then
                println (s"[probe-limit] wall=$elapsed clock=$clock created=$created exited=$exited active=${created-exited} events=$events moves=$moves stationary=$stationary")
                System.exit (124)
end RuntimeProbe
'@
[IO.File]::WriteAllText("$process/RuntimeProbe.scala", $probe, $utf8)
if ($WorkingTree) {
    Rewrite "$process/Model.scala" '        RuntimeProbe.start ()' "        RuntimeProbe.start ()`n        RuntimeProbe.liveSize = () => if isAnimating then dgAni.liveStateSize else 0"
    $animator = "$dest/src/main/scala/scalation/animation/DgAnimator.scala"
    Rewrite $animator '    private var liveTimer:' "    private var benchmarkCaptured = false`n    private var liveTimer:"
    Rewrite $animator '    end renderLiveSnapshot' "        if !benchmarkCaptured && clock >= 7200 then`n            saveImage (`"target/runtime-investigation/$Label/live.png`")`n            benchmarkCaptured = true`n    end renderLiveSnapshot"
}
[IO.File]::WriteAllText("$dest/revision.txt", (git rev-parse $Revision), $utf8)
Write-Output "Prepared $Label ($Revision): $dest"
