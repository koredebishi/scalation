param([string]$Revision = '4176c1642', [string]$Label = 'current')
$ErrorActionPreference = 'Stop'
$root = (Get-Location).Path
$dest = Join-Path $root "target/runtime-investigation/$Label"
New-Item -ItemType Directory -Force -Path $dest | Out-Null
git archive --format=zip "--output=$dest/source.zip" $Revision src/main/scala
if ($LASTEXITCODE -ne 0) { throw 'git archive failed' }
Expand-Archive -LiteralPath "$dest/source.zip" -DestinationPath $dest -Force
$process = "$dest/src/main/scala/scalation/simulation/process"
$utf8 = New-Object System.Text.UTF8Encoding($false)
function Rewrite([string]$path, [string]$before, [string]$after) {
    $body = [IO.File]::ReadAllText($path)
    if (!$body.Contains($before)) { throw "Missing instrumentation anchor: $path : $before" }
    [IO.File]::WriteAllText($path, $body.Replace($before, $after), $utf8)
}
Rewrite "$process/model/CalRoute101_3.scala" 'animating: Boolean = true' 'animating: Boolean = false'
Rewrite "$process/Model.scala" '        while simulating && !agenda.isEmpty do' "        RuntimeProbe.start ()`n        while simulating && !agenda.isEmpty do"
Rewrite "$process/Model.scala" '            _theActor = agenda.dequeue()' "            RuntimeProbe.event (clock, numActors)`n            _theActor = agenda.dequeue()"
Rewrite "$process/Model.scala" '        hasFinished()' "        RuntimeProbe.finish (clock, numActors)`n        hasFinished()"
Rewrite "$process/Sink.scala" '        val ctime = director.clock' "        RuntimeProbe.exited += 1`n        val ctime = director.clock"
Rewrite "$process/VTransport.scala" '            motion.updateV(actor, length)' "            val probeBefore = actor.disp`n            motion.updateV(actor, length)"
Rewrite "$process/VTransport.scala" '            val cp = calcPoint2(actor.disp)' "            RuntimeProbe.moves += 1`n            if actor.disp <= probeBefore then RuntimeProbe.stationary += 1`n            val cp = calcPoint2(actor.disp)"
$probe = @'
package scalation.simulation.process

/** Identical measurement overlay for historical snapshots; never changes traffic state. */
object RuntimeProbe:
    var exited = 0L
    var events = 0L
    var moves = 0L
    var stationary = 0L
    private var started = 0L
    private var last = 0L
    def start (): Unit =
        started = System.nanoTime ()
        last = started
    def finish (clock: Double, created: Int): Unit =
        val elapsed = (System.nanoTime () - started) / 1e9
        println (s"[probe-complete] wall=$elapsed clock=$clock created=$created exited=$exited active=${created-exited} events=$events moves=$moves stationary=$stationary")
    def event (clock: Double, created: Int): Unit =
        events += 1
        if events % 10000 == 0 then
            val now = System.nanoTime ()
            val elapsed = (now - started) / 1e9
            if now - last >= 10000000000L then
                println (s"[probe] wall=$elapsed clock=$clock created=$created exited=$exited active=${created-exited} events=$events moves=$moves stationary=$stationary")
                last = now
            if elapsed >= java.lang.Double.parseDouble (System.getProperty ("bench.seconds", "180")) then
                println (s"[probe-limit] wall=$elapsed clock=$clock created=$created exited=$exited active=${created-exited} events=$events moves=$moves stationary=$stationary")
                System.exit (124)
end RuntimeProbe
'@
[IO.File]::WriteAllText("$process/RuntimeProbe.scala", $probe, $utf8)
[IO.File]::WriteAllText("$dest/revision.txt", (git rev-parse $Revision), $utf8)
Write-Output "Prepared $Label ($Revision): $dest"
