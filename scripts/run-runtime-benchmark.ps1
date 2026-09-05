param([string]$Label = 'current', [int]$Seconds = 180, [switch]$Animated, [string]$RunName = 'run')
$ErrorActionPreference = 'Stop'
$dest = "target/runtime-investigation/$Label"
$dependencies = Get-Content benchmark-classpath.log | ForEach-Object {
    if ($_ -match 'Attributed\((.+\.jar)\)') { $Matches[1] }
}
if (!$dependencies) { throw 'Missing dependency list in benchmark-classpath.log; obtain with sbt "show Runtime / fullClasspath"' }
$classes = (Resolve-Path "$dest/classes").Path
$classpath = (@($classes) + @($dependencies)) -join ';'
$headless = (!$Animated).ToString().ToLowerInvariant()
$animate = $Animated.IsPresent.ToString().ToLowerInvariant()
$arguments = @('-Xmx4g', "-Djava.awt.headless=$headless", "-Dbench.animate=$animate", "-Dbench.seconds=$Seconds", '-cp',
    ('"' + $classpath.Replace('\', '/') + '"'), 'scripts/RuntimeBenchmark.java')
[IO.File]::WriteAllLines((Join-Path (Get-Location) "$dest/$RunName.args"), $arguments)
$ErrorActionPreference = 'Continue'
& java "@$dest/$RunName.args" > "$dest/$RunName.log" 2>&1
$result = $LASTEXITCODE
[IO.File]::WriteAllText((Join-Path (Get-Location) "$dest/$RunName-exit-code.txt"), "$result")
Write-Output "$Label exit=$result (124 means the measurement time limit)"
exit $result
