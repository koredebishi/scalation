param([string]$Label = 'current')
$ErrorActionPreference = 'Stop'
$dest = "target/runtime-investigation/$Label"
$launcher = Get-ChildItem "$env:LOCALAPPDATA/Coursier/Cache/arc/https/github.com/sbt/sbt/releases/download" -Filter sbt-launch.jar -Recurse | Select-Object -First 1 -ExpandProperty FullName
if (!$launcher) { throw 'Cached sbt launcher not found' }
# A Java argument file avoids nested Windows batch-file quoting entirely.
$arguments = @(
    '-Xmx4g', '-Dsbt.supershell=false', '-jar',
    ('"' + $launcher.Replace('\', '/') + '"'),
    ('"set target := file(\"' + $dest + '/build\").getAbsoluteFile"'),
    ('"set Compile / scalaSource := file(\"' + $dest + '/src/main/scala\").getAbsoluteFile"'),
    ('"set Compile / classDirectory := file(\"' + $dest + '/classes\").getAbsoluteFile"'),
    'compile'
)
[IO.File]::WriteAllLines((Join-Path (Get-Location) "$dest/sbt.args"), $arguments)
$ErrorActionPreference = 'Continue' # Native stderr warnings must not abort PowerShell.
& java "@$dest/sbt.args" > "$dest/compile.log" 2>&1
exit $LASTEXITCODE
