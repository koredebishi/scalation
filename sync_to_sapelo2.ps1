# Sync ScalaLation code to sapelo2
# Excludes build artifacts, git history, logs, and IDE files

param(
    [string] $User = $env:SAPELO2_USER,
    [string] $HostName = $(if ($env:SAPELO2_HOST) { $env:SAPELO2_HOST } else { 'sapelo2.gacrc.uga.edu' })
)

if (-not (Get-Command ssh -ErrorAction SilentlyContinue)) {
    Write-Error "OpenSSH client not found on this system. On Windows, install the 'OpenSSH Client' optional feature.";
    exit 1
}
if (-not (Get-Command scp -ErrorAction SilentlyContinue)) {
    Write-Error "scp not found. It is typically included with OpenSSH Client."
    exit 1
}

# Compute SSH target
$Target = if ($User) { "$User@$HostName" } else { $HostName }

Write-Host "Starting sync to sapelo2 ($Target)..." -ForegroundColor Green

# Optionally prime known_hosts to avoid prompt on first connection
Write-Host "Creating remote directories..." -ForegroundColor Yellow
ssh -o StrictHostKeyChecking=accept-new $Target "mkdir -p ~/scalation_2.0/src ~/scalation_2.0/data ~/scalation_2.0/project"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

# Copy source code
Write-Host "Copying source files..." -ForegroundColor Yellow
scp -o StrictHostKeyChecking=accept-new -r "C:\Simulation\scalation_2.0\src" "${Target}:~/scalation_2.0/"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

# Copy data files (excluding large files already in .gitignore)
Write-Host "Copying data files..." -ForegroundColor Yellow
scp -o StrictHostKeyChecking=accept-new -r "C:\Simulation\scalation_2.0\data" "${Target}:~/scalation_2.0/"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

# Copy build files
Write-Host "Copying build configuration..." -ForegroundColor Yellow
scp -o StrictHostKeyChecking=accept-new "C:\Simulation\scalation_2.0\build.sbt" "${Target}:~/scalation_2.0/"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
scp -o StrictHostKeyChecking=accept-new "C:\Simulation\scalation_2.0\build_all.sh" "${Target}:~/scalation_2.0/"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

# Copy project configuration (excluding target)
Write-Host "Copying project configuration..." -ForegroundColor Yellow
scp -o StrictHostKeyChecking=accept-new "C:\Simulation\scalation_2.0\project\build.properties" "${Target}:~/scalation_2.0/project/"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }

# Copy other important files
Write-Host "Copying additional files..." -ForegroundColor Yellow
scp -o StrictHostKeyChecking=accept-new "C:\Simulation\scalation_2.0\runMain" "${Target}:~/scalation_2.0/" 2>$null
# don't exit on optional file failures
scp -o StrictHostKeyChecking=accept-new "C:\Simulation\scalation_2.0\.gitignore" "${Target}:~/scalation_2.0/"
if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
scp -o StrictHostKeyChecking=accept-new "C:\Simulation\scalation_2.0\README.html" "${Target}:~/scalation_2.0/" 2>$null

Write-Host "`nSync completed successfully!" -ForegroundColor Green
Write-Host "Remote location: ${Target}:~/scalation_2.0" -ForegroundColor Cyan
