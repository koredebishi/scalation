# sync_to_sapelo.ps1 - Sync local code to Sapelo HPC from Windows
#
# Usage:
#   .\sync_to_sapelo.ps1
#
# Requires: OpenSSH client (built into Windows 10+) or PuTTY/pscp
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Configuration
$REMOTE_USER = "krb84578"
$REMOTE_HOST = "sapelo2.gacrc.uga.edu"  # Login node (use xfer.gacrc.uga.edu for large transfers)
$REMOTE_PATH = "/scratch/krb84578/workDir/scalation_2.0"
$LOCAL_PATH = "C:\Simulation\scalation_2.0"

Write-Host "======================================" -ForegroundColor Green
Write-Host "  Syncing scalation_2.0 to Sapelo    " -ForegroundColor Green
Write-Host "======================================" -ForegroundColor Green
Write-Host ""

# Key files to sync (source code only, no build artifacts)
$SYNC_ITEMS = @(
    "src",
    "project/plugins.sbt",
    "project/build.properties",
    "build.sbt",
    "data",
    "context",
    "run_CalibrationArray.sbatch",
    "run_CalibrateSPSA.sbatch",
    "CLAUDE.md"
)

Write-Host "Files/directories to sync:" -ForegroundColor Yellow
foreach ($item in $SYNC_ITEMS) {
    Write-Host "  + $item"
}
Write-Host ""

# Check if scp is available
$scpPath = Get-Command scp -ErrorAction SilentlyContinue
if (-not $scpPath) {
    Write-Host "ERROR: scp not found. Please install OpenSSH or use Git Bash." -ForegroundColor Red
    exit 1
}

Write-Host "Using scp to transfer files..." -ForegroundColor Yellow
Write-Host "You may be prompted for your password multiple times." -ForegroundColor Yellow
Write-Host ""

# Create remote directory structure if needed
Write-Host "Ensuring remote directories exist..." -ForegroundColor Cyan
ssh "${REMOTE_USER}@${REMOTE_HOST}" "mkdir -p ${REMOTE_PATH}/project ${REMOTE_PATH}/src ${REMOTE_PATH}/data ${REMOTE_PATH}/context"

# Sync each item
foreach ($item in $SYNC_ITEMS) {
    $localItem = Join-Path $LOCAL_PATH $item
    if (Test-Path $localItem) {
        Write-Host "Syncing: $item" -ForegroundColor Cyan

        if (Test-Path $localItem -PathType Container) {
            # It's a directory - use -r flag
            scp -r "$localItem" "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}/"
        } else {
            # It's a file
            $remotePath = Split-Path $item -Parent
            if ($remotePath) {
                scp "$localItem" "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}/${remotePath}/"
            } else {
                scp "$localItem" "${REMOTE_USER}@${REMOTE_HOST}:${REMOTE_PATH}/"
            }
        }
    } else {
        Write-Host "  Skipping (not found): $item" -ForegroundColor Yellow
    }
}

Write-Host ""
Write-Host "======================================" -ForegroundColor Green
Write-Host "  Sync complete!                     " -ForegroundColor Green
Write-Host "======================================" -ForegroundColor Green
Write-Host ""
Write-Host "Next steps on Sapelo:" -ForegroundColor Yellow
Write-Host "  1. ssh ${REMOTE_USER}@sapelo2.gacrc.uga.edu"
Write-Host "  2. cd ${REMOTE_PATH}"
Write-Host "  3. ml Java/21.0.5"
Write-Host "  4. sbt assembly"
Write-Host "  5. sbatch run_CalibrationArray.sbatch"

