# Helper: Login to Sapelo2 (UGA GACRC) via SSH
# Usage examples:
#   powershell -ExecutionPolicy Bypass -File .\login_sapelo2.ps1 -User yourid
#   powershell -ExecutionPolicy Bypass -File .\login_sapelo2.ps1 -User yourid -SetupKey
# Environment overrides:
#   $env:SAPELO2_USER (preferred default)
#   $env:SAPELO2_HOST (default: sapelo2.gacrc.uga.edu)

[CmdletBinding()]
param(
    [string] $User = $env:SAPELO2_USER,
    [string] $Host = $(if ($env:SAPELO2_HOST) { $env:SAPELO2_HOST } else { 'sapelo2.gacrc.uga.edu' }),
    [switch] $SetupKey,
    [switch] $AgentForwarding
)

function Require-Command {
    param([string]$Name)
    if (-not (Get-Command $Name -ErrorAction SilentlyContinue)) {
        throw "Required command '$Name' not found. On Windows, install 'OpenSSH Client' optional feature or add it to PATH."
    }
}

try {
    Require-Command ssh
    Require-Command ssh-keygen
} catch {
    Write-Error $_
    Write-Host "To install OpenSSH Client (Admin PowerShell):" -ForegroundColor Yellow
    Write-Host "  Add-WindowsCapability -Online -Name OpenSSH.Client~~~~0.0.1.0" -ForegroundColor Gray
    exit 1
}

if (-not $User) {
    $User = Read-Host "Enter your Sapelo2 username"
}

$Target = "$User@$Host"
$SshDir = Join-Path $env:USERPROFILE ".ssh"
$KeyPath = Join-Path $SshDir "id_ed25519"
$PubKeyPath = "$KeyPath.pub"

if ($SetupKey) {
    if (-not (Test-Path $SshDir)) { New-Item -ItemType Directory -Path $SshDir | Out-Null }
    if (-not (Test-Path $KeyPath)) {
        Write-Host "Generating SSH key (ed25519)..." -ForegroundColor Yellow
        ssh-keygen -t ed25519 -N "" -f "$KeyPath" | Out-Null
    } else {
        Write-Host "Existing SSH key found at $KeyPath" -ForegroundColor Yellow
    }

    Write-Host "Installing public key on remote ($Target)..." -ForegroundColor Yellow
    # Append public key to authorized_keys on remote in a safe manner
    Get-Content -Raw "$PubKeyPath" | ssh -o StrictHostKeyChecking=accept-new "$Target" "mkdir -p ~/.ssh && chmod 700 ~/.ssh && cat >> ~/.ssh/authorized_keys && chmod 600 ~/.ssh/authorized_keys"
    if ($LASTEXITCODE -ne 0) { Write-Error "Failed to install public key on remote."; exit $LASTEXITCODE }
    Write-Host "Public key installed. Future logins should not prompt for a password." -ForegroundColor Green
}

$sshArgs = @('-o','StrictHostKeyChecking=accept-new',$Target)
if ($AgentForwarding) { $sshArgs = @('-A') + $sshArgs }

Write-Host "Opening SSH session: ssh $($sshArgs -join ' ')" -ForegroundColor Green
ssh @sshArgs

