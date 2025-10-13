# Sync ScalaLation code to sapelo2
# Excludes build artifacts, git history, logs, and IDE files

Write-Host "Starting sync to sapelo2..." -ForegroundColor Green

# Create remote directory structure
Write-Host "Creating remote directories..." -ForegroundColor Yellow
ssh sapelo2 "mkdir -p ~/scalation_2.0/src ~/scalation_2.0/data ~/scalation_2.0/project"

# Copy source code
Write-Host "Copying source files..." -ForegroundColor Yellow
scp -r C:\Simulation\scalation_2.0\src sapelo2:~/scalation_2.0/

# Copy data files (excluding large files already in .gitignore)
Write-Host "Copying data files..." -ForegroundColor Yellow
scp -r C:\Simulation\scalation_2.0\data sapelo2:~/scalation_2.0/

# Copy build files
Write-Host "Copying build configuration..." -ForegroundColor Yellow
scp C:\Simulation\scalation_2.0\build.sbt sapelo2:~/scalation_2.0/
scp C:\Simulation\scalation_2.0\build_all.sh sapelo2:~/scalation_2.0/

# Copy project configuration (excluding target)
Write-Host "Copying project configuration..." -ForegroundColor Yellow
scp C:\Simulation\scalation_2.0\project\build.properties sapelo2:~/scalation_2.0/project/

# Copy other important files
Write-Host "Copying additional files..." -ForegroundColor Yellow
scp C:\Simulation\scalation_2.0\runMain sapelo2:~/scalation_2.0/
scp C:\Simulation\scalation_2.0\.gitignore sapelo2:~/scalation_2.0/
scp C:\Simulation\scalation_2.0\README.html sapelo2:~/scalation_2.0/

Write-Host "`nSync completed successfully!" -ForegroundColor Green
Write-Host "Remote location: sapelo2:~/scalation_2.0" -ForegroundColor Cyan

