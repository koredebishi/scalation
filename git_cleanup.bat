@echo off
cd /d C:\Simulation\scalation_2.0

echo === Checking Git status ===
git status

echo.
echo === Removing target/ from Git index (keeping files on disk) ===
git rm -r --cached target/
git rm -r --cached project/target/

echo.
echo === Adding .gitignore changes ===
git add .gitignore

echo.
echo === Committing changes ===
git commit -m "Remove target/ files from Git tracking and fix .gitignore encoding"

echo.
echo === Pushing to GitHub ===
git push

echo.
echo === Done! ===
pause

