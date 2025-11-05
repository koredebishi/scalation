#!/bin/bash
#SBATCH --partition=batch
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --time=100:00:00
#SBATCH --output=/scratch/krb84578/workDir/scalation/log/calibrate101DE_%j.out
#SBATCH --error=/scratch/krb84578/workDir/scalation/log/calibrate101DE_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== Job started on $(hostname) at $(date) ==="

ml purge
ml Java/21.0.5

export PATH=$HOME/apps/sbt/bin:$PATH
export XDG_RUNTIME_DIR=/scratch/krb84578/.sbt_runtime   # ✅ safer absolute path
mkdir -p $XDG_RUNTIME_DIR
chmod 700 $XDG_RUNTIME_DIR

# --- Manual copy expected ---
# Run this once before submitting:
#   rsync -av $HOME/scalation/ /scratch/krb84578/workDir/scalation/
# Make sure data, src, and build.sbt exist before job starts.

cd /scratch/krb84578/workDir/scalation
echo "Running from: $(pwd)"
ls -lh

echo "=== Starting runCalibrate_DifferentialEvolution ==="
date
time sbt -Dsbt.server.forcestart=false "runMain scalation.simulation.process.runCalibrate_DifferentialEvolution"
date

echo "=== Job completed successfully at $(date) runCalibrate_DifferentialEvolution==="