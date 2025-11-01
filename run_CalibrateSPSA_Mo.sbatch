#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --time=48:00:00
#SBATCH --output=/scratch/krb84578/scalation_2.0/log/calibrate101_%j.out
#SBATCH --error=/scratch/krb84578/scalation_2.0/log/calibrate101_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== Job started on $(hostname) at $(date) ==="

ml purge
ml Java/21.0.5

export PATH=$HOME/apps/sbt/bin:$PATH
export XDG_RUNTIME_DIR=$SCRATCH/.sbt_runtime
mkdir -p $XDG_RUNTIME_DIR
chmod 700 $XDG_RUNTIME_DIR

# --- Manual copy expected ---
# You should manually run this once before submitting:
#   cp -r $HOME/scalation_2.0 $SCRATCH/
# or better:
#   rsync -av $HOME/scalation_2.0/ $SCRATCH/scalation_2.0/
# Make sure data and sbt build files are in /scratch/krb84578/scalation_2.0 before job starts.

cd /scratch/krb84578/scalation_2.0
echo "Running from: $(pwd)"
ls -lh

echo "=== Starting runCalibrateCalRoute101_OptimizerCompare ==="
date
time sbt -Dsbt.server.forcestart=false "runMain scalation.simulation.process.runCalibrate_NelderMead"
date

echo "=== Job completed successfully at $(date) ==="
