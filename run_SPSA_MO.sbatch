#!/bin/bash
#SBATCH --job-name=SPSA_Mo_Cal
#SBATCH --ntasks=1
#SBATCH --partition=batch
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=168:00:00
#SBATCH --output=/scratch/krb84578/workDir/scalation_2.0/log/spsa_mo/spsa_mo_%j.out
#SBATCH --error=/scratch/krb84578/workDir/scalation_2.0/log/spsa_mo/spsa_mo_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== SPSA_Mo Calibration started on $(hostname) at $(date) ==="
echo "Job ID: ${SLURM_JOB_ID}"

ml purge
ml Java/21.0.5

cd /scratch/krb84578/workDir/scalation_2.0 || exit 1

# Create log directory if it doesn't exist
mkdir -p /scratch/krb84578/workDir/scalation_2.0/log/spsa_mo

echo "Running SPSA_Mo Optimizer with 20 iterations"

# Set temp directory to avoid /run/user permission issues
export TMPDIR=/scratch/krb84578/workDir/scalation_2.0/tmp
mkdir -p $TMPDIR

date
time sbt -J-Xmx28G "runMain scalation.simulation.process.runCalibrate_SPSA_Mo"
date

echo "=== SPSA_Mo Calibration completed at $(date) ==="
