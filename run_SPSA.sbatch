#!/bin/bash
#SBATCH --job-name=SPSA_Cal
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=288:00:00         # 12 days - safe buffer for 60 iterations (120 runs × 2hrs + overhead)
#SBATCH --output=/scratch/krb84578/workDir/scalation/log/spsa/spsa_%j.out
#SBATCH --error=/scratch/krb84578/workDir/scalation/log/spsa/spsa_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== SPSA Calibration started on $(hostname) at $(date) ==="
echo "Job ID: ${SLURM_JOB_ID}"

ml purge
ml Java/21.0.5

cd /scratch/krb84578/scalation

# Create log directory if it doesn't exist
mkdir -p /scratch/krb84578/workDir/scalation/log/spsa

echo "Running SPSA Optimizer with 60 iterations"
echo "Using JAR: target/scala-3.6.3/scalation_3-0.1.0-SNAPSHOT.jar"

date
time java -Xmx28G -cp "target/scala-3.6.3/scalation_3-0.1.0-SNAPSHOT.jar" \
    scalation.simulation.process.runCalibrate_SPSA
date

echo "=== SPSA Calibration completed at $(date) ==="

