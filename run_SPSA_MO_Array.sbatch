#!/bin/bash
#SBATCH --job-name=SPSA_Array
#SBATCH --array=0-4              # 5 independent runs with different random seeds
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=168:00:00         # 7 days - sufficient for 40 iterations
#SBATCH --output=/scratch/krb84578/workDir/scalation/log/spsa_mo_array/run_%A_%a.out
#SBATCH --error=/scratch/krb84578/workDir/scalation/log/spsa_mo_array/run_%A_%a.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== SPSA_MO Array Job ${SLURM_ARRAY_TASK_ID} started on $(hostname) at $(date) ==="
echo "Array Job ID: ${SLURM_ARRAY_JOB_ID}, Task ID: ${SLURM_ARRAY_TASK_ID}"

ml purge
ml Java/21.0.5

cd /scratch/krb84578/scalation

# Create log directory if it doesn't exist
mkdir -p /scratch/krb84578/workDir/scalation/log/spsa_mo_array

echo "Running SPSA_MO Optimizer (Run ${SLURM_ARRAY_TASK_ID}/5) with 40 iterations"
echo "Using JAR: target/scala-3.6.3/scalation_3-0.1.0-SNAPSHOT.jar"

# Each array task uses a different random seed
SEED=$((1000 + SLURM_ARRAY_TASK_ID))
echo "Random seed: ${SEED}"

date
time java -Xmx28G -Drandom.seed=${SEED} \
    -cp "target/scala-3.6.3/scalation_3-0.1.0-SNAPSHOT.jar" \
    scalation.simulation.process.runCalibrate_SPSA_Mo
date

echo "=== SPSA_MO Array Job ${SLURM_ARRAY_TASK_ID} completed at $(date) ==="

