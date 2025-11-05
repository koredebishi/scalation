#!/bin/bash
#SBATCH --array=0-1              # 2 optimizers: 0=NelderMead, 1=GeneticAlgorithm
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --time=100:00:00
#SBATCH --output=/scratch/krb84578/workDir/scalation/log/calibrate_%A_%a.out
#SBATCH --error=/scratch/krb84578/workDir/scalation/log/calibrate_%A_%a.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== Array Job ${SLURM_ARRAY_TASK_ID} started on $(hostname) at $(date) ==="

ml purge
ml Java/21.0.5

cd /scratch/krb84578/scalation

# Define which optimizer to run based on array task ID
case ${SLURM_ARRAY_TASK_ID} in
    0)
        OPTIMIZER="runCalibrate_NelderMead"
        ;;
    1)
        OPTIMIZER="runCalibrate_GA"
        ;;
esac

echo "Running optimizer: ${OPTIMIZER}"
echo "Using pre-built JAR from: target/scala-3.6.3/scalation_3-0.1.0-SNAPSHOT.jar"

# Run using the JAR directly (NO SBT - no locks!)
date
time java -Xmx60G -cp "target/scala-3.6.3/scalation_3-0.1.0-SNAPSHO
T.jar" \
    scalation.simulation.process.${OPTIMIZER}
date

echo "=== Array Job ${SLURM_ARRAY_TASK_ID} completed at $(date) ==="


