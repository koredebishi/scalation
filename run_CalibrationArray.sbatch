#!/bin/bash
#SBATCH --job-name=calibration_array
#SBATCH --partition=batch
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=168:00:00
#SBATCH --array=1-10
#SBATCH --output=/scratch/krb84578/workDir/scalation_2.0/log/calibration_%A_%a.out
#SBATCH --error=/scratch/krb84578/workDir/scalation_2.0/log/calibration_%A_%a.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# CALIBRATION ARRAY JOB (v3 - Feb 2026)
# MACRO-only: 10 jobs (2 models x 5 optimizers), all using corridor-level fitness
# Micro runs dropped - macro is aligned with ANNSIM 2026 paper baseline
#
# Array Index Mapping:
#   1-5:   IDM   + (SPSA, SPSA_Mo, NelderMead, GA, DE) + MACRO fitness
#   6-10:  Gipps + (SPSA, SPSA_Mo, NelderMead, GA, DE) + MACRO fitness
#
# Fitness: 0.2 * flowNRMSE + 0.8 * speedNRMSE + flowPenalty(threshold=2.5)
# Bounds:  tight, centered on ANNSIM validated params (s=5, a=4, b=-2, T=3, tau=0.5)
# GA:      boundedFunc wrapper ensures mutation cannot escape bounds
#
# Expected runtimes:
#   SPSA, SPSA_Mo, NelderMead: ~10-15 hrs (100 iter)
#   GA, DE: ~24 hrs (40 gen x 20 pop = 800 evals)
#
# Usage:
#   1. Build fat JAR once:  sbt assembly
#   2. Submit array job:    sbatch run_CalibrationArray.sbatch
#   3. Monitor:             squeue -u $USER
#   4. Check logs:          tail -f log/calibration_*_*.out
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

echo "========================================"
echo "CALIBRATION ARRAY JOB (v3 - Feb 2026, MACRO only)"
echo "========================================"
echo "Job ID:        $SLURM_JOB_ID"
echo "Array Task ID: $SLURM_ARRAY_TASK_ID"
echo "Host:          $(hostname)"
echo "Started:       $(date)"
echo "========================================"

# Decode task ID to model/optimizer for logging
# All jobs are MACRO fitness (jobs 1-10 only)
MODELS=("IDM" "Gipps")
OPTIMIZERS=("SPSA" "SPSA_Mo" "NelderMead" "GA" "DE")

WITHIN_MODE=$(( ($SLURM_ARRAY_TASK_ID - 1) % 10 ))
MODEL_IDX=$(( $WITHIN_MODE / 5 ))
OPT_IDX=$(( $WITHIN_MODE % 5 ))

echo "Fitness Mode:  MACRO"
echo "Model:         ${MODELS[$MODEL_IDX]}"
echo "Optimizer:     ${OPTIMIZERS[$OPT_IDX]}"
echo "========================================"

# Load Java 21
ml purge
ml Java/21.0.5

# Navigate to project directory
cd /scratch/krb84578/workDir/scalation_2.0
echo "Working directory: $(pwd)"

# Check for fat JAR
JAR_PATH="target/scala-3.7.4/scalation-assembly.jar"
if [ ! -f "$JAR_PATH" ]; then
    echo "ERROR: Fat JAR not found at $JAR_PATH"
    echo "Please run 'sbt assembly' first on the login node."
    exit 1
fi

echo "Using JAR: $JAR_PATH"
echo "JAR size:  $(ls -lh $JAR_PATH | awk '{print $5}')"
echo ""

# Run calibration - SLURM_ARRAY_TASK_ID is automatically passed via environment
echo "=== Starting Calibration ==="
date
time java -Xmx28G -jar "$JAR_PATH"
EXIT_CODE=$?
date

echo ""
echo "========================================"
echo "Job $SLURM_ARRAY_TASK_ID (${MODELS[$MODEL_IDX]} + ${OPTIMIZERS[$OPT_IDX]} + MACRO) completed"
echo "Exit code: $EXIT_CODE"
echo "Finished:  $(date)"
echo "========================================"

exit $EXIT_CODE
