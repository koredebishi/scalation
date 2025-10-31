#!/bin/bash
#SBATCH --job-name=calibrate_array_%a
#SBATCH --partition=highmem_p
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --array=0-2                    # 3 jobs: 0=SPSA, 1=NelderMead, 2=GA
#SBATCH --cpus-per-task=4              # Each optimizer gets 4 CPUs
#SBATCH --mem=20G                      # Each gets 20GB RAM
#SBATCH --time=144:00:00               # 120 hours max per job
#SBATCH --output=/home/krb84578/scalation_2.0/log/calibrate_%a_%j.out
#SBATCH --error=/home/krb84578/scalation_2.0/log/calibrate_%a_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== Job Array Task $SLURM_ARRAY_TASK_ID started on $(hostname) at $(date) ==="
echo "Job ID: $SLURM_JOB_ID"
echo "Array Task ID: $SLURM_ARRAY_TASK_ID"

# Load modules
ml purge
ml Java/21.0.5

# Setup SBT environment
export PATH=$HOME/apps/sbt/bin:$PATH
export XDG_RUNTIME_DIR=$HOME/.sbt_runtime
mkdir -p $XDG_RUNTIME_DIR
chmod 700 $XDG_RUNTIME_DIR

# Navigate to project directory
cd /home/krb84578/scalation_2.0
echo "Running from: $(pwd)"

# Define optimizer names
OPTIMIZERS=("SPSA" "NelderMead" "GA")
OPTIMIZER=${OPTIMIZERS[$SLURM_ARRAY_TASK_ID]}

echo "=== Running Optimizer: $OPTIMIZER ==="
echo "=== Start time: $(date) ==="

# Run the specific optimizer
time sbt -Dsbt.server.forcestart=false "runMain scalation.simulation.process.runCalibrate_${OPTIMIZER}"

echo "=== End time: $(date) ==="
echo "=== Job Array Task $SLURM_ARRAY_TASK_ID completed successfully ==="

