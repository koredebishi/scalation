#!/bin/bash
#SBATCH --job-name=ODE_Experiments
#SBATCH --ntasks=1
#SBATCH --partition=batch
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=168:00:00
#SBATCH --output=/scratch/krb84578/workDir/scalation_experiments/log/experiments/experiments_%j.out
#SBATCH --error=/scratch/krb84578/workDir/scalation_experiments/log/experiments/experiments_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== ODE Integrator Experiments started on $(hostname) at $(date) ==="
echo "Job ID: ${SLURM_JOB_ID}"

ml purge
ml Java/21.0.5

cd /scratch/krb84578/workDir/scalation_experiments || exit 1

# Create experiments directory for output files
mkdir -p /scratch/krb84578/workDir/scalation_experiments/log/experiments

echo "Running All Experiments: 5 integrators × 2 arrival processes = 10 runs"
echo "Integrators: DOPRI5, RK4, RK3, RK2, Ballistic"
echo "Arrivals: Erlang2S, Poisson"

# Use Coursier cache - verified paths
SCALA3_LIB="/home/krb84578/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.7.4/scala3-library_3-3.7.4.jar"
SCALA_LIB="/home/krb84578/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar"

# Verify JARs exist
if [ ! -f "$SCALA3_LIB" ]; then echo "ERROR: scala3-library not found at $SCALA3_LIB"; exit 1; fi
if [ ! -f "$SCALA_LIB" ]; then echo "ERROR: scala-library not found at $SCALA_LIB"; exit 1; fi

CP="target/scala-3.7.4/scalation_3-0.1.0-SNAPSHOT.jar:$SCALA3_LIB:$SCALA_LIB"

date
echo "Starting experiments..."
time java -Xmx28G -cp "$CP" scalation.simulation.process.runAllExperiments
date

echo "=== All Experiments completed at $(date) ==="
echo "Results saved to: log/experiments/"
echo "  - experiment_summary.txt (comparison table)"
echo "  - <arrival>_<integrator>_data.csv (raw simulation data)"
echo "  - <arrival>_<integrator>_fitness.txt (fitness metrics)"

