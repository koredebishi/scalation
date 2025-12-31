#!/bin/bash
#SBATCH --job-name=SPSA_Cal
#SBATCH --ntasks=1
#SBATCH --partition=batch
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=168:00:00
#SBATCH --output=/scratch/krb84578/workDir/scalation_2.0/log/spsa/spsa_%j.out
#SBATCH --error=/scratch/krb84578/workDir/scalation_2.0/log/spsa/spsa_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== SPSA Calibration started on $(hostname) at $(date) ==="
echo "Job ID: ${SLURM_JOB_ID}"

ml purge
ml Java/21.0.5

cd /scratch/krb84578/workDir/scalation_2.0 || exit 1

mkdir -p /scratch/krb84578/workDir/scalation_2.0/log/spsa

echo "Running SPSA Optimizer"

# Use Coursier cache - verified paths
SCALA3_LIB="/home/krb84578/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.7.4/scala3-library_3-3.7.4.jar"
SCALA_LIB="/home/krb84578/.cache/coursier/v1/https/repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar"

# Verify JARs exist
if [ ! -f "$SCALA3_LIB" ]; then echo "ERROR: scala3-library not found at $SCALA3_LIB"; exit 1; fi
if [ ! -f "$SCALA_LIB" ]; then echo "ERROR: scala-library not found at $SCALA_LIB"; exit 1; fi

CP="target/scala-3.7.4/scalation_3-0.1.0-SNAPSHOT.jar:$SCALA3_LIB:$SCALA_LIB"

date
time java -Xmx28G -cp "$CP" scalation.simulation.process.runCalibrate_SPSA
date

echo "=== SPSA Calibration completed at $(date) ==="



