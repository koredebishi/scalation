#!/bin/bash
#SBATCH --job-name=SPSA_Mo_Cal
#SBATCH --ntasks=1
#SBATCH --partition=batch
#SBATCH --cpus-per-task=8
#SBATCH --mem=32G
#SBATCH --time=168:00:00
#SBATCH --output=/scratch/krb84578/workDir/scalation_MO/log/spsa_mo/spsa_mo_%j.out
#SBATCH --error=/scratch/krb84578/workDir/scalation_MO/log/spsa_mo/spsa_mo_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== SPSA_Mo Calibration started on $(hostname) at $(date) ==="
echo "Job ID: ${SLURM_JOB_ID}"

ml purge
ml Java/21.0.5

cd /scratch/krb84578/workDir/scalation_MO || exit 1

# Create log directory if it doesn't exist
mkdir -p /scratch/krb84578/workDir/scalation_MO/log/spsa_mo

echo "Running SPSA_Mo Optimizer with 20 iterations"

# Ensure JAR is compiled
if [ ! -f "target/scala-3.7.4/scalation_3-0.1.0-SNAPSHOT.jar" ]; then
    echo "ERROR: JAR not found. Compile first on login node with: sbt package"
    exit 1
fi

# Download Scala 3 library if not cached
SCALA_LIB_DIR="/scratch/krb84578/workDir/scala-libs"
mkdir -p $SCALA_LIB_DIR

SCALA3_LIB="$SCALA_LIB_DIR/scala3-library_3-3.7.4.jar"
SCALA_LIB="$SCALA_LIB_DIR/scala-library-2.13.16.jar"

if [ ! -f "$SCALA3_LIB" ]; then
    echo "Downloading Scala 3 library..."
    curl -sL -o "$SCALA3_LIB" "https://repo1.maven.org/maven2/org/scala-lang/scala3-library_3/3.7.4/scala3-library_3-3.7.4.jar"
fi

if [ ! -f "$SCALA_LIB" ]; then
    echo "Downloading Scala 2.13 library..."
    curl -sL -o "$SCALA_LIB" "https://repo1.maven.org/maven2/org/scala-lang/scala-library/2.13.16/scala-library-2.13.16.jar"
fi

# Build classpath
CP="target/scala-3.7.4/scalation_3-0.1.0-SNAPSHOT.jar:$SCALA3_LIB:$SCALA_LIB"

date
echo "Classpath: $CP"
time java -Xmx28G -cp "$CP" scalation.simulation.process.runCalibrate_SPSA_Mo
date

echo "=== SPSA_Mo Calibration completed at $(date) ==="
