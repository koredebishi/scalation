#!/bin/bash
# Pre-compile the project ONCE before submitting array jobs
# Run this FIRST on the login node or as a separate job

echo "=== Pre-compilation script started at $(date) ==="

ml purge
ml Java/21.0.5

export PATH=$HOME/apps/sbt/bin:$PATH
export XDG_RUNTIME_DIR=$SCRATCH/.sbt_runtime
mkdir -p $XDG_RUNTIME_DIR
chmod 700 $XDG_RUNTIME_DIR

cd /scratch/krb84578/scalation

echo "Compiling project..."
sbt clean compile package

echo "JAR built at: target/scala-3.6.3/scalation_3-0.1.0-SNAPSHOT.jar"
ls -lh target/scala-3.6.3/*.jar

echo "=== Pre-compilation completed at $(date) ==="
echo ""
echo "Now you can submit the array job:"
echo "  sbatch run_CalibrationArray.sbatch"

