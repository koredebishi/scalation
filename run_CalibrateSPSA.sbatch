#!/bin/bash
#SBATCH --partition=batch
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16
#SBATCH --mem=64G
#SBATCH --time=240:00:00         # 10 days - safe buffer for SPSA_MO optimization
#SBATCH --output=/scratch/krb84578/workDir/scalation/log/calibrate101SPSA_%j.out
#SBATCH --error=/scratch/krb84578/workDir/scalation/log/calibrate101SPSA_%j.err
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=krb84578@uga.edu

echo "=== Job started on $(hostname) at $(date) ==="

ml purge
ml Java/21.0.5

cd /scratch/krb84578/workDir/scalation
echo "Running from: $(pwd)"
ls -lh

echo "=== Starting runCalibrate_SPSA_Mo (FASTEST optimizer) ==="
echo "Using pre-built JAR to avoid sbt locks and faster startup"
date
time java -Xmx60G -cp "target/scala-3.6.3/scalation_3-0.1.0-SNAPSHOT.jar" \
    scalation.simulation.process.runCalibrate_SPSA_Mo
date

echo "=== Job completed successfully at $(date) ==="
