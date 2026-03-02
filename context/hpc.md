# HPC - Sapelo2

## Directories

| Purpose | Path |
|---------|------|
| Home (git) | `/home/krb84578/scalation_2.0` |
| SPSA runner | `/scratch/krb84578/workDir/scalation_2.0` |
| Nelder-Mead/MO runner | `/scratch/krb84578/workDir/scalation_MO` |

## Common Commands

### Load Environment
```bash
module load Java/21.0.5
```

### Compile
```bash
cd /scratch/krb84578/workDir/scalation_2.0
sbt clean package
```

### Submit Jobs
```bash
sbatch run_SPSA.sbatch
sbatch run_NelderMead.sbatch
```

### Monitor Jobs
```bash
# Watch output
tail -f log/spsa/spsa_*.out
tail -f log/nm/nm_*.out

# Check queue
squeue -u krb84578

# Kill job
scancel <job_id>
```

## Job Configuration

- **Cluster:** Sapelo2
- **Wall time:** 30 days (for optimization runs)
- **Experiment matrix:** 3 CF models × 2-3 optimizers × N runs

