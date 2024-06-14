#!/bin/bash -l
#SBATCH --output=trio.array_%A_%a.out
#SBATCH --error=trio.array_%A_%a.err
#SBATCH --time=8:00:00
#SBATCH --nodes=1
#SBATCH --array=1-14 #number of PGS in list
#SBATCH --mem-per-cpu=38GB

## Set up job environment:
source /cluster/bin/jobsetup
module purge
module add R/3.5.0

echo "I am job $SLURM_JOBID"

# Use array index to get PGS name from list 
job=$(sed -n "$SLURM_ARRAY_TASK_ID"p listPGS)

Rscript --vanilla 02_a_mimic_models_trio.r $job 

