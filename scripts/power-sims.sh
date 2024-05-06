#!/bin/bash -l
#SBATCH --output=sims_%A_%a.out
#SBATCH --error=sims_%A_%a.err
#SBATCH --time=6:00:00
#SBATCH --cpus-per-task=32
#SBATCH --ntasks=1
#SBATCH --nodes=1
#SBATCH --array=1
#SBATCH --mem-per-cpu=1GB

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1

module add r/4.1.1-gcc-9.4.0-withx-rmath-standalone-python-3.8.12

Rscript --vanilla power-sims.r

