#!/bin/bash
#SBATCH --job-name=ap4_validate
#SBATCH --account=epadecarb
#SBATCH --partition=normal_q
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=8G
#SBATCH --time=01:00:00
#SBATCH --output=AP4_Investigation_2026-09-20/slurm-%j.log

set -euo pipefail
module load MATLAB
module list
matlab -batch "run('START_AP4_ARC.m')"
