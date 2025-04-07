#!/bin/bash
#SBATCH --account=epadecarb
#SBATCH --partition=normal_q
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=80
#SBATCH --cpus-per-task=1  # Reduce the number of cores to avoid OOM
#SBATCH --time=24:00:00    # Set a more appropriate time limit

module load containers/singularity

singularity exec --bind /projects /projects/arcsingularity/ood-rstudio141717-basic_4.1.0.sif Rscript "/projects/epadecarb/2 Generation Expansion Model/2 R Codes/1 Comp Days/Dispatch_Curve_iteration_1150.R"

module reset
