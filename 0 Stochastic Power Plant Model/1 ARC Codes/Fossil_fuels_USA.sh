#!/bin/bash
##salloc  --account=epadecarb --nodes=1 --ntasks-per-node=1 --cpus-per-task=100
#SBATCH --account=epadecarb
#SBATCH --partition=normal_q
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=50
#SBATCH --time=3-24:00:00    # Set a more appropriate time limit

module load containers/singularity

singularity exec --bind /work,/projects /projects/arcsingularity/ood-rstudio141717-basic_4.1.0.sif Rscript /home/amirgazar/Fossil_Fuels/Fossil_Fuels_ARC_USA.R

module reset
