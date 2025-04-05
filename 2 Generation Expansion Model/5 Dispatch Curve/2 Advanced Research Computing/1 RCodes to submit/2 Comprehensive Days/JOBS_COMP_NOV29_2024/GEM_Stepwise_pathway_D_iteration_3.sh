#!/bin/bash
#SBATCH --account=epadecarb
#SBATCH --partition=normal_q
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=50
#SBATCH --cpus-per-task=1  # Reduce the number of cores to avoid OOM
#SBATCH --time=28:00:00    # Set a more appropriate time limit

module load containers/singularity

singularity exec --bind /work,/projects /projects/arcsingularity/ood-rstudio141717-basic_4.1.0.sif Rscript /home/amirgazar/Paper2_Nov_2024/Stepwise/GEM_ARC_Stepwise_pathway_D_iteration_3.R

module reset
