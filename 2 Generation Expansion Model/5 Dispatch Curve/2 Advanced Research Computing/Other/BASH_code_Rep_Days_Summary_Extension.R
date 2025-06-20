bash_script <- "#!/bin/bash
#SBATCH --account=epadecarb
#SBATCH --partition=normal_q
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=50
#SBATCH --cpus-per-task=1  # Reduce the number of cores to avoid OOM
#SBATCH --time=24:00:00    # Set a more appropriate time limit

module load containers/singularity

singularity exec --bind /work,/projects /projects/arcsingularity/ood-rstudio141717-basic_4.1.0.sif Rscript /projects/epadecarb/Decarb_Paper/GEM_ARC_Rep_Days_Expansion_Summary.R

module reset"

# Saving the BASH file
bash_file <- "GEM_Rep_Days_Expansion_Summary.sh"
writeLines(bash_script, bash_file)
