bash_script <- "#!/bin/bash
#SBATCH --account=epadecarb
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --time=1:00:00
#SBATCH --mem=4G

module load containers/singularity

singularity exec --bind /work,/projects /projects/arcsingularity/ood-rstudio141717-basic_4.1.0.sif Rscript /home/amirgazar/Capacity_Expansion/R_Environment.R"

# Saving the BASH file
bash_file <- "R_Environment.sh"
writeLines(bash_script, bash_file)
