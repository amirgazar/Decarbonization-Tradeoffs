# Define paths and parameters
original_R_file <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/GEM_ARC_Rep_Days.R" # Full path to the original R file
output_folder <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 RCodes to submit/1 Representative Days" # Full path to the folder where new files will be saved
n_sim <- 6*6 # Number of simulations (number of R and bash files to generate)
variable_name <- "iteration" # Variable to modify in the R file

# Loop to generate R and bash files
for (i in 1:n_sim) {
  # ----- Modify R File -----
  # Read the original R file
  content <- readLines(original_R_file)
  # Modify the variable
  content <- gsub(
    pattern = paste0(variable_name, " <- .*"),
    replacement = paste0(variable_name, " <- ", i - 1),
    x = content
  )
  # Save the modified R file
  new_r_file <- file.path(output_folder, paste0("GEM_ARC_Rep_Days_", i, ".R"))
  writeLines(content, new_r_file)
  
  # ----- Generate Bash Script -----
  # Define bash script content
  bash_script <- paste0("#!/bin/bash
#SBATCH --account=epadecarb
#SBATCH --partition=normal_q
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=50
#SBATCH --cpus-per-task=1  # Reduce the number of cores to avoid OOM
#SBATCH --time=24:00:00    # Set a more appropriate time limit

module load containers/singularity

singularity exec --bind /work,/projects /projects/arcsingularity/ood-rstudio141717-basic_4.1.0.sif Rscript /projects/epadecarb/Decarb_Paper/Rep_Days/GEM_ARC_Rep_Days_", i, ".R

module reset")
  
  # Save the bash script
  bash_file <- file.path(output_folder, paste0("GEM_Rep_Days_", i, ".sh"))
  writeLines(bash_script, bash_file)
}
