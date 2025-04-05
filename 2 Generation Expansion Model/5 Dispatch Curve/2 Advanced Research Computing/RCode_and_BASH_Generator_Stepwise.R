# Load necessary library
library(data.table)

# Define paths and parameters
original_R_file <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/GEM_ARC_Stepwise.R" # Full path to the original R file
output_folder <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 RCodes to submit/2 Comprehensive Days" # Full path to the folder where new files will be saved
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/1 Decarbonization Pathways/Hourly_Installed_Capacity.csv"
Hourly_Installed_Capacity <- fread(file_path)
pathways <- unique(Hourly_Installed_Capacity$Pathway) # Extract unique pathways
n_sim <- 3 # Number of iterations per pathway
start_sim <- 16-1 #First random sequence selection (minus 1)
  
# Loop to generate R and Bash files for each pathway and iteration
for (p in seq_along(pathways)) {
  pathway <- pathways[p] # Current pathway
  for (i in 1:n_sim) {
    # ----- Modify R File -----
    # Read the original R file
    content <- readLines(original_R_file)
    
    # Modify the variable for iteration and pathway_id
    content <- gsub(
      pattern = paste0("iteration <- .*"),
      replacement = paste0("iteration <- ", i - 1),
      x = content
    )
    
    content <- gsub(
      pattern = paste0("start_sim <- .*"),
      replacement = paste0("start_sim <- ", i + start_sim),
      x = content
    )
    
    content <- gsub(
      pattern = paste0("pathway_id <- .*"),
      replacement = paste0("pathway_id <- ", p),
      x = content
    )

    # Save the modified R file with pathway-specific and iteration-specific naming
    new_r_file <- file.path(output_folder, paste0("GEM_ARC_Stepwise_pathway_", pathway, "_iteration_", i + start_sim, ".R"))
    writeLines(content, new_r_file)
    
    # ----- Generate Bash Script -----
    # Define bash script content
    bash_script <- paste0("#!/bin/bash
#SBATCH --account=epadecarb
#SBATCH --partition=normal_q
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=50
#SBATCH --cpus-per-task=1  # Reduce the number of cores to avoid OOM
#SBATCH --time=28:00:00    # Set a more appropriate time limit

module load containers/singularity

singularity exec --bind /work,/projects /projects/arcsingularity/ood-rstudio141717-basic_4.1.0.sif Rscript /projects/epadecarb/Decarb_Paper/Stepwise/GEM_ARC_Stepwise_pathway_", pathway, "_iteration_", i + start_sim, ".R

module reset")
    
    # Save the bash script with pathway-specific and iteration-specific naming
    bash_file <- file.path(output_folder, paste0("GEM_Stepwise_pathway_", pathway, "_iteration_", i + start_sim, ".sh"))
    writeLines(bash_script, bash_file)
  }
}
