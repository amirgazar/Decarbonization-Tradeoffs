# Load necessary library
library(data.table)

# Define paths and parameters
original_R_file <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/2 Dispatch Curve.R" # Full path to the original R file
output_folder <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 RCodes to submit/1 Comp Days" # Full path to the folder where new files will be saved
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/1 Decarbonization Pathways/Hourly_Installed_Capacity.csv"
Hourly_Installed_Capacity <- fread(file_path)
pathways <- unique(Hourly_Installed_Capacity$Pathway) # Extract unique pathways
n_sim <- 5 # Number of iterations per pathway

# Loop to generate R and Bash files for each pathway and iteration
for (p in seq_along(pathways)) {
  pathway <- pathways[p] # Current pathway
  for (i in 1:n_sim) {
    # ----- Modify R File -----
    # Read the original R file
    content <- readLines(original_R_file)
    
    # Modify the variable for iteration and pathway_id
    content <- gsub(
      pattern = paste0("simulation <- .*"),
      replacement = paste0("simulation <- ", i),
      x = content
    )
    
    content <- gsub(
      pattern = paste0("pathway <- .*"),
      replacement = paste0("pathway <- ", p),
      x = content
    )

    # Save the modified R file with pathway-specific and iteration-specific naming
    new_r_file <- file.path(output_folder, paste0("Dispatch_Curve_", pathway, "_sim_", i, ".R"))
    writeLines(content, new_r_file)
    
    # ----- Generate Bash Script -----
    # Define bash script content
    bash_script <- paste0("#!/bin/bash
#SBATCH --account=epadecarb
#SBATCH --partition=normal_q
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=80
#SBATCH --cpus-per-task=1  # Reduce the number of cores to avoid OOM
#SBATCH --time=2:00:00    # Set a more appropriate time limit

module load containers/singularity

singularity exec --bind /projects /projects/arcsingularity/ood-rstudio141717-basic_4.1.0.sif Rscript \"/projects/epadecarb/2 Generation Expansion Model/2 R Codes/2 Dispatch Curve.R\"

module reset")
    
    # Save the bash script with pathway-specific and iteration-specific naming
    bash_file <- file.path(output_folder, paste0("Dispatch_Curve_", pathway, "_sim_", i, ".sh"))
    writeLines(bash_script, bash_file)
  }
}
