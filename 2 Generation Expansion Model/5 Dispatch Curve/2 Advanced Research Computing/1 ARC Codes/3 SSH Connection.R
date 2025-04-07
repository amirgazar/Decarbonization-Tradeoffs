# Install/Import SSH library 
library(ssh)

setwd("/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing")

host <- "tinkercliffs2.arc.vt.edu"
username <- "amirgazar" # VT PID
password <- "V?i7Bj!-a3AEZHP"  # VT.edu Password

session <- ssh::ssh_connect(paste0(username, "@", host), passwd = password) # Make sure you authenticate DUO Security

#---- Model Updated April 6 2025
#---- Simple Code 
# Load bash script
bash_file <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/BASH_File.sh"
# Upload the Bash script to the remote server
ssh::scp_upload(session, bash_file, to = "/projects/epadecarb/2 Generation Expansion Model/2 R Codes")

# Load R script
R_file <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/2 Dispatch Curve.R"
ssh::scp_upload(session, R_file, to = "/projects/epadecarb/2 Generation Expansion Model/2 R Codes/")

# Define the command to submit the SLURM job + Change directory
commands <- 'cd "/projects/epadecarb/2 Generation Expansion Model/2 R Codes/" && sbatch -A epadecarb BASH_File.sh'
# Execute the command
ssh::ssh_exec_wait(session, command = commands)


#---- Batch processing
# Uploading Rcode
Rcode_folder <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 RCodes to submit/1 Comp Days" # Path to local folder containing R files
remote_folder <- "/projects/epadecarb/2 Generation Expansion Model/2 R Codes/1 Comp Days/" # Remote folder path
r_files <- list.files(Rcode_folder, pattern = "\\.R$", full.names = TRUE)
for (r_file in r_files) {
  ssh::scp_upload(session, r_file, to = remote_folder)
}

# Load bash script
# Get list of Bash files in the folder
bash_files <- list.files(Rcode_folder, pattern = "\\.sh$", full.names = TRUE)
# Loop through each Bash file
for (bash_file in bash_files) {
  # Get the base name of the Bash file
  bash_file_name <- basename(bash_file)
  ssh::scp_upload(session, bash_file, to = remote_folder)
  # Execute the uploaded Bash script on the remote server
  commands <- paste0("cd ", remote_folder, " && sbatch -A epadecarb ", bash_file_name)
  ssh::ssh_exec_wait(session, command = commands)
}


#------- 
# CHECK JOB STATUS
ssh::ssh_exec_wait(session, command = "seff 2559171")
ssh::ssh_exec_wait(session, command = "sacct --job=2559171")

# Cancel JOB
ssh::ssh_exec_wait(session, command = "scancel 2553375")


# Disconnect
#-----
ssh::ssh_disconnect(session)


# Otherstuff
# Define commands
commands <- "showusage; ls; pwd"
ssh::ssh_exec_wait(session, command = "cd")
ssh::ssh_exec_wait(session, command = "mkdir arc3-ws")
ssh::ssh_exec_wait(session, command = commands)
ssh::ssh_exec_wait(session, command = "cd arc3-ws")
ssh::ssh_exec_wait(session, command = "ls")
ssh::ssh_exec_wait(session, command = "cp /globalscratch/ckuhlman/arc-workshops-mar-2024/arc3.pres.exercises.final.jun.2024.tar.gz   .")
ssh::ssh_exec_wait(session, command = "tar xvzf arc3.pres.exercises.final.jun.2024.tar.gz")

ssh::ssh_exec_wait(session, command = "cd arc3; ls")
ssh::ssh_exec_wait(session, command = "ls -l *")


# --- UPLOAD DATA SETS ----
# Define remote folder
remote_folder <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/"

# List of full local paths to upload (excluding peak_demand_data.csv)
files_to_upload <- c(
  # 1. Installed Capacities and Facilities Data
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/1 Decarbonization Pathways/Hourly_Installed_Capacity.csv",
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/2 SMR/1 SMR Facility Data/SMR_Facility_Data.csv",
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/3 Large nuclear hydro and bio/1 Clean Baseload Facility Data/CleanBaseload_Facility_Data.csv",
  
  # Fossil Fuels Data
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv",
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv",
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/2 Fossil Fuels Generation and Emissions/Fossil_Fuel_hr_maxmin.csv",
  
  # Wind and Solar CF
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/1 Wind and Solar/1 Wind and Solar CF/offwind_CF.csv",
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/1 Wind and Solar/1 Wind and Solar CF/onwind_CF.csv",
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/1 Wind and Solar/1 Wind and Solar CF/solar_CF.csv",
  
  # Fossil Generation and Emissions
  #"/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/2 Fossil Fuels Generation and Emissions/Fossil_Fuel_Generation_Emissions.csv",
  
  # Imports
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/3 Imports/1 Imports CF/Imports_CF.csv",
  
  # Demand
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/demand_data.csv",
  
  # Random Sequence
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/4 Randomization/1 Randomized Data/Random_Sequence.csv"
)

# Upload each file to the remote server
for (file_path in files_to_upload) {
  cat("Uploading:", file_path, "\n")
  ssh::scp_upload(session, file_path, to = remote_folder)
}

