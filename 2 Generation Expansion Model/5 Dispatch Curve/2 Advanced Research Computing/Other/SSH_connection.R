# Install/Import SSH library 
library(ssh)

setwd("/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing")

host <- "tinkercliffs2.arc.vt.edu"
username <- "amirgazar" # VT PID
password <- "av@/tNiHBN(LR4("  # VT.edu Password

session <- ssh::ssh_connect(paste0(username, "@", host), passwd = password) # Make sure you authenticate DUO Security

#-------
# Stepwise (Comprehensive days)
# Uploading Rcode
Rcode_folder <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 RCodes to submit/2 Comprehensive Days" # Path to local folder containing R files
remote_folder <- "/projects/epadecarb/Decarb_Paper/Stepwise/" # Remote folder path
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

#----
# Summarizing comprehensive days results
# Load bash script
bash_file <- "GEM_Stepwise_Summary.sh"
# Upload the Bash script to the remote server
ssh::scp_upload(session, bash_file, to = "/projects/epadecarb/Decarb_Paper/")

# Load R script
R_file <- "GEM_ARC_Stepwise_Summary.R"
ssh::scp_upload(session, R_file, to = "/projects/epadecarb/Decarb_Paper/")
# Define the command to submit the SLURM job + Change directory
commands <- "cd /projects/epadecarb/Decarb_Paper/ && sbatch -A epadecarb GEM_Stepwise_Summary.sh"
# Execute the command
ssh::ssh_exec_wait(session, command = commands)

#----
# Rep_Days
# Uploading Rcode
Rcode_folder <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 RCodes to submit/1 Representative Days" # Path to local folder containing R files
remote_folder <- "/projects/epadecarb/Decarb_Paper/Rep_Days/" # Remote folder path
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

#----
# Summarizing Rep days results
# Load bash script
bash_file <- "GEM_Rep_Days_Expansion_Summary.sh"
# Upload the Bash script to the remote server
ssh::scp_upload(session, bash_file, to = "/projects/epadecarb/Decarb_Paper/")

# Load R script
R_file <- "GEM_ARC_Rep_Days_Expansion_Summary.R"
ssh::scp_upload(session, R_file, to = "/projects/epadecarb/Decarb_Paper/")
# Define the command to submit the SLURM job + Change directory
commands <- "cd /projects/epadecarb/Decarb_Paper/ && sbatch -A epadecarb GEM_Rep_Days_Expansion_Summary.sh"
# Execute the command
ssh::ssh_exec_wait(session, command = commands)

#-----
# Hourly results
# Load R script
R_file <- "GEM_ARC_Hourly_Results.R"
ssh::scp_upload(session, R_file, to = "/projects/epadecarb/Decarb_Paper/")

# Load bash script
bash_file <- "GEM_Hourly_Results.sh"
# Upload the Bash script to the remote server
ssh::scp_upload(session, bash_file, to = "/projects/epadecarb/Decarb_Paper/")

# Define the command to submit the SLURM job + Change directory
commands <- "cd /projects/epadecarb/Decarb_Paper/ && sbatch -A epadecarb GEM_Hourly_Results.sh"
# Execute the command
ssh::ssh_exec_wait(session, command = commands)

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
