# Install/Import SSH library 
library(ssh)

setwd("/Users/amirgazar/Documents/GitHub/EPA_Debarbonization/ARC SSH Fossil Fuels USA")

host <- "tinkercliffs2.arc.vt.edu"
username <- "amirgazar" # VT PID
password <- "av@/tNiHBN(LR4("  # VT.edu Password

session <- ssh::ssh_connect(paste0(username, "@", host), passwd = password) # Make sure you authenticate DUO Security

# Load bash script
bash_file <- "Fossil_Fuels_USA.sh"
# Upload the Bash script to the remote server
ssh::scp_upload(session, bash_file, to = "/home/amirgazar/Fossil_Fuels")


# Load R script
R_file <- "Fossil_Fuels_ARC_USA.R"
ssh::scp_upload(session, R_file, to = "/home/amirgazar/Fossil_Fuels")

# Define the command to submit the SLURM job + Change directory
commands <- "cd /home/amirgazar/Fossil_Fuels && sbatch -A epadecarb Fossil_Fuels_USA.sh"
# Execute the command
ssh::ssh_exec_wait(session, command = commands)

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
