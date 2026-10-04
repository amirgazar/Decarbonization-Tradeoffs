# Base-R helpers using macOS/Linux OpenSSH. No R packages or stored passwords.
# Sourcing defines functions only: no connection, upload, or job submission.
# Set local_dir to the AP4_ARC folder if you move it.
arc_config <- list(
  host = "amirgazar@tinkercliffs1.arc.vt.edu",
  local_dir = Sys.getenv("PHASED_AP4_ARC", unset=""),
  remote_dir = "ap4_validation_uploads"
)

arc_command <- function(program, args) {
  executable <- Sys.which(program)
  if (!nzchar(executable)) stop("Required program missing: ", program)
  status <- system2(executable, args = vapply(args, shQuote, character(1)))
  if (status != 0L) stop(program, " failed (exit ", status, "). Read the output above.")
  invisible(status)
}
arc_remote <- function(command) {
  arc_command("ssh", c("-o", "ConnectTimeout=15", arc_config$host, command))
}
arc_check <- function() arc_remote("hostname")

arc_upload <- function() {
  archive <- file.path(arc_config$local_dir, "uploads", "AP4_ARC_Validation.zip")
  stopifnot(file.exists(archive))
  # Remote directory is relative to your ARC home, avoiding machine-specific paths.
  arc_remote(paste("mkdir -p", shQuote(arc_config$remote_dir)))
  destination <- paste0(arc_config$host, ":", arc_config$remote_dir, "/AP4_ARC_Validation.zip")
  arc_command("scp", c(archive, destination))
  message("Uploaded. Call arc_extract() next.")
}
arc_extract <- function() {
  # -n preserves any existing files and results. Use a new remote_dir for a fresh rerun.
  arc_remote(paste("unzip -n", shQuote(paste0(arc_config$remote_dir, "/AP4_ARC_Validation.zip")),
                   "-d", shQuote(arc_config$remote_dir)))
}
arc_submit <- function() {
  # Explicit call submits a 4-core, 8-GB, one-hour CPU job against epadecarb.
  # Prefer a fresh remote_dir per repeat run; fixed-name native outputs are reused.
  work <- paste0(arc_config$remote_dir, "/AP4_ARC")
  arc_remote(paste("cd", shQuote(work), "&& sbatch run_ap4.slurm"))
}
arc_status <- function() arc_remote("squeue -u amirgazar")
arc_download <- function() {
  local <- arc_config$local_dir
  remote_analysis <- paste0(arc_config$host, ":", arc_config$remote_dir,
                           "/AP4_ARC/AP4_Investigation_2026-09-20")
  dir.create(file.path(local, "outputs"), showWarnings=FALSE, recursive=TRUE)
  dir.create(file.path(local, "logs"), showWarnings=FALSE, recursive=TRUE)
  arc_command("scp", c("-r", paste0(remote_analysis, "/native_validation"), file.path(local, "outputs")))
  arc_command("scp", c(paste0(remote_analysis, "/ARC_MATLAB_run.log"), file.path(local, "logs")))
}
arc_download_logs <- function() {
  # Useful for a failed run; downloads MATLAB and Slurm logs with OpenSSH/SFTP globbing.
  destination <- file.path(arc_config$local_dir, "logs")
  dir.create(destination, showWarnings=FALSE, recursive=TRUE)
  remote <- paste0(arc_config$host, ":", arc_config$remote_dir,
                   "/AP4_ARC/AP4_Investigation_2026-09-20/*.log")
  arc_command("scp", c(remote, destination))
}
