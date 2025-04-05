# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Specify the path to install packages
lib_path <- "/home/amirgazar/Capacity_Expansion/env"

# Create the directory if it doesn't exist
if (!dir.exists(lib_path)) {
  dir.create(lib_path, recursive = TRUE)
}

# Install remotes package to handle version-specific installations
if (!requireNamespace("remotes", quietly = TRUE, lib.loc = lib_path)) {
  install.packages("remotes", lib = lib_path)
  library(remotes, lib.loc = lib_path)
}

# Function to install and load packages
install_and_load <- function(package, lib, version = NULL) {
  if (!requireNamespace(package, quietly = TRUE, lib.loc = lib)) {
    if (is.null(version)) {
      install.packages(package, lib = lib)
    } else {
      remotes::install_version(package, version = version, lib = lib)
    }
    if (!requireNamespace(package, quietly = TRUE, lib.loc = lib)) {
      stop(paste("Package", package, "is not available in the library path:", lib))
    }
  }
  library(package, character.only = TRUE, lib.loc = lib)
}

# Install and load packages
install_and_load("rlang", lib_path, "1.1.4")  # Install specific version of rlang first
install_and_load("httr", lib_path, "1.4.7")
install_and_load("htmltools", lib_path, "0.5.8.1")
install_and_load("jsonlite", lib_path, "1.8.8")
install_and_load("data.table", lib_path, "1.15.4")
install_and_load("lubridate", lib_path, "1.7.10")  # Use an older version of lubridate that doesn't depend on timechange
install_and_load("parallel", lib_path)  # Install the parallel package

# Verify installation
packages <- c("httr", "htmltools", "jsonlite", "data.table", "lubridate", "rlang", "parallel")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE, lib.loc = lib_path)) {
    stop(paste(pkg, "was not installed successfully."))
  }
}

# Load packages to verify
library(httr, lib.loc = lib_path)
library(htmltools, lib.loc = lib_path)
library(jsonlite, lib.loc = lib_path)
library(data.table, lib.loc = lib_path)
library(lubridate, lib.loc = lib_path)
library(parallel, lib.loc = lib_path)

# Print success message
cat("All packages were installed and loaded successfully.\n")
