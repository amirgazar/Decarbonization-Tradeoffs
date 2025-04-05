# Dispatch curve, offsetting demand with generation
# Specify the existing library path
lib_path <- "/home/amirgazar/Fossil_Fuels/env"

# Function to load packages
load_packages <- function(package, lib) {
  if (!require(package, character.only = TRUE, lib.loc = lib)) {
    stop(paste("Package", package, "is not available in the library path:", lib))
  }
}

# Load the packages from the specified library
load_packages("httr", lib_path)
load_packages("htmltools", lib_path)
load_packages("jsonlite", lib_path)
load_packages("data.table", lib_path)
load_packages("lubridate", lib_path)

# Load data in batches of 5 files at a time
hourly_files <- list.files("/home/amirgazar/Capacity_Expansion/Results/Stepwise", pattern = "Hourly_Results_NE_stepwise", full.names = TRUE)

batch_size <- 5
output_file <- "/home/amirgazar/Capacity_Expansion/Results/Summary/Final/Hourly_Results_combined.csv"

# Initialize the output file
if (file.exists(output_file)) file.remove(output_file)

for (i in seq(1, length(hourly_files), by = batch_size)) {
  batch_files <- hourly_files[i:min(i + batch_size - 1, length(hourly_files))]
  batch_data <- rbindlist(lapply(batch_files, readRDS))
  
  # Append the batch to the output CSV
  fwrite(batch_data, output_file, append = TRUE)
  
  # Clear memory after processing each batch
  rm(batch_data)
  gc()
}


# Pushover alert - Pushover credentials
pushover_user <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST("https://api.pushover.net/1/messages.json",
                 body = list(
                   token = pushover_token,
                   user = pushover_user,
                   message = "The Summary_code executed."
                 ),
                 encode = "form")
