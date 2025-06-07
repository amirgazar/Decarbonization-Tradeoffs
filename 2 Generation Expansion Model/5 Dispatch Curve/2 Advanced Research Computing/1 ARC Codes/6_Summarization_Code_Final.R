# Specify the existing library path
lib_path <- "/projects/epadecarb/2 Generation Expansion Model/1 Environment/env"

# Function to load packages from that library
load_packages <- function(package, lib) {
  if (!require(package, character.only = TRUE, lib.loc = lib)) {
    stop(paste("Package", package, "is not available in the library path:", lib))
  }
}
# Load required packages
pkgs <- c("httr","htmltools","jsonlite","data.table","lubridate","zoo")
invisible(lapply(pkgs, load_packages, lib_path))

# Hourly
# Directory for summary outputs
summary_results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/"

# --- Combine all chunk outputs into final files ---
results_path_final <- summary_results_path

yearly_files   <- list.files(results_path_final,
                             pattern = "Yearly_Results_Chunk_.*\\.csv$",
                             full.names = TRUE)
shortages_files <- list.files(results_path_final,
                              pattern = "Yearly_Results_Shortages_Chunk_.*\\.csv$",
                              full.names = TRUE)

Yearly_Results           <- rbindlist(lapply(yearly_files,   fread))
Yearly_Results_Shortages <- rbindlist(lapply(shortages_files, fread))

final_results_path <- file.path(results_path_final, "Final")
dir.create(final_results_path, showWarnings = FALSE, recursive = TRUE)

write.csv(Yearly_Results,           file.path(final_results_path, "Yearly_Results.csv"),           row.names = FALSE)
write.csv(Yearly_Results_Shortages, file.path(final_results_path, "Yearly_Results_Shortages.csv"), row.names = FALSE)

# Facility
# Yearly
yearly_files <- list.files(
  summary_results_path,
  pattern = "Yearly_Facility_Level_Results_Chunk_.*\\.csv$",
  full.names = TRUE
)
Yearly_Facility_Level_Results <- rbindlist(lapply(yearly_files, fread))
fwrite(
  Yearly_Facility_Level_Results,
  file.path(final_results_path, "Yearly_Facility_Level_Results.csv")
)

# Monthly
monthly_files <- list.files(
  summary_results_path,
  pattern = "Monthly_Facility_Level_Results_Chunk_.*\\.csv$",
  full.names = TRUE
)
Monthly_Facility_Level_Results <- rbindlist(lapply(monthly_files, fread))
fwrite(
  Monthly_Facility_Level_Results,
  file.path(final_results_path, "Monthly_Facility_Level_Results.csv")
)


# --- Notify via Pushover when done ---
pushover_user  <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST(
  "https://api.pushover.net/1/messages.json",
  body = list(
    token   = pushover_token,
    user    = pushover_user,
    message = "The Summary_code executed."
  ),
  encode = "form"
)
