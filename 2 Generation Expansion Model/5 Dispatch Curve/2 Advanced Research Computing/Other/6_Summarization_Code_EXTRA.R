# Specify the existing library path
lib_path <- "/projects/epadecarb/2 Generation Expansion Model/1 Environment/env"

# Function to load packages
load_packages <- function(package, lib) {
  if (!require(package, character.only = TRUE, lib.loc = lib)) {
    stop(paste("Package", package, "is not available in the library path:", lib))
  }
}

# Load the packages from the specified library
packages <- c("httr", "htmltools", "jsonlite", "data.table", "lubridate", "zoo")
invisible(lapply(packages, load_packages, lib = lib_path))

# --- Final step: Load and combine intermediate results into final data sets ---
#results_path_final <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized"

# Identify intermediate CSV files
#yearly_files <- list.files(results_path_final, pattern = "Yearly_Results_Chunk_.*\\.csv$", full.names = TRUE)
#shortages_files <- list.files(results_path_final, pattern = "Yearly_Results_Shortages_Chunk_.*\\.csv$", full.names = TRUE)

# Combine intermediate results
#Yearly_Results <- rbindlist(lapply(yearly_files, fread))
#Yearly_Results_Shortages <- rbindlist(lapply(shortages_files, fread))

# Save final combined datasets
#final_results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/Final/"
#write.csv(Yearly_Results, file.path(final_results_path, "Yearly_Results.csv"))
#write.csv(Yearly_Results_Shortages, file.path(final_results_path, "Yearly_Results_Shortages.csv"))

# --- Final step: Load and combine intermediate results into final data sets ---
results_path_final <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized"

# Identify intermediate CSV files
yearly_facility_files <- list.files(results_path_final, pattern = "Yearly_Facility_Level_Results_Chunk_.*\\.csv$", full.names = TRUE)
monthly_facility_files <- list.files(results_path_final, pattern = "Monthly_Facility_Level_Results_Chunk_.*\\.csv$", full.names = TRUE)

# Combine intermediate results
Yearly_Facility_Level_Results <- rbindlist(lapply(yearly_facility_files, fread))
Monthly_Facility_Level_Results <- rbindlist(lapply(monthly_facility_files, fread))

# Save final combined datasets
final_results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/Final/"
write.csv(Yearly_Facility_Level_Results, file.path(final_results_path, "Yearly_Facility_Level_Results.csv"))
write.csv(Monthly_Facility_Level_Results, file.path(final_results_path, "Monthly_Facility_Level_Results.csv"))

# Pushover alert - Send notification that processing is complete.
pushover_user <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST("https://api.pushover.net/1/messages.json",
                 body = list(
                   token = pushover_token,
                   user = pushover_user,
                   message = "The Summary_code executed."
                 ),
                 encode = "form")
