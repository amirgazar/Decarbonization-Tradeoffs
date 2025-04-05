# Required libraries
library(httr)
library(jsonlite)
library(arrow)
library(dplyr)
library(stringr)
library(tools)
library(data.table)

# --- Parameters ---
# Define the base directory where data is saved
base_dir <- "/Users/amirgazar/Documents/GitHub/U.S. EPA CAMPD Data"

# Define the states to process. You can use all states or a subset.
stateCodes <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", 
                "GA", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", 
                "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", 
                "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", 
                "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", "WA", "WI", 
                "WV", "WY")

# Expected years
expected_years <- 1995:2024

# API settings
apiKEY <- "vJRq6xPw4kljMCHda79tIRor5rGxJkDVpQK3JdWa"
apiUrlBase <- "https://api.epa.gov/easey"
bucketUrlBase <- "https://api.epa.gov/easey/bulk-files/"

# --- Step 1: Get Bulk Files API Data (once) ---
servicesUrl <- paste0(apiUrlBase, "/camd-services/bulk-files?API_KEY=", apiKEY)
res <- GET(servicesUrl)
if (res$status_code > 399) {
  stop("Error retrieving bulk files from API. Status code: ", res$status_code)
}
bulkFiles <- fromJSON(rawToChar(res$content))
bulkFiles$lastUpdated <- strftime(bulkFiles$lastUpdated, "%Y-%m-%dT%H:%M:%S")

# --- Loop Over Each State ---
for (stateCode in stateCodes) {
  cat("=============================================\n")
  cat("Processing state:", stateCode, "\n")
  
  # Define state directory (assumes data was saved under a folder named with the state code)
  state_dir <- file.path(base_dir, stateCode)
  
  # Define the main hourly emissions file for the state (assumes naming convention)
  main_emissions_file <- file.path(state_dir, paste0("Hourly_Emissions_", stateCode, ".parquet"))
  
  if (!file.exists(main_emissions_file)) {
    cat("Main hourly emissions file for", stateCode, "not found at:", main_emissions_file, "\n")
    next
  }
  
  # --- Read the Existing Emissions Data ---
  emissions_data <- data.table(read_parquet(main_emissions_file))
  
  # Check if the data has a 'year' column
  if (!"Date" %in% names(emissions_data)) {
    cat("Data for state", stateCode, "does not have a 'year' column. Skipping.\n")
    next
  }
  
  # Ensure the year column is numeric
  years_present <- sort(unique(as.numeric(year(emissions_data$Date))))
  missing_years <- setdiff(expected_years, years_present)
  
  cat("Years present for", stateCode, ":", years_present, "\n")
  cat("Missing years for", stateCode, ":", missing_years, "\n")
  
  # --- Download Missing Years ---
  if (length(missing_years) > 0) {
    for (year_missing in missing_years) {
      cat("-------------------------------------------------\n")
      cat("Processing missing year:", year_missing, "for state:", stateCode, "\n")
      
      # Filter bulkFiles for the specified criteria:
      #   - Emissions data
      #   - Hourly subtype
      #   - Matching state
      #   - Matching year
      files_for_year <- bulkFiles[
        bulkFiles$metadata$dataType == "Emissions" &
          bulkFiles$metadata$dataSubType == "Hourly" &
          bulkFiles$metadata$stateCode == stateCode &
          as.numeric(bulkFiles$metadata$year) == year_missing, ]
      
      if (nrow(files_for_year) == 0) {
        cat("No bulk file available for", stateCode, "in year", year_missing, "\n")
        next
      }
      
      # Initialize a container for the year's data
      emissions_year_data <- data.frame()
      
      for (i in 1:nrow(files_for_year)) {
        s3Path <- files_for_year[i, "s3Path"]
        fileUrl <- paste0(bucketUrlBase, s3Path)
        cat("Downloading file for", stateCode, "year", year_missing, ":", fileUrl, "\n")
        
        # Create a temporary file with the same extension as the source file
        temp_file <- tempfile(fileext = paste0(".", file_ext(s3Path)))
        
        tryCatch({
          # Increase timeout if necessary (here set to 600 seconds)
          resp <- GET(fileUrl, write_disk(temp_file, overwrite = TRUE), timeout(600))
          if (resp$status_code != 200) {
            stop("File download failed with status code: ", resp$status_code)
          }
          
          # Read the file based on its extension
          ext <- tolower(file_ext(s3Path))
          if (ext == "csv") {
            tempData <- read.csv(temp_file)
          } else if (ext == "parquet") {
            tempData <- read_parquet(temp_file)
          } else {
            stop("Unsupported file format: ", ext)
          }
          
          # Append the data (use bind_rows to ensure proper merging)
          emissions_year_data <- bind_rows(emissions_year_data, tempData)
          
        }, error = function(e) {
          cat("Error downloading/reading file for", stateCode, "year", year_missing, ":", fileUrl, "\n")
          print(e)
        }, finally = {
          unlink(temp_file)
        })
      }
      
      # If data was retrieved for this year, save it to its own file
      if (nrow(emissions_year_data) > 0) {
        output_file <- file.path(state_dir, paste0("Hourly_Emissions_", year_missing, "_", stateCode, ".parquet"))
        write_parquet(emissions_year_data, output_file)
        cat("Saved emissions data for", stateCode, "year", year_missing, "to", output_file, "\n")
      } else {
        cat("No data retrieved for", stateCode, "year", year_missing, "\n")
      }
    }
  } else {
    cat("No missing years for", stateCode, "\n")
  }
  
  cat("Completed processing for state:", stateCode, "\n")
}
