# Required libraries
library(httr)
library(jsonlite)
library(data.table)
library(dplyr)
library(stringr)
library(arrow)
library(tools)  
library(dplyr)

# Define the function to download facilities and emissions data
download_state_emissions_data <- function(apiKEY, years, timeOfLastRun, chunk_size = 1, 
                                          base_dir = "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EPA CAMPD Data") {
  
  # API base URL
  apiUrlBase <- "https://api.epa.gov/easey"
  bucketUrlBase <- "https://api.epa.gov/easey/bulk-files/"
  
  # List of states to process 
  stateCodes <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", 
                  "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", 
                  "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", 
                  "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", 
                  "WA", "WI", "WV", "WY")
  
  # For testing, we use only Texas (TX)
  stateCodes <- c("TX")
  
  # Loop through each state one by one
  for (stateCode in stateCodes) {
    print(paste("Processing state:", stateCode))
    
    # Create a directory for the state if it doesn't exist
    state_dir <- file.path(base_dir, stateCode)
    dir.create(state_dir, showWarnings = FALSE, recursive = TRUE)
    
    # --------------------------
    # Download Facilities Data
    # --------------------------
    query <- list(year = paste0(years, collapse = '|'), 
                  stateCode = stateCode)
    
    facilitiesUrl <- paste0(apiUrlBase, "/streaming-services/facilities/attributes?API_KEY=", apiKEY)
    res <- GET(facilitiesUrl, query = query)
    
    if (res$status_code > 399) {
      print(paste("Error retrieving facilities data for state:", stateCode))
      print(paste("HTTP Status Code:", res$status_code))
      next  # Skip to next state
    }
    
    facilitiesData <- fromJSON(rawToChar(res$content), flatten = TRUE)
    if (nrow(facilitiesData) == 0) {
      print(paste("No facilities data available for state:", stateCode))
      next
    }
    
    # Save facilities data as Parquet
    write_parquet(
      facilitiesData,
      file.path(state_dir, paste0("Facilities_Data_", stateCode, ".parquet"))
    )
    print(paste("Facilities data for state", stateCode, "saved successfully."))
    
    # --------------------------
    # Download Emissions Data
    # --------------------------
    servicesUrl <- paste0(apiUrlBase, "/camd-services/bulk-files?API_KEY=", apiKEY)
    res <- GET(servicesUrl)
    
    if (res$status_code > 399) {
      print(paste("Error retrieving emissions files for state:", stateCode))
      next
    }
    
    bulkFiles <- fromJSON(rawToChar(res$content))
    bulkFiles$lastUpdated <- strftime(bulkFiles$lastUpdated, "%Y-%m-%dT%H:%M:%S")
    newBulkFiles <- bulkFiles[bulkFiles$lastUpdated > timeOfLastRun, ]
    emissionsFiles <- newBulkFiles[newBulkFiles$metadata$dataType == "Emissions", ]
    hourlyEmissionsFiles <- emissionsFiles[emissionsFiles$metadata$dataSubType == "Hourly", ]
    stateHourlyEmissionsFiles <- hourlyEmissionsFiles[hourlyEmissionsFiles$metadata$stateCode == stateCode, ]
    
    # Filter based on the years of interest
    stateHourlyEmissionsFiles <- stateHourlyEmissionsFiles[stateHourlyEmissionsFiles$metadata$year %in% years, ]
    
    if (nrow(stateHourlyEmissionsFiles) == 0) {
      print(paste("No emissions data available for state:", stateCode))
      next
    }
    
    # Process and save each year separately
    for (yr in years) {
      print(paste("Processing emissions data for state:", stateCode, "year:", yr))
      
      # Filter files for the current year
      files_year <- stateHourlyEmissionsFiles[stateHourlyEmissionsFiles$metadata$year == yr, ]
      
      if (nrow(files_year) == 0) {
        print(paste("No emissions data available for state:", stateCode, "year:", yr))
        next
      }
      
      stateHourlyEmissionsData_chunk <- data.frame()
      
      for (i in 1:nrow(files_year)) {
        s3Path <- files_year[i, "s3Path"]
        fileUrl <- paste0(bucketUrlBase, s3Path)
        print(paste0("Downloading file for state: ", stateCode, ", year: ", yr, " - ", fileUrl))
        
        # Create a temporary file with the same extension as the original file
        temp_file <- tempfile(fileext = paste0(".", file_ext(s3Path)))
        
        tryCatch({
          # Download the file using httr::GET() with write_disk
          resp <- GET(fileUrl, write_disk(temp_file, overwrite = TRUE), timeout(1200))
          if (resp$status_code != 200) {
            stop("File download failed with status code: ", resp$status_code)
          }
          
          # Determine file extension to read correctly
          ext <- tolower(file_ext(s3Path))
          if (ext == "csv") {
            tempData <- read.csv(temp_file)
          } else if (ext == "parquet") {
            tempData <- read_parquet(temp_file)
          } else {
            stop("Unsupported file format: ", ext)
          }
          
          # Append the data to the cumulative data frame for the current year
          stateHourlyEmissionsData_chunk <- rbind(stateHourlyEmissionsData_chunk, tempData)
          gc()  # Free memory after each file download
          
        }, error = function(e) {
          print(paste("Error reading file:", fileUrl))
          print(e)
        }, finally = {
          unlink(temp_file)  # Clean up the temporary file
        })
      }
      
      # Save the combined emissions data for the current year as Parquet
      output_file <- file.path(state_dir, paste0("Hourly_Emissions_", stateCode, "_", yr, ".parquet"))
      write_parquet(stateHourlyEmissionsData_chunk, output_file)
      print(paste("Emissions data for state", stateCode, "year", yr, "saved successfully."))
      
      # Clear the data and run garbage collection to free memory
      rm(stateHourlyEmissionsData_chunk)
      gc()
    }
    
    # Clear facilities data from memory before processing the next state
    rm(facilitiesData)
    gc()
  }
}

# Define parameters
apiKEY <- "vJRq6xPw4kljMCHda79tIRor5rGxJkDVpQK3JdWa"
years <- 1995:2024
timeOfLastRun <- strftime("1995-01-01T00:00:00", "%Y-%m-%dT%H:%M:%S")

# Call the function to download facilities and emissions data state by state
download_state_emissions_data(apiKEY, years, timeOfLastRun, chunk_size = 1)
