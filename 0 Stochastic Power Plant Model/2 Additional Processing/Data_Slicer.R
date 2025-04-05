# Load the packages from the specified library
library("httr")
library("htmltools")
library("jsonlite")
library("data.table")
library("lubridate")

# Define state codes (using the subset or full list as needed)
stateCodes <- c("CT", "ME", "MA", "NH", "RI", "VT")
# For the full list, you could use:
# stateCodes <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", 
#                 "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", 
#                 "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", 
#                 "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", 
#                 "WA", "WI", "WV", "WY")

# Define the base directory
base_dir <- "/Users/amirgazar/Documents/GitHub/States Historical Data"

# Loop over each state
for (state in stateCodes) {
  
  # Define the state directory dynamically
  state_dir <- file.path(base_dir, state)
  
  # Define the path to read the emissions data
  emissions_data_path <- file.path(state_dir, paste0("Hourly_Stochastic_Gen_", state, ".csv"))
  
  # Read the CSV file into a data.table
  emissions_data <- fread(emissions_data_path)
  
  # Split the dataset by unique Facility_Unit.ID
  facility_datasets <- split(emissions_data, emissions_data$Facility_Unit.ID)
  
  # Define the 'Stochastic Data' subdirectory
  stochastic_dir <- file.path(state_dir, "Stochastic Data")
  if (!dir.exists(stochastic_dir)) {
    dir.create(stochastic_dir)
  }
  
  # Save each dataset separately as a CSV file in 'Stochastic Data'
  lapply(names(facility_datasets), function(facility_id) {
    facility_data <- facility_datasets[[facility_id]]
    # Define the path to save the CSV file for each facility inside 'Stochastic Data'
    output_path <- file.path(stochastic_dir, paste0(facility_id, ".csv"))
    # Write the dataset to a CSV file
    fwrite(facility_data, output_path)
  })
  gc()
}
