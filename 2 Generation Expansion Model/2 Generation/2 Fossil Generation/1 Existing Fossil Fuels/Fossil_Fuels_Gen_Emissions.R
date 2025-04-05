# Load libraries
library(data.table)

# Define New England state codes
stateCodes <- c("CT", "ME", "MA", "NH", "RI", "VT")

# Define base directory
base_dir <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EPA CAMPD/"
all_emissions_data <- list()

# Loop through each state
for (state in stateCodes) {
  state_dir <- file.path(base_dir, state)
  emissions_data_path <- file.path(state_dir, paste0("Hourly_Stochastic_Gen_", state, ".csv"))
  
  if (file.exists(emissions_data_path)) {
    # Read the CSV file
    emissions_data <- fread(emissions_data_path)
    
    # Add to the list
    all_emissions_data[[state]] <- emissions_data
  } else {
    message(paste("File not found for state:", state))
  }
}

# Combine all data into a single data table
combined_emissions_data <- rbindlist(all_emissions_data, fill = TRUE)

# Load facilities data and remove any facility that doesnt exist in that dataset
path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
facilities_data_NE <- fread(path)

# Get unique Facility_Unit.ID values in facilities_data_NE
valid_facility_ids <- unique(facilities_data_NE$Facility_Unit.ID)

# Filter combined_emissions_data to retain only matching Facility_Unit.IDs
filtered_combined_emissions_data <- combined_emissions_data[Facility_Unit.ID %in% valid_facility_ids]

# Define the path to save the filtered data
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/2 Fossil Fuels Generation and Emissions/Fossil_Fuel_Generation_Emissions.csv"

# Save the filtered data to a CSV file
fwrite(filtered_combined_emissions_data, file = output_path)

