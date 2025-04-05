# Load libraries
library(data.table)

# Load facilities data
path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
facilities_data_NE <- fread(path)
setDT(facilities_data_NE)

# Selecting a template facility
template_facility_data <- facilities_data_NE[Facility_Unit.ID == "56047_1"]

# We need 18 facility (3xunit in each), starting 2033, one facility comes online per year
# Define the starting year and number of facilities
start_year <- 2033
num_facilities <- 18

# Create new facility names and corresponding years
new_facilities <- data.table(
  Facility_ID = paste0("NGCC", 3000:(3000 + num_facilities - 1)),  # Sequential facility names
  Year_Online = start_year:(start_year + num_facilities - 1)  # Sequential years
)

# Expand each facility into 3 units (Unit_ID: 1, 2, 3)
new_facility_units <- new_facilities[, .(
  Unit_ID = 1:2,
  Similar_Facility_Unit_ID = template_facility_data$Facility_Unit.ID,
  CF = 0.85,
  Estimated_NameplateCapacity_MW = template_facility_data$Estimated_NameplateCapacity_MW,
  mean_CO2_tons_MW = template_facility_data$mean_CO2_tons_MW,
  mean_NOx_lbs_MW = template_facility_data$mean_NOx_lbs_MW,
  mean_SO2_lbs_MW = template_facility_data$mean_SO2_lbs_MW,
  mean_Heat_Input_mmBtu = template_facility_data$mean_Heat_Input_mmBtu,
  Ramp = 1,
  Retirement_year = 2100,
  Commercial_Operation_Date = as.Date(paste0(Year_Online, "-01-01"))
), by = Facility_ID]

# Create unique Facility_Unit IDs
new_facility_units[, Facility_Unit.ID := paste0(Facility_ID, "_", Unit_ID)]

# Save the data.table as a CSV file
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv"
fwrite(new_facility_units, file_path)

