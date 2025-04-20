# Dispatch curve, offsetting demand with generation
# Specify the existing library path
lib_path <- "/projects/epadecarb/2 Generation Expansion Model/1 Environment/env"

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
load_packages("zoo", lib_path)

# Load EVOLL curve
path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/evoll_curve.csv"
#path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/ISO-NE Unmet Demand/evoll_curve.csv"
Cost_Curve <- fread(path)
# Old/existing fossil fuels
path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/Fossil_Fuel_Facilities_Data.csv"
#path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(path)
# Remove unnecessary columns from Fossil_Fuels_NPC
Fossil_Fuels_NPC <- Fossil_Fuels_NPC[, .(Facility_Unit.ID, State, Fuel_type_1 = Primary_Fuel_Type, Fuel_type_2 = Secondary_Fuel_Type, latitude = Latitude, longitude = Longitude, Ramp_hr = Ramp, Fossil.NPC_MW = Estimated_NameplateCapacity_MW, Max_Hourly_HI_Rate)]

# Load Capacity data
path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/1 Stepwise"
#path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/0 Test"
#path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/Test Results"
files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# Separate files based on type
facility_files <- grep("Facility_Level_Results", files, value = TRUE)
hourly_files <- grep("Hourly_Results", files, value = TRUE)

# Split files into chunks of 10
facility_chunks <- split(facility_files, ceiling(seq_along(facility_files) / 10))
hourly_chunks <- split(hourly_files, ceiling(seq_along(hourly_files) / 200))

# Function to process facility files
process_facility_files <- function(chunk_files, chunk_index) {
  # Define base results directory
  results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/"
  #results_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/Test Results/"
  
  Facility_Level_Results <- data.table()
  
  # Read and process each file in the chunk
  for (i in seq_along(chunk_files)) {
    data <- fread(chunk_files[i])
    data[, Year := year(Date)]
    Facility_Level_Results <- rbind(Facility_Level_Results, data)
  }
  
  # Merge datasets and calculate total_HI_mmBtu
  Facility_Level_Results <- merge(
    Facility_Level_Results,
    Fossil_Fuels_NPC,
    by = "Facility_Unit.ID"
  )
  
  Facility_Level_Results[Max_Hourly_HI_Rate < HI_mmBtu, HI_mmBtu := Max_Hourly_HI_Rate]
  
  Yearly_Facility_Level_Results <- Facility_Level_Results[, .(
    total_generation_GWh = sum(Gen_MWh_adj, na.rm = TRUE) / 1e3,
    total_CO2_tons = sum(CO2_tons, na.rm = TRUE),
    total_NOx_lbs = sum(NOx_lbs, na.rm = TRUE),
    total_SO2_lbs = sum(SO2_lbs, na.rm = TRUE),
    total_HI_mmBtu = sum(HI_mmBtu, na.rm = TRUE)
  ), by = .(Year, Pathway, Simulation, Facility_Unit.ID)]
  
  Static_Facility_Info <- unique(Facility_Level_Results[, .(Facility_Unit.ID, State, Fuel_type_1, Fuel_type_2, latitude, longitude, Ramp_hr, Fossil.NPC_MW)])
  Yearly_Facility_Level_Results <- merge(Yearly_Facility_Level_Results, Static_Facility_Info, by = "Facility_Unit.ID", all.x = TRUE)
  
  write.csv(
    Yearly_Facility_Level_Results,
    paste0(results_path, "Yearly_Facility_Level_Results_Chunk_", chunk_index, ".csv"),
    row.names = FALSE
  )
  
  # Summarizing monthly fossil fuel facilities generation data for this chunk
  Facility_Level_Results$Month <- as.numeric(format(as.Date(Facility_Level_Results$Date), "%m"))
  Monthly_Facility_Level_Results <- Facility_Level_Results[, .(
    total_generation_GWh = sum(Gen_MWh_adj, na.rm = TRUE) / 1e3,
    total_CO2_tons = sum(CO2_tons, na.rm = TRUE),
    total_NOx_lbs = sum(NOx_lbs, na.rm = TRUE),
    total_SO2_lbs = sum(SO2_lbs, na.rm = TRUE),
    total_HI_mmBtu = sum(HI_mmBtu, na.rm = TRUE)
  ), by = .(Year, Month, Pathway, Simulation, Facility_Unit.ID, Fuel_type_1)]
  
  fuel_unit_mapping <- list(
    "Pipeline Natural Gas" = "NG",
    "Other Oil" = "DFO",
    "Diesel Oil" = "DFO",
    "Residual Oil" = "RFO",
    "Coal" = "BIT",
    "Wood" = "WC",
    "Tire Derived Fuel" = "TDF"
  )
  
  map_fuel_unit <- function(primary_fuel) {
    return(fuel_unit_mapping[[primary_fuel]])
  }
  
  Monthly_Facility_Level_Results$ENERGY_SOURCE <- sapply(Monthly_Facility_Level_Results$Fuel_type_1, map_fuel_unit)
  
  write.csv(
    Monthly_Facility_Level_Results,
    paste0(results_path, "Monthly_Facility_Level_Results_Chunk_", chunk_index, ".csv"),
    row.names = FALSE
  )
}

# Function to process hourly files
process_hourly_files <- function(chunk_files, chunk_index) {
  Hourly_Results <- data.table()
  
  # Define the output path
  results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/"
  #results_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/Test Results/"

  
  # Read and process each file in the chunk
  for (i in seq_along(chunk_files)) {
    data <- fread(chunk_files[i])
    Hourly_Results <- rbind(Hourly_Results, data)
  }
  
  # Summarizing hourly generation data for this chunk
  # Get all numeric columns
  numeric_columns <- names(Hourly_Results)[sapply(Hourly_Results, is.numeric)]
  
  # Define exclusion patterns or exact names
  excluded_columns <- c("Hour", "DayLabel", "Year", "Simulation")
  excluded_patterns <- "_CF$"
  
  # Filter out the unwanted columns
  unit_columns <- numeric_columns[
    !numeric_columns %in% excluded_columns & 
      !grepl(excluded_patterns, numeric_columns)
  ]
  
  Yearly_Results <- Hourly_Results[, lapply(.SD, sum, na.rm = TRUE), by = .(Simulation, Year, Pathway), .SDcols = unit_columns]
  mw_columns <- grep("_MWh$", names(Yearly_Results), value = TRUE)
  Yearly_Results[, (mw_columns) := lapply(.SD, function(x) x / 1e6), .SDcols = mw_columns]
  setnames(Yearly_Results, old = mw_columns, new = sub("_MWh$", "_TWh", mw_columns))
  write.csv(Yearly_Results, paste0(results_path, "Yearly_Results_Chunk_", chunk_index, ".csv"))
  
  # Summarizing shortages for this chunk 
  Hourly_Results[, Shortage_ratio := Calibrated_Shortage_MWh / Demand]
  shortage_columns <- "Shortage_ratio"
  
  # Interpolate the $/MWh values for Shortage_ratio
  Hourly_Results[, Cost_USD_per_MWh := approx(Cost_Curve$Percentage, Cost_Curve$Cost_per_MWh, Shortage_ratio)$y]
  
  # Replace NA values in Cost_USD_per_MWh with zero
  Hourly_Results[is.na(Cost_USD_per_MWh), Cost_USD_per_MWh := 0]
  
  # Calculate total costs
  Hourly_Results[, Unmet_Demand_USD := Demand * Shortage_ratio * Cost_USD_per_MWh]
  
  # Summarize data for Sim, Pathway, year
  Yearly_Results_Shortages <- Hourly_Results[, .(
    Unmet_Demand_total_MWh = sum(Calibrated_Shortage_MWh, na.rm = TRUE), 
    Unmet_Demand_USD_total = sum(Unmet_Demand_USD, na.rm = TRUE)
  ), by = .(Simulation, Year, Pathway)]
  
  write.csv(Yearly_Results_Shortages, paste0(results_path, "Yearly_Results_Shortages_Chunk_", chunk_index, ".csv"))
}

# Process each chunk of hourly files
for (chunk_index in seq_along(hourly_chunks)) {
  process_hourly_files(hourly_chunks[[chunk_index]], chunk_index)
}

# Process each chunk of facility files
for (chunk_index in seq_along(facility_chunks)) {
  process_facility_files(facility_chunks[[chunk_index]], chunk_index)
}

# Load and combine intermediate results into final data sets
results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized"
yearly_files <- list.files(results_path, pattern = "Yearly_Results_Chunk_.*\\.csv$", full.names = TRUE)
yearly_facility_files <- list.files(results_path, pattern = "Yearly_Facility_Level_Results_Chunk_.*\\.csv$", full.names = TRUE)
monthly_facility_files <- list.files(results_path, pattern = "Monthly_Facility_Level_Results_Chunk_.*\\.csv$", full.names = TRUE)
shortages_files <- list.files(results_path, pattern = "Yearly_Results_Shortages_Chunk_.*\\.csv$", full.names = TRUE)

Yearly_Results <- rbindlist(lapply(yearly_files, fread))
Yearly_Facility_Level_Results <- rbindlist(lapply(yearly_facility_files, fread))
Monthly_Facility_Level_Results <- rbindlist(lapply(monthly_facility_files, fread))
Yearly_Results_Shortages <- rbindlist(lapply(shortages_files, fread))

# Save final combined datasets
results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/Final/"
write.csv(Yearly_Results, file.path(results_path, "Yearly_Results.csv"))
write.csv(Yearly_Facility_Level_Results, file.path(results_path, "Yearly_Facility_Level_Results.csv"))
write.csv(Monthly_Facility_Level_Results, file.path(results_path, "Monthly_Facility_Level_Results.csv"))
write.csv(Yearly_Results_Shortages, file.path(results_path, "Yearly_Results_Shortages.csv"))

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
