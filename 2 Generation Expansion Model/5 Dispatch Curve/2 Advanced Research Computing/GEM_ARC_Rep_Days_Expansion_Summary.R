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


#---summarizing data
# Load EVOLL curve
path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/evoll_curve.csv"
Cost_Curve <- fread(path)
# Old/existing fossil fuels
path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(path)
# Remove unnecessary columns from Fossil_Fuels_NPC
Fossil_Fuels_NPC <- Fossil_Fuels_NPC[, .(Facility_Unit.ID, Max_Hourly_HI_Rate)]

# Load Prepared Capacity data
path <- "/projects/epadecarb/Decarb_Paper/Results/Rep_Days/"
files <- list.files(path = path, pattern = "\\.csv$", full.names = TRUE)

# Separate files based on type
facility_files <- grep("Facility_Level_Results_rep_days", files, value = TRUE)
hourly_files <- grep("Hourly_Results_NE_rep_days", files, value = TRUE)

# Split files into chunks of 10
facility_chunks <- split(facility_files, ceiling(seq_along(facility_files)))
hourly_chunks <- split(hourly_files, ceiling(seq_along(hourly_files)))

# Function to process facility files
process_facility_files <- function(chunk_files, chunk_index) {
  Facility_Level_Results <- data.table()
  
  # Read and process each file in the chunk
  for (i in seq_along(chunk_files)) {
    data <- fread(chunk_files[i])
    Facility_Level_Results <- rbind(Facility_Level_Results, data)
  }
  
  # Summarizing fossil fuel facilities generation data for this chunk
  Facility_Level_Results$HI_mmBtu <- as.numeric(Facility_Level_Results$HI_mmBtu)
  # Merge datasets and calculate total_HI_mmBtu
  Facility_Level_Results <- merge(
    Facility_Level_Results,
    Fossil_Fuels_NPC,
    by = "Facility_Unit.ID"
  )
  Facility_Level_Results[Max_Hourly_HI_Rate < HI_mmBtu, HI_mmBtu := Max_Hourly_HI_Rate]
  
  Yearly_Facility_Level_Results <- Facility_Level_Results[, .(
    total_generation_GWh = sum(Fossil.gen_MW, na.rm = TRUE) / 10e3,
    total_CO2_tons = sum(CO2_tons, na.rm = TRUE),
    total_NOx_lbs = sum(NOx_lbs, na.rm = TRUE),
    total_SO2_lbs = sum(SO2_lbs, na.rm = TRUE),
    total_HI_mmBtu = sum(HI_mmBtu, na.rm = TRUE)
  ), by = .(Year, Pathway, Simulation, Facility_Unit.ID)]
  
  Static_Facility_Info <- unique(Facility_Level_Results[, .(Facility_Unit.ID, State, Fuel_type_1, Fuel_type_2, latitude, longitude, Ramp_hr, Fossil.NPC_MW)])
  Yearly_Facility_Level_Results <- merge(Yearly_Facility_Level_Results, Static_Facility_Info, by = "Facility_Unit.ID", all.x = TRUE)
  write.csv(Yearly_Facility_Level_Results, paste0("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Yearly_Facility_Level_Results_Chunk_", chunk_index, ".csv"))
  
  # Summarizing monthly fossil fuel facilities generation data for this chunk
  Facility_Level_Results$Month <- as.numeric(format(as.Date(Facility_Level_Results$Date), "%m"))
  Monthly_Facility_Level_Results <- Facility_Level_Results[, .(
    total_generation_GWh = sum(Fossil.gen_MW, na.rm = TRUE) / 10e3,
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
  write.csv(Monthly_Facility_Level_Results, paste0("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Monthly_Facility_Level_Results_Chunk_", chunk_index, ".csv"))
}

# Function to process hourly files
process_hourly_files <- function(chunk_files, chunk_index) {
  Hourly_Results <- data.table()
  
  # Read and process each file in the chunk
  for (i in seq_along(chunk_files)) {
    data <- fread(chunk_files[i])
    Hourly_Results <- rbind(Hourly_Results, data)
  }
  Hourly_Results$HI.total_hr_mmBtu <- as.numeric(Hourly_Results$HI.total_hr_mmBtu)
  
  # Summarizing hourly generation data for this chunk
  unit_columns <- grep("_MW$|_lbs$|_tons$|_mmBtu$", names(Hourly_Results), value = TRUE)
  Yearly_Results <- Hourly_Results[, lapply(.SD, sum, na.rm = TRUE), by = .(Simulation, Year, Pathway), .SDcols = unit_columns]
  mw_columns <- grep("_MW$", names(Yearly_Results), value = TRUE)
  Yearly_Results[, (mw_columns) := lapply(.SD, function(x) x / 1e6), .SDcols = mw_columns]
  setnames(Yearly_Results, old = mw_columns, new = sub("_MW$", "_TWh", mw_columns))
  write.csv(Yearly_Results, paste0("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Yearly_Results_Chunk_", chunk_index, ".csv"))
  
  Percentile_columns <- grep("Percentile_", names(Hourly_Results), value = TRUE)
  Percentile_Results <- Hourly_Results[, lapply(.SD, mean, na.rm = TRUE), by = .(Simulation, Year, Pathway), .SDcols = Percentile_columns]
  write.csv(Percentile_Results, paste0("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Percentile_Results_Chunk_", chunk_index, ".csv"))
  
  logical_columns <- names(Hourly_Results)[sapply(Hourly_Results, is.logical)]
  Logical_Results <- Hourly_Results[, lapply(.SD, function(x) sum(x, na.rm = TRUE)), by = .(Simulation, Year, Pathway), .SDcols = logical_columns]
  write.csv(Logical_Results, paste0("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Logical_Results_Chunk_", chunk_index, ".csv"))
  
  # Total shortage costs
  Hourly_Results[, Shortage_ratio := Shortage.total_hr_MW / Demand.total_hr_MW]
  shortage_columns <- "Shortage_ratio"

  # Interpolate the $/MWh values for Shortage_ratio
  Hourly_Results[, Cost_USD_per_MWh := approx(Cost_Curve$Percentage, Cost_Curve$Cost_per_MWh, Shortage_ratio)$y]
  
  # Replace NA values in Cost_USD_per_MWh with zero
  Hourly_Results[is.na(Cost_USD_per_MWh), Cost_USD_per_MWh := 0]
  
  # Calculate total costs
  Hourly_Results[, Unmet_Demand_USD := Demand.total_hr_MW * Shortage_ratio * Cost_USD_per_MWh]
  
  # Summarize data for Sim, Pathway, year
  Yearly_Results_Shortages <- Hourly_Results[, .(
    Unmet_Demand_total_MWh = sum(Shortage.total_hr_MW, na.rm = TRUE), 
    Unmet_Demand_USD_total = sum(Unmet_Demand_USD, na.rm = TRUE)
  ), by = .(Simulation, Year, Pathway)]
  
  write.csv(Yearly_Results_Shortages, paste0("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Yearly_Results_Shortages_Chunk_", chunk_index, ".csv"))
}

# Process each chunk of facility files
for (chunk_index in seq_along(facility_chunks)) {
  process_facility_files(facility_chunks[[chunk_index]], chunk_index)
}

# Process each chunk of hourly files
for (chunk_index in seq_along(hourly_chunks)) {
  process_hourly_files(hourly_chunks[[chunk_index]], chunk_index)
}

# Load and combine intermediate results into final data sets
yearly_files <- list.files("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days", pattern = "Yearly_Results_Chunk_.*\\.csv$", full.names = TRUE)
percentile_files <- list.files("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days", pattern = "Percentile_Results_Chunk_.*\\.csv$", full.names = TRUE)
logical_files <- list.files("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days", pattern = "Logical_Results_Chunk_.*\\.csv$", full.names = TRUE)
yearly_facility_files <- list.files("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days", pattern = "Yearly_Facility_Level_Results_Chunk_.*\\.csv$", full.names = TRUE)
monthly_facility_files <- list.files("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days", pattern = "Monthly_Facility_Level_Results_Chunk_.*\\.csv$", full.names = TRUE)
shortages_files <- list.files("/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days", pattern = "Yearly_Results_Shortages_Chunk_.*\\.csv$", full.names = TRUE)

Yearly_Results <- rbindlist(lapply(yearly_files, fread))
Percentile_Results <- rbindlist(lapply(percentile_files, fread))
Logical_Results <- rbindlist(lapply(logical_files, fread))
Yearly_Facility_Level_Results <- rbindlist(lapply(yearly_facility_files, fread))
Monthly_Facility_Level_Results <- rbindlist(lapply(monthly_facility_files, fread))
Yearly_Results_Shortages <- rbindlist(lapply(shortages_files, fread))

# Save final combined datasets
write.csv(Yearly_Results, "/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Final/Yearly_Results_rep_days.csv")
write.csv(Percentile_Results, "/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Final/Percentile_Results_rep_days.csv")
write.csv(Logical_Results, "/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Final/Logical_Results_rep_days.csv")
write.csv(Yearly_Facility_Level_Results, "/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Final/Yearly_Facility_Level_Results_rep_days.csv")
write.csv(Monthly_Facility_Level_Results, "/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Final/Monthly_Facility_Level_Results_rep_days.csv")
write.csv(Yearly_Results_Shortages, "/projects/epadecarb/Decarb_Paper/Results/Summary/Rep_Days/Final/Yearly_Results_rep_days_Shortages.csv")

# Pushover alert - Pushover credentials
pushover_user <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST("https://api.pushover.net/1/messages.json",
                 body = list(
                   token = pushover_token,
                   user = pushover_user,
                   message = "The Summary_code_rep_days executed."
                 ),
                 encode = "form")
