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

# Load EVOLL curve
path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/evoll_curve.csv"
Cost_Curve <- fread(path)

# Old/existing fossil fuels
path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(path)
# Remove unnecessary columns from Fossil_Fuels_NPC
Fossil_Fuels_NPC <- Fossil_Fuels_NPC[, .(
  Facility_Unit.ID, 
  State, 
  Fuel_type_1 = Primary_Fuel_Type, 
  Fuel_type_2 = Secondary_Fuel_Type, 
  latitude = Latitude, 
  longitude = Longitude, 
  Ramp_hr = Ramp, 
  Fossil.NPC_MW = Estimated_NameplateCapacity_MW, 
  Max_Hourly_HI_Rate
)]

# Define the results summary directory used later in the script
summary_results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/"

# --- STEP: Identify already processed simulation+pathway combinations ---
# This helper function reads existing summary CSVs (for a given pattern) and extracts unique Simulation||Pathway keys.
get_existing_simulation_pathways <- function(pattern, path = summary_results_path) {
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  keys <- character()
  if (length(files) > 0) {
    for (file in files) {
      dt <- fread(file, select = c("Simulation", "Pathway"))
      file_keys <- paste(dt$Simulation, dt$Pathway, sep = "||")
      keys <- unique(c(keys, file_keys))
    }
  }
  return(keys)
}

# Get existing (Simulation, Pathway) keys from summary outputs.
existing_sim_hourly <- get_existing_simulation_pathways("Yearly_Results_Chunk_.*\\.csv")
existing_sim_facility <- get_existing_simulation_pathways("Yearly_Facility_Level_Results_Chunk_.*\\.csv")

# --- Load Capacity data ---
data_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/1 Stepwise"
files <- list.files(path = data_path, pattern = "\\.csv$", full.names = TRUE)

# Separate files based on type
facility_files_all <- grep("Facility_Level_Results", files, value = TRUE)
hourly_files_all <- grep("Hourly_Results", files, value = TRUE)

# --- NEW FUNCTION: Filter files based on Simulation and Pathway ---
filter_files_by_sim_pathway <- function(file_list, existing_keys) {
  filtered <- character(0)
  for (f in file_list) {
    dt <- fread(f, nrows = 1, select = c("Simulation", "Pathway"))
    key <- paste(dt$Simulation[1], dt$Pathway[1], sep = "||")
    if (!(key %in% existing_keys)) {
      filtered <- c(filtered, f)
    }
  }
  return(filtered)
}

# Filter out files already processed
facility_files <- filter_files_by_sim_pathway(facility_files_all, existing_sim_facility)
hourly_files   <- filter_files_by_sim_pathway(hourly_files_all, existing_sim_hourly)

# --- Split files into chunks (only with files that haven't been processed) ---
facility_chunks <- split(facility_files, ceiling(seq_along(facility_files) / 10))
hourly_chunks   <- split(hourly_files, ceiling(seq_along(hourly_files) / 200))

# Function to process facility files (with memory cleanup)
process_facility_files <- function(chunk_files, chunk_index) {
  results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/"
  Facility_Level_Results <- data.table()
  
  # Process each file in the chunk
  for (i in seq_along(chunk_files)) {
    temp_data <- fread(chunk_files[i])
    temp_data[, Year := year(Date)]
    Facility_Level_Results <- rbind(Facility_Level_Results, temp_data)
    # Remove temp_data from memory after processing each file
    rm(temp_data)
    gc()
  }
  
  # Merge with fossil fuels data and perform calculations
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
  
  Static_Facility_Info <- unique(Facility_Level_Results[, .(
    Facility_Unit.ID, State, Fuel_type_1, Fuel_type_2, latitude, longitude, 
    Ramp_hr, Fossil.NPC_MW
  )])
  Yearly_Facility_Level_Results <- merge(Yearly_Facility_Level_Results, 
                                         Static_Facility_Info, by = "Facility_Unit.ID", 
                                         all.x = TRUE)
  
  # Write the summarized facility-level results
  outfile1 <- paste0(results_path, "Yearly_Facility_Level_Results_Chunk_", chunk_index, ".csv")
  write.csv(Yearly_Facility_Level_Results, outfile1, row.names = FALSE)
  
  # Also, summarize monthly results
  Facility_Level_Results[, Month := as.numeric(format(as.Date(Date), "%m"))]
  Monthly_Facility_Level_Results <- Facility_Level_Results[, .(
    total_generation_GWh = sum(Gen_MWh_adj, na.rm = TRUE) / 1e3,
    total_CO2_tons = sum(CO2_tons, na.rm = TRUE),
    total_NOx_lbs = sum(NOx_lbs, na.rm = TRUE),
    total_SO2_lbs = sum(SO2_lbs, na.rm = TRUE),
    total_HI_mmBtu = sum(HI_mmBtu, na.rm = TRUE)
  ), by = .(Year, Month, Pathway, Simulation, Facility_Unit.ID, Fuel_type_1)]
  
  # Map fuel types to energy source codes
  fuel_unit_mapping <- list(
    "Pipeline Natural Gas" = "NG",
    "Other Oil" = "DFO",
    "Diesel Oil" = "DFO",
    "Residual Oil" = "RFO",
    "Coal" = "BIT",
    "Wood" = "WC",
    "Tire Derived Fuel" = "TDF"
  )
  Monthly_Facility_Level_Results[, ENERGY_SOURCE := sapply(Fuel_type_1, 
                                                           function(x) fuel_unit_mapping[[x]])]
  
  outfile2 <- paste0(results_path, "Monthly_Facility_Level_Results_Chunk_", chunk_index, ".csv")
  write.csv(Monthly_Facility_Level_Results, outfile2, row.names = FALSE)
  
  # Remove large intermediate objects before finishing
  rm(Facility_Level_Results, Yearly_Facility_Level_Results, Monthly_Facility_Level_Results, Static_Facility_Info)
  gc()
}

# Function to process hourly files (with memory cleanup)
process_hourly_files <- function(chunk_files, chunk_index) {
  results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/"
  Hourly_Results <- data.table()
  
  # Process each hourly file in this chunk
  for (i in seq_along(chunk_files)) {
    temp_data <- fread(chunk_files[i])
    Hourly_Results <- rbind(Hourly_Results, temp_data)
    rm(temp_data)  # Remove temporary object after merging
    gc()
  }
  
  # Summarize hourly data into yearly totals
  numeric_columns <- names(Hourly_Results)[sapply(Hourly_Results, is.numeric)]
  excluded_columns <- c("Hour", "DayLabel", "Year", "Simulation")
  excluded_patterns <- "_CF$"
  
  unit_columns <- numeric_columns[!numeric_columns %in% excluded_columns & 
                                    !grepl(excluded_patterns, numeric_columns)]
  
  Yearly_Results <- Hourly_Results[, lapply(.SD, sum, na.rm = TRUE), 
                                   by = .(Simulation, Year, Pathway), .SDcols = unit_columns]
  
  # Adjust energy units as required
  mw_columns <- grep("_MWh$", names(Yearly_Results), value = TRUE)
  Yearly_Results[, (mw_columns) := lapply(.SD, function(x) x / 1e6), .SDcols = mw_columns]
  setnames(Yearly_Results, old = mw_columns, new = sub("_MWh$", "_TWh", mw_columns))
  
  outfile <- paste0(results_path, "Yearly_Results_Chunk_", chunk_index, ".csv")
  write.csv(Yearly_Results, outfile)
  
  # Process shortages for this chunk
  Hourly_Results[, Shortage_ratio := Calibrated_Shortage_MWh / Demand]
  Hourly_Results[, Cost_USD_per_MWh := approx(Cost_Curve$Percentage, 
                                              Cost_Curve$Cost_per_MWh, 
                                              Shortage_ratio)$y]
  Hourly_Results[is.na(Cost_USD_per_MWh), Cost_USD_per_MWh := 0]
  Hourly_Results[, Unmet_Demand_USD := Demand * Shortage_ratio * Cost_USD_per_MWh]
  
  Yearly_Results_Shortages <- Hourly_Results[, .(
    Unmet_Demand_total_MWh = sum(Calibrated_Shortage_MWh, na.rm = TRUE), 
    Unmet_Demand_USD_total = sum(Unmet_Demand_USD, na.rm = TRUE)
  ), by = .(Simulation, Year, Pathway)]
  
  outfile_short <- paste0(results_path, "Yearly_Results_Shortages_Chunk_", chunk_index, ".csv")
  write.csv(Yearly_Results_Shortages, outfile_short)
  
  # Clean up memory by removing the large Hourly_Results data table
  rm(Hourly_Results, Yearly_Results, Yearly_Results_Shortages)
  gc()
}

# --- Process each chunk of hourly files ---
#for (chunk_index in seq_along(hourly_chunks)) {
#  process_hourly_files(hourly_chunks[[chunk_index]], chunk_index)
  # Optionally remove the processed chunk from the list if no longer needed.
#  rm(hourly_chunks[[chunk_index]])
#  gc()
#}

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

# --- Process each chunk of facility files ---
for (chunk_index in seq_along(facility_chunks)) {
  process_facility_files(facility_chunks[[chunk_index]], chunk_index)
  gc()
}

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
