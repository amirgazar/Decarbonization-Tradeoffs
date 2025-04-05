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

# Load Capacity data
path <- "/home/amirgazar/Capacity_Expansion/Results/Stepwise"
files <- list.files(path = path, pattern = "\\.rds$", full.names = TRUE)

# Read all RDS files into a list and convert each to a data.table
rds_list <- lapply(files, function(file) {
  data <- readRDS(file)
  setDT(data)  # Convert to data.table
  return(data)
})

names(rds_list) <- basename(files)

# Calculate the split point for Facility and Hourly Results
split_point <- length(rds_list) / 2

# Lists to store Facility and Hourly Results
facility_list <- list()
hourly_list <- list()

# Automatically assign Simulation numbers and append to lists
for (i in 1:split_point) {
  facility_data <- rds_list[[i]][, Simulation := i]
  hourly_data <- rds_list[[split_point + i]][, Simulation := i]
  
  facility_list[[i]] <- facility_data
  hourly_list[[i]] <- hourly_data
}

# Pushover alert - Pushover credentials
pushover_user <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST("https://api.pushover.net/1/messages.json",
                 body = list(
                   token = pushover_token,
                   user = pushover_user,
                   message = "Summary code data loaded successfully"
                 ),
                 encode = "form")


# Combine all Facility_Level_Results and Hourly_Results into two data.tables
Facility_Level_Results <- rbindlist(facility_list)
Hourly_Results <- rbindlist(hourly_list)

# Free up memory by removing intermediate lists
rm(rds_list, facility_list, hourly_list)
gc()


## Summarizing hourly generation data
# Identify columns with units at the end (e.g., ending with '_MW', '_lbs', '_tons', '_mmBtu')
unit_columns <- grep("_MW$|_lbs$|_tons$|_mmBtu$", names(Hourly_Results), value = TRUE)

# Identify CF columns (ending with '_CF')
cf_columns <- grep("_CF$", names(Hourly_Results), value = TRUE)

# Identify logical columns
logical_columns <- names(Hourly_Results)[sapply(Hourly_Results, is.logical)]

# Identify columns with units at the end (e.g., ending with '_MW', '_lbs', '_tons', '_mmBtu')
unit_columns <- grep("_MW$|_lbs$|_tons$|_mmBtu$", names(Hourly_Results), value = TRUE)

# Summing only those columns by Simulation, Year, and Scenario
Yearly_Results <- Hourly_Results[, lapply(.SD, sum, na.rm = TRUE), by = .(Simulation, Year, Scenario), .SDcols = unit_columns]

# Convert MW columns to TWh and rename them
mw_columns <- grep("_MW$", names(Yearly_Results), value = TRUE)
Yearly_Results[, (mw_columns) := lapply(.SD, function(x) x / 1e6), .SDcols = mw_columns]
setnames(Yearly_Results, old = mw_columns, new = sub("_MW$", "_TWh", mw_columns))

# Define the file paths with unique identifiers
saveRDS(Yearly_Results, "/home/amirgazar/Capacity_Expansion/Results/Summary/Yearly_Results.rds")

# Identify Percentile columns
Percentile_columns <- grep("Percentile_", names(Hourly_Results), value = TRUE)

# Summing unit columns, mean of CF columns, and count of TRUE values for logical columns by Simulation, Year, and Scenario
Percentile_Results <- Hourly_Results[, c(
  lapply(.SD, mean, na.rm = TRUE)
), 
by = .(Simulation, Year, Scenario), 
.SDcols = c(Percentile_columns)]

# Identify logical columns
logical_columns <- names(Hourly_Results)[sapply(Hourly_Results, is.logical)]
Logical_Results <- Hourly_Results[, c(
  lapply(.SD, function(x) sum(x, na.rm = TRUE))
), 
by = .(Simulation, Year, Scenario), 
.SDcols = c(logical_columns)]


## Summarizing fossil fuel facilities generation data
Yearly_Facility_Level_Results <- Facility_Level_Results[, .(
  total_generation_GWh = sum(Fossil.gen_MW, na.rm = TRUE)/10e3,
  total_CO2_tons = sum(CO2_tons, na.rm = TRUE),
  total_NOx_lbs = sum(NOx_lbs, na.rm = TRUE),
  total_SO2_lbs = sum(SO2_lbs, na.rm = TRUE),
  total_HI_mmBtu = sum(HI_mmBtu, na.rm = TRUE)
), by = .(Year, Scenario, Simulation, Facility_Unit.ID)]

# Extract static information for each facility
Static_Facility_Info <- unique(Facility_Level_Results[, .(Facility_Unit.ID, State, Fuel_type_1, Fuel_type_2, latitude, longitude, Ramp_hr, Fossil.NPC_MW)])

# Merge static information with yearly results
Yearly_Facility_Level_Results <- merge(Yearly_Facility_Level_Results, Static_Facility_Info, by = "Facility_Unit.ID", all.x = TRUE)

# Define the file paths with unique identifiers
saveRDS(Yearly_Facility_Level_Results, "/home/amirgazar/Capacity_Expansion/Results/Summary/Yearly_Facility_Level_Results.rds")


# Monthly results
## Summarizing fossil fuel facilities generation data
Facility_Level_Results$Month <- as.numeric(format(as.Date(Facility_Level_Results$Date), "%m"))
Monthly_Facility_Level_Results <- Facility_Level_Results[, .(
  total_generation_GWh = sum(Fossil.gen_MW, na.rm = TRUE)/10e3,
  total_CO2_tons = sum(CO2_tons, na.rm = TRUE),
  total_NOx_lbs = sum(NOx_lbs, na.rm = TRUE),
  total_SO2_lbs = sum(SO2_lbs, na.rm = TRUE),
  total_HI_mmBtu = sum(HI_mmBtu, na.rm = TRUE)
), by = .(Year, Month, Scenario, Simulation, Facility_Unit.ID, Fuel_type_1)]

# Define the mapping for Primary.Fuel.Type to Fuel_Code
fuel_unit_mapping <- list(
  "Pipeline Natural Gas" = "NG",
  "Other Oil" = "DFO",
  "Diesel Oil" = "DFO",
  "Residual Oil" = "RFO",
  "Coal" = "BIT",
  "Wood" = "WC",
  "Tire Derived Fuel" = "TDF"
)
# Function to map Primary.Fuel.Type to Fuel_unit
map_fuel_unit <- function(primary_fuel) {
  return(fuel_unit_mapping[[primary_fuel]])
}

# Apply the mapping function to create Fuel_unit column
Monthly_Facility_Level_Results$ENERGY_SOURCE <- sapply(Monthly_Facility_Level_Results$Fuel_type_1, map_fuel_unit)

# Define the file paths with unique identifiers
saveRDS(Monthly_Facility_Level_Results, "/home/amirgazar/Capacity_Expansion/Results/Summary/Monthly_Facility_Level_Results.rds")


# Identify unmet demand
# Shortages
Hourly_Results[, Shortage_ratio := Shortage.total_hr_MW / Demand.total_hr_MW]
shortage_columns <- "Shortage_ratio"

# Count shortages and mean them
Yearly_Shortages_mean <- Hourly_Results[, lapply(.SD, function(x) {
  mean(x[x != 0], na.rm = TRUE)
}), by = .(Simulation, Year, Scenario), .SDcols = shortage_columns]
Yearly_Shortages_mean[is.na(Shortage_ratio), Shortage_ratio := 0]
# Rename columns to indicate mean calculation
new_mean_names <- paste0(shortage_columns, "_mean")
setnames(Yearly_Shortages_mean, shortage_columns, new_mean_names)

# Calculate the standard deviation for yearly shortages, excluding zeros
Yearly_Shortages_sd <- Hourly_Results[, lapply(.SD, function(x) {
  sd(x[x != 0], na.rm = TRUE)
}), by = .(Simulation, Year, Scenario), .SDcols = shortage_columns]

# Replace NA values in the 'Std' column with 0
Yearly_Shortages_sd[is.na(Shortage_ratio), Shortage_ratio := 0]

# Rename columns to indicate standard deviation calculation
new_sd_names <- paste0(shortage_columns, "_sd")
setnames(Yearly_Shortages_sd, shortage_columns, new_sd_names)

Yearly_Shortages_mean <- merge(Yearly_Shortages_mean, Yearly_Shortages_sd, by = c("Simulation", "Year", "Scenario"), all.x = TRUE)

# Create a logical vector indicating rows where any of the shortage columns are non-zero
non_zero_hours <- Hourly_Results[, rowSums(.SD != 0) > 0, .SDcols = shortage_columns]

# Filter Hourly_Results to only include rows where non_zero_hours is TRUE
filtered_Hourly_Results <- Hourly_Results[non_zero_hours]
Shortage_Counts <- filtered_Hourly_Results[Shortage_ratio > 0, .N, by = .(Simulation, Year, Scenario)]
setnames(Shortage_Counts, "N", "Shortage_Count")

# Calculate the mean of Demand.total_hr_MW for each group
Yearly_Demand_mean <- filtered_Hourly_Results[, .(Demand.mean_yr_MW = mean(Demand.total_hr_MW, na.rm = TRUE)), 
                                              by = .(Simulation, Year, Scenario)]

# Merge the results back to the original Yearly_Shortages_mean if needed
Yearly_Shortages <- merge(Yearly_Shortages_mean, Yearly_Demand_mean, by = c("Simulation", "Year", "Scenario"), all.x = TRUE)
Yearly_Shortages[is.na(Demand.mean_yr_MW), Demand.mean_yr_MW := 0]

# Merge the count and mean shortage ratio data tables
Yearly_Results_Shortages <- merge(Yearly_Shortages, Shortage_Counts, by = c("Simulation", "Year", "Scenario"), all.x = TRUE)
Yearly_Results_Shortages[is.na(Shortage_Count), Shortage_Count := 0]

file_path <- "/home/amirgazar/Capacity_Expansion/Results/Summary/Yearly_Results_Shortages.rds"
saveRDS(Yearly_Results_Shortages, file_path)


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

