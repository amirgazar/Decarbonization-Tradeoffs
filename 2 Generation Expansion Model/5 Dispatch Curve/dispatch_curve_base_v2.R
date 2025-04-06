# Dispatch curve, offsetting demand with generation
library(data.table)
library(httr)
library(htmltools)
library(jsonlite)
library(lubridate)

# Read necessary files
## 1. Installed Capacities and Facilities Data
# 1.1 Capacity for Wind, Solar, Large Nuclear, Hydro, Bio, Battery, SMRs, Imports and New Natural Gas
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/1 Decarbonization Pathways/Hourly_Installed_Capacity.csv"
Hourly_Installed_Capacity <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/2 SMR/1 SMR Facility Data/SMR_Facility_Data.csv"
SMR_Facility_Data <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/3 Large nuclear hydro and bio/1 Clean Baseload Facility Data/CleanBaseload_Facility_Data.csv"
CleanBaseload_Facility_Data <- fread(file_path)

# 1.1 Fossil Fuels Data
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv"
New_Fossil_Fuels_NPC <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/2 Fossil Fuels Generation and Emissions/Fossil_Fuel_hr_maximums.csv"
Fossil_Fuels_hr_max <- fread(file_path)

## 2. Probabilistic Generation Data and Capacity Factors
# 2.1 Wind and Solar
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/1 Wind and Solar/1 Wind and Solar CF/offwind_CF.csv"
Offwind_CF <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/1 Wind and Solar/1 Wind and Solar CF/onwind_CF.csv"
Onwind_CF <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/1 Wind and Solar/1 Wind and Solar CF/solar_CF.csv"
Solar_CF <- fread(file_path)

# 2.2 Fossil fuels
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/2 Fossil Fuels Generation and Emissions/Fossil_Fuel_Generation_Emissions.csv"
Fossil_Fuels_Gen <- fread(file_path)

# 2.3 Imports
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/3 Imports/1 Imports CF/Imports_CF.csv"
Imports_CF <- fread(file_path)

## 3. Demand 
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/demand_data.csv"
Demand_data <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/peak_demand_data.csv"
Peak_demand <- fread(file_path)

## 4. Random Sequence
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/4 Randomization/1 Randomized Data/Random_Sequence.csv"
Random_sequence <- fread(file_path)

## 5. Set Keys for data.tables
setkey(Hourly_Installed_Capacity, Year, DayLabel, Hour, Pathway)
setkey(Solar_CF, DayLabel, Hour, Percentile)
setkey(Onwind_CF, DayLabel, Hour, Percentile)
setkey(Offwind_CF, DayLabel, Hour, Percentile)
setkey(Imports_CF, DayLabel, Percentile)
setkey(Fossil_Fuels_Gen, DayLabel, Hour, Facility_Unit.ID)
setkey(Demand_data, Date, Hour)
setkey(Peak_demand, Date)
setkey(Fossil_Fuels_hr_max, Date, Hour)


# Fossil Fuel Sampling Function
library(data.table)

fossil_sampler_by_hour <- function(dispatch_data, idx_fossil) {
  # Convert dispatch_data and fossil data to data.table if they aren't already
  dispatch_dt <- as.data.table(dispatch_data)
  fossil_dt   <- as.data.table(Fossil_Fuels_Gen)
  
  # Create a master list of fossil facility IDs from your NPC table.
  unique_units <- unique(Fossil_Fuels_NPC$Facility_Unit.ID)
  
  # Define the variable prefixes to sample.
  var_prefixes <- c("Gen_", "CO2_", "NOx_", "HI_")
  
  # Loop over each row (each time step) in dispatch data.
  results <- lapply(seq_len(nrow(dispatch_dt)), function(i) {
    # Get current time from dispatch row.
    curr_day  <- dispatch_dt[i, DayLabel]
    curr_hour <- dispatch_dt[i, Hour]
    
    # Subset fossil data for the current DayLabel and Hour.
    fossil_subset <- fossil_dt[DayLabel == curr_day & Hour == curr_hour]
    if (nrow(fossil_subset) == 0) return(NULL)
    
    # For each facility present at this time step:
    facility_results <- lapply(unique(fossil_subset$Facility_Unit.ID), function(unit) {
      # Take the first available row for the facility.
      facility_row <- fossil_subset[Facility_Unit.ID == unit][1]
      
      # Determine the facility's index in the master list.
      unit_idx <- which(unique_units == unit)
      if (length(unit_idx) == 0) return(NULL)
      
      # Get the precomputed random index for this facility at this hour.
      rand_idx <- idx_fossil[i, unit_idx]
      
      # For each variable prefix, use lapply to sample a value.
      sampled_vals <- lapply(var_prefixes, function(prefix) {
        # Find columns that start with the prefix.
        cols <- grep(paste0("^", prefix), names(facility_row), value = TRUE)
        if (length(cols) == 0) return(NA_real_)
        # Adjust rand_idx to the number of columns available.
        col_idx <- ((rand_idx - 1) %% length(cols)) + 1
        as.numeric(facility_row[[cols[col_idx]]])
      })
      
      # Name the sampled values as Sampled_Gen, Sampled_CO2, etc.
      names(sampled_vals) <- paste0("Sampled_", sub("_$", "", var_prefixes))
      
      # Return a one-row data.table with the time, facility ID, and sampled values.
      data.table(
        DayLabel = curr_day,
        Hour = curr_hour,
        Facility_Unit.ID = unit,
        sampled_vals
      )
    })
    
    # Combine the facility results for the current time step.
    rbindlist(facility_results)
  })
  
  # Combine all time steps into one data.table.
  result_dt <- rbindlist(results, fill = TRUE)
  return(result_dt)
}

fossil_constraints <- 

# Example usage:
sampled_fossil <- fossil_sampler_by_hour(dispatch_data, idx_fossil)

# Dispatch Curve
dispatch_curve <- function(sim, pathway) {
  
  # --- Step 1: Filter and Merge Data ---
  # Filter capacity data for the chosen pathway and year 2025.
  cap_data <- Hourly_Installed_Capacity[Pathway == pathway & Year >= 2025]
  
  # Merge with Demand_data on Date and Hour.
  dispatch_data <- merge(Demand_data, cap_data, by = c("Date", "Hour"))
  
  # Order by Date and Hour
  setorder(dispatch_data, Date, Hour)
  
  # --- Step 2: Extract Random Percentile Indices ---
  random_vector <- Random_sequence[[sim]]
  n_random <- length(random_vector)
  
  # Number of hours (rows) in the dispatch simulation
  n_hours <- nrow(dispatch_data)
  
  # Fossil fuels
  unique_units <- unique(Fossil_Fuels_NPC$Facility_Unit.ID)
  n_units <- length(unique_units)
  
  n_total_rand <- 6 + n_units
  
  # For each hour we need 6 random numbers.
  # Compute a base index for each hour; note that these indices are 0-indexed.
  base_indices <- ((0:(n_hours - 1)) * n_total_rand)
  
  # For each asset, compute the index into the random_vector (using modulo to cycle)
  idx_solar    <- (base_indices + 0) %% n_random + 1
  idx_onshore  <- (base_indices + 1) %% n_random + 1
  idx_offshore <- (base_indices + 2) %% n_random + 1
  idx_impQC    <- (base_indices + 3) %% n_random + 1
  idx_impNYISO <- (base_indices + 4) %% n_random + 1
  idx_impNBSO  <- (base_indices + 5) %% n_random + 1
  
  shift_values <- 6:(6 + n_units - 1) #fossil 
  idx_fossil <- sapply(shift_values, function(s) (base_indices + s) %% n_random + 1)
  
  # Assign the random numbers as the percentile indices for each asset.
  # (Assumes the random_vector values are integers corresponding to available percentiles.)
  dispatch_data[, Percentile_Solar    := random_vector[idx_solar]]
  dispatch_data[, Percentile_Onshore  := random_vector[idx_onshore]]
  dispatch_data[, Percentile_Offshore := random_vector[idx_offshore]]
  dispatch_data[, Percentile_ImpQC    := random_vector[idx_impQC]]
  dispatch_data[, Percentile_ImpNYISO := random_vector[idx_impNYISO]]
  dispatch_data[, Percentile_ImpNBSO  := random_vector[idx_impNBSO]]

  # --- Step 3: Lookup CF Values from the CF Datasets ---
  # (Assumes that the CF datasets are keyed on DayLabel, Hour, and Percentile)
  # Solar CF lookup
  dispatch_data <- merge(dispatch_data, 
                         Solar_CF[, .(DayLabel, Hour, Percentile, CF)],
                         by.x = c("DayLabel", "Hour", "Percentile_Solar"),
                         by.y = c("DayLabel", "Hour", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "CF", "Solar_CF")
  
  # Onshore Wind CF lookup
  dispatch_data <- merge(dispatch_data, 
                         Onwind_CF[, .(DayLabel, Hour, Percentile, CF)],
                         by.x = c("DayLabel", "Hour", "Percentile_Onshore"),
                         by.y = c("DayLabel", "Hour", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "CF", "Onshore_CF")
  
  # Offshore Wind CF lookup
  dispatch_data <- merge(dispatch_data, 
                         Offwind_CF[, .(DayLabel, Hour, Percentile, CF)],
                         by.x = c("DayLabel", "Hour", "Percentile_Offshore"),
                         by.y = c("DayLabel", "Hour", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "CF", "Offshore_CF")
  
  

  # Imports CF lookups (assumes Imports_CF is keyed by DayLabel and Percentile)
  # Import QC
  dispatch_data <- merge(dispatch_data, 
                         Imports_CF[, .(DayLabel, Percentile, imports_QC)],
                         by.x = c("DayLabel", "Percentile_ImpQC"),
                         by.y = c("DayLabel", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "imports_QC", "Import_QC_CF")
  
  # Import NYISO
  dispatch_data <- merge(dispatch_data, 
                         Imports_CF[, .(DayLabel, Percentile, imports_NYISO)],
                         by.x = c("DayLabel", "Percentile_ImpNYISO"),
                         by.y = c("DayLabel", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "imports_NYISO", "Import_NYISO_CF")
  
  # Import NBSO
  dispatch_data <- merge(dispatch_data, 
                         Imports_CF[, .(DayLabel, Percentile, imports_NBSO)],
                         by.x = c("DayLabel", "Percentile_ImpNBSO"),
                         by.y = c("DayLabel", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "imports_NBSO", "Import_NBSO_CF")
  

  # --- Step 4: Calculate Clean Generation ---
  # For variable renewables, multiply installed capacity by the looked-up CF values.
  dispatch_data[, Solar_MWh    := Solar * Solar_CF]
  dispatch_data[, Onshore_MWh  := `Onshore Wind` * Onshore_CF]
  dispatch_data[, Offshore_MWh := `Offshore Wind` * Offshore_CF]
  
  # For baseload assets, use fixed CF values from CleanBaseload_Facility_Data.
  nuclear_CF <- as.numeric(CleanBaseload_Facility_Data[technology == "Nuclear", value])
  hydro_CF   <- as.numeric(CleanBaseload_Facility_Data[technology == "Hydropower", value])
  bio_CF     <- as.numeric(CleanBaseload_Facility_Data[technology == "Biopower", value])
  
  dispatch_data[, Nuclear_MWh := Nuclear * nuclear_CF]
  dispatch_data[, Hydro_MWh   := Hydropower * hydro_CF]
  dispatch_data[, Biomass_MWh := Biomass * bio_CF]
  
  # Maxmimum Imports CF
  imports_max_CF <- 0.95
  
  # SMRs
  SMR_CF <- SMR_Facility_Data$CF
  dispatch_data[, SMR_MWh := SMR * SMR_CF]

  # Sum clean generation from all sources.
  dispatch_data[, Clean_MWh := Solar_MWh + Onshore_MWh + Offshore_MWh +
                  Nuclear_MWh + Hydro_MWh + Biomass_MWh + SMR_MWh]
  
  # New Fossil Fuels
  New_Fossil_Fuels_NPC <- New_Fossil_Fuels_NPC[1]
  NFF_CF <- New_Fossil_Fuels_NPC$CF
  dispatch_data[, New_Fossil_Fuel_MWh := `New NG` * NFF_CF]
  
  # Old Fossil Fuels
  Old_Fossil_CF <- fossil_sampler(dispatch_data, idx_fossil)
  
  
  # --- Step 5: Calculate Fossil Fuel and Import Generation Requirements ---
  # Order by Date and Hour
  setorder(dispatch_data, Date, Hour)
  
  # Calculate import generation using the looked-up CFs.
  dispatch_data[, Import_QC    := `Imports QC` * Import_QC_CF]
  dispatch_data[, Import_NYISO := `Imports NYISO` * Import_NYISO_CF]
  dispatch_data[, Import_NBSO  := `Imports NBSO` * Import_NBSO_CF]
  dispatch_data[, Total_import_MWh := Import_QC + Import_NYISO + Import_NBSO]
  
  
  # Fossil fuel generation is the residual if clean generation does not meet demand.
  dispatch_data[, Fossil_required_MWh := pmax(Demand - Clean_MWh - Total_import_MWh, 0)]
  
  
  # --- Step 6: Battery Storage Integration ---
  # Here we simulate battery operations:
  #   - When Clean_MWh exceeds Demand, surplus energy charges the battery.
  #   - When Demand exceeds Clean_MWh, the battery discharges to help meet demand.
  #
  # We assume that the maximum battery capacity is given in the "Storage" column
  # (from the capacity data) and that the initial storage level is provided.
  # Initial storage state (in energy units, e.g. MWh)
  # Initial storage state (in energy units, e.g. MWh)
  storage_status_initial <- 0
  
  # Define round-trip efficiency and derive charging/discharging efficiency.
  rt_eff <- 0.85
  eta <- sqrt(rt_eff)  # ~0.922
  
  # Compute net energy balance per hour (positive = surplus; negative = deficit)
  dispatch_data[, net_energy := Clean_MWh - Demand]
  
  # Optionally, enforce a row-specific power limit.
  # For an 8‑hour battery, the max charge/discharge per hour is (row capacity)/8.
  dispatch_data[, battery_power_limit := Storage / 8]
  dispatch_data[, net_energy := pmin(pmax(net_energy, -battery_power_limit), battery_power_limit)]
  
  # Use Reduce to compute the storage state row-by-row.
  # We iterate over row indices so that each iteration can access the corresponding capacity.
  storage_status_vec <- Reduce(
    function(prev, i) {
      net <- dispatch_data$net_energy[i]
      capacity <- dispatch_data$Storage[i]
      
      # Apply efficiency: if charging (net >= 0), use charging efficiency; if discharging, adjust accordingly.
      new_storage <- if (net >= 0) {
        prev + net * eta
      } else {
        prev + net / eta
      }
      
      # Constrain the new storage to lie between 0 and the current row's capacity.
      min(max(new_storage, 0), capacity)
    },
    seq_len(nrow(dispatch_data)),
    init = storage_status_initial,
    accumulate = TRUE
  )[-1]  # Remove the initial value
  
  # Update the data table with the computed storage state.
  dispatch_data[, Storage_status := storage_status_vec]
  
  # Compute battery flow (change in storage from the previous hour).
  dispatch_data[, Battery_flow := c(0, diff(Storage_status))]
  
  # Separate battery flow into charging and discharging components.
  dispatch_data[, Battery_charge := ifelse(Battery_flow > 0, Battery_flow, 0)]
  dispatch_data[, Battery_discharge := ifelse(Battery_flow < 0, -Battery_flow, 0)]
  
  # Now update fossil fuel requirements.
  # Effective generation is Clean_MWh plus contributions from battery discharge and imports.
  dispatch_data[, Shortage_MWh := pmax(Demand - (Clean_MWh + Battery_discharge + 
                                                  Total_import_MWh + Old_Fossil_Fuels_hr_max_MWh + New_Fossil_Fuel_MWh), 0)]
  
  # Optionally, remove temporary net_energy column.
  dispatch_data[, c("net_energy", "Percentile_ImpNBSO", "Percentile_ImpNYISO", 
                    "Percentile_ImpQC", "Percentile_Offshore", "Percentile_Onshore", "Percentile_Solar") := NULL]
  
  # Posterior distribution 1) Old Fossil Fuels, 2) Imports
  
  # Posterior distribution: if there is a shortage, re-sample imports using max CF
  dispatch_data[Shortage_MWh > 0, `:=`(
    Import_QC    = `Imports QC` * imports_max_CF,
    Import_NYISO = `Imports NYISO` * imports_max_CF,
    Import_NBSO  = `Imports NBSO` * imports_max_CF
  )]
  
  dispatch_data[Shortage_MWh > 0, Total_import_MWh := Import_QC + Import_NYISO + Import_NBSO]
  # Re-calculation battery charging/discharging
  if (pathway %in% c("A", "D")) {
    dispatch_data <- merge(dispatch_data,
                           Fossil_Fuels_hr_max[, .(Date, Hour, Old_Fossil_Fuels_hr_max_MWh = max_gen_hr_no_retirement_MW)],
                           by = c("Date", "Hour"), all.x = TRUE)
  } else {
    dispatch_data <- merge(dispatch_data,
                           Fossil_Fuels_hr_max[, .(Date, Hour, Old_Fossil_Fuels_hr_max_MWh = max_gen_hr_retirement_MW)],
                           by = c("Date", "Hour"), all.x = TRUE)
  }
  
  # Re-calculation shortages
  dispatch_data[, Shortage_MWh := pmax(
    Demand - (Clean_MWh + Battery_discharge + Total_import_MWh + Old_Fossil_Fuels_hr_max_MWh + New_Fossil_Fuel_MWh),
    0
  )]
  
  return(dispatch_data)
}

# Example usage:
result <- dispatch_curve(sim = 1, pathway = "B2")
head(result[, .(Date, Hour, Demand, Clean_MWh, Fossil_required_MWh)])



# Prepare pathways and unique dates/hours
pathways <- unique(Hourly_Installed_Capacity$Pathway)
pathways <- pathways[5:7]
unique_hours <- sort(unique(Demand_data$Hour))
num_percentiles <- ncol(Random_sequence)
unique_dates <- as.Date(sort(unique(Demand_data$Date)), origin = "1970-01-01")
start_date <- as.Date("2050-01-01")
end_date <- as.Date("2050-01-01")
unique_dates <- unique_dates[unique_dates >= start_date & unique_dates <= end_date]

# Number of samples
n_samples <- length(unique_dates) * length(unique_hours)

# Define the number of simulations
n_simulations <- 1

# Percentile sequences for old fossil fuels
n_facility <- uniqueN(Fossil_Fuels_NPC$Facility_ID)  # number of unique facilities
start_index <- 7
end_index <- n_facility + start_index - 1
facility_percentile_map <- data.table(
  Facility_ID = unique(Fossil_Fuels_NPC$Facility_ID),
  Percentile_Index = seq(start_index, end_index)
)
Fossil_Fuels_NPC <- merge(Fossil_Fuels_NPC, facility_percentile_map, by = "Facility_ID", all.x = TRUE)
setkey(Fossil_Fuels_NPC, Facility_Unit.ID)

# Use lapply to run all simulations
all_simulation_results <- lapply(1:n_simulations, function(sim) {
  # Pre-generate the sampled percentiles for each date and hour combination
  Random_sequence_sampled <- Random_sequence[, sim:(n_samples + sim - 1)]
  print(sim)
  
  lapply(pathways, function(pathway) {
    # Index for Random_sequence
    percentile_index <- 1
    storage_status <- 0
    prev_gen <- data.table(Facility_Unit.ID = Fossil_Fuels_NPC$Facility_Unit.ID, Prev_Gen_MWh = 0)
    
    # Print Pathway to Monitor Status
    print(pathway)
    
    lapply(seq_along(unique_dates), function(date_idx) {
      date <- unique_dates[date_idx]

      lapply(seq_along(unique_hours), function(hour_idx) {
        hour <- unique_hours[hour_idx]
        
        # Use the current percentile sequence based on the index
        percentile <- Random_sequence_sampled[[percentile_index]]
        percentile_index <<- percentile_index %% n_samples + 1  # Cycle through the percentiles
        
        result <- dispatch_curve(sim, pathway, date, hour, percentile, storage_status, prev_gen)
        
        # Update storage and previous generation
        storage_status <<- result$result_hr$Storage.status_hr_MWh
        if (nrow(result$result_facility_level) != 0) {
          prev_gen_updated <- result$result_facility_level[, .(Facility_Unit.ID, Fossil.gen_MWh)]
          result_prev_gen <- prev_gen_updated[, .(Facility_Unit.ID, Fossil.gen_MWh)]
          setnames(result_prev_gen, "Fossil.gen_MWh", "Prev_Gen_MWh")
          prev_gen[result_prev_gen, on = .(Facility_Unit.ID), Prev_Gen_MWh := i.Prev_Gen_MWh]
        }
        return(result)
      })
    })
  })
})

# Combining results across all simulations
combined_results_hr <- rbindlist(lapply(all_simulation_results, function(sim_results) {
  rbindlist(lapply(sim_results, function(pathway_results) {
    rbindlist(lapply(pathway_results, function(date_results) {
      rbindlist(lapply(date_results, `[[`, "result_hr"))
    }))
  }))
}))

combined_results_facility_level <- rbindlist(lapply(all_simulation_results, function(sim_results) {
  rbindlist(lapply(sim_results, function(pathway_results) {
    rbindlist(lapply(pathway_results, function(date_results) {
      rbindlist(lapply(date_results, `[[`, "result_facility_level"))
    }))
  }))
}))

# Save results to CSV files
write.csv(combined_results_hr, "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/1 Test Results/Hourly_Results_NE.csv", row.names = FALSE)
write.csv(combined_results_facility_level, "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/1 Test Results/Facility_Level_Results.csv", row.names = FALSE)

# Pushover alert
pushover_user <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST("https://api.pushover.net/1/messages.json",
                 body = list(
                   token = pushover_token,
                   user = pushover_user,
                   message = "The code executed successfully."
                 ),
                 encode = "form")

#----OLD----
# 6. Define function to process data for each hour
dispatch_curve <- function(sim, pathway, date, hour, percentile, storage_status, prev_gen) {
  results_hr <- list()
  results_facility_level <- list()
  date_selected <- as.Date(date, origin = "1970-01-01")
  day_label <- yday(date_selected)
  year <- year(date_selected)
  
  ## 1. Demand
  Demand_hour <- Demand_data[J(date_selected, hour), Demand]
  
  # 1.1 Peak demand Likelihood
  week <- date_selected + 0:6 # Week ahead dates
  Demand_peak <- Peak_demand[Date %in% week, ]
  Demand_peak[, Peak_Identified := Peak != "UNLIKELY"]
  Demand_peak_filtered <- Demand_peak[Peak != "UNLIKELY"]
  # Get the current date and time
  current_datetime <- as.POSIXct(paste(date_selected, sprintf("%02d:00:00", hour)), format="%Y-%m-%d %H:%M:%S")
  # Calculate time difference in hours directly within the data.table
  Demand_peak_filtered[, Time_To_Peak := difftime(
    as.POSIXct(paste(Date, sprintf("%02d:00:00", Peak_Hour)), format="%Y-%m-%d %H:%M:%S"),
    current_datetime,
    units = "hours"
  )]
  
  # 1.2 Peak ramping threshold
  # Assuming maximum ramp is 24 hours
  Demand_peak_filtered$start_ramp <- ifelse(Demand_peak_filtered$Time_To_Peak <= 24, TRUE, FALSE)
  
  ## 2. Generation
  # 2.1 Filter installed capacity for clean gen, imports, solar  and new gas
  Installed_Capacity_hour <- Hourly_Installed_Capacity[J(year, day_label, hour, pathway), ]
  
  # 2.2 Clean Generation
  # Wind - Solar
  Solar_hour_CF <- Solar_CF[J(day_label, hour, percentile[1]), CF]
  Onwind_hour_CF <- Onwind_CF[J(day_label, hour, percentile[2]), CF]
  Offwind_hour_CF <- Offwind_CF[J(day_label, hour, percentile[3]), CF]
  
  Solar_hour <- Solar_hour_CF * Installed_Capacity_hour$Solar
  Onwind_hour <- Onwind_hour_CF * Installed_Capacity_hour$`Onshore Wind`
  Offwind_hour <- Offwind_hour_CF * Installed_Capacity_hour$`Offshore Wind`
  
  # Nuclear, Hydro and Bio
  Nuclear_hour <- Installed_Capacity_hour$Nuclear * as.numeric(CleanBaseload_Facility_Data[CleanBaseload_Facility_Data$technology == "Nuclear", "value"])
  Hydro_hour <- Installed_Capacity_hour$Hydropower * as.numeric(CleanBaseload_Facility_Data[CleanBaseload_Facility_Data$technology == "Hydropower", "value"])
  Bio_hour <- Installed_Capacity_hour$Biomass * as.numeric(CleanBaseload_Facility_Data[CleanBaseload_Facility_Data$technology == "Biopower", "value"])
  Nuclear_Hydro_Bio_hour <- sum(Nuclear_hour, Hydro_hour, Bio_hour)
  
  # Total clean gen
  Clean_gen_hour <- Nuclear_Hydro_Bio_hour + Offwind_hour + Onwind_hour + Solar_hour
  
  
  ## 2.3 Maximum Battery Storage Capacity
  Battery_storage_capacity_max <- Installed_Capacity_hour$Storage
  
  ## 2.4 Electricity imports from other jurisdictions
  Imports_QC_CF <- Imports_CF[J(day_label, percentile[4]), imports_QC]
  Imports_NYISO_CF <- Imports_CF[J(day_label, percentile[5]), imports_NYISO]
  Imports_NBSO_CF <- Imports_CF[J(day_label, percentile[6]), imports_NBSO]
  
  Imports_QC <- Imports_QC_CF * Installed_Capacity_hour$`Imports QC`
  Imports_NYISO <- Imports_NYISO_CF * Installed_Capacity_hour$`Imports NYISO`
  Imports_NBSO <- Imports_NBSO_CF * Installed_Capacity_hour$`Imports NBSO`
  
  Imports_hour <- data.table(
    Imports_QC = Imports_QC,
    Imports_NYISO = Imports_NYISO,
    Imports_NBSO = Imports_NBSO
  )
  
  ## 2.3 SMRs
  SMR_max_hour <- SMR_Facility_Data$CF * Installed_Capacity_hour$SMR
  # SMR Constraints
  SMR_gen_hour <- SMR_max_hour * SMR_Facility_Data$Minimum_Power_Output_MWh/SMR_Facility_Data$Nameplate_Capacity_MWh
  
  ## 2.4 Fossil Fuels
  # 2.4.1 New Fossil Fuels
  New_Fossil_Fuels_NPC <- New_Fossil_Fuels_NPC[1] # select the template as needed
  New_Fossil_Fuel_max_gen_hour <- New_Fossil_Fuels_NPC$CF * Installed_Capacity_hour$`New NG`
  
  # 2.4.2 Old Fossil Fuels Generation
  gen_emissions_hour <- Fossil_Fuels_NPC[, {
    Percentile <- percentile[Percentile_Index]
    
    # Filter relevant data
    filtered_data <- Fossil_Fuels_Gen[Facility_Unit.ID == .BY$Facility_Unit.ID & 
                                        DayLabel == day_label & 
                                        Hour == hour]
    # Create calculated columns
    .(Percentile,
      Gen_X = filtered_data[[paste0("Gen_", Percentile)]],
      CO2_X = filtered_data[[paste0("CO2_", Percentile)]],
      NOx_X = filtered_data[[paste0("NOx_", Percentile)]],
      SO2_X = filtered_data[[paste0("SO2_", Percentile)]],
      HI_X = filtered_data[[paste0("HI_", Percentile)]])
  }, by = Facility_Unit.ID]
  
  # Merge calculated data back into Fossil_Fuels_NPC without affecting the original
  Fossil_Fuels_gen_hour <- merge(Fossil_Fuels_NPC, gen_emissions_hour, by = "Facility_Unit.ID", all.x = TRUE)
  Fossil_Fuels_gen_hour <- Fossil_Fuels_gen_hour[prev_gen, 
                                                 on = .(Facility_Unit.ID), 
                                                 Prev_Gen_MWh := i.Prev_Gen_MWh]
  setnames(Fossil_Fuels_gen_hour, "Gen_X", "Fossil_Fuel_gen")
  Fossil_Fuels_gen_hour[, Fossil_Fuel_CF_hour := Fossil_Fuel_gen / Estimated_NameplateCapacity_MWh]
  Fossil_Fuels_gen_hour[is.na(Fossil_Fuel_gen), Fossil_Fuel_gen := 0]
  Fossil_Fuels_gen_hour[is.na(Fossil_Fuel_CF_hour), Fossil_Fuel_CF_hour := 0]
  setorderv(Fossil_Fuels_gen_hour, cols = "Fossil_Fuel_CF_hour", order = -1)
  
  # Ensure generational/technical constraints are met
  # Historical min and max
  Fossil_Fuels_gen_hour[Fossil_Fuel_gen < min_gen_MWh, Fossil_Fuel_gen := min_gen_MWh]
  Fossil_Fuels_gen_hour[Fossil_Fuel_gen > max_gen_MWh, Fossil_Fuel_gen := max_gen_MWh]
  
  # Heat inputs
  Fossil_Fuels_gen_hour[HI_X > Max_Hourly_HI_Rate, Fossil_Fuel_gen := Fossil_Fuel_gen * Max_Hourly_HI_Rate/HI_X]
  
  # Apply ramp rate constraints based on previous hour generation
  Fossil_Fuels_gen_hour[is.na(Prev_Gen_MWh), Prev_Gen_MWh := 0]
  Fossil_Fuels_gen_hour[, max_gen_hour_MWh := pmin(Prev_Gen_MWh + Estimated_NameplateCapacity_MWh/Ramp, Estimated_NameplateCapacity_MWh * Fossil_Fuel_CF_hour)]
  Fossil_Fuels_gen_hour[, min_gen_hour_MWh := pmax(Prev_Gen_MWh - Estimated_NameplateCapacity_MWh/Ramp, 0)]
  
  Fossil_Fuels_gen_hour[, Fossil_Fuel_gen := pmax(pmin(Fossil_Fuel_gen, max_gen_hour_MWh), min_gen_hour_MWh)]
  
  # Apply retirements
  if (!(pathway %in% c("A", "D"))) {
    Fossil_Fuels_gen_hour <- Fossil_Fuels_gen_hour[Fossil_Fuels_gen_hour$Retirement_year > year, ]
  }
  
  Fossil_Fuels_gen_hour <- Fossil_Fuels_gen_hour[Fossil_Fuel_gen != 0]
  Fossil_Fuels_gen_hour_max <- sum(Fossil_Fuels_gen_hour$Fossil_Fuel_gen, na.rm = TRUE)
  
  # 2.4.2 Old Fossil Fuels Emissions CO2, NOx, SO2, HI
  cols_to_fill <- c("CO2_X", "NOx_X", "SO2_X", "HI_X")
  estimates <- c("mean_CO2_tons_MWh_estimate", "mean_NOx_lbs_MWh_estimate", 
                 "mean_SO2_lbs_MWh_estimate", "mean_HI_mmBtu_per_MWh")
  
  # Replace NA or Inf with the corresponding mean estimate
  Fossil_Fuels_gen_hour[, (cols_to_fill) := lapply(seq_along(cols_to_fill), function(i) {
    col <- cols_to_fill[i]
    estimate <- estimates[i]
    fifelse(is.na(get(col)) | is.infinite(get(col)), get(estimate), get(col))
  })]
  
  # Replace remaining NA or Inf with group-wise mean by Primary_Fuel_Type
  Fossil_Fuels_gen_hour[, (cols_to_fill) := lapply(.SD, function(col) {
    fifelse(is.na(col) | is.infinite(col), mean(col, na.rm = TRUE), col)
  }), by = Primary_Fuel_Type, .SDcols = cols_to_fill]
  
  
  
  ## 3. Generation versus Demand
  if (Demand_hour < Clean_gen_hour + SMR_gen_hour) {
    Demand_for_Fossil_Fuels_hour <- 0
    Clean_gen_hour <- Clean_gen_hour + SMR_gen_hour
    excess_energy <- Clean_gen_hour - Demand_hour
    wasted_energy <- excess_energy - (Battery_storage_capacity_max - storage_status)
    if (wasted_energy < 0) {wasted_energy <- 0}
    storage_status <- sum(storage_status + excess_energy)
    storage_status <- min(storage_status, Battery_storage_capacity_max)
    storage_hr_charging <- storage_status < Battery_storage_capacity_max
    storage_hr_discharging <- FALSE
  } else {
    SMR_gen_hour <- max(SMR_gen_hour, min(SMR_max_hour, Demand_hour - Clean_gen_hour - storage_status))
    Clean_gen_hour <- Clean_gen_hour + SMR_gen_hour
    Demand_for_Fossil_Fuels_hour <- max(0, Demand_hour - Clean_gen_hour - storage_status)
    storage_hr_discharging <- storage_status > 0
    storage_hr_charging <- FALSE
    storage_status <- storage_status + Clean_gen_hour - Demand_hour
    if (storage_status < 0) {storage_status <- 0}
    wasted_energy <- 0
  }
  
  # Initializing variables to track used imports
  Demand_for_Imports_hour <- list(Imports_QC=0, Imports_NYISO=0, Imports_NBSO=0)
  
  # Sequentially reduce demand using imports
  if (Demand_for_Fossil_Fuels_hour > 0) {
    for (source in names(Imports_hour)) {
      available_import <- Imports_hour[[source]]
      import_needed <- min(Demand_for_Fossil_Fuels_hour, available_import)
      Demand_for_Fossil_Fuels_hour <- Demand_for_Fossil_Fuels_hour - import_needed
      Demand_for_Imports_hour[[source]] <- Demand_for_Imports_hour[[source]] + import_needed
    }
  }
  
  # Fossil Fuels Allocation
  # Attempt to meet demand with New Fossil Fuels first
  new_fossil_gen = min(New_Fossil_Fuel_max_gen_hour, Demand_for_Fossil_Fuels_hour)
  displaced_load_new = New_Fossil_Fuel_max_gen_hour - new_fossil_gen
  Demand_for_Fossil_Fuels_hour = Demand_for_Fossil_Fuels_hour - new_fossil_gen
  Total_Fossil_Fuels_gen_hour_new = new_fossil_gen
  
  # Attempt to meet remaining demand with Old Fossil Fuels
  if (Fossil_Fuels_gen_hour_max > Demand_for_Fossil_Fuels_hour) {
    displaced_load <- Fossil_Fuels_gen_hour_max - Demand_for_Fossil_Fuels_hour
    gen_shortage <- 0
    Total_gen_hour <- Demand_hour 
    Total_Fossil_Fuels_gen_hour <- Demand_for_Fossil_Fuels_hour
  } else {
    displaced_load <- 0
    gen_shortage <- Demand_for_Fossil_Fuels_hour - Fossil_Fuels_gen_hour_max
    Total_gen_hour <- Clean_gen_hour + Fossil_Fuels_gen_hour_max
    Total_Fossil_Fuels_gen_hour <- Fossil_Fuels_gen_hour_max
  }
  
  # Identify which fossil fuel facilities are used
  if (gen_shortage <= 0) {
    total_consumed <- 0
    consumed_loads <- 0
    consumed_facilities <- c()
    eliminated_facilities <- c()
    for (i in 1:length(Fossil_Fuels_gen_hour$Fossil_Fuel_gen)) {
      if (Fossil_Fuels_gen_hour$Fossil_Fuel_gen[i] != 0) {
        if (total_consumed + Fossil_Fuels_gen_hour$Fossil_Fuel_gen[i] <= Demand_for_Fossil_Fuels_hour) {
          total_consumed <- total_consumed + Fossil_Fuels_gen_hour$Fossil_Fuel_gen[i]
          consumed_loads <- c(consumed_loads, Fossil_Fuels_gen_hour$Fossil_Fuel_gen[i])
          consumed_facilities <- c(consumed_facilities, Fossil_Fuels_gen_hour$Facility_Unit.ID[i])
        } else {
          remaining_to_eliminate <- Demand_for_Fossil_Fuels_hour - total_consumed
          total_consumed <- Demand_for_Fossil_Fuels_hour
          consumed_loads <- c(consumed_loads, remaining_to_eliminate)
          eliminated_facilities <- c(eliminated_facilities, Fossil_Fuels_gen_hour$Facility_Unit.ID[i])
        }
      }
    }
    
    consumed_facilities_hour <- data.table(Facility = consumed_facilities)
    rows_to_append <- Fossil_Fuels_gen_hour[length(consumed_facilities) + 1, .(Facility = Facility_Unit.ID)]
    consumed_facilities_hour <- rbind(consumed_facilities_hour, rows_to_append, fill = TRUE)
    
    consumed_facility_ids <- consumed_facilities_hour$Facility
    
    loads_for_consumed_facilities <- Fossil_Fuels_gen_hour[consumed_facility_ids, on = .(Facility_Unit.ID), nomatch = 0, .(Facility_Unit.ID, Fossil_Fuel_gen)]
    
    sum_load_before_last <- sum(head(loads_for_consumed_facilities$Fossil_Fuel_gen, -1), na.rm = TRUE)
    
    remaining_load <- Demand_for_Fossil_Fuels_hour - sum_load_before_last
    end <- length(loads_for_consumed_facilities$Fossil_Fuel_gen)
    last_facility_load <- loads_for_consumed_facilities$Fossil_Fuel_gen[[end]]
    percentage_reduction <- (remaining_load) / last_facility_load
    
    loads_for_consumed_facilities$Fossil_Fuel_gen[[end]] <- loads_for_consumed_facilities$Fossil_Fuel_gen[[end]] * (percentage_reduction)
    
    load_vector <- unlist(loads_for_consumed_facilities$Fossil_Fuel_gen, use.names = FALSE)
    load_vector[is.na(load_vector)] <- 0
  } else {
    consumed_facilities_hour <- c()
    consumed_facilities_hour$Facility <- Fossil_Fuels_gen_hour$Facility_Unit.ID
    load_vector <- Fossil_Fuels_gen_hour$Fossil_Fuel_gen
  }
  
  # add all info to Final_Fossil_fuels
  Fossil_Fuels_used <- data.table(Facility_Unit.ID = unlist(consumed_facilities_hour), Load_MWh = load_vector)
  
  # Ramp up if peak is coming
  if (!is.na(Demand_peak_filtered$start_ramp[1]) && Demand_peak_filtered$start_ramp[1] == TRUE) {
    Fossil_Fuels_Final <- Fossil_Fuels_used[Fossil_Fuels_gen_hour, on = .(Facility_Unit.ID)]
    Fossil_Fuels_Final$Load_MWh[is.na(Fossil_Fuels_Final$Load_MWh)] <- 0
    Fossil_Fuels_Final[, Load_MWh := pmax(pmax(Load_MWh, min_gen_MWh), max_gen_MWh)]
    peak_incoming <- TRUE
  } else {
    Fossil_Fuels_Final <- Fossil_Fuels_used[Fossil_Fuels_gen_hour, on = .(Facility_Unit.ID)]
    Fossil_Fuels_Final$Load_MWh[is.na(Fossil_Fuels_Final$Load_MWh)] <- 0
    Fossil_Fuels_Final[, Load_MWh := pmin(pmax(Load_MWh, min_gen_MWh), max_gen_MWh)]
    peak_incoming <- FALSE
  } 
  
  Fossil_Fuels_Final <- Fossil_Fuels_Final[Load_MWh != 0]
  
  Fossil_Fuels_Final[, Gen_ratio := Load_MWh / Fossil_Fuel_gen] # How much each facility is used
  Fossil_Fuels_Final[, `:=`(
    CO2_tons = Load_MWh * CO2_X,
    NOx_lbs = Load_MWh * NOx_X,
    SO2_lbs = Load_MWh * SO2_X,
    HI_mmBtu = HI_X * Load_MWh
  )]
  
  # Total emissions
  # New fossil emissions + old fossil emissions
  Total_CO2_hr <- Total_Fossil_Fuels_gen_hour_new * New_Fossil_Fuels_NPC$mean_CO2_tons_MWh  + sum(Fossil_Fuels_Final$CO2_tons, na.rm = TRUE)
  Total_NOx_hr <- Total_Fossil_Fuels_gen_hour_new * New_Fossil_Fuels_NPC$mean_NOx_lbs_MWh  + sum(Fossil_Fuels_Final$NOx_lbs, na.rm = TRUE)
  Total_SO2_hr <- Total_Fossil_Fuels_gen_hour_new * New_Fossil_Fuels_NPC$mean_SO2_lbs_MWh  + sum(Fossil_Fuels_Final$SO2_lbs, na.rm = TRUE)
  Total_HI_hr <- Total_Fossil_Fuels_gen_hour_new * New_Fossil_Fuels_NPC$mean_Heat_Input_mmBtu  + sum(Fossil_Fuels_Final$HI_mmBtu, na.rm = TRUE)
  
  # Total fossil generation and Curtailed fossil fuels
  Total_Fossil_Fuels_gen_hour <- sum(Fossil_Fuels_Final$Load_MWh, na.rm = TRUE)
  Total_Fossil_Fuels_gen_hour_ALL <- Total_Fossil_Fuels_gen_hour + Total_Fossil_Fuels_gen_hour_new
  Total_gen_hr <- Clean_gen_hour + Total_Fossil_Fuels_gen_hour_ALL
  
  wasted_fossil <-  Total_Fossil_Fuels_gen_hour - Demand_for_Fossil_Fuels_hour
  wasted_fossil <- ifelse(wasted_fossil > 0, wasted_fossil, 0)
  
  
  # Hourly results for all of NE 
  result_hr <- data.table(Simulation = sim,
                          Year = year, DayLabel = day_label, Date = date_selected, Hour = hour, Pathway = pathway,
                          Percentile_Solar = percentile[1], Solar.gen_hr_MWh = Solar_hour,
                          Percentile_Onwind = percentile[2], Onwind.gen_hr_MWh = Onwind_hour,
                          Percentile_Offwind = percentile[3], Offwind.gen_hr_MWh = Offwind_hour,
                          Nuclear.gen_hr_MWh = Nuclear_hour, Hydro.gen_hr_MWh = Hydro_hour, Bio.gen_hr_MWh = Bio_hour,
                          SMR.gen_hr_MWh = SMR_gen_hour,
                          Percentile_Import_QC = percentile[4], Import.QC_hr_MWh = Demand_for_Imports_hour$Imports_QC,
                          Percentile_Import_NYISO = percentile[5], Import.NYISO_hr_MWh = Demand_for_Imports_hour$Imports_NYISO,
                          Percentile_Import_NB = percentile[6], Import.NB_hr_MWh = Demand_for_Imports_hour$Imports_NBSO,
                          Fossil_old.gen_hr_MWh = Total_Fossil_Fuels_gen_hour,
                          Fossil_new.gen_hr_MWh = Total_Fossil_Fuels_gen_hour_new,
                          Fossil.gen_hr_MWh = Total_Fossil_Fuels_gen_hour_ALL,
                          Clean.gen_hr_MWh = Clean_gen_hour, Imports.total_hr_MWh = sum(unlist(Demand_for_Imports_hour), na.rm = TRUE),
                          Total.gen_hr_MWh = Total_gen_hr,
                          Demand.total_hr_MWh = Demand_hour, 
                          ExcessClean.gen_hr_MWh = wasted_energy,
                          ExcessFossil.gen_hr_MWh = wasted_fossil,
                          Peak.status_hour = peak_incoming,
                          Shortage.total_hr_MWh = gen_shortage,
                          Storage.status_hr_MWh = storage_status,
                          Storage.charging_hours = storage_hr_charging,
                          Storage.discharging_hours = storage_hr_discharging,
                          CO2.total_hr_tons = Total_CO2_hr,
                          NOx.total_hr_lbs = Total_NOx_hr,
                          SO2.total_hr_lbs = Total_SO2_hr,
                          HI.total_hr_mmBtu = Total_HI_hr
  )
  
  # Facility level gen and emissions (fossil fuels)
  result_facility_level <- data.table(Simulation = sim,
                                      Year = year, DayLabel = day_label, Date = date_selected, Hour = hour, Pathway = pathway, 
                                      Facility_Unit.ID = Fossil_Fuels_Final$Facility_Unit.ID, State = Fossil_Fuels_Final$State,
                                      Fuel_type_1 = Fossil_Fuels_Final$Primary_Fuel_Type, Fuel_type_2 = Fossil_Fuels_Final$Secondary_Fuel_Type,
                                      latitude = Fossil_Fuels_Final$Latitude, longitude = Fossil_Fuels_Final$Longitude,
                                      Ramp_hr = Fossil_Fuels_Final$Ramp, Fossil.NPC_MWh = Fossil_Fuels_Final$Estimated_NameplateCapacity_MWh,
                                      Percentile = Fossil_Fuels_Final$Percentile,
                                      CF = Fossil_Fuels_Final$Load_MWh / Fossil_Fuels_Final$Estimated_NameplateCapacity_MWh,
                                      Fossil.gen_MWh = Fossil_Fuels_Final$Load_MWh, CO2_tons = Fossil_Fuels_Final$CO2_tons,
                                      NOx_lbs = Fossil_Fuels_Final$NOx_lbs, SO2_lbs = Fossil_Fuels_Final$SO2_lbs, HI_mmBtu = Fossil_Fuels_Final$HI_mmBtu
  )
  
  return(list(result_hr = result_hr, result_facility_level = result_facility_level))
}
#----OLD----
