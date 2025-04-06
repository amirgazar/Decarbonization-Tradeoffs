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
Fossil_Fuels_NPC$Ramp <- ceiling(Fossil_Fuels_NPC$Ramp)
Fossil_Fuels_NPC$Ramp_MWh <- Fossil_Fuels_NPC$Estimated_NameplateCapacity_MW/ Fossil_Fuels_NPC$Ramp

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv"
New_Fossil_Fuels_NPC <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/2 Fossil Fuels Generation and Emissions/Fossil_Fuel_hr_maxmin.csv"
Fossil_Fuels_hr_maxmin <- fread(file_path)

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
setkey(Fossil_Fuels_hr_maxmin, Date, Hour)

# FUNCTION: Applies Dispatch Curve
dispatch_curve <- function(sim, pathway) {
  #####################################################################
  # STEP 1: Filter and Merge Data
  #####################################################################
  # Filter capacity data for the chosen pathway (years ≥2025) and merge with demand data.
  cap_data <- Hourly_Installed_Capacity[Pathway == pathway & Year >= 2025]
  dispatch_data <- merge(Demand_data, cap_data, by = c("Date", "Hour"))
  dispatch_data[, Simulation := sim]
  setorder(dispatch_data, Date, Hour)
  
  #####################################################################
  # STEP 2: Extract Random Percentile Indices
  #####################################################################
  # Retrieve random vector and compute indices for each asset.
  random_vector <- Random_sequence[[sim]]
  n_random <- length(random_vector)
  n_hours <- nrow(dispatch_data)
  n_total_rand <- 6
  base_indices <- ((0:(n_hours - 1)) * n_total_rand)
  
  idx_solar    <- (base_indices + 0) %% n_random + 1
  idx_onshore  <- (base_indices + 1) %% n_random + 1
  idx_offshore <- (base_indices + 2) %% n_random + 1
  idx_impHQ    <- (base_indices + 3) %% n_random + 1   # For Quebec (HQ) imports
  idx_impNYISO <- (base_indices + 4) %% n_random + 1
  idx_impNBSO  <- (base_indices + 5) %% n_random + 1
  
  dispatch_data[, Percentile_Solar    := random_vector[idx_solar]]
  dispatch_data[, Percentile_Onshore  := random_vector[idx_onshore]]
  dispatch_data[, Percentile_Offshore := random_vector[idx_offshore]]
  dispatch_data[, Percentile_ImpHQ    := random_vector[idx_impHQ]]
  dispatch_data[, Percentile_ImpNYISO := random_vector[idx_impNYISO]]
  dispatch_data[, Percentile_ImpNBSO  := random_vector[idx_impNBSO]]
  
  #####################################################################
  # STEP 3: Lookup Capacity Factor (CF) Values
  #####################################################################
  # Merge CF data for Solar.
  dispatch_data <- merge(dispatch_data, 
                         Solar_CF[, .(DayLabel, Hour, Percentile, CF)],
                         by.x = c("DayLabel", "Hour", "Percentile_Solar"),
                         by.y = c("DayLabel", "Hour", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "CF", "Solar_CF")
  
  # Merge CF data for Onshore Wind.
  dispatch_data <- merge(dispatch_data, 
                         Onwind_CF[, .(DayLabel, Hour, Percentile, CF)],
                         by.x = c("DayLabel", "Hour", "Percentile_Onshore"),
                         by.y = c("DayLabel", "Hour", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "CF", "Onshore_CF")
  
  # Merge CF data for Offshore Wind.
  dispatch_data <- merge(dispatch_data, 
                         Offwind_CF[, .(DayLabel, Hour, Percentile, CF)],
                         by.x = c("DayLabel", "Hour", "Percentile_Offshore"),
                         by.y = c("DayLabel", "Hour", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "CF", "Offshore_CF")
  
  # Merge CF data for Quebec (HQ) imports.
  dispatch_data <- merge(dispatch_data, 
                         Imports_CF[, .(DayLabel, Percentile, imports_QC)],
                         by.x = c("DayLabel", "Percentile_ImpHQ"),
                         by.y = c("DayLabel", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "imports_QC", "Import_HQ_CF")
  
  # Merge CF data for NYISO imports.
  dispatch_data <- merge(dispatch_data, 
                         Imports_CF[, .(DayLabel, Percentile, imports_NYISO)],
                         by.x = c("DayLabel", "Percentile_ImpNYISO"),
                         by.y = c("DayLabel", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "imports_NYISO", "Import_NYISO_CF")
  
  # Merge CF data for NBSO imports.
  dispatch_data <- merge(dispatch_data, 
                         Imports_CF[, .(DayLabel, Percentile, imports_NBSO)],
                         by.x = c("DayLabel", "Percentile_ImpNBSO"),
                         by.y = c("DayLabel", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "imports_NBSO", "Import_NBSO_CF")
  
  #####################################################################
  # STEP 4: Calculate Clean Generation
  #####################################################################
  # Compute generation for variable renewables (in MWh).
  dispatch_data[, Solar_MWh    := Solar_MW * Solar_CF]
  dispatch_data[, Onshore_MWh  := Onshore_Wind_MW * Onshore_CF]
  dispatch_data[, Offshore_MWh := Offshore_Wind_MW * Offshore_CF]
  
  # Compute generation for baseload assets using fixed CF values.
  nuclear_CF <- as.numeric(CleanBaseload_Facility_Data[technology == "Nuclear", value])
  hydro_CF   <- as.numeric(CleanBaseload_Facility_Data[technology == "Hydropower", value])
  bio_CF     <- as.numeric(CleanBaseload_Facility_Data[technology == "Biopower", value])
  
  dispatch_data[, Nuclear_MWh := Nuclear_MW * nuclear_CF]
  dispatch_data[, Hydro_MWh   := Hydropower_MW * hydro_CF]
  dispatch_data[, Biomass_MWh := Biomass_MW * bio_CF]
  
  # Compute generation from SMRs.
  SMR_CF <- SMR_Facility_Data$CF
  dispatch_data[, SMR_MWh := SMR_MW * SMR_CF]
  
  # Sum all clean generation.
  dispatch_data[, Clean_MWh := Solar_MWh + Onshore_MWh + Offshore_MWh +
                  Nuclear_MWh + Hydro_MWh + Biomass_MWh + SMR_MWh]
  setorder(dispatch_data, Date, Hour)
  
  #####################################################################
  # STEP 5: Battery Storage Integration
  #####################################################################
  # Calculate net energy (clean generation minus demand) for battery operations.
  storage_status_initial <- 0
  rt_eff <- 0.85          # Round-trip efficiency
  eta <- sqrt(rt_eff)     # Charging/discharging efficiency
  
  dispatch_data[, net_energy := Clean_MWh - Demand]
  dispatch_data[, battery_power_limit := Storage_MW / 8]
  dispatch_data[, net_energy := pmin(pmax(net_energy, -battery_power_limit), battery_power_limit)]
  
  # Compute storage state hour-by-hour.
  storage_status_vec <- Reduce(
    function(prev, i) {
      net <- dispatch_data$net_energy[i]
      capacity <- dispatch_data$Storage_MW[i]
      new_storage <- if (net >= 0) prev + net * eta else prev + net / eta
      min(max(new_storage, 0), capacity)
    },
    seq_len(nrow(dispatch_data)),
    init = storage_status_initial,
    accumulate = TRUE
  )[-1]
  
  dispatch_data[, Storage_status := storage_status_vec]
  dispatch_data[, Battery_flow := c(0, diff(Storage_status))]
  dispatch_data[, Battery_charge := ifelse(Battery_flow > 0, Battery_flow, 0)]
  dispatch_data[, Battery_discharge := ifelse(Battery_flow < 0, -Battery_flow, 0)]
  
  #####################################################################
  # STEP 6: Fossil Fuel Generation Requirements
  #####################################################################
  # Compute generation from new fossil fuels.
  New_Fossil_Fuels_NPC <- New_Fossil_Fuels_NPC[1]
  NFF_CF <- New_Fossil_Fuels_NPC$CF
  dispatch_data[, New_Fossil_Fuel_MWh := New_NG_MW * NFF_CF]
  
  # Merge maximum hourly capacity for old fossil fuels.
  if (pathway %in% c("A", "D")) {
    dispatch_data <- merge(dispatch_data,
                           Fossil_Fuels_hr_maxmin[, .(Date, Hour, Old_Fossil_Fuels_hr_max_MWh = max_gen_hr_no_retirement_MW)],
                           by = c("Date", "Hour"), all.x = TRUE)
  } else {
    dispatch_data <- merge(dispatch_data,
                           Fossil_Fuels_hr_maxmin[, .(Date, Hour, Old_Fossil_Fuels_hr_max_MWh = max_gen_hr_retirement_MW)],
                           by = c("Date", "Hour"), all.x = TRUE)
  }
  
  #####################################################################
  # STEP 7: Import Generation and Splitting QC Imports
  #####################################################################
  # Calculate total import generation (in MWh) for each source.
  dispatch_data[, Spot_Market_Imports_HQ_MWh := Spot_Market_Imports_HQ_MW * Import_HQ_CF]
  dispatch_data[, Import_NYISO_MWh    := Imports_NYISO_MW * Import_NYISO_CF]
  dispatch_data[, Import_NBSO_MWh     := Imports_NBSO_MW * Import_NBSO_CF]
  
  #  long-term.
  imports_max_CF <- 0.95
  dispatch_data[, Long_Term_Imports_HQ_MWh := Long_Term_Imports_HQ_MW * imports_max_CF]
  
  # Sum total imports
  dispatch_data[, Total_import_MWh := Long_Term_Imports_HQ_MWh + Spot_Market_Imports_HQ_MWh + Import_NYISO_MWh + Import_NBSO_MWh]
  dispatch_data[, Total_import_max_MWh := (Imports_NBSO_MW + Imports_NYISO_MW + Imports_HQ_MW) * imports_max_CF]
  
  #####################################################################
  # STEP 8: Fossil Generation and Shortage Calculation (Posterior Distribution)
  #####################################################################
  # Compute fossil generation needed to cover remaining demand.
  dispatch_data[, Fossil_required_MWh := pmax(Demand - Clean_MWh - Total_import_MWh - Battery_discharge, 0)]
  dispatch_data[, Old_Fossil_Fuels_net_MWh := pmin(Old_Fossil_Fuels_hr_max_MWh, Fossil_required_MWh)]
  
  dispatch_data[, Shortage_MWh := round(pmax(Demand - (Clean_MWh + Battery_discharge + 
                                                         Total_import_MWh + Old_Fossil_Fuels_net_MWh + New_Fossil_Fuel_MWh), 0), 2)]
  
  # Resample imports if a shortage exists.
  dispatch_data[, Total_import_net_MWh := ifelse(
    Shortage_MWh > 0,
    Total_import_MWh + pmin(Shortage_MWh, Total_import_max_MWh - Total_import_MWh),
    pmax(0, pmin(Total_import_MWh, Demand - (Clean_MWh + Battery_discharge + Old_Fossil_Fuels_net_MWh + New_Fossil_Fuel_MWh)))
  )]
  
  # Recalculate shortage.
  dispatch_data[, Shortage_MWh := round(pmax(
    Demand - (Clean_MWh + Battery_discharge + Total_import_net_MWh + Old_Fossil_Fuels_net_MWh + New_Fossil_Fuel_MWh),
    0
  ), 2)]
  
  # Calculate Curtailments rounded to 2 decimal points
  dispatch_data[, Curtailments_MWh := round(
    pmax(0, (Clean_MWh + Battery_discharge + Old_Fossil_Fuels_net_MWh + New_Fossil_Fuel_MWh + Total_import_net_MWh) - Demand - Battery_charge), 
    2
  )]
  
  #####################################################################
  # STEP 9: Adjust columns
  #####################################################################
  # Remove temporary columns.
  dispatch_data[, c("net_energy", "Percentile_ImpNBSO", "Percentile_ImpNYISO", 
                    "Percentile_ImpHQ", "Percentile_Offshore", "Percentile_Onshore", "Percentile_Solar", 
                    "Fossil_required_MWh") := NULL]
  gc()
  return(dispatch_data)
}

# FUNCTION: Fossil Fuel Constraints and Calculates Emissions 
dispatch_curve_adjustments <- function(results) {
  # --- Preliminaries: Initialize Simulation Parameters ---
  sim <- results$Simulation[1]
  pathway <- results$Pathway[1]
  random_vector <- Random_sequence[[sim]]
  n_random <- length(random_vector)
  n_hours <- nrow(results)
  
  # --- Fossil Fuels: Filter and Prepare Facility Data ---
  # Retain only facilities scheduled to retire after 2025.
  Fossil_Fuels_NPC <- Fossil_Fuels_NPC[Fossil_Fuels_NPC$Retirement_year > 2025, ]
  # Extract unique facility unit IDs from the filtered fossil fuel data.
  unique_units <- unique(Fossil_Fuels_NPC$Facility_Unit.ID)
  n_units <- length(unique_units)
  n_total_rand <- 6 + n_units
  
  # --- Index Calculation for Randomization ---
  # Calculate the base index for each hour (0-indexed).
  base_indices <- ((0:(n_hours - 1)) * n_total_rand)
  
  # For each facility, compute the cyclic index from the random vector using modulo arithmetic.
  shift_values <- 6:(6 + n_units - 1)  # Offset for fossil facilities
  idx_fossil <- sapply(shift_values, function(s) (base_indices + s) %% n_random + 1)
  
  # Assemble a long-format data.table that maps percentiles to each facility.
  results_updated <- rbindlist(
    lapply(seq_along(unique_units), function(j) {
      data.table(
        Date = results$Date,
        DayLabel = results$DayLabel,
        Hour = results$Hour,
        Pathway = pathway,
        Simulation = sim,
        Old_Fossil_Fuels_net_MWh = results$Old_Fossil_Fuels_net_MWh,
        Facility_Unit.ID = unique_units[j],
        Percentile = random_vector[idx_fossil[, j]]
      )
    })
  )
  
  # --- Merge Percentile Data with Generation Data ---
  # Step 1: Merge the percentile data with Fossil_Fuels_Gen by DayLabel, Hour, and Facility_Unit.ID.
  setkey(results_updated, DayLabel, Hour, Facility_Unit.ID)
  setkey(Fossil_Fuels_Gen, DayLabel, Hour, Facility_Unit.ID)
  results_updated <- Fossil_Fuels_Gen[results_updated, nomatch = 0]
  
  # --- Extract Percentile-Specific Generation and Emissions Data ---
  # Define the list of measurement fields to extract based on the percentile.
  fields <- c("Gen", "CO2", "NOx", "SO2", "HI")
  
  # Use mapply to extract the appropriate value for each field using the corresponding percentile.
  for (field in fields) {
    results_updated[[paste0(field, "_value")]] <- mapply(function(p, row) {
      colname <- paste0(field, "_", p)
      if (colname %in% names(row)) row[[colname]] else NA_real_
    }, results_updated$Percentile, split(results_updated, seq_len(nrow(results_updated))))
  }
  
  # Rename the extracted columns for clarity.
  setnames(results_updated, old = c("Gen_value", "CO2_value", "NOx_value", "SO2_value", "HI_value"),
           new = c("Gen_MWh", "CO2_tons", "NOx_lbs", "SO2_lbs", "HI_mmBtu"))
  
  # Retain only the essential columns for further processing.
  results_updated <- results_updated[, .(Date, DayLabel, Hour, Facility_Unit.ID, Old_Fossil_Fuels_net_MWh,
                                         Gen_MWh, CO2_tons, NOx_lbs, SO2_lbs, HI_mmBtu)]
  
  # Merge additional facility details from the Fossil_Fuels_NPC dataset.
  Fossil_Fuels_NPC_subset <- Fossil_Fuels_NPC[, .(Facility_Unit.ID, Estimated_NameplateCapacity_MW, 
                                                  min_gen_MW, max_gen_MW, Ramp, Ramp_MWh, Retirement_year)]
  results_updated <- Fossil_Fuels_NPC_subset[results_updated, on = .(Facility_Unit.ID), nomatch = 0]
  
  # --- Filter Out Retired Facilities ---
  # Exclude facilities that have retired (if the pathway is not "A" or "D").
  if (!(pathway %in% c("A", "D"))) {
    results_updated <- results_updated[results_updated$Retirement_year >= year(results_updated$Date), ]
  }
  
  # --- Calculate Capacity Factor (CF) ---
  # Compute the capacity factor as generation divided by the estimated nameplate capacity.
  results_updated$CF_hr <- results_updated$Gen_MWh / results_updated$Estimated_NameplateCapacity_MW
  setorder(results_updated, Date, Hour, CF_hr)
  
  # --- Adjust Generation to Meet Capacity Constraints ---
  results_updated <- results_updated[, {
    dt <- copy(.SD)  # Create a modifiable copy of .SD
    total_gen <- sum(dt$Gen_MWh)
    net_cap <- unique(dt$Old_Fossil_Fuels_net_MWh)
    
    if (total_gen <= net_cap) {
      dt[, Gen_MWh_used := Gen_MWh]
      dt  # Return the modified copy
    } else {
      dt_sorted <- dt[order(CF_hr)]
      overage <- total_gen - net_cap
      Gen_MWh_orig <- dt_sorted$Gen_MWh
      reducible_raw <- pmin(Gen_MWh_orig, overage)
      cumulative_reducible <- cumsum(reducible_raw)
      reducible <- ifelse(cumulative_reducible <= overage, reducible_raw,
                          pmax(0, overage - shift(cumulative_reducible, fill = 0)))
      Gen_MWh_adj <- Gen_MWh_orig - reducible
      dt_sorted[, Gen_MWh_used := Gen_MWh_adj]
      dt_sorted  # Return the modified sorted copy
    }
  }, by = .(Date, Hour)]
  
  
  # --- Reorder Data Chronologically ---
  setorder(results_updated, Facility_Unit.ID, Date, Hour)
  
  # Note: The 'results_updated' table now contains key columns such as Date (YYYY-MM-DD), Hour (numeric),
  # Facility_Unit.ID, Gen_MWh_used, Ramp_MWh, etc.
  
  # --- Time Series Adjustments ---
  ### Step 1: Create a Timestamp
  # Combine Date and Hour into a POSIXct timestamp (using Eastern Time).
  results_updated[, timestamp := as.POSIXct(paste(Date, sprintf("%02d:00:00", (Hour - 1))), 
                                            format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]
  
  ### Step 2: Generate a Complete Hourly Sequence per Facility
  # For each facility, create a full sequence of hourly timestamps and merge with existing data.
  results_updated <- results_updated[, {
    ts_seq <- seq(min(timestamp), max(timestamp), by = "hour")
    dt_complete <- data.table(timestamp = ts_seq)
    merge(dt_complete, .SD, by = "timestamp", all.x = TRUE)
  }, by = Facility_Unit.ID]
  
  ### Step 3: Update Date and Hour from the New Timestamp
  # Recompute the Date and Hour fields based on the created timestamp.
  results_updated[, Date := as.Date(format(timestamp, tz = "America/New_York"))]
  results_updated[, Hour := hour(timestamp) + 1]
  
  ### Step 4: Fill Missing Ramp_MWh Values
  # Since Ramp_MWh is fixed per facility, fill in any missing values using last observation carried forward,
  # then fill any remaining gaps using next observation carried backward.
  results_updated[, Ramp_MWh := nafill(Ramp_MWh, type = "locf"), by = Facility_Unit.ID]
  results_updated[, Ramp_MWh := nafill(Ramp_MWh, type = "nocb"), by = Facility_Unit.ID]
  
  ### Step 5: Initialize Adjusted Generation Column
  # Set up the adjusted generation column and replace any missing values with 0.
  results_updated[, Gen_MWh_adj := Gen_MWh_used]
  results_updated[is.na(Gen_MWh_adj), Gen_MWh_adj := 0]
  
  ### Step 6: Backward Adjustment Pass
  # Traverse each facility's time series backward to ensure that preceding hours are adjusted upward if a later hour
  # requires higher generation (respecting the ramp limit).
  results_updated[, Gen_MWh_adj := {
    temp <- Gen_MWh_adj
    ramp <- unique(Ramp_MWh)[1]
    if (is.na(ramp)) ramp <- 24
    n <- .N
    if (n < 2) {
      temp
    } else {
      for (i in n:2) {
        if (i - 1 < 1) next
        if (!is.na(temp[i])) {
          required_prev <- temp[i] - ramp
          if (!is.na(required_prev)) {
            if (length(temp[i-1]) == 0 || is.na(temp[i-1]) || temp[i-1] < required_prev) {
              temp[i-1] <- required_prev
            }
          }
        }
      }
      temp
    }
  }, by = Facility_Unit.ID]
  
  ### Step 7: Forward Adjustment Pass
  # Process the time series forward to ensure that increases between consecutive hours do not exceed the ramp limit.
  results_updated[, Gen_MWh_adj := {
    temp <- Gen_MWh_adj
    ramp <- unique(Ramp_MWh)[1]
    if (is.na(ramp)) ramp <- 24
    n <- .N
    if (n < 2) {
      temp
    } else {
      for (i in 2:n) {
        if (!is.na(temp[i - 1]) && !is.na(temp[i])) {
          delta <- temp[i] - temp[i - 1]
          if (delta < -ramp) {
            temp[i] <- temp[i - 1] - ramp
          }
        }
      }
      temp
    }
  }, by = Facility_Unit.ID]
  
  # Fill missing minimum generation values per facility using the last observed value.
  results_updated[, min_gen_MW := nafill(min_gen_MW, type = "locf"), by = Facility_Unit.ID]
  # Ensure that adjusted generation is not below the minimum and round the result.
  results_updated[, Gen_MWh_adj := round(pmax(Gen_MWh_adj, min_gen_MW), 2)]
  
  # --- Cleanup Temporary Columns ---
  # Remove columns that are no longer needed for subsequent calculations.
  results_updated[, c("Ramp", "Ramp_MWh", "Estimated_NameplateCapacity_MW", 
                      "min_gen_MW", "max_gen_MW", "Retirement_year", "Gen_MWh_used", 
                      "CF_hr", "timestamp") := NULL]
  
  # Update DayLabel to represent the day of the year.
  results_updated[, DayLabel := as.integer(format(Date, "%j"))]
  # Replace missing values in Old_Fossil_Fuels_net_MWh with 0.
  results_updated[is.na(Old_Fossil_Fuels_net_MWh), Old_Fossil_Fuels_net_MWh := 0]
  
  # --- Emission Calculations ---
  # Step 1: Compute Emission Intensity Ratios (per MWh of generation).
  results_updated[, `:=`(
    CO2_intensity = CO2_tons / Gen_MWh,
    NOx_intensity = NOx_lbs / Gen_MWh,
    SO2_intensity = SO2_lbs / Gen_MWh,
    HI_intensity  = HI_mmBtu / Gen_MWh
  )]
  
  # Step 2: Calculate Mean Emission Intensities per Facility
  mean_intensities <- results_updated[
    , .(
      mean_CO2_ton_per_MWh = mean(CO2_intensity[is.finite(CO2_intensity)], na.rm = TRUE),
      mean_NOx_lb_per_MWh  = mean(NOx_intensity[is.finite(NOx_intensity)], na.rm = TRUE),
      mean_SO2_lb_per_MWh  = mean(SO2_intensity[is.finite(SO2_intensity)], na.rm = TRUE),
      mean_HI_mmbtu_per_MWh = mean(HI_intensity[is.finite(HI_intensity)], na.rm = TRUE)
    ),
    by = Facility_Unit.ID
  ]
  
  # Step 3: Merge Facility Mean Intensities Back into Main Data
  results_updated <- merge(results_updated, mean_intensities, by = "Facility_Unit.ID", all.x = TRUE)
  
  # Calculate overall global mean intensities.
  global_means <- mean_intensities[, .(
    global_mean_CO2 = mean(mean_CO2_ton_per_MWh, na.rm = TRUE),
    global_mean_NOx = mean(mean_NOx_lb_per_MWh, na.rm = TRUE),
    global_mean_SO2 = mean(mean_SO2_lb_per_MWh, na.rm = TRUE),
    global_mean_HI  = mean(mean_HI_mmbtu_per_MWh, na.rm = TRUE)
  )]
  
  # Replace any missing facility mean intensities with the corresponding global mean values.
  results_updated[is.na(mean_CO2_ton_per_MWh), mean_CO2_ton_per_MWh := global_means$global_mean_CO2]
  results_updated[is.na(mean_NOx_lb_per_MWh), mean_NOx_lb_per_MWh := global_means$global_mean_NOx]
  results_updated[is.na(mean_SO2_lb_per_MWh), mean_SO2_lb_per_MWh := global_means$global_mean_SO2]
  results_updated[is.na(mean_HI_mmbtu_per_MWh), mean_HI_mmbtu_per_MWh := global_means$global_mean_HI]
  
  # Step 4: Estimate Emissions Based on Adjusted Generation
  results_updated[, `:=`(
    CO2_tons  = Gen_MWh_adj * mean_CO2_ton_per_MWh,
    NOx_lbs   = Gen_MWh_adj * mean_NOx_lb_per_MWh,
    SO2_lbs   = Gen_MWh_adj * mean_SO2_lb_per_MWh,
    HI_mmBtu  = Gen_MWh_adj * mean_HI_mmbtu_per_MWh
  )]
  
  # Step 5: Remove Raw Intensity Columns, Retaining Only the Mean Values
  results_updated[, c("Gen_MWh", "CO2_intensity", "NOx_intensity", "SO2_intensity", "HI_intensity",
                      "mean_HI_mmbtu_per_MWh", "mean_SO2_lb_per_MWh", "mean_NOx_lb_per_MWh", "mean_CO2_ton_per_MWh" ) := NULL]
  gc()
  return(results_updated)
}

# FUNCTION: Re-calculates Battery Status and Shortages
dispatch_curve_calibrations <- function(dispatch_curve_results, fossil_fuels_hourly_results) {
  # --- Step 1: Aggregate Adjusted Fossil Fuel Generation and Emissions ---
  fossil_agg <- fossil_fuels_hourly_results[, 
                                            .(Old_Fossil_Fuels_adj_MWh = sum(Gen_MWh_adj, na.rm = TRUE),
                                              CO2_tons = sum(CO2_tons, na.rm = TRUE),
                                              NOx_lbs = sum(NOx_lbs, na.rm = TRUE),
                                              SO2_lbs = sum(SO2_lbs, na.rm = TRUE),
                                              HI_mmBtu  = sum(HI_mmBtu, na.rm = TRUE)),
                                            by = .(Date, Hour)]
  
  # --- Step 2: Merge Aggregated Fossil Data with Dispatch Curve Results ---
  final_results <- merge(dispatch_curve_results, fossil_agg, by = c("Date", "Hour"), all.x = TRUE)
  final_results[is.na(Old_Fossil_Fuels_adj_MWh), Old_Fossil_Fuels_adj_MWh := 0]
  
  # --- Step 3: Recalculate Battery Integration Using Calibrated Net Energy ---
  # The idea: fossil fuels now contribute to net energy. If net energy is negative,
  # battery discharges (covering shortage); if positive, excess generation charges battery.
  # Assumptions: 
  #  - dispatch_curve_results has columns: Clean_MWh, New_Fossil_Fuel_MWh, Total_import_MWh, 
  #    Storage_MW, and Demand.
  #  - Battery power limit is Storage_MW/8.
  final_results[, battery_power_limit := Storage_MW / 8]
  
  rt_eff <- 0.85
  eta <- sqrt(rt_eff)
  
  # Calculate calibrated net energy (without battery) by including fossil generation
  final_results[, net_energy_calibrated := (Clean_MWh + Old_Fossil_Fuels_adj_MWh + 
                                              New_Fossil_Fuel_MWh + Total_import_MWh) - Demand]
  # Limit the net energy to battery charging/discharging power limits:
  final_results[, net_energy_calibrated := pmin(pmax(net_energy_calibrated, -battery_power_limit), battery_power_limit)]
  
  # Ensure the data is ordered chronologically:
  setorder(final_results, Date, Hour)
  storage_status_initial <- 0
  
  # Simulate battery state hour-by-hour using a similar dynamic approach as in dispatch_curve.
  storage_status_vec <- Reduce(function(prev, i) {
    net <- final_results$net_energy_calibrated[i]
    capacity <- final_results$Storage_MW[i]
    new_storage <- if (net >= 0) prev + net * eta else prev + net / eta
    min(max(new_storage, 0), capacity)
  }, seq_len(nrow(final_results)), init = storage_status_initial, accumulate = TRUE)[-1]
  
  final_results[, Calibrated_Storage_status := storage_status_vec]
  final_results[, Calibrated_Battery_flow := c(0, diff(Calibrated_Storage_status))]
  final_results[, Calibrated_Battery_charge := ifelse(Calibrated_Battery_flow > 0, Calibrated_Battery_flow, 0)]
  final_results[, Calibrated_Battery_discharge := ifelse(Calibrated_Battery_flow < 0, -Calibrated_Battery_flow, 0)]
  
  # --- Step 4: Adjust Imports Based on Calibrated Shortage ---
  # First, compute the preliminary shortage after battery adjustments:
  final_results[, Calibrated_Shortage_MWh := round(
    pmax(Demand - (Clean_MWh + Old_Fossil_Fuels_adj_MWh + New_Fossil_Fuel_MWh +
                     Total_import_MWh + Calibrated_Battery_discharge), 0), 2)]
  
  # If a shortage remains, allow additional imports (up to the maximum limit) to help cover it.
  final_results[, Calibrated_Total_import_net_MWh := ifelse(
    Calibrated_Shortage_MWh > 0,
    Total_import_MWh + pmin(Calibrated_Shortage_MWh, Total_import_max_MWh - Total_import_MWh),
    Total_import_MWh
  )]
  
  # Recalculate the final shortage with the adjusted imports:
  final_results[, Calibrated_Shortage_MWh := round(
    pmax(Demand - (Clean_MWh + Old_Fossil_Fuels_adj_MWh + New_Fossil_Fuel_MWh +
                     Calibrated_Total_import_net_MWh + Calibrated_Battery_discharge), 0), 2)]
  
  # --- (Optional) Step 5: Compute a Net Energy Balance ---
  final_results[, Calibrated_net_energy := (Clean_MWh + Old_Fossil_Fuels_adj_MWh + New_Fossil_Fuel_MWh +
                                              Calibrated_Total_import_net_MWh + Calibrated_Battery_discharge) - Demand]
  
  return(final_results)
}


# Function Execution
dispatch_curve_results <- dispatch_curve(sim = 1, pathway = "A")
dispatch_curve_results <- dispatch_curve_results[227704:227904, ] # Test
fossil_fuels_hourly_results <- dispatch_curve_adjustments(dispatch_curve_results)
final_hourly_results <- dispatch_curve_calibrations(dispatch_curve_results, fossil_fuels_hourly_results)


## ----- OLD ------
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

