## ----- 0. Load libraries and the environment #  ------
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

## ----- 1. Load the data #  ------
## 1. Installed Capacities and Facilities Data
# 1.1 Capacity for Wind, Solar, Large Nuclear, Hydro, Bio, Battery, SMRs, Imports and New Natural Gas
file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/Hourly_Installed_Capacity.csv"
Hourly_Installed_Capacity <- fread(file_path)

file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/SMR_Facility_Data.csv"
SMR_Facility_Data <- fread(file_path)

file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/CleanBaseload_Facility_Data.csv"
CleanBaseload_Facility_Data <- fread(file_path)

# 1.2 Fossil Fuels Data
file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(file_path)
Fossil_Fuels_NPC$Ramp <- ceiling(Fossil_Fuels_NPC$Ramp)
Fossil_Fuels_NPC$Ramp_MWh <- Fossil_Fuels_NPC$Estimated_NameplateCapacity_MW / Fossil_Fuels_NPC$Ramp
# Retain only facilities scheduled to retire after 2025.
Fossil_Fuels_NPC <- Fossil_Fuels_NPC[Retirement_year >= 2025]

file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/New_Fossil_Fuel_Facilities_Data.csv"
New_Fossil_Fuels_NPC <- fread(file_path)

file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/Fossil_Fuel_hr_maxmin.csv"
Fossil_Fuels_hr_maxmin <- fread(file_path)

## 2. Probabilistic Generation Data and Capacity Factors
# 2.1 Wind and Solar
file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/offwind_CF.csv"
Offwind_CF <- fread(file_path)

file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/onwind_CF.csv"
Onwind_CF <- fread(file_path)

file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/solar_CF.csv"
Solar_CF <- fread(file_path)

# 2.2 Fossil fuels
file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/Fossil_Fuel_Generation_Emissions.csv"
Fossil_Fuels_Gen <- fread(file_path)

# 2.3 Imports
file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/Imports_CF.csv"
Imports_CF <- fread(file_path)

## 3. Demand 
file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/demand_data.csv"
Demand_data <- fread(file_path)

## 4. Random Sequence
file_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/Random_Sequence.csv"
Random_sequence <- fread(file_path)

## 5. Set Keys for data.tables
setkey(Hourly_Installed_Capacity, Year, DayLabel, Hour, Pathway)
setkey(Solar_CF, DayLabel, Hour, Percentile)
setkey(Onwind_CF, DayLabel, Hour, Percentile)
setkey(Offwind_CF, DayLabel, Hour, Percentile)
setkey(Imports_CF, DayLabel, Percentile)
setkey(Fossil_Fuels_Gen, DayLabel, Hour, Facility_Unit.ID)
setkey(Demand_data, Date, Hour)
setkey(Fossil_Fuels_hr_maxmin, Date, Hour)

## ----- fix dates
Hourly_Installed_Capacity[, Date := as.Date(Date)] 
Demand_data[, Date := as.Date(Date)]
Fossil_Fuels_hr_maxmin[, Date := as.Date(Date)]


## ----- 2. Functions #  ------
# FUNCTION: Applies Dispatch Curve
dispatch_curve <- function(sim, pathway) {
    #  ------
    # STEP 1: Filter and Merge Data
    #  ------
    # Filter capacity data for the chosen pathway (years ≥2025) and merge with demand data.
    cap_data <- Hourly_Installed_Capacity[Pathway == pathway & Year >= 2025]
    dispatch_data <- merge(Demand_data, cap_data, by = c("Date", "Hour"))
    dispatch_data[, Simulation := sim]
    setorder(dispatch_data, Date, Hour)
    
    #  ------
      # STEP 2: Extract Random Percentile Indices
      #  ------
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
    
    #  ------
      # STEP 3: Lookup Capacity Factor (CF) Values
      #  ------
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
    
    #  ------
      # STEP 4: Calculate Clean Generation
      #  ------
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
    
    #  ------
      # STEP 5: Battery Storage Integration
      #  ------
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
    
    #  ------
      # STEP 6: Fossil Fuel Generation Requirements
      #  ------
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
    
    #  ------
      # STEP 7: Import Generation and Splitting QC Imports
      #  ------
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
    
    #  ------
      # STEP 8: Fossil Generation and Shortage Calculation (Posterior Distribution)
      #  ------
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
    
    #  ------
      # STEP 9: Adjust columns
      #  ------
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
  unique_units <- unique(Fossil_Fuels_NPC$Facility_Unit.ID)
  n_units <- length(unique_units)
  n_total_rand <- 6 + n_units
  
  # --- Index Calculation for Randomization ---
  base_indices <- ((0:(n_hours - 1)) * n_total_rand)
  shift_values <- 6:(6 + n_units - 1)  # Offsets for fossil facilities
  idx_fossil <- sapply(shift_values, function(s) (base_indices + s) %% n_random + 1)
  
  # Assemble a long-format data.table mapping percentiles to each facility.
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
  setkey(results_updated, DayLabel, Hour, Facility_Unit.ID)
  setkey(Fossil_Fuels_Gen, DayLabel, Hour, Facility_Unit.ID)
  results_updated <- Fossil_Fuels_Gen[results_updated, nomatch = 0]
  
  # --- Vectorized Extraction of Percentile-Specific Generation and Emission Data ---
  fields <- c("Gen", "CO2", "NOx", "SO2", "HI")
  for(field in fields) {
    # Identify all relevant columns for the field (e.g., "Gen_1", "Gen_2", etc.)
    field_cols <- grep(paste0("^", field, "_"), names(results_updated), value = TRUE)
    if(length(field_cols) > 0) {
      # Create a vector of desired column names based on the Percentile value.
      desired_cols <- paste0(field, "_", results_updated$Percentile)
      # For each row, find the index of the desired column name within field_cols.
      idx <- match(desired_cols, field_cols)
      # Convert the relevant columns to a matrix.
      mat <- as.matrix(results_updated[, ..field_cols])
      # Use matrix indexing to extract the value (if a match wasn’t found, NA is returned).
      results_updated[, (paste0(field, "_value")) := mat[cbind(seq_len(.N), idx)]]
    } else {
      results_updated[, (paste0(field, "_value")) := NA_real_]
    }
  }
  
  # Rename the extracted columns for clarity.
  setnames(results_updated,
           old = c("Gen_value", "CO2_value", "NOx_value", "SO2_value", "HI_value"),
           new = c("Gen_MWh", "CO2_tons", "NOx_lbs", "SO2_lbs", "HI_mmBtu"))
  
  # Retain only the essential columns.
  results_updated <- results_updated[, .(Date, DayLabel, Hour, Facility_Unit.ID, Old_Fossil_Fuels_net_MWh,
                                         Gen_MWh, CO2_tons, NOx_lbs, SO2_lbs, HI_mmBtu)]
  
  # Merge additional facility details from the Fossil_Fuels_NPC dataset.
  Fossil_Fuels_NPC_subset <- Fossil_Fuels_NPC[, .(Facility_Unit.ID, Estimated_NameplateCapacity_MW, 
                                                  min_gen_MW, max_gen_MW, Ramp, Ramp_MWh, Retirement_year, 
                                                  mean_CO2_tons_MW_estimate, mean_CO2_tons_MW, mean_NOx_lbs_MW_estimate,
                                                  mean_NOx_lbs_MW, mean_SO2_lbs_MW_estimate, mean_SO2_lbs_MW, mean_HI_mmBtu_per_MW_estimate = mean_HI_mmBtu_per_MW)]
  results_updated <- merge(Fossil_Fuels_NPC_subset, results_updated, by = "Facility_Unit.ID", all.y = TRUE)
  
  # --- Filter Out Retired Facilities ---
  if (!(pathway %in% c("A", "D"))) {
    results_updated <- results_updated[Retirement_year >= year(Date)]
  }
  
  # --- Calculate Capacity Factor (CF) ---
  results_updated[, CF_hr := Gen_MWh / Estimated_NameplateCapacity_MW]
  setorder(results_updated, Date, Hour, CF_hr)
  
  # --- Adjust Generation to Meet Capacity Constraints ---
  results_updated <- results_updated[, {
    dt <- copy(.SD)  # Work on a copy to avoid modifying locked .SD
    total_gen <- sum(dt$Gen_MWh)
    net_cap <- unique(dt$Old_Fossil_Fuels_net_MWh)
    
    if (total_gen <= net_cap) {
      dt[, Gen_MWh_used := Gen_MWh]
      dt
    } else {
      dt_sorted <- dt[order(CF_hr)]
      overage <- total_gen - net_cap
      Gen_MWh_orig <- dt_sorted$Gen_MWh
      reducible_raw <- pmin(Gen_MWh_orig, overage)
      cumulative_reducible <- cumsum(reducible_raw)
      reducible <- ifelse(cumulative_reducible <= overage,
                          reducible_raw,
                          pmax(0, overage - shift(cumulative_reducible, fill = 0)))
      dt_sorted[, Gen_MWh_used := Gen_MWh_orig - reducible]
      dt_sorted
    }
  }, by = .(Date, Hour)]
  
  setorder(results_updated, Facility_Unit.ID, Date, Hour)
  
  # --- Time Series Adjustments ---
  # Create a POSIXct timestamp (using Eastern Time).
  results_updated[, timestamp := as.POSIXct(paste(Date, sprintf("%02d:00:00", Hour - 1)), 
                                            format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]
  
  # For each facility, generate a full sequence of hourly timestamps and merge with existing data.
  results_updated <- results_updated[, {
    ts_seq <- seq(min(timestamp), max(timestamp), by = "hour")
    dt_complete <- data.table(timestamp = ts_seq)
    merge(dt_complete, .SD, by = "timestamp", all.x = TRUE)
  }, by = Facility_Unit.ID]
  
  # Update Date and Hour based on the new timestamp.
  results_updated[, Date := as.Date(format(timestamp, tz = "America/New_York"))]
  results_updated[, Hour := hour(timestamp) + 1]
  
  # Fill missing Ramp_MWh values using last observation carried forward then backward.
  results_updated[, Ramp_MWh := na.locf(Ramp_MWh, na.rm = FALSE), by = Facility_Unit.ID]
  results_updated[, Ramp_MWh := na.locf(Ramp_MWh, fromLast = TRUE, na.rm = FALSE), by = Facility_Unit.ID]
  
  # Initialize adjusted generation; replace missing values with 0.
  results_updated[, Gen_MWh_adj := fifelse(is.na(Gen_MWh_used), 0, Gen_MWh_used)]
  
  # --- Backward Adjustment Pass ---
  results_updated[, Gen_MWh_adj := {
    temp <- Gen_MWh_adj
    ramp <- unique(Ramp_MWh)[1]
    if (is.na(ramp)) ramp <- 24
    n <- .N
    if (n < 2) temp else {
      for (i in n:2) {
        if (!is.na(temp[i])) {
          required_prev <- temp[i] - ramp
          if (is.na(temp[i - 1]) || temp[i - 1] < required_prev)
            temp[i - 1] <- required_prev
        }
      }
      temp
    }
  }, by = Facility_Unit.ID]
  
  # --- Forward Adjustment Pass ---
  results_updated[, Gen_MWh_adj := {
    temp <- Gen_MWh_adj
    ramp <- unique(Ramp_MWh)[1]
    if (is.na(ramp)) ramp <- 24
    n <- .N
    if (n < 2) temp else {
      for (i in 2:n) {
        if (!is.na(temp[i - 1]) && !is.na(temp[i])) {
          delta <- temp[i] - temp[i - 1]
          if (delta < -ramp)
            temp[i] <- temp[i - 1] - ramp
        }
      }
      temp
    }
  }, by = Facility_Unit.ID]
  
  # Fill missing minimum generation values.
  results_updated[, min_gen_MW := na.locf(min_gen_MW, na.rm = FALSE), by = Facility_Unit.ID]
  # Ensure that adjusted generation is not below the minimum and round the result.
  results_updated[, Gen_MWh_adj := round(pmax(Gen_MWh_adj, min_gen_MW), 2)]
  
  # --- Cleanup Temporary Columns ---
  results_updated[, c("Ramp", "Ramp_MWh", "Estimated_NameplateCapacity_MW", 
                      "min_gen_MW", "max_gen_MW", "Retirement_year",
                      "Gen_MWh", "CF_hr", "timestamp") := NULL]
  
  # Update DayLabel to represent the day of the year.
  results_updated[, DayLabel := as.integer(format(Date, "%j"))]
  results_updated[is.na(Old_Fossil_Fuels_net_MWh), Old_Fossil_Fuels_net_MWh := 0]
  
  # --- Emission Calculations ---
  # Compute emission intensity ratios per MWh of generation.
  results_updated[, `:=`(
    CO2_intensity = CO2_tons / Gen_MWh_adj,
    NOx_intensity = NOx_lbs / Gen_MWh_adj,
    SO2_intensity = SO2_lbs / Gen_MWh_adj,
    HI_intensity  = HI_mmBtu / Gen_MWh_adj
  )]
  
  # Calculate mean emission intensities per facility.
  mean_intensities <- results_updated[, .(
    mean_CO2_ton_per_MWh = mean(CO2_intensity[is.finite(CO2_intensity)], na.rm = TRUE),
    mean_NOx_lb_per_MWh  = mean(NOx_intensity[is.finite(NOx_intensity)], na.rm = TRUE),
    mean_SO2_lb_per_MWh  = mean(SO2_intensity[is.finite(SO2_intensity)], na.rm = TRUE),
    mean_HI_mmBtu_per_MWh = mean(HI_intensity[is.finite(HI_intensity)], na.rm = TRUE)
  ), by = Facility_Unit.ID]
  
  # Calculate overall global mean intensities.
  global_means <- mean_intensities[, .(
    global_mean_CO2 = mean(mean_CO2_ton_per_MWh, na.rm = TRUE),
    global_mean_NOx = mean(mean_NOx_lb_per_MWh, na.rm = TRUE),
    global_mean_SO2 = mean(mean_SO2_lb_per_MWh, na.rm = TRUE),
    global_mean_HI  = mean(mean_HI_mmBtu_per_MWh, na.rm = TRUE)
  )]
  
  # Replace any missing facility mean intensities with the corresponding global means.
  mean_intensities[is.na(mean_CO2_ton_per_MWh), mean_CO2_ton_per_MWh := global_means$global_mean_CO2]
  mean_intensities[is.na(mean_NOx_lb_per_MWh), mean_NOx_lb_per_MWh := global_means$global_mean_NOx]
  mean_intensities[is.na(mean_SO2_lb_per_MWh), mean_SO2_lb_per_MWh := global_means$global_mean_SO2]
  mean_intensities[is.na(mean_HI_mmBtu_per_MWh), mean_HI_mmBtu_per_MWh := global_means$global_mean_HI]
  
  # Merge facility mean intensities back.
  results_updated <- merge(results_updated, mean_intensities, by = "Facility_Unit.ID", all.x = TRUE)
  
  # For CO2: Create a new column 'CO2_ton_per_MWh'
  results_updated[, CO2_ton_per_MWh := ifelse(
    !is.na(mean_CO2_tons_MW), 
    mean_CO2_tons_MW, 
    ifelse(
      !is.na(mean_CO2_tons_MW_estimate),
      mean_CO2_tons_MW_estimate,
      CO2_intensity
    )
  )]
  
  # For NOx: Create 'NOx_lb_per_MWh'
  results_updated[, NOx_lb_per_MWh := ifelse(
    !is.na(mean_NOx_lbs_MW),
    mean_NOx_lbs_MW,
    ifelse(
      !is.na(mean_NOx_lbs_MW_estimate),
      mean_NOx_lbs_MW_estimate,
      NOx_intensity
    )
  )]
  
  # For SO2: Create 'SO2_lb_per_MWh'
  results_updated[, SO2_lb_per_MWh := ifelse(
    !is.na(mean_SO2_lbs_MW),
    mean_SO2_lbs_MW,
    ifelse(
      !is.na(mean_SO2_lbs_MW_estimate),
      mean_SO2_lbs_MW_estimate,
      SO2_intensity
    )
  )]
  
  # For HI: Create a new column 'HI_mmBtu_per_MW'
  results_updated[, HI_mmBtu_per_MWh := ifelse(
    !is.na(mean_HI_mmBtu_per_MW_estimate),
    mean_HI_mmBtu_per_MW_estimate,
    mean_HI_mmBtu_per_MWh
  )]
  
  # Estimate emissions based on adjusted generation.
  # Update CO2_tons only if its value is NA, infinite, or zero.
  results_updated[
    is.na(CO2_tons) | is.infinite(CO2_tons) | (CO2_tons == 0),
    CO2_tons := fcoalesce(Gen_MWh_adj, 0) * fcoalesce(CO2_ton_per_MWh, 0)
  ]
  
  # Update NOx_lbs only if its value is NA, infinite, or zero.
  results_updated[
    is.na(NOx_lbs) | is.infinite(NOx_lbs) | (NOx_lbs == 0),
    NOx_lbs := fcoalesce(Gen_MWh_adj, 0) * fcoalesce(NOx_lb_per_MWh, 0)
  ]
  
  # Update SO2_lbs only if its value is NA, infinite, or zero.
  results_updated[
    is.na(SO2_lbs) | is.infinite(SO2_lbs) | (SO2_lbs == 0),
    SO2_lbs := fcoalesce(Gen_MWh_adj, 0) * fcoalesce(SO2_lb_per_MWh, 0)
  ]
  
  # Update HI_mmBtu only if its value is NA, infinite, or zero.
  results_updated[
    is.na(HI_mmBtu) | is.infinite(HI_mmBtu) | (HI_mmBtu == 0),
    HI_mmBtu := fcoalesce(Gen_MWh_adj, 0) * fcoalesce(HI_mmBtu_per_MWh, 0)
  ]
  
  
  # Remove raw intensity columns, keeping only the mean values.
  results_updated[, c("CO2_intensity", "NOx_intensity", "SO2_intensity", "HI_intensity",
                      "mean_HI_mmBtu_per_MWh", "mean_SO2_lb_per_MWh", "mean_NOx_lb_per_MWh", "mean_CO2_ton_per_MWh") := NULL]
  
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


## ----- 3. Run the simulations ------
# Set all pathways from Hourly_Installed_Capacity$Pathway
pathways <- unique(Hourly_Installed_Capacity$Pathway)
n_simulations <- 10
sim_start <- 11
sim_idx <- sim_start:(n_simulations + sim_start - 1)

# Loop over each simulation and pathway combination
for(sim in sim_idx) {
  for(pathway in pathways) {
    
    # Define the directory to search
    results_dir <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/1 Stepwise/"
    
    # Create a pattern that matches any hourly results file for the current simulation and pathway
    pattern <- sprintf("^Hourly_Results_sim%d_path%s_time.*\\.csv$", sim, pathway)
    existing_files <- list.files(path = results_dir, pattern = pattern, full.names = TRUE)
    
    # If matching files exist, skip the current iteration
    if(length(existing_files) > 0) {
      cat(sprintf("File(s) for simulation %d with pathway %s already exist. Skipping.\n", sim, pathway))
      next
    }
    
    cat("Running simulation:", sim, "\n")
    cat("Processing pathway:", pathway, "\n")
    
    # Execute the dispatch curve for the current simulation and pathway
    dispatch_curve_results <- dispatch_curve(sim, pathway)
    
    # Get fossil fuels hourly adjustments based on the dispatch results
    fossil_fuels_hourly_results <- dispatch_curve_adjustments(dispatch_curve_results)
    
    # Calibrate the final hourly results using the dispatch results and the adjustments
    final_hourly_results <- dispatch_curve_calibrations(dispatch_curve_results, fossil_fuels_hourly_results)
    
    # Tag results with simulation and pathway information for later identification
    final_hourly_results$Simulation <- sim
    final_hourly_results$Pathway <- pathway
    fossil_fuels_hourly_results$Simulation <- sim
    fossil_fuels_hourly_results$Pathway <- pathway
    
    ## ----- 4. Save results to CSV files ------
    # Get the current timestamp
    timestamp <- format(Sys.time(), "%Y_%m_%d_%H%M")
    
    # Define the file paths with simulation and pathway included in the file name
    hourly_results_path <- sprintf("%sHourly_Results_sim%d_path%s_time%s.csv", 
                                   results_dir, sim, pathway, timestamp)
    facility_level_results_path <- sprintf("%sFacility_Level_Results_sim%d_path%s_time%s.csv", 
                                           results_dir, sim, pathway, timestamp)
    
    # Save the results to CSV files
    write.csv(final_hourly_results, hourly_results_path, row.names = FALSE)
    write.csv(fossil_fuels_hourly_results, facility_level_results_path, row.names = FALSE)
    
    # Remove objects from memory to free up space for the next simulation/pathway iteration
    rm(dispatch_curve_results, fossil_fuels_hourly_results, final_hourly_results)
    gc()  # Force garbage collection to reclaim memory
  }
}


## ----- 4. Notification Tool #  ------
# Pushover alert - Pushover credentials
pushover_user <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST("https://api.pushover.net/1/messages.json",
                 body = list(
                   token = pushover_token,
                   user = pushover_user,
                   message = "The code executed successfully."
                 ),
                 encode = "form")

