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
  
  return(dispatch_data)
}


# FUNCTION: Applies Fossil Fuel Constraints, Calculates Emissions 
# and re-calculates Battery Status and Shortages
dispatch_curve_adjustments <- function(results) {
  # --- Preliminaries ---
  sim <- results$Simulation[1]
  pathway <- results$Pathway[1]
  random_vector <- Random_sequence[[sim]]
  n_random <- length(random_vector)
  n_hours <- nrow(results)

  # Fossil fuels
  # Filter rows where Retirement_year > 2025
  Fossil_Fuels_NPC <- Fossil_Fuels_NPC[Fossil_Fuels_NPC$Retirement_year > 2025, ]
  # Get unique Facility_Unit.ID values from the filtered data
  unique_units <- unique(Fossil_Fuels_NPC$Facility_Unit.ID)
  n_units <- length(unique_units)
  n_total_rand <- 6 + n_units
  
  # Compute a base index for each hour; note that these indices are 0-indexed.
  base_indices <- ((0:(n_hours - 1)) * n_total_rand)
  
  # For each asset, compute the index into the random_vector (using modulo to cycle)
  shift_values <- 6:(6 + n_units - 1) #fossil 
  idx_fossil <- sapply(shift_values, function(s) (base_indices + s) %% n_random + 1)
  # Build a long data.table of percentiles
  results_updated <- rbindlist(
    lapply(seq_along(unique_units), function(j) {
      data.table(
        Date = results$Date,
        DayLabel = results$DayLabel,
        Hour = results$Hour,
        Old_Fossil_Fuels_net_MWh = results$Old_Fossil_Fuels_net_MWh,
        Facility_Unit.ID = unique_units[j],
        Percentile = random_vector[idx_fossil[, j]]
      )
    })
  )
  
  # Step 1: Join percentiles_dt with Fossil_Fuels_Gen by DayLabel, Hour, Facility_Unit.ID
  setkey(results_updated, DayLabel, Hour, Facility_Unit.ID)
  setkey(Fossil_Fuels_Gen, DayLabel, Hour, Facility_Unit.ID)
  
  results_updated <- Fossil_Fuels_Gen[results_updated, nomatch = 0]
  
  # List of fields to extract per percentile
  fields <- c("Gen", "CO2", "NOx", "SO2", "HI")
  
  # Use mapply to extract all fields in one go
  for (field in fields) {
    results_updated[[paste0(field, "_value")]] <- mapply(function(p, row) {
      colname <- paste0(field, "_", p)
      if (colname %in% names(row)) row[[colname]] else NA_real_
    }, results_updated$Percentile, split(results_updated, seq_len(nrow(results_updated))))
  }
  
  setnames(results_updated, old = c("Gen_value", "CO2_value", "NOx_value", "SO2_value", "HI_value"),
           new = c("Gen_MWh", "CO2_tons", "NOx_lbs", "SO2_lbs", "HI_mmBtu"))
  
  # Keep only relevant output
  results_updated <- results_updated[, .(Date, DayLabel, Hour, Facility_Unit.ID, Old_Fossil_Fuels_net_MWh,
                                       Gen_MWh, CO2_tons, NOx_lbs, SO2_lbs, HI_mmBtu)]
  
  Fossil_Fuels_NPC_subset <- Fossil_Fuels_NPC[, .(Facility_Unit.ID, Estimated_NameplateCapacity_MW, Max_Hourly_HI_Rate,
                                                  min_gen_MW, max_gen_MW, Ramp, Ramp_MWh, Retirement_year)]
  
  results_updated <- Fossil_Fuels_NPC_subset[results_updated, on = .(Facility_Unit.ID), nomatch = 0]
  
  # Remove retired facilities
  if (!(pathway %in% c("A", "D"))) {
    results_updated <- results_updated[results_updated$Retirement_year >= year(results_updated$Date), ]
  }
  
  # Calculate CF
  results_updated$CF_hr <- results_updated$Gen_MWh / results_updated$Estimated_NameplateCapacity_MW
  setorder(results_updated, Date, Hour, CF_hr)
  
  results_updated <- results_updated[, {
    total_gen <- sum(Gen_MWh)
    net_cap <- unique(Old_Fossil_Fuels_net_MWh)
    
    if (total_gen <= net_cap) {
      .SD[, Gen_MWh_used := Gen_MWh]  # No change needed, just copy original to used
    } else {
      # Order by lowest capacity factor
      .SD_sorted <- .SD[order(CF_hr)]
      
      overage <- total_gen - net_cap
      
      Gen_MWh_orig <- .SD_sorted$Gen_MWh
      
      # Calculate how much can be reduced from each row
      reducible_raw <- pmin(Gen_MWh_orig, overage)
      cumulative_reducible <- cumsum(reducible_raw)
      reducible <- ifelse(cumulative_reducible <= overage, reducible_raw, pmax(0, overage - shift(cumulative_reducible, fill = 0)))
      
      # Calculate adjusted values
      Gen_MWh_adj <- Gen_MWh_orig - reducible
      
      .SD_sorted[, Gen_MWh_used := Gen_MWh_adj]
      
      # Restore original row order within group
      .SD_sorted[order(match(Facility_Unit.ID, .SD$Facility_Unit.ID))]
    }
  }, by = .(Date, Hour)]
  
  # Ensure chronological order
  setorder(results_updated, Facility_Unit.ID, Date, Hour)
  

  return(results_updated)
}


# Function Execution
results <- dispatch_curve(sim = 1, pathway = "B1")
results <- results[1:100, ] # Test
adjusted_results <- dispatch_curve_adjustments(results)


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
