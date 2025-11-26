## ===========================
## Decarbonization Dispatch Run
## ===========================

## ----- 0) Libraries -----
suppressPackageStartupMessages({
  library(data.table)
  library(httr)
  library(htmltools)
  library(jsonlite)
  library(lubridate)
  library(zoo)
  library(digest)   # for SHA256 checksums in the manifest
})

# record start time for manifest
._run_started_at <- Sys.time()

## ----- 0.1) Project paths -----
proj_root <- getwd()  # WORKING DIRECTORY
p <- function(...) file.path(proj_root, ...)
`%+%` <- function(a,b) paste0(a,b)

## ----- 0.2) Runtime configuration (battery + run notes) -----
cfg <- list(
  rt_eff = 0.85,                 # round trip efficiency (symmetric)
  duration_hours = 8,            # energy-to-power duration (h)
  inverter_col = "Inverter_MW",  # optional column in Hourly_Installed_Capacity
  allow_grid_charging = FALSE,   # (no price model here)
  curtailment_only_charging = TRUE,
  retention_hours = Inf,         # SOC_t = SOC_{t-1} * exp(-1/retention_hours)
  allow_multiday_carry = TRUE,   # if FALSE, SOC resets at midnight
  high_penalty_unserved = 10000  # documentation only
)

## ----- 1) Load data -----
# 1.1 Capacities & facilities
Hourly_Installed_Capacity <- fread(p("1 Decarbonization Pathways", "Hourly_Installed_Capacity.csv"))

SMR_Facility_Data <- fread(p("2 Generation Expansion Model","2 Generation","1 Clean Generation",
                             "2 SMR","1 SMR Facility Data","SMR_Facility_Data.csv"))

CleanBaseload_Facility_Data <- fread(p("2 Generation Expansion Model","2 Generation","1 Clean Generation",
                                       "3 Large nuclear hydro and bio","1 Clean Baseload Facility Data",
                                       "CleanBaseload_Facility_Data.csv"))

# 1.2 Fossil fuels
Fossil_Fuels_NPC <- fread(p("2 Generation Expansion Model","2 Generation","2 Fossil Generation",
                            "1 Existing Fossil Fuels","1 Fossil Fuels Facilities Data",
                            "Fossil_Fuel_Facilities_Data.csv"))
Fossil_Fuels_NPC$Ramp <- ceiling(Fossil_Fuels_NPC$Ramp)
Fossil_Fuels_NPC$Ramp_MWh <- Fossil_Fuels_NPC$Estimated_NameplateCapacity_MW / Fossil_Fuels_NPC$Ramp
Fossil_Fuels_NPC <- Fossil_Fuels_NPC[Retirement_year >= 2025]

New_Fossil_Fuels_NPC <- fread(p("2 Generation Expansion Model","2 Generation","2 Fossil Generation",
                                "2 New Fossil Fuels","1 New Fossil Fuels Facilities Data",
                                "New_Fossil_Fuel_Facilities_Data.csv"))

Fossil_Fuels_hr_maxmin <- fread(p("2 Generation Expansion Model","2 Generation","2 Fossil Generation",
                                  "1 Existing Fossil Fuels","2 Fossil Fuels Generation and Emissions",
                                  "Fossil_Fuel_hr_maxmin.csv"))

# 2) Probabilistic generation & CFs
Offwind_CF <- fread(p("2 Generation Expansion Model","2 Generation","1 Clean Generation",
                      "1 Wind and Solar","1 Wind and Solar CF","offwind_CF.csv"))
Onwind_CF  <- fread(p("2 Generation Expansion Model","2 Generation","1 Clean Generation",
                      "1 Wind and Solar","1 Wind and Solar CF","onwind_CF.csv"))
Solar_CF   <- fread(p("2 Generation Expansion Model","2 Generation","1 Clean Generation",
                      "1 Wind and Solar","1 Wind and Solar CF","solar_CF.csv"))

Fossil_Fuels_Gen <- fread(p("2 Generation Expansion Model","2 Generation","2 Fossil Generation", "1 Existing Fossil Fuels",
                            "2 Fossil Fuels Generation and Emissions","Fossil_Fuel_Generation_Emissions.csv"))

Imports_CF <- fread(p("2 Generation Expansion Model","3 Imports","1 Imports CF","Imports_CF.csv"))

# 3) Demand
Demand_data <- fread(p("2 Generation Expansion Model","1 Demand","1 Hourly Demand","demand_data.csv"))

# 4) Random sequence
Random_sequence <- fread(p("2 Generation Expansion Model","4 Randomization","1 Randomized Data","Random_Sequence.csv"))

# 5) Keys
setkey(Hourly_Installed_Capacity, Year, DayLabel, Hour, Pathway)
setkey(Solar_CF, DayLabel, Hour, Percentile)
setkey(Onwind_CF, DayLabel, Hour, Percentile)
setkey(Offwind_CF, DayLabel, Hour, Percentile)
setkey(Imports_CF, DayLabel, Percentile)
setkey(Fossil_Fuels_Gen, DayLabel, Hour, Facility_Unit.ID)
setkey(Demand_data, Date, Hour)
setkey(Fossil_Fuels_hr_maxmin, Date, Hour)

## ----- 2) Functions -----
# ---- Dispatch curve ----
dispatch_curve <- function(sim, pathway) {
  # STEP 1: Filter & merge
  cap_data <- Hourly_Installed_Capacity[Pathway == pathway & Year >= 2025]
  dispatch_data <- merge(Demand_data, cap_data, by = c("Date", "Hour"))
  dispatch_data[, `:=`(Simulation = sim)]
  setorder(dispatch_data, Date, Hour)
  
  # STEP 2: Random percentiles
  random_vector <- Random_sequence[[sim]]
  n_random <- length(random_vector)
  n_hours <- nrow(dispatch_data)
  n_total_rand <- 6
  base_indices <- ((0:(n_hours - 1)) * n_total_rand)
  
  idx_solar    <- (base_indices + 0) %% n_random + 1
  idx_onshore  <- (base_indices + 1) %% n_random + 1
  idx_offshore <- (base_indices + 2) %% n_random + 1
  idx_impHQ    <- (base_indices + 3) %% n_random + 1
  idx_impNYISO <- (base_indices + 4) %% n_random + 1
  idx_impNBSO  <- (base_indices + 5) %% n_random + 1
  
  dispatch_data[, Percentile_Solar    := random_vector[idx_solar]]
  dispatch_data[, Percentile_Onshore  := random_vector[idx_onshore]]
  dispatch_data[, Percentile_Offshore := random_vector[idx_offshore]]
  dispatch_data[, Percentile_ImpHQ    := random_vector[idx_impHQ]]
  dispatch_data[, Percentile_ImpNYISO := random_vector[idx_impNYISO]]
  dispatch_data[, Percentile_ImpNBSO  := random_vector[idx_impNBSO]]
  
  # STEP 3: CF (availability) lookups
  dispatch_data <- merge(dispatch_data,
                         Solar_CF[, .(DayLabel, Hour, Percentile, CF)],
                         by.x = c("DayLabel", "Hour", "Percentile_Solar"),
                         by.y = c("DayLabel", "Hour", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "CF", "Solar_CF")
  
  dispatch_data <- merge(dispatch_data,
                         Onwind_CF[, .(DayLabel, Hour, Percentile, CF)],
                         by.x = c("DayLabel", "Hour", "Percentile_Onshore"),
                         by.y = c("DayLabel", "Hour", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "CF", "Onshore_CF")
  
  dispatch_data <- merge(dispatch_data,
                         Offwind_CF[, .(DayLabel, Hour, Percentile, CF)],
                         by.x = c("DayLabel", "Hour", "Percentile_Offshore"),
                         by.y = c("DayLabel", "Hour", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "CF", "Offshore_CF")
  
  dispatch_data <- merge(dispatch_data,
                         Imports_CF[, .(DayLabel, Percentile, imports_QC)],
                         by.x = c("DayLabel", "Percentile_ImpHQ"),
                         by.y = c("DayLabel", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "imports_QC", "Import_HQ_CF")
  
  dispatch_data <- merge(dispatch_data,
                         Imports_CF[, .(DayLabel, Percentile, imports_NYISO)],
                         by.x = c("DayLabel", "Percentile_ImpNYISO"),
                         by.y = c("DayLabel", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "imports_NYISO", "Import_NYISO_CF")
  
  dispatch_data <- merge(dispatch_data,
                         Imports_CF[, .(DayLabel, Percentile, imports_NBSO)],
                         by.x = c("DayLabel", "Percentile_ImpNBSO"),
                         by.y = c("DayLabel", "Percentile"),
                         all.x = TRUE)
  setnames(dispatch_data, "imports_NBSO", "Import_NBSO_CF")
  
  # STEP 4: Clean generation (AF = availability factors — aliases added)
  dispatch_data[, Solar_MWh    := Solar_MW * Solar_CF]
  dispatch_data[, Onshore_MWh  := Onshore_Wind_MW * Onshore_CF]
  dispatch_data[, Offshore_MWh := Offshore_Wind_MW * Offshore_CF]
  
  nuclear_CF <- as.numeric(CleanBaseload_Facility_Data[technology == "Nuclear", value])
  hydro_CF   <- as.numeric(CleanBaseload_Facility_Data[technology == "Hydropower", value])
  bio_CF     <- as.numeric(CleanBaseload_Facility_Data[technology == "Biopower", value])
  
  dispatch_data[, Nuclear_MWh := Nuclear_MW * nuclear_CF]
  dispatch_data[, Hydro_MWh   := Hydropower_MW * hydro_CF]
  dispatch_data[, Biomass_MWh := Biomass_MW * bio_CF]
  
  SMR_CF <- SMR_Facility_Data$CF
  dispatch_data[, SMR_MWh := SMR_MW * SMR_CF]
  
  dispatch_data[, Clean_MWh := Solar_MWh + Onshore_MWh + Offshore_MWh +
                  Nuclear_MWh + Hydro_MWh + Biomass_MWh + SMR_MWh]
  # AF alias columns for clarity in outputs/docs
  dispatch_data[, `:=`(Solar_AF = Solar_CF, Onshore_AF = Onshore_CF, Offshore_AF = Offshore_CF)]
  setorder(dispatch_data, Date, Hour)
  
  # STEP 5: Battery integration (symmetric RTE; guards; inverter limit; duration/retention)
  rt_eff <- cfg$rt_eff
  eta <- sqrt(rt_eff)
  duration_h <- cfg$duration_hours
  rho <- if (is.finite(cfg$retention_hours)) exp(-1 / cfg$retention_hours) else 1.0
  
  inverter_col_present <- cfg$inverter_col %in% names(dispatch_data)
  dispatch_data[, derived_power_limit := Storage_MW / duration_h]
  if (inverter_col_present) {
    dispatch_data[, inverter_limit_MW := pmin(get(cfg$inverter_col), derived_power_limit, na.rm = TRUE)]
  } else {
    dispatch_data[, inverter_limit_MW := derived_power_limit]
  }
  dispatch_data[, battery_power_limit := pmax(0, inverter_limit_MW)]
  
  # Imports (and max import capability)
  dispatch_data[, Spot_Market_Imports_HQ_MWh := Spot_Market_Imports_HQ_MW * Import_HQ_CF]
  dispatch_data[, Import_NYISO_MWh    := Imports_NYISO_MW * Import_NYISO_CF]
  dispatch_data[, Import_NBSO_MWh     := Imports_NBSO_MW * Import_NBSO_CF]
  imports_max_CF <- 0.95
  dispatch_data[, Long_Term_Imports_HQ_MWh := Long_Term_Imports_HQ_MW * imports_max_CF]
  dispatch_data[, Total_import_MWh := Long_Term_Imports_HQ_MWh + Spot_Market_Imports_HQ_MWh +
                  Import_NYISO_MWh + Import_NBSO_MWh]
  # FIX: correct cap (no Imports_HQ_MW)
  dispatch_data[, Total_import_max_MWh :=
                  (Long_Term_Imports_HQ_MW + Spot_Market_Imports_HQ_MW + Imports_NYISO_MW + Imports_NBSO_MW) * imports_max_CF]
  
  # Surplus/deficit based on clean + imports only
  dispatch_data[, surplus_clean_imp := (Clean_MWh + Total_import_MWh) - Demand]
  dispatch_data[, deficit_clean_imp := pmax(0, Demand - (Clean_MWh + Total_import_MWh))]
  
  nH <- nrow(dispatch_data)
  soc <- numeric(nH); soc_prev <- 0.0
  is_midnight <- dispatch_data$Hour == 1
  charge_grid  <- numeric(nH)
  discharge_grid <- numeric(nH)
  
  for (i in seq_len(nH)) {
    cap_MWh <- dispatch_data$Storage_MW[i]
    p_lim   <- dispatch_data$battery_power_limit[i]
    
    # retention + optional daily reset
    soc_prev <- soc_prev * rho
    if (!cfg$allow_multiday_carry && is_midnight[i]) soc_prev <- 0.0
    
    headroom_soc  <- pmax(0, cap_MWh - soc_prev)
    energy_soc    <- pmax(0, soc_prev)
    
    # Discharge only if clean + imports insufficient
    d_cap_grid <- dispatch_data$deficit_clean_imp[i]
    d_grid <- min(d_cap_grid, p_lim, energy_soc * eta)
    
    if (d_grid > 0) {
      c_grid <- 0
      soc_new <- soc_prev - d_grid / eta
    } else {
      if (cfg$curtailment_only_charging) {
        c_cap_grid <- pmax(0, dispatch_data$surplus_clean_imp[i])
      } else if (cfg$allow_grid_charging) {
        c_cap_grid <- p_lim
      } else {
        c_cap_grid <- pmax(0, dispatch_data$surplus_clean_imp[i])
      }
      c_grid <- min(c_cap_grid, p_lim, headroom_soc / eta)
      d_grid <- 0
      soc_new <- soc_prev + c_grid * eta
    }
    
    soc_new <- min(max(soc_new, 0), cap_MWh)
    
    soc[i] <- soc_new
    charge_grid[i] <- c_grid
    discharge_grid[i] <- d_grid
    soc_prev <- soc_new
  }
  
  dispatch_data[, Storage_status := soc]
  dispatch_data[, Battery_charge_grid := charge_grid]        # MWh absorbed from grid
  dispatch_data[, Battery_discharge_grid := discharge_grid]  # MWh delivered to grid
  dispatch_data[, Battery_flow := Battery_charge_grid * eta - Battery_discharge_grid / eta] # SOC delta (for transparency)
  
  # Invariant checks (warn only)
  if (any(dispatch_data$Battery_charge_grid > 0 & dispatch_data$Battery_discharge_grid > 0, na.rm = TRUE)) {
    warning("Battery charging and discharging in the same hour detected; check guards.")
  }
  if (any(dispatch_data$surplus_clean_imp > 0 & dispatch_data$Battery_discharge_grid > 0, na.rm = TRUE)) {
    warning("Battery discharged during surplus_clean_imp > 0; expected zero.")
  }
  
  # STEP 6: Fossil requirements
  NFF_row <- New_Fossil_Fuels_NPC[1]
  NFF_CF <- NFF_row$CF
  dispatch_data[, New_Fossil_Fuel_MWh := New_NG_MW * NFF_CF]
  
  if (pathway %in% c("A", "D")) {
    dispatch_data <- merge(dispatch_data,
                           Fossil_Fuels_hr_maxmin[, .(Date, Hour, Old_Fossil_Fuels_hr_max_MWh = max_gen_hr_no_retirement_MW)],
                           by = c("Date", "Hour"), all.x = TRUE)
  } else {
    dispatch_data <- merge(dispatch_data,
                           Fossil_Fuels_hr_maxmin[, .(Date, Hour, Old_Fossil_Fuels_hr_max_MWh = max_gen_hr_retirement_MW)],
                           by = c("Date", "Hour"), all.x = TRUE)
  }
  
  # STEP 7: Fossil dispatch & shortage
  dispatch_data[, Fossil_required_MWh := pmax(Demand - Clean_MWh - Total_import_MWh - Battery_discharge_grid, 0)]
  dispatch_data[, Old_Fossil_Fuels_net_MWh := pmin(Old_Fossil_Fuels_hr_max_MWh, Fossil_required_MWh)]
  
  dispatch_data[, Shortage_MWh := round(pmax(
    Demand - (Clean_MWh + Battery_discharge_grid + Total_import_MWh +
                Old_Fossil_Fuels_net_MWh + New_Fossil_Fuel_MWh), 0), 2)]
  
  dispatch_data[, Total_import_net_MWh := ifelse(
    Shortage_MWh > 0,
    Total_import_MWh + pmin(Shortage_MWh, Total_import_max_MWh - Total_import_MWh),
    pmax(0, pmin(Total_import_MWh, Demand - (Clean_MWh + Battery_discharge_grid +
                                               Old_Fossil_Fuels_net_MWh + New_Fossil_Fuel_MWh)))
  )]
  
  dispatch_data[, Shortage_MWh := round(pmax(
    Demand - (Clean_MWh + Battery_discharge_grid + Total_import_net_MWh +
                Old_Fossil_Fuels_net_MWh + New_Fossil_Fuel_MWh), 0), 2)]
  
  dispatch_data[, Curtailments_MWh := round(
    pmax(0, (Clean_MWh + Battery_discharge_grid + Old_Fossil_Fuels_net_MWh +
               New_Fossil_Fuel_MWh + Total_import_net_MWh) - Demand - Battery_charge_grid), 2)]
  
  # Energy balance transparency
  dispatch_data[, Balance_residual := round(
    (Clean_MWh + Total_import_net_MWh + Old_Fossil_Fuels_net_MWh + New_Fossil_Fuel_MWh +
       Battery_discharge_grid) - (Demand + Battery_charge_grid + Curtailments_MWh + Shortage_MWh), 6)]
  
  # STEP 8: Cleanup
  dispatch_data[, c("Percentile_ImpNBSO","Percentile_ImpNYISO","Percentile_ImpHQ",
                    "Percentile_Offshore","Percentile_Onshore","Percentile_Solar",
                    "Fossil_required_MWh","derived_power_limit") := NULL]
  gc()
  return(dispatch_data)
}

# ---- Fossil constraints + emissions ----
dispatch_curve_adjustments <- function(results) {
  sim <- results$Simulation[1]
  pathway <- results$Pathway[1]
  random_vector <- Random_sequence[[sim]]
  n_random <- length(random_vector)
  n_hours <- nrow(results)
  
  unique_units <- unique(Fossil_Fuels_NPC$Facility_Unit.ID)
  n_units <- length(unique_units)
  n_total_rand <- 6 + n_units
  
  base_indices <- ((0:(n_hours - 1)) * n_total_rand)
  shift_values <- 6:(6 + n_units - 1)
  idx_fossil <- sapply(shift_values, function(s) (base_indices + s) %% n_random + 1)
  
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
  
  setkey(results_updated, DayLabel, Hour, Facility_Unit.ID)
  setkey(Fossil_Fuels_Gen, DayLabel, Hour, Facility_Unit.ID)
  results_updated <- Fossil_Fuels_Gen[results_updated, nomatch = 0]
  
  fields <- c("Gen", "CO2", "NOx", "SO2", "HI")
  for(field in fields) {
    field_cols <- grep(paste0("^", field, "_"), names(results_updated), value = TRUE)
    if(length(field_cols) > 0) {
      desired_cols <- paste0(field, "_", results_updated$Percentile)
      idx <- match(desired_cols, field_cols)
      mat <- as.matrix(results_updated[, ..field_cols])
      results_updated[, (paste0(field, "_value")) := mat[cbind(seq_len(.N), idx)]]
    } else {
      results_updated[, (paste0(field, "_value")) := NA_real_]
    }
  }
  
  setnames(results_updated,
           old = c("Gen_value","CO2_value","NOx_value","SO2_value","HI_value"),
           new = c("Gen_MWh","CO2_tons","NOx_lbs","SO2_lbs","HI_mmBtu"))
  
  results_updated <- results_updated[, .(Date, DayLabel, Hour, Facility_Unit.ID, Old_Fossil_Fuels_net_MWh,
                                         Gen_MWh, CO2_tons, NOx_lbs, SO2_lbs, HI_mmBtu)]
  
  Fossil_Fuels_NPC_subset <- Fossil_Fuels_NPC[, .(Facility_Unit.ID, Estimated_NameplateCapacity_MW,
                                                  min_gen_MW, max_gen_MW, Ramp, Ramp_MWh, Retirement_year,
                                                  mean_CO2_tons_MW_estimate, mean_CO2_tons_MW, mean_NOx_lbs_MW_estimate,
                                                  mean_NOx_lbs_MW, mean_SO2_lbs_MW_estimate, mean_SO2_lbs_MW,
                                                  mean_HI_mmBtu_per_MW_estimate = mean_HI_mmBtu_per_MW)]
  results_updated <- merge(Fossil_Fuels_NPC_subset, results_updated, by = "Facility_Unit.ID", all.y = TRUE)
  
  if (!(pathway %in% c("A", "D"))) {
    results_updated <- results_updated[Retirement_year >= year(Date)]
  }
  
  results_updated[, CF_hr := Gen_MWh / Estimated_NameplateCapacity_MW]
  setorder(results_updated, Date, Hour, CF_hr)
  
  results_updated <- results_updated[, {
    dt <- copy(.SD)
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
  
  # Time series adjustments (ramp passes)
  results_updated[, timestamp := as.POSIXct(paste(Date, sprintf("%02d:00:00", Hour - 1)),
                                            format = "%Y-%m-%d %H:%M:%S", tz = "America/New_York")]
  results_updated <- results_updated[, {
    ts_seq <- seq(min(timestamp), max(timestamp), by = "hour")
    dt_complete <- data.table(timestamp = ts_seq)
    merge(dt_complete, .SD, by = "timestamp", all.x = TRUE)
  }, by = Facility_Unit.ID]
  
  results_updated[, Date := as.Date(format(timestamp, tz = "America/New_York"))]
  results_updated[, Hour := hour(timestamp) + 1]
  
  results_updated[, Ramp_MWh := na.locf(Ramp_MWh, na.rm = FALSE), by = Facility_Unit.ID]
  results_updated[, Ramp_MWh := na.locf(Ramp_MWh, fromLast = TRUE, na.rm = FALSE), by = Facility_Unit.ID]
  
  results_updated[, Gen_MWh_adj := fifelse(is.na(Gen_MWh_used), 0, Gen_MWh_used)]
  
  results_updated[, Gen_MWh_adj := {
    temp <- Gen_MWh_adj
    ramp <- unique(Ramp_MWh)[1]; if (is.na(ramp)) ramp <- 24
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
  
  results_updated[, Gen_MWh_adj := {
    temp <- Gen_MWh_adj
    ramp <- unique(Ramp_MWh)[1]; if (is.na(ramp)) ramp <- 24
    n <- .N
    if (n < 2) temp else {
      for (i in 2:n) {
        if (!is.na(temp[i - 1]) && !is.na(temp[i])) {
          delta <- temp[i] - temp[i - 1]
          if (delta < -ramp) temp[i] <- temp[i - 1] - ramp
        }
      }
      temp
    }
  }, by = Facility_Unit.ID]
  
  results_updated[, min_gen_MW := na.locf(min_gen_MW, na.rm = FALSE), by = Facility_Unit.ID]
  results_updated[, Gen_MWh_adj := round(pmax(Gen_MWh_adj, min_gen_MW), 2)]
  
  results_updated[, c("Ramp","Ramp_MWh","Estimated_NameplateCapacity_MW",
                      "min_gen_MW","max_gen_MW","Retirement_year",
                      "Gen_MWh","CF_hr","timestamp") := NULL]
  
  results_updated[, DayLabel := as.integer(format(Date, "%j"))]
  results_updated[is.na(Old_Fossil_Fuels_net_MWh), Old_Fossil_Fuels_net_MWh := 0]
  
  # Emissions (intensities fallback to global means if missing)
  results_updated[, `:=`(
    CO2_intensity = CO2_tons / Gen_MWh_adj,
    NOx_intensity = NOx_lbs / Gen_MWh_adj,
    SO2_intensity = SO2_lbs / Gen_MWh_adj,
    HI_intensity  = HI_mmBtu / Gen_MWh_adj
  )]
  
  mean_intensities <- results_updated[, .(
    mean_CO2_ton_per_MWh = mean(CO2_intensity[is.finite(CO2_intensity)], na.rm = TRUE),
    mean_NOx_lb_per_MWh  = mean(NOx_intensity[is.finite(NOx_intensity)], na.rm = TRUE),
    mean_SO2_lb_per_MWh  = mean(SO2_intensity[is.finite(SO2_intensity)], na.rm = TRUE),
    mean_HI_mmBtu_per_MWh = mean(HI_intensity[is.finite(HI_intensity)], na.rm = TRUE)
  ), by = Facility_Unit.ID]
  
  global_means <- mean_intensities[, .(
    global_mean_CO2 = mean(mean_CO2_ton_per_MWh, na.rm = TRUE),
    global_mean_NOx = mean(mean_NOx_lb_per_MWh, na.rm = TRUE),
    global_mean_SO2 = mean(mean_SO2_lb_per_MWh, na.rm = TRUE),
    global_mean_HI  = mean(mean_HI_mmBtu_per_MWh, na.rm = TRUE)
  )]
  
  mean_intensities[is.na(mean_CO2_ton_per_MWh),  mean_CO2_ton_per_MWh  := global_means$global_mean_CO2]
  mean_intensities[is.na(mean_NOx_lb_per_MWh),   mean_NOx_lb_per_MWh   := global_means$global_mean_NOx]
  mean_intensities[is.na(mean_SO2_lb_per_MWh),   mean_SO2_lb_per_MWh   := global_means$global_mean_SO2]
  mean_intensities[is.na(mean_HI_mmBtu_per_MWh), mean_HI_mmBtu_per_MWh := global_means$global_mean_HI]
  
  results_updated <- merge(results_updated, mean_intensities, by = "Facility_Unit.ID", all.x = TRUE)
  
  results_updated[, CO2_ton_per_MWh := fifelse(!is.na(mean_CO2_tons_MW), mean_CO2_tons_MW,
                                               fifelse(!is.na(mean_CO2_tons_MW_estimate), mean_CO2_tons_MW_estimate,
                                                       mean_CO2_ton_per_MWh))]
  results_updated[, NOx_lb_per_MWh := fifelse(!is.na(mean_NOx_lbs_MW), mean_NOx_lbs_MW,
                                              fifelse(!is.na(mean_NOx_lbs_MW_estimate), mean_NOx_lbs_MW_estimate,
                                                      mean_NOx_lb_per_MWh))]
  results_updated[, SO2_lb_per_MWh := fifelse(!is.na(mean_SO2_lbs_MW), mean_SO2_lbs_MW,
                                              fifelse(!is.na(mean_SO2_lbs_MW_estimate), mean_SO2_lbs_MW_estimate,
                                                      mean_SO2_lb_per_MWh))]
  results_updated[, HI_mmBtu_per_MWh := fifelse(!is.na(mean_HI_mmBtu_per_MW_estimate),
                                                mean_HI_mmBtu_per_MW_estimate, mean_HI_mmBtu_per_MWh)]
  
  results_updated[, CO2_tons := fcoalesce(CO2_ton_per_MWh, 0) * fcoalesce(Gen_MWh_adj, 0)]
  results_updated[, NOx_lbs := fcoalesce(NOx_lb_per_MWh, 0) * fcoalesce(Gen_MWh_adj, 0)]
  results_updated[, SO2_lbs := fcoalesce(SO2_lb_per_MWh, 0) * fcoalesce(Gen_MWh_adj, 0)]
  results_updated[, HI_mmBtu := fcoalesce(HI_mmBtu_per_MWh, 0) * fcoalesce(Gen_MWh_adj, 0)]
  
  results_updated[, c("CO2_intensity","NOx_intensity","SO2_intensity","HI_intensity",
                      "mean_HI_mmBtu_per_MWh","mean_SO2_lb_per_MWh","mean_NOx_lb_per_MWh","mean_CO2_ton_per_MWh",
                      "mean_CO2_tons_MW_estimate","mean_CO2_tons_MW",
                      "mean_NOx_lbs_MW_estimate","mean_NOx_lbs_MW",
                      "mean_SO2_lbs_MW_estimate","mean_SO2_lbs_MW",
                      "mean_HI_mmBtu_per_MW_estimate") := NULL]
  
  return(results_updated)
}

# ---- Calibrations (recompute battery & imports after fossil adj) ----
dispatch_curve_calibrations <- function(dispatch_curve_results, fossil_fuels_hourly_results) {
  fossil_agg <- fossil_fuels_hourly_results[
    , .(Old_Fossil_Fuels_adj_MWh = sum(Gen_MWh_adj, na.rm = TRUE),
        CO2_tons = sum(CO2_tons, na.rm = TRUE),
        NOx_lbs = sum(NOx_lbs, na.rm = TRUE),
        SO2_lbs = sum(SO2_lbs, na.rm = TRUE),
        HI_mmBtu  = sum(HI_mmBtu, na.rm = TRUE)),
    by = .(Date, Hour)]
  
  final_results <- merge(dispatch_curve_results, fossil_agg, by = c("Date", "Hour"), all.x = TRUE)
  final_results[is.na(Old_Fossil_Fuels_adj_MWh), Old_Fossil_Fuels_adj_MWh := 0]
  
  # Battery re-run
  rt_eff <- cfg$rt_eff; eta <- sqrt(rt_eff)
  duration_h <- cfg$duration_hours
  rho <- if (is.finite(cfg$retention_hours)) exp(-1 / cfg$retention_hours) else 1.0
  
  inverter_col_present2 <- cfg$inverter_col %in% names(final_results)
  final_results[, derived_power_limit := Storage_MW / duration_h]
  if (inverter_col_present2) {
    final_results[, inverter_limit_MW := pmin(get(cfg$inverter_col), derived_power_limit, na.rm = TRUE)]
  } else {
    final_results[, inverter_limit_MW := derived_power_limit]
  }
  final_results[, battery_power_limit := pmax(0, inverter_limit_MW)]
  
  final_results[, surplus_clean_imp := (Clean_MWh + Total_import_net_MWh) - Demand]
  final_results[, deficit_clean_imp := pmax(0, Demand - (Clean_MWh + Total_import_net_MWh))]
  
  nH2 <- nrow(final_results)
  soc2 <- numeric(nH2); soc_prev2 <- 0.0
  is_midnight2 <- final_results$Hour == 1
  c_grid2 <- numeric(nH2); d_grid2 <- numeric(nH2)
  
  for (i in seq_len(nH2)) {
    cap_MWh <- final_results$Storage_MW[i]
    p_lim   <- final_results$battery_power_limit[i]
    
    soc_prev2 <- soc_prev2 * rho
    if (!cfg$allow_multiday_carry && is_midnight2[i]) soc_prev2 <- 0.0
    
    headroom_soc  <- pmax(0, cap_MWh - soc_prev2)
    energy_soc    <- pmax(0, soc_prev2)
    
    d_cap_grid <- final_results$deficit_clean_imp[i]
    d_try <- min(d_cap_grid, p_lim, energy_soc * eta)
    
    if (d_try > 0) {
      d_grid <- d_try; c_grid <- 0
      soc_new <- soc_prev2 - d_grid / eta
    } else {
      if (cfg$curtailment_only_charging) {
        c_cap_grid <- pmax(0, final_results$surplus_clean_imp[i])
      } else if (cfg$allow_grid_charging) {
        c_cap_grid <- p_lim
      } else {
        c_cap_grid <- pmax(0, final_results$surplus_clean_imp[i])
      }
      c_grid <- min(c_cap_grid, p_lim, headroom_soc / eta)
      d_grid <- 0
      soc_new <- soc_prev2 + c_grid * eta
    }
    
    soc_new <- min(max(soc_new, 0), cap_MWh)
    
    soc2[i] <- soc_new
    c_grid2[i] <- c_grid
    d_grid2[i] <- d_grid
    soc_prev2 <- soc_new
  }
  
  final_results[, Calibrated_Storage_status := soc2]
  final_results[, Calibrated_Battery_charge_grid := c_grid2]
  final_results[, Calibrated_Battery_discharge_grid := d_grid2]
  
  # Invariant checks
  if (any(final_results$Calibrated_Battery_charge_grid > 0 & final_results$Calibrated_Battery_discharge_grid > 0, na.rm = TRUE)) {
    warning("Calibrated: charging and discharging in the same hour detected; check guards.")
  }
  if (any(final_results$surplus_clean_imp > 0 & final_results$Calibrated_Battery_discharge_grid > 0, na.rm = TRUE)) {
    warning("Calibrated: discharged during surplus_clean_imp > 0; expected zero.")
  }
  
  # Imports recalibration
  final_results[, Calibrated_Shortage_MWh := round(
    pmax(Demand - (Clean_MWh + Old_Fossil_Fuels_adj_MWh + New_Fossil_Fuel_MWh +
                     Total_import_MWh + Calibrated_Battery_discharge_grid), 0), 2)]
  
  final_results[, Calibrated_Total_import_net_MWh := ifelse(
    Calibrated_Shortage_MWh > 0,
    Total_import_MWh + pmin(Calibrated_Shortage_MWh, Total_import_max_MWh - Total_import_MWh),
    Total_import_MWh
  )]
  
  final_results[, extra_needed := pmax(Calibrated_Total_import_net_MWh - Total_import_MWh, 0)]
  imports_max_CF <- 0.95
  final_results[, `:=`(
    headroom_LT_HQ   = Long_Term_Imports_HQ_MW * abs(imports_max_CF - Import_HQ_CF),
    headroom_Spot_HQ = Spot_Market_Imports_HQ_MW * abs(imports_max_CF - Import_HQ_CF),
    headroom_NYISO   = Imports_NYISO_MW       * abs(imports_max_CF - Import_NYISO_CF),
    headroom_NBSO    = Imports_NBSO_MW        * abs(imports_max_CF - Import_NBSO_CF)
  )]
  final_results[, c("extra_LT_HQ","extra_Spot_HQ","extra_NYISO","extra_NBSO") := {
    need <- extra_needed
    a_lt <- pmin(need, headroom_LT_HQ);   need <- need - a_lt
    a_sp <- pmin(need, headroom_Spot_HQ); need <- need - a_sp
    a_ny <- pmin(need, headroom_NYISO);   need <- need - a_ny
    a_nb <- pmin(need, headroom_NBSO)
    list(a_lt, a_sp, a_ny, a_nb)
  }]
  final_results[, `:=`(
    Calibrated_Long_Term_Imports_HQ_MWh   = Long_Term_Imports_HQ_MWh   + extra_LT_HQ,
    Calibrated_Spot_Market_Imports_HQ_MWh = Spot_Market_Imports_HQ_MWh + extra_Spot_HQ,
    Calibrated_Import_NYISO_MWh           = Import_NYISO_MWh           + extra_NYISO,
    Calibrated_Import_NBSO_MWh            = Import_NBSO_MWh            + extra_NBSO
  )]
  final_results[, c("extra_needed","headroom_LT_HQ","headroom_Spot_HQ","headroom_NYISO",
                    "headroom_NBSO","extra_LT_HQ","extra_Spot_HQ","extra_NYISO","extra_NBSO") := NULL]
  
  final_results[, Calibrated_Shortage_MWh := round(
    pmax(Demand - (Clean_MWh + Old_Fossil_Fuels_adj_MWh + New_Fossil_Fuel_MWh +
                     Calibrated_Total_import_net_MWh + Calibrated_Battery_discharge_grid), 0), 2)]
  
  final_results[, Calibrated_net_energy := (Clean_MWh + Old_Fossil_Fuels_adj_MWh + New_Fossil_Fuel_MWh +
                                              Calibrated_Total_import_net_MWh + Calibrated_Battery_discharge_grid) - Demand]
  final_results[, Calibrated_Curtailments_MWh := round(
    pmax(0, (Clean_MWh + Calibrated_Battery_discharge_grid + Old_Fossil_Fuels_adj_MWh +
               New_Fossil_Fuel_MWh + Calibrated_Total_import_net_MWh) - Demand - Calibrated_Battery_charge_grid), 2)]
  
  final_results[, Balance_residual := round(
    (Clean_MWh + Calibrated_Total_import_net_MWh + Old_Fossil_Fuels_adj_MWh + New_Fossil_Fuel_MWh +
       Calibrated_Battery_discharge_grid) - (Demand + Calibrated_Battery_charge_grid +
                                               Calibrated_Curtailments_MWh + Calibrated_Shortage_MWh), 6)]
  
  final_results[, c("derived_power_limit") := NULL]
  return(final_results)
}

## ----- 3) Run simulations -----
pathways <- unique(Hourly_Installed_Capacity$Pathway)
n_simulations <- 1
pathways<-"B1"

simulation_results <- lapply(1:n_simulations, function(sim) {
  cat("Running simulation:", sim, "\n")
  lapply(pathways, function(pathway) {
    cat("  Pathway:", pathway, "\n")
    
    dcr <- dispatch_curve(sim, pathway)
    dcr <- dcr[1:200,] # Testing the code
    
    # No artificial truncation — run full horizon
    ffh <- dispatch_curve_adjustments(dcr)
    fhr <- dispatch_curve_calibrations(dcr, ffh)
    
    fhr$Simulation <- sim; fhr$Pathway <- pathway
    ffh$Simulation <- sim; ffh$Pathway <- pathway
    
    list(final_hourly = fhr, fossil_fuels = ffh)
  })
})

combined_final_hourly_results <- rbindlist(lapply(simulation_results, function(sim_results) {
  rbindlist(lapply(sim_results, function(res) res$final_hourly))
}), use.names = TRUE, fill = TRUE)

combined_fossil_fuels_results <- rbindlist(lapply(simulation_results, function(sim_results) {
  rbindlist(lapply(sim_results, function(res) res$fossil_fuels))
}), use.names = TRUE, fill = TRUE)

## ----- 4) Save results (relative paths) -----
hourly_csv   <- p("2 Generation Expansion Model","5 Dispatch Curve","1 Test Results","Hourly_Results_NE.csv")
facility_csv <- p("2 Generation Expansion Model","5 Dispatch Curve","1 Test Results","Facility_Level_Results.csv")
fwrite(combined_final_hourly_results, hourly_csv)
fwrite(combined_fossil_fuels_results, facility_csv)

## ----- 5) Notification (via env vars; safe defaults) -----
pushover_user  <- Sys.getenv("PUSHOVER_USER",  unset = NA)
pushover_token <- Sys.getenv("PUSHOVER_TOKEN", unset = NA)
if (!is.na(pushover_user) && !is.na(pushover_token)) {
  try({
    POST("https://api.pushover.net/1/messages.json",
         body = list(token = pushover_token, user = pushover_user,
                     message = "Dispatch run completed."), encode = "form")
  }, silent = TRUE)
}

## ----- 6) Diagnostics: correlations & plot export -----
# 6a) Hourly correlations among main drivers (from realized run)
cor_file <- p("2 Generation Expansion Model","5 Dispatch Curve","1 Test Results","correlations_hourly.csv")
diag_cols <- intersect(c("Solar_MWh","Onshore_MWh","Offshore_MWh","Demand"),
                       names(combined_final_hourly_results))
if (length(diag_cols) >= 2) {
  cm <- cor(combined_final_hourly_results[, ..diag_cols], use = "pairwise.complete.obs")
  fwrite(as.data.table(cbind(variable = rownames(cm), cm)), cor_file)
}

# 6b) Improved plot: CO2 by pathway + SOC panel
fig_file <- p("2 Generation Expansion Model","5 Dispatch Curve","1 Test Results","emissions_and_soc.png")
png(fig_file, width = 1600, height = 1000, res = 150)
par(mfrow = c(2,1), mar = c(4,4,2,1), cex = 1.1)

pathway_groups <- split(combined_final_hourly_results, combined_final_hourly_results$Pathway)
colors <- rainbow(length(pathway_groups))
max_index <- max(sapply(pathway_groups, nrow))
yl1 <- range(combined_final_hourly_results$CO2_tons, na.rm = TRUE)

plot(NA, xlim = c(1, max_index), ylim = yl1, xlab = "Hour index", ylab = "CO2 (tons)",
     main = "(a) CO2 by pathway")
i <- 1
for (group in pathway_groups) { lines(1:nrow(group), group$CO2_tons, col = colors[i]); i <- i + 1 }
legend("topright", legend = names(pathway_groups), col = colors, lty = 1, bty = "n", cex = 0.9)

if ("Calibrated_Storage_status" %in% names(combined_final_hourly_results)) {
  yl2 <- range(combined_final_hourly_results$Calibrated_Storage_status, na.rm = TRUE)
  plot(NA, xlim = c(1, max_index), ylim = yl2, xlab = "Hour index", ylab = "SOC (MWh)",
       main = "(b) Storage state of charge")
  i <- 1
  for (group in pathway_groups) { lines(1:nrow(group), group$Calibrated_Storage_status, col = colors[i]); i <- i + 1 }
} else {
  plot.new(); title("(b) SOC not available"); box()
}
dev.off()

## ----- 7) Spatial transparency (zones list, if present) -----
zones_path <- p("2 Generation Expansion Model","5 Dispatch Curve","1 Test Results","zones_list.csv")
if ("Zone" %in% names(Hourly_Installed_Capacity)) {
  zlist <- unique(Hourly_Installed_Capacity[Year >= 2025, .(Pathway, Zone)])
  fwrite(zlist, zones_path)
}

## ----- 8) Run manifest (comprehensive; inputs & outputs + checksums) -----
inputs_list <- c(
  p("1 Decarbonization Pathways","Hourly_Installed_Capacity.csv"),
  p("2 Generation Expansion Model","2 Generation","1 Clean Generation","2 SMR","1 SMR Facility Data","SMR_Facility_Data.csv"),
  p("2 Generation Expansion Model","2 Generation","1 Clean Generation","3 Large nuclear hydro and bio","1 Clean Baseload Facility Data","CleanBaseload_Facility_Data.csv"),
  p("2 Generation Expansion Model","2 Generation","2 Fossil Generation","1 Existing Fossil Fuels","1 Fossil Fuels Facilities Data","Fossil_Fuel_Facilities_Data.csv"),
  p("2 Generation Expansion Model","2 Generation","2 Fossil Generation","2 New Fossil Fuels","1 New Fossil Fuels Facilities Data","New_Fossil_Fuel_Facilities_Data.csv"),
  p("2 Generation Expansion Model","2 Generation","1 Clean Generation","1 Wind and Solar","1 Wind and Solar CF","offwind_CF.csv"),
  p("2 Generation Expansion Model","2 Generation","1 Clean Generation","1 Wind and Solar","1 Wind and Solar CF","onwind_CF.csv"),
  p("2 Generation Expansion Model","2 Generation","1 Clean Generation","1 Wind and Solar","1 Wind and Solar CF","solar_CF.csv"),
  p("2 Generation Expansion Model","2 Generation","2 Fossil Generation","1 Existing Fossil Fuels","2 Fossil Fuels Generation and Emissions","Fossil_Fuel_Generation_Emissions.csv"),
  p("2 Generation Expansion Model","3 Imports","1 Imports CF","Imports_CF.csv"),
  p("2 Generation Expansion Model","1 Demand","1 Hourly Demand","demand_data.csv"),
  p("2 Generation Expansion Model","4 Randomization","1 Randomized Data","Random_Sequence.csv"),
  p("2 Generation Expansion Model","2 Fossil Generation","1 Existing Fossil Fuels","2 Fossil Fuels Generation and Emissions","Fossil_Fuel_hr_maxmin.csv")
)

outputs_list <- c(
  hourly_csv,
  facility_csv,
  fig_file,
  cor_file,
  zones_path
)

finfo <- function(f) list(
  path = f,
  exists = file.exists(f),
  size_bytes = if (file.exists(f)) file.info(f)$size else NA,
  sha256 = if (file.exists(f)) digest(file = f, algo = "sha256") else NA
)

manifest <- list(
  identity = list(
    run_id = format(._run_started_at, "%Y%m%d_%H%M%S"),
    scenario_names = unique(combined_final_hourly_results$Pathway),
    simulation_count = length(unique(combined_final_hourly_results$Simulation))
  ),
  timestamps = list(
    started_at = as.character(._run_started_at),
    finished_at = as.character(Sys.time()),
    timezone = Sys.timezone()
  ),
  environment = list(
    R = R.version$version.string,
    packages = as.list(c(
      data.table = as.character(packageVersion("data.table")),
      lubridate  = as.character(packageVersion("lubridate")),
      zoo        = as.character(packageVersion("zoo")),
      jsonlite   = as.character(packageVersion("jsonlite")),
      httr       = as.character(packageVersion("httr")),
      digest     = as.character(packageVersion("digest"))
    ))
  ),
  inputs  = lapply(inputs_list, finfo),
  outputs = lapply(outputs_list, finfo),
  model_notes = list(
    objective = "Serve hourly demand with physical constraints; record Shortage_MWh explicitly. No economic objective.",
    storage_equations = "SOC_t = min(max(SOC_{t-1}*rho + eta*c_t - d_t/eta, 0), Emax); 0<=c_t,d_t<=Pmax; d_t>0 only if Clean+Imports<Demand.",
    availability_factors = "Solar_AF/Onshore_AF/Offshore_AF denote availability factors (weather-driven), not ex-post capacity factors."
  ),
  battery_settings = list(
    rt_eff = cfg$rt_eff,
    eta = sqrt(cfg$rt_eff),
    duration_hours = cfg$duration_hours,
    inverter_col = cfg$inverter_col,
    retention_hours = cfg$retention_hours,
    allow_multiday_carry = cfg$allow_multiday_carry,
    curtailment_only_charging = cfg$curtailment_only_charging
  )
)
manifest_path <- p("2 Generation Expansion Model","5 Dispatch Curve","1 Test Results","run_manifest.json")
writeLines(jsonlite::toJSON(manifest, auto_unbox = TRUE, pretty = TRUE), manifest_path)
cat("Wrote manifest:", manifest_path, "\n")

## ===== End of script =====

