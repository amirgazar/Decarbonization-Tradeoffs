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

# Load the predicted hourly demand
# Load wind and solar simulations 
# Load oil and gas simulations 
# Load imports

# Fossil Fuels

## 1. Installed Capacities and Facilities Data
# 1.1 Capacity for Wind, Solar, Large Nuclear, Hydro, Bio, Battery, SMRs, Imports and New Natural Gas
file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/Hourly_Installed_Capacity.csv"
Hourly_Installed_Capacity <- fread(file_path)

file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/SMR_Facility_Data.csv"
SMR_Facility_Data <- fread(file_path)

file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/CleanBaseload_Facility_Data.csv"
CleanBaseload_Facility_Data <- fread(file_path)

# 1.1 Fossil Fuels Data
file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(file_path)

file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/New_Fossil_Fuel_Facilities_Data.csv"
New_Fossil_Fuels_NPC <- fread(file_path)

## 2. Probabilistic Generation Data and Capacity Factors
# 2.1 Wind and Solar
file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/offwind_CF.csv"
Offwind_CF <- fread(file_path)

file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/onwind_CF.csv"
Onwind_CF <- fread(file_path)

file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/solar_CF.csv"
Solar_CF <- fread(file_path)

# 2.2 Fossil fuels
file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/Fossil_Fuel_Generation_Emissions.csv"
Fossil_Fuels_Gen <- fread(file_path)

# 2.3 Imports
file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/Imports_CF.csv"
Imports_CF <- fread(file_path)

## 3. Demand 
file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/demand_data.csv"
Demand_data <- fread(file_path)

file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/peak_demand_data.csv"
Peak_demand <- fread(file_path)

## 4. Random Sequence
file_path <- "/home/amirgazar/Paper2_Nov_2024/Datasets/Random_Sequence.csv"
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


# Define function to process data for each hour
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
  SMR_gen_hour <- SMR_max_hour * SMR_Facility_Data$Minimum_Power_Output_MW/SMR_Facility_Data$Nameplate_Capacity_MW
  
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
                                                 Prev_Gen_MW := i.Prev_Gen_MW]
  setnames(Fossil_Fuels_gen_hour, "Gen_X", "Fossil_Fuel_gen")
  Fossil_Fuels_gen_hour[, Fossil_Fuel_CF_hour := Fossil_Fuel_gen / Estimated_NameplateCapacity_MW]
  Fossil_Fuels_gen_hour[is.na(Fossil_Fuel_gen), Fossil_Fuel_gen := 0]
  Fossil_Fuels_gen_hour[is.na(Fossil_Fuel_CF_hour), Fossil_Fuel_CF_hour := 0]
  setorderv(Fossil_Fuels_gen_hour, cols = "Fossil_Fuel_CF_hour", order = -1)
  
  # Ensure generational/technical constraints are met
  # Historical min and max
  Fossil_Fuels_gen_hour[Fossil_Fuel_gen < min_gen_MW, Fossil_Fuel_gen := min_gen_MW]
  Fossil_Fuels_gen_hour[Fossil_Fuel_gen > max_gen_MW, Fossil_Fuel_gen := max_gen_MW]
  # Heat inputs
  Fossil_Fuels_gen_hour[HI_X > Max_Hourly_HI_Rate, Fossil_Fuel_gen := Fossil_Fuel_gen * Max_Hourly_HI_Rate/HI_X]
  
  # Apply ramp rate constraints based on previous hour generation
  Fossil_Fuels_gen_hour[is.na(Prev_Gen_MW), Prev_Gen_MW := 0]
  Fossil_Fuels_gen_hour[, max_gen_hour_MW := pmin(Prev_Gen_MW + Estimated_NameplateCapacity_MW/Ramp, Estimated_NameplateCapacity_MW * Fossil_Fuel_CF_hour)]
  Fossil_Fuels_gen_hour[, min_gen_hour_MW := pmax(Prev_Gen_MW - Estimated_NameplateCapacity_MW/Ramp, 0)]
  
  Fossil_Fuels_gen_hour[, Fossil_Fuel_gen := pmax(pmin(Fossil_Fuel_gen, max_gen_hour_MW), min_gen_hour_MW)]
  
  if (!(pathway %in% c("A", "D"))) {
    Fossil_Fuels_gen_hour <- Fossil_Fuels_gen_hour[Fossil_Fuels_gen_hour$Retirement_year > year, ]
  }
  
  Fossil_Fuels_gen_hour <- Fossil_Fuels_gen_hour[Fossil_Fuel_gen != 0]
  Fossil_Fuels_gen_hour_max <- sum(Fossil_Fuels_gen_hour$Fossil_Fuel_gen, na.rm = TRUE)
  
  # 2.4.2 Old Fossil Fuels Emissions CO2, NOx, SO2, HI
  cols_to_fill <- c("CO2_X", "NOx_X", "SO2_X", "HI_X")
  estimates <- c("mean_CO2_tons_MW_estimate", "mean_NOx_lbs_MW_estimate", 
                 "mean_SO2_lbs_MW_estimate", "mean_HI_mmBtu_per_MW")
  
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
  Fossil_Fuels_used <- data.table(Facility_Unit.ID = unlist(consumed_facilities_hour), Load_MW = load_vector)
  
  # Ramp up if peak is coming
  if (!is.na(Demand_peak_filtered$start_ramp[1]) && Demand_peak_filtered$start_ramp[1] == TRUE) {
    Fossil_Fuels_Final <- Fossil_Fuels_used[Fossil_Fuels_gen_hour, on = .(Facility_Unit.ID)]
    Fossil_Fuels_Final$Load_MW[is.na(Fossil_Fuels_Final$Load_MW)] <- 0
    Fossil_Fuels_Final[, Load_MW := pmax(pmax(Load_MW, min_gen_MW), max_gen_MW)]
    peak_incoming <- TRUE
  } else {
    Fossil_Fuels_Final <- Fossil_Fuels_used[Fossil_Fuels_gen_hour, on = .(Facility_Unit.ID)]
    Fossil_Fuels_Final$Load_MW[is.na(Fossil_Fuels_Final$Load_MW)] <- 0
    Fossil_Fuels_Final[, Load_MW := pmin(pmax(Load_MW, min_gen_MW), max_gen_MW)]
    peak_incoming <- FALSE
  } 
  
  Fossil_Fuels_Final <- Fossil_Fuels_Final[Load_MW != 0]
  
  Fossil_Fuels_Final[, Gen_ratio := Load_MW / Fossil_Fuel_gen] # How much each facility is used
  Fossil_Fuels_Final[, `:=`(
    CO2_tons = Load_MW * CO2_X,
    NOx_lbs = Load_MW * NOx_X,
    SO2_lbs = Load_MW * SO2_X,
    HI_mmBtu = HI_X * Gen_ratio
  )]
  
  # Total emissions
  # New fossil emissions + old fossil emissions
  Total_CO2_hr <- Total_Fossil_Fuels_gen_hour_new * New_Fossil_Fuels_NPC$mean_CO2_tons_MW  + sum(Fossil_Fuels_Final$CO2_tons, na.rm = TRUE)
  Total_NOx_hr <- Total_Fossil_Fuels_gen_hour_new * New_Fossil_Fuels_NPC$mean_NOx_lbs_MW  + sum(Fossil_Fuels_Final$NOx_lbs, na.rm = TRUE)
  Total_SO2_hr <- Total_Fossil_Fuels_gen_hour_new * New_Fossil_Fuels_NPC$mean_SO2_lbs_MW  + sum(Fossil_Fuels_Final$SO2_lbs, na.rm = TRUE)
  Total_HI_hr <- Total_Fossil_Fuels_gen_hour_new * New_Fossil_Fuels_NPC$mean_Heat_Input_mmBtu  + sum(Fossil_Fuels_Final$HI_mmBtu, na.rm = TRUE)
  
  # Total fossil generation and Curtailed fossil fuels
  Total_Fossil_Fuels_gen_hour <- sum(Fossil_Fuels_Final$Load_MW, na.rm = TRUE)
  Total_Fossil_Fuels_gen_hour_ALL <- Total_Fossil_Fuels_gen_hour + Total_Fossil_Fuels_gen_hour_new
  Total_gen_hr <- Clean_gen_hour + Total_Fossil_Fuels_gen_hour_ALL
  
  wasted_fossil <-  Total_Fossil_Fuels_gen_hour - Demand_for_Fossil_Fuels_hour
  wasted_fossil <- ifelse(wasted_fossil > 0, wasted_fossil, 0)
  
  
  # Hourly results for all of NE 
  result_hr <- data.table(Simulation = sim,
                          Year = year, DayLabel = day_label, Date = date_selected, Hour = hour, Pathway = pathway,
                          Percentile_Solar = percentile[1], Solar.gen_hr_MW = Solar_hour,
                          Percentile_Onwind = percentile[2], Onwind.gen_hr_MW = Onwind_hour,
                          Percentile_Offwind = percentile[3], Offwind.gen_hr_MW = Offwind_hour,
                          Nuclear.gen_hr_MW = Nuclear_hour, Hydro.gen_hr_MW = Hydro_hour, Bio.gen_hr_MW = Bio_hour,
                          SMR.gen_hr_MW = SMR_gen_hour,
                          Percentile_Import_QC = percentile[4], Import.QC_hr_MW = Demand_for_Imports_hour$Imports_QC,
                          Percentile_Import_NYISO = percentile[5], Import.NYISO_hr_MW = Demand_for_Imports_hour$Imports_NYISO,
                          Percentile_Import_NB = percentile[6], Import.NB_hr_MW = Demand_for_Imports_hour$Imports_NBSO,
                          Fossil_old.gen_hr_MW = Total_Fossil_Fuels_gen_hour,
                          Fossil_new.gen_hr_MW = Total_Fossil_Fuels_gen_hour_new,
                          Fossil.gen_hr_MW = Total_Fossil_Fuels_gen_hour_ALL,
                          Clean.gen_hr_MW = Clean_gen_hour, Imports.total_hr_MW = sum(unlist(Demand_for_Imports_hour), na.rm = TRUE),
                          Total.gen_hr_MW = Total_gen_hr,
                          Demand.total_hr_MW = Demand_hour, 
                          ExcessClean.gen_hr_MW = wasted_energy,
                          ExcessFossil.gen_hr_MW = wasted_fossil,
                          Peak.status_hour = peak_incoming,
                          Shortage.total_hr_MW = gen_shortage,
                          Storage.status_hr_MW = storage_status,
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
                                      Ramp_hr = Fossil_Fuels_Final$Ramp, Fossil.NPC_MW = Fossil_Fuels_Final$Estimated_NameplateCapacity_MW,
                                      Percentile = Fossil_Fuels_Final$Percentile,
                                      CF = Fossil_Fuels_Final$Load_MW / Fossil_Fuels_Final$Estimated_NameplateCapacity_MW,
                                      Fossil.gen_MW = Fossil_Fuels_Final$Load_MW, CO2_tons = Fossil_Fuels_Final$CO2_tons,
                                      NOx_lbs = Fossil_Fuels_Final$NOx_lbs, SO2_lbs = Fossil_Fuels_Final$SO2_lbs, HI_mmBtu = Fossil_Fuels_Final$HI_mmBtu
  )
  
  return(list(result_hr = result_hr, result_facility_level = result_facility_level))
}

# Sequential processing
pathways <- unique(Hourly_Installed_Capacity$Pathway)
unique_hours <- sort(unique(Demand_data$Hour))
num_percentiles <- ncol(Random_sequence)

unique_dates <- as.Date(sort(unique(Demand_data$Date)), origin = "1970-01-01")
start_date <- as.Date("2025-01-01")
end_date <- as.Date("2050-12-31")
unique_dates <- unique_dates[unique_dates >= start_date & unique_dates <= end_date]

# Number of samples
n_samples <- length(unique_dates) * length(unique_hours)

# Define the number of simulations
# Define the number of simulations
iteration <- 1
start_sim <- 1 + iteration
n_simulations <- 1 + iteration
all_simulation_results <- list()

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

# Set pathway
pathway_id <- 1
pathways <- pathways[pathway_id]

# Monte Carlo Simulation
all_simulation_results <- lapply(start_sim:n_simulations, function(sim) {
  # Pre-generate the sampled percentiles for each date and hour combination
  Random_sequence_sampled <- Random_sequence[, sim:(n_samples + sim - 1)]
  print(sim)
  
  lapply(pathways, function(pathway) {
    # Index for Random_sequence
    percentile_index <- 1
    storage_status <- 0
    prev_gen <- data.table(Facility_Unit.ID = Fossil_Fuels_NPC$Facility_Unit.ID, Prev_Gen_MW = 0)
    
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
        storage_status <<- result$result_hr$Storage.status_hr_MW
        if (nrow(result$result_facility_level) != 0) {
          prev_gen_updated <- result$result_facility_level[, .(Facility_Unit.ID, Fossil.gen_MW)]
          result_prev_gen <- prev_gen_updated[, .(Facility_Unit.ID, Fossil.gen_MW)]
          setnames(result_prev_gen, "Fossil.gen_MW", "Prev_Gen_MW")
          prev_gen[result_prev_gen, on = .(Facility_Unit.ID), Prev_Gen_MW := i.Prev_Gen_MW]
        }
        return(result)
      })
    })
  })
})

# Combining results across all simulations
combined_results_hr <- rbindlist(lapply(all_simulation_results, function(sim_results) {
  rbindlist(lapply(sim_results, function(scenario_results) {
    rbindlist(lapply(scenario_results, function(date_results) {
      rbindlist(lapply(date_results, `[[`, "result_hr"))
    }))
  }))
}))

combined_results_facility_level <- rbindlist(lapply(all_simulation_results, function(sim_results) {
  rbindlist(lapply(sim_results, function(scenario_results) {
    rbindlist(lapply(scenario_results, function(date_results) {
      rbindlist(lapply(date_results, `[[`, "result_facility_level"))
    }))
  }))
}))


# Get the current timestamp
timestamp <- format(Sys.time(), "%Y_%m_%d_%H%M")

# Define the file paths with unique identifiers
hourly_results_file <- sprintf("/home/amirgazar/Paper2_Nov_2024/Results/Stepwise/Hourly_Results_NE_stepwise_%s_pathway_%s_iteration_%s.csv", timestamp, pathways, iteration)
facility_level_results_file <- sprintf("/home/amirgazar/Paper2_Nov_2024/Results/Stepwise/Facility_Level_Results_stepwise_%s_pathway_%s_iteration_%s.csv", timestamp, pathways, iteration)

# Saving the results to csv files
write.csv(combined_results_hr, hourly_results_file)
write.csv(combined_results_facility_level, facility_level_results_file)

# Pushover alert - Pushover credentials
pushover_user <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST("https://api.pushover.net/1/messages.json",
                 body = list(
                   token = pushover_token,
                   user = pushover_user,
                   message = "The stepwise_code executed."
                 ),
                 encode = "form")

