# Load necessary libraries
library(data.table)
library(lubridate)

# Load mapping data
file.path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/demand_data_and_rep_days.csv"
demand_data_and_rep_days <- fread(file.path)

# Extract unique representative days
unique_rep_days <- unique(demand_data_and_rep_days$Representative_Date) # Replace `Representative_Day` with the actual column name
unique_rep_days <- as.Date(unique_rep_days) # Ensure the dates are in Date format

# Filter demand_data_and_rep_days by the unique representative days
filtered_data <- demand_data_and_rep_days[Date %in% unique_rep_days, .(Date, Hour, Demand)] # Replace `Date` and `Demand` with actual column names
setnames(filtered_data, "Demand", "Demand_rep")

# Left join demand_data_and_rep_days by the new dataset
joined_data <- merge(demand_data_and_rep_days, filtered_data, by.x = c("Representative_Date", "Hour"), by.y = c("Date", "Hour"), all.x = TRUE, allow.cartesian=TRUE)

# Create the factor column
joined_data[, Factor := Demand / Demand_rep]

# Extension
extended_dates <- data.table(rep_date = unique_rep_days)

# Create a sequence of dates for each representative date (3 days before and after)
extended_dates <- extended_dates[, .(rep_date_extended = seq(rep_date - 3, rep_date + 3, by = "day")), by = rep_date]

# Assign factor 1 to the new rows
extended_dates[, Factor_new := 1]

# Merge the new extended dates with the original dataset
merged_data <- merge(
  joined_data,
  extended_dates,
  by.x = "Date",
  by.y = "rep_date_extended",
  all = TRUE
)

demand_factor_data <- merged_data[!is.na(Factor_new), Factor := Factor_new]
demand_factor_data[, c("DayLabel", "Demand", "rep_date", "Factor_new", "Demand_rep") := NULL]

# Specify the folder containing the files
folder_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/3 Intemediate Results [if any]/2 Representative Results Samples"

# Step 2: List all CSV files in the folder
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Step 3: Identify unique starting names
unique_starts <- unique(sub("_[^_]+_\\d+\\.csv$", "", basename(files)))

# Step 4: Initialize a list to store combined data tables
combined_data <- list()

# Step 5: Loop through each unique start name and combine files with matching starts
for (start in unique_starts) {
  # Find files that match the start name
  matching_files <- files[grep(paste0("^", start), basename(files))]
  
  # Read and row-bind all matching files
  combined_data[[start]] <- rbindlist(lapply(matching_files, fread))
}

# Step 6: Process the combined data for specific patterns
# Define patterns to search
patterns <- c("Facility_Level_Results_rep_days", "Hourly_Results_NE_rep_days")
combined_results <- list()

for (pattern in patterns) {
  # Find matching keys in the combined_data list
  matching_keys <- grep(paste0("^", pattern), names(combined_data), value = TRUE)
  
  # Extract and combine data for each pattern
  combined_data_set <- do.call(
    rbind,
    lapply(matching_keys, function(key) combined_data[[key]])
  )
  
  # Store in results list
  combined_results[[pattern]] <- combined_data_set
}

# Step 7: Access combined results
Facility_Level_Results_rep_days <- combined_results[["Facility_Level_Results_rep_days"]]
Hourly_Results_NE_rep_days <- combined_results[["Hourly_Results_NE_rep_days"]]
Hourly_Results_NE_rep_days[["HI.total_hr_mmBtu"]] <- as.numeric(Hourly_Results_NE_rep_days[["HI.total_hr_mmBtu"]])
Facility_Level_Results_rep_days[["HI_mmBtu"]] <- as.numeric(Facility_Level_Results_rep_days[["HI_mmBtu"]])


# All facilities: 
Hourly_Results_NE_rep_days_ALL <- data.table()
for (sim in unique(Hourly_Results_NE_rep_days$Simulation)) {
  for (path in unique(Hourly_Results_NE_rep_days$Pathway)) {
    
    # Filter for specific Simulation and Pathway
    subset_data <- Hourly_Results_NE_rep_days[Simulation == sim & Pathway == path]
    subset_data <- merge(demand_factor_data, subset_data, by = c("Date", "Hour"), all = TRUE)
    subset_data[, `:=`(V1 = NULL, Simulation = sim, Pathway = path, Year = year(Date), DayLabel = yday(Date))]
    
    
    # Get all column names after 'Percentile_Solar'
    cols_names_fill_NA <- names(subset_data)[which(names(subset_data) == "Percentile_Solar"):length(names(subset_data))]
    
    # Fill NA values for all these columns
    for (col in cols_names_fill_NA) {
      # Extract non-NA representative values for the column, ensuring no duplicates
      rep_values <- subset_data[!is.na(get(col)), .(value = first(get(col))), 
                                by = .(Representative_Date, Hour)]
      
      # Join back the representative values to the rows with NA
      subset_data[is.na(get(col)), (col) := rep_values[.SD, on = .(Representative_Date, Hour), value, allow.cartesian = TRUE]]
    }

    # Identify columns to modify
    columns_to_multiply <- cols_names_fill_NA
    columns_to_exclude <- c("Percentile_Solar", "Percentile_Onwind", "Percentile_Offwind", 
                            "Percentile_Import_QC", "Percentile_Import_NYISO", "Percentile_Import_NB",
                            "Peak.status_hour", "Storage.charging_hours", "Storage.discharging_hours")      
    columns_to_modify <- setdiff(columns_to_multiply, columns_to_exclude)        
    
    # Apply factor conditionally
    subset_data[, (columns_to_modify) := lapply(.SD, function(col) 
      ifelse(Representative_Date != Date, col * Factor, col)
    ), .SDcols = columns_to_modify]
    
    
    # Update the main data.table with the filled subset
    Hourly_Results_NE_rep_days_ALL <- rbind(Hourly_Results_NE_rep_days_ALL, subset_data)
  }
}

rep_day_counts <- table(Hourly_Results_NE_rep_days_ALL$Representative_Date)

# Facility level
Facility_Level_Results_rep_days_ALL <- data.table()
for (sim in unique(Facility_Level_Results_rep_days$Simulation)) {
  for (path in unique(Facility_Level_Results_rep_days$Pathway)) {
    
    # Filter for specific Simulation and Pathway
    subset_data <- Facility_Level_Results_rep_days[Simulation == sim & Pathway == path]
    subset_data <- merge(demand_factor_data, subset_data, by = c("Date", "Hour"), all = TRUE)
    subset_data[, `:=`(V1 = NULL, Simulation = sim, Pathway = path, Year = year(Date), DayLabel = yday(Date))]
    
    
    # Get all column names after 'Percentile_Solar'
    cols_names_fill_NA <- names(subset_data)[which(names(subset_data) == "Facility_Unit.ID"):length(names(subset_data))]
    
    # Fill NA values for all these columns
    for (col in cols_names_fill_NA) {
      # Extract non-NA representative values for the column, ensuring no duplicates
      rep_values <- subset_data[!is.na(get(col)), .(value = first(get(col))), 
                                by = .(Representative_Date, Hour)]
      
      # Join back the representative values to the rows with NA
      subset_data[is.na(get(col)), (col) := rep_values[.SD, on = .(Representative_Date, Hour), value, allow.cartesian = TRUE]]
    }
    
    # Identify columns to modify
    columns_to_multiply <- cols_names_fill_NA
    columns_to_exclude <- c("Facility_Unit.ID", "State", "Fuel_type_1", "Fuel_type_2", "latitude",        
                            "longitude", "Ramp_hr", "Fossil.NPC_MW", "Percentile")      
    columns_to_modify <- setdiff(columns_to_multiply, columns_to_exclude)        
    
    # Apply factor conditionally
    subset_data[, (columns_to_modify) := lapply(.SD, function(col) 
      ifelse(Representative_Date != Date, col * Factor, col)
    ), .SDcols = columns_to_modify]
    
    
    # Update the main data.table with the filled subset
    Facility_Level_Results_rep_days_ALL <- rbind(Facility_Level_Results_rep_days_ALL, subset_data)
  }
}

# Save the combined data tables back to CSV
fwrite(Hourly_Results_NE_rep_days, file.path(folder_path, "Hourly_Results_NE_rep_days.csv"))
fwrite(Facility_Level_Results_rep_days_ALL, file.path(folder_path, "Hourly_Results_Rep_Days_extended.csv"))
fwrite(Facility_Level_Results_rep_days_ALL, file.path(folder_path, "Facility_Level_Results_Rep_Days_extended.csv"))
