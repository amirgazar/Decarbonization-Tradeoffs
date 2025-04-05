# Load necessary libraries
library(data.table)

# Define folder path
folder_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/3 Intemediate Results [if any]/1 Comprehensive Results Samples/1 Sample Dec 05 2024"
csv_files <- list.files(folder_path, pattern = "^Hourly_Results_NE_stepwise.*\\.csv$", full.names = TRUE)
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results"

# Initialize an empty list to store processed dt
summary_list <- list()

# Loop through each file and calculate summary statistics
for (file in csv_files) {
  # Read the file
  dt <- fread(file)
  
  # Remove unwanted column
  dt[, V1 := NULL]
  
  # Calculate lag and battery change
  dt[, Prev_Storage := shift(Storage.status_hr_MW, type = "lag", fill = 0)]
  dt[, Battery.Change := Storage.status_hr_MW - Prev_Storage]
  dt[, Battery.Discharged := pmax(0, -Battery.Change)] # Only discharging
  
  # Calculate Renewable Penetration
  dt[, `:=`(
    Renewable.Penetration = (Solar.gen_hr_MW + Onwind.gen_hr_MW + Offwind.gen_hr_MW) / Total.gen_hr_MW)]
  
  
  # Summarize by year
  summary_dt <- dt[, .(
    RP_mean = mean(Renewable.Penetration, na.rm = TRUE),
    RP_max = max(Renewable.Penetration, na.rm = TRUE),
    RP_min = min(Renewable.Penetration, na.rm = TRUE),
    Battery_Discharges_TWh = sum(Battery.Discharged, na.rm = TRUE)/1e6,
    Shortages_TWh = sum(Shortage.total_hr_MW, na.rm = TRUE)/1e6,
    CO2_tons = sum(CO2.total_hr_tons, na.rm = TRUE)
  ), by = c("Year", "Simulation", "Pathway")]
  
  
  # Append summary to the list
  summary_list[[file]] <- summary_dt
}

# Combine all summaries into a single table
combined_summary <- rbindlist(summary_list)

# Save combined NPV results to a single CSV file
write.csv(combined_summary, file = file.path(output_path, "Battery_Data.csv"), row.names = FALSE)
