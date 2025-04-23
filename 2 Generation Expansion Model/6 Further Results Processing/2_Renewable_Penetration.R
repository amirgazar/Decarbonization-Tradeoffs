# Load necessary libraries
library(data.table)

# Define folder path
folder_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/3 Intemediate Results [if any]/1 Comprehensive Results Samples"
csv_files <- list.files(folder_path, pattern = "^Hourly_Results_.*\\.csv$", full.names = TRUE)
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results"

# Initialize an empty list to store processed dt
summary_list <- list()

# Loop through each file and calculate summary statistics
for (file in csv_files) {
  # Read the file
  dt <- fread(file)
  
  # Calculate Renewable Penetration
  dt[, `:=`(
    Renewable.Penetration = (Solar_MWh + Onshore_MWh + Offshore_MWh) / (Calibrated_Total_import_net_MWh + Old_Fossil_Fuels_adj_MWh + Clean_MWh + New_Fossil_Fuel_MWh))]
  
  # Summarize by year
  summary_dt <- dt[, .(
    RP_mean = mean(Renewable.Penetration, na.rm = TRUE),
    RP_max = max(Renewable.Penetration, na.rm = TRUE),
    RP_min = min(Renewable.Penetration, na.rm = TRUE),
    Battery_Discharges_GWh = sum(Calibrated_Battery_discharge, na.rm = TRUE)/1e3,
    Shortages_TWh = sum(Calibrated_Shortage_MWh, na.rm = TRUE)/1e6,
    CO2_tons = sum(CO2_tons, na.rm = TRUE)
  ), by = c("Year", "Simulation", "Pathway")]
  
  
  # Append summary to the list
  summary_list[[file]] <- summary_dt
}

# Combine all summaries into a single table
combined_summary <- rbindlist(summary_list)

# Save combined NPV results to a single CSV file
write.csv(combined_summary, file = file.path(output_path, "Battery_Data.csv"), row.names = FALSE)
