# Load libraries
library(data.table)
library(readxl)

# NPV Calculator
calculate_npv <- function(dt, rate, base_year, col) {
  npv <- sum(dt[[col]] / (1 + rate)^(dt[['Year']] - base_year))
  return(npv)
}

discount_rate <- 0.025
base_year <- 2024

# Load Capacity data
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/1 Decarbonization Pathways/Decarbonization_Pathways.xlsx"
sheet_names <- excel_sheets(file_path)
data_tables <- list()
# Loop through each sheet, read it into a data table, and add the Pathway column
for (sheet in sheet_names) {
  sheet_data <- as.data.table(read_excel(file_path, sheet = sheet))
  sheet_data[, Pathway := sheet]
  data_tables[[sheet]] <- sheet_data
}
decarbonization_pathways <- rbindlist(data_tables, fill = TRUE)
setorder(decarbonization_pathways, Pathway, Year)

# Calculate new capacity built each year
import_columns <- c("Imports QC", "Imports NYISO", "Imports NBSO")
for (col in import_columns) {
  new_col_name <- paste0("new_capacity_", gsub(" ", "_", col))  # Create new column names
  decarbonization_pathways[, (new_col_name) := get(col) - shift(get(col), 1, type = "lag"), by = Pathway]
  decarbonization_pathways[is.na(get(new_col_name)), (new_col_name) := 0]
}

decarbonization_pathways[, new_capacity := round(new_capacity_Imports_QC + new_capacity_Imports_NYISO + new_capacity_Imports_NBSO, 2)]
decarbonization_pathways[, total_capacity := round(`Imports QC` + `Imports NYISO` + `Imports NBSO`, 2)]

# Define CAPEX and FOM costs ranges using New England Clean Energy Connect values
CAPEX_cost_lower <- 0.7e6 # Lower range $/MW
CAPEX_cost_upper <- 1e6 # Upper range $/MW
FOM_cost_lower <- 700  # Lower range $/MW per year
FOM_cost_upper <- 1e3   # Upper range $/MW per year

# Calculate CAPEX and FOM for each year with lower costs
decarbonization_pathways[, CAPEX_lower := new_capacity * CAPEX_cost_lower]

# Calculate FOM for each year including NYISO and NB jurisdictions
decarbonization_pathways[, FOM_lower := total_capacity * FOM_cost_lower]

# Calculate CAPEX and FOM for each year with upper costs
decarbonization_pathways[, CAPEX_upper := new_capacity * CAPEX_cost_upper]

# Calculate FOM for each year including NYISO and NB jurisdictions
decarbonization_pathways[, FOM_upper := total_capacity * FOM_cost_upper]

# Calculate NPV for CAPEX and FOM with lower costs
capex_npv_lower <- decarbonization_pathways[, .(NPV_CAPEX_Lower = calculate_npv(.SD, discount_rate, base_year, "CAPEX_lower")), by = Pathway]
fom_npv_lower <- decarbonization_pathways[, .(NPV_FOM_Lower = calculate_npv(.SD, discount_rate, base_year, "FOM_lower")), by = Pathway]

# Calculate NPV for CAPEX and FOM with upper costs
capex_npv_upper <- decarbonization_pathways[, .(NPV_CAPEX_Upper = calculate_npv(.SD, discount_rate, base_year, "CAPEX_upper")), by = Pathway]
fom_npv_upper <- decarbonization_pathways[, .(NPV_FOM_Upper = calculate_npv(.SD, discount_rate, base_year, "FOM_upper")), by = Pathway]

# Combine NPV results for lower and upper costs
npv_results_lower <- merge(capex_npv_lower, fom_npv_lower, by = "Pathway")
npv_results_upper <- merge(capex_npv_upper, fom_npv_upper, by = "Pathway")

# Rename columns for clarity
setnames(npv_results_lower, c("NPV_CAPEX_Lower", "NPV_FOM_Lower"), c("NPV_CAPEX", "NPV_FOM"))
setnames(npv_results_upper, c("NPV_CAPEX_Upper", "NPV_FOM_Upper"), c("NPV_CAPEX", "NPV_FOM"))

# Add cost type information
npv_results_lower[, Cost_Type := "Lower"]
npv_results_upper[, Cost_Type := "Upper"]

# Combine all results
npv_results <- rbind(npv_results_lower, npv_results_upper)

combined_npvs_summary <- npv_results[, .(
  mean_CAPEX = mean(NPV_CAPEX, na.rm = TRUE)/1e9,
  mean_FOM = mean(NPV_FOM, na.rm = TRUE)/1e9
), by = .(Pathway, Cost_Type)] 

# Save the NPV results to a CSV file
write.csv(npv_results, file = "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results/CAPEX_FOM_Imports.csv", row.names = FALSE)
