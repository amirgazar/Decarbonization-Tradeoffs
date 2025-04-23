# Load libraries
library(data.table)
library(readxl)
library(fredr)

# NPV Calculator
calculate_npv <- function(dt, rate, base_year, col) {
  npv <- sum(dt[[col]] / (1 + rate)^(dt[['Year']] - base_year))
  return(npv)
}

discount_rate <- 0.07
base_year <- 2024

# API KEY 63522eae4ec927d6f1d9d86bf7826cc8
fredr_set_key("63522eae4ec927d6f1d9d86bf7826cc8") 
cpi_data <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"), observation_end = as.Date("2024-01-01"))

# Extracting CPI values for specific years
cpi_2021 <- filter(cpi_data, year(date) == 2021) %>% summarise(YearlyAvg = mean(value))
cpi_2022 <- filter(cpi_data, year(date) == 2022) %>% summarise(YearlyAvg = mean(value))
cpi_2024 <- filter(cpi_data, year(date) == 2024) %>% summarise(YearlyAvg = mean(value))


# Calculating conversion rate
conversion_rate_2021 <- cpi_2024$YearlyAvg / cpi_2021$YearlyAvg
conversion_rate_2022 <- cpi_2024$YearlyAvg / cpi_2022$YearlyAvg

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
Imports_Capacity <- decarbonization_pathways[, total_capacity := round(`Imports QC` + `Imports NYISO` + `Imports NBSO`, 2)]
Imports_Capacity <- Imports_Capacity[, .(Year, Pathway, Imports_QC = `Imports QC`, Imports_NYISO = `Imports NYISO`, Imports_NB = `Imports NBSO`, total_capacity)]


# Load Imports Costs (i.e cost of market rate) US Market - 2021
# Source: https://www.researchgate.net/publication/356444411_Cost_of_Long-Distance_Energy_Transmission_by_Different_Carriers
lower_Imports <- 20.8 * conversion_rate_2021 # $/MWh/1000 miles
upper_Imports <- 62.5 * conversion_rate_2021 # $/MWh/1000 miles

# Costs for CHPE - 2022
# 0.5 to 2 cents per KWh or 5-20 dollars per MWh 
CHPE_cost <- 2 * conversion_rate_2022
CHPE_cost <- CHPE_cost*100/(1000/339)
# CHPE Length 339 miles 
# Source Ryans NY Paper
lower_Imports_QC <- CHPE_cost - (upper_Imports-lower_Imports)  # $/MWh/1000 miles
upper_Imports_QC <- CHPE_cost # $/MWh/1000 miles

# Load Generation data
#-- Stepwise
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"

Yearly_Results <- as.data.table(fread(file_path))
Yearly_Results[, V1 := NULL]
Yearly_Results[, V1 := NULL]

# Assume every 1200 MW = 340 Miles
# Calculate total miles for each jurisdiction
conversion_factor <- 340 / 1200 # Miles per MW or thousand miles per GW

# Calculate total miles and costs for each jurisdiction
Imports_Capacity[, QC_Miles := Imports_QC * conversion_factor ]
Imports_Capacity[, NB_Miles := Imports_NB * conversion_factor ]
Imports_Capacity[, NYISO_Miles := Imports_NYISO * conversion_factor ]

Imports_Capacity[, QC_USD_TWh_lower := lower_Imports_QC * QC_Miles * 1e6/1e3] # USD/ TWh/1000miles
Imports_Capacity[, NB_USD_TWh_lower := lower_Imports_QC * NB_Miles  * 1e3]
Imports_Capacity[, NYISO_USD_TWh_lower := lower_Imports * NYISO_Miles  * 1e3]

Imports_Capacity[, QC_USD_TWh_upper := upper_Imports_QC * QC_Miles * 1e3]
Imports_Capacity[, NB_USD_TWh_upper := upper_Imports_QC * NB_Miles * 1e3]
Imports_Capacity[, NYISO_USD_TWh_upper := upper_Imports * NYISO_Miles * 1e3]

# Define a function to process import costs for each Pathway
process_imports <- function(import_costs, yearly_results, pathway, simulation, jurisdiction) {
  # Filter the data for the specific Pathway and simulation
  costs <- import_costs[Pathway == pathway]
  yearly <- yearly_results[Pathway == pathway & Simulation == simulation]
  
  # Merge the costs with yearly results
  merged_data <- merge(yearly, costs, by = "Year")
  
  # Map correct column names
  if (jurisdiction == "QC") {
    # Sum spot + long-term imports
    merged_data[, Import_QC_TWh := Spot_Market_Imports_HQ_TWh + Long_Term_Imports_HQ_TWh]
    import_col <- "Import_QC_TWh"
    lower_cost_col <- "QC_USD_TWh_lower"
    upper_cost_col <- "QC_USD_TWh_upper"
  } else if (jurisdiction == "NYISO") {
    import_col <- "Import_NYISO_TWh"
    lower_cost_col <- "NYISO_USD_TWh_lower"
    upper_cost_col <- "NYISO_USD_TWh_upper"
  } else if (jurisdiction == "NB") {
    import_col <- "Import_NBSO_TWh"
    lower_cost_col <- "NB_USD_TWh_lower"
    upper_cost_col <- "NB_USD_TWh_upper"
  } else {
    stop("Invalid jurisdiction specified.")
  }
  
  # Calculate actual costs
  merged_data[, Actual_Cost_Lower := get(lower_cost_col) * get(import_col)]
  merged_data[, Actual_Cost_Upper := get(upper_cost_col) * get(import_col)]
  
  # Calculate NPV
  npv_lower <- calculate_npv(merged_data, discount_rate, base_year, "Actual_Cost_Lower")
  npv_upper <- calculate_npv(merged_data, discount_rate, base_year, "Actual_Cost_Upper")
  
  list(Var_OM_NPV_Lower = npv_lower, Var_OM_NPV_Upper = npv_upper)
}

# Initialize results list
npv_results <- list()

# Loop variables
simulations <- unique(Yearly_Results$Simulation)
pathways <- unique(Yearly_Results$Pathway)
jurisdictions <- c("QC", "NYISO", "NB")

# Process the NPV for each combination of simulation, Pathway, and jurisdiction
for (sim in simulations) {
  for (path in pathways) {
    for (jur in jurisdictions) {
      result <- process_imports(Imports_Capacity, Yearly_Results, path, sim, jur)
      npv_results[[paste0(sim, "_", path, "_", jur, "_Var_OM_NPV_Lower")]] <- result$Var_OM_NPV_Lower
      npv_results[[paste0(sim, "_", path, "_", jur, "_Var_OM_NPV_Upper")]] <- result$Var_OM_NPV_Upper
    }
  }
}

# Combine NPV results into a single data.table
combined_npvs <- rbindlist(lapply(names(npv_results), function(name) {
  parts <- strsplit(name, "_")[[1]]
  data.table(Simulation = parts[1], Pathway = parts[2], Jurisdiction = parts[3], Cost_Type = ifelse(grepl("Lower", name), "Lower", "Upper"), NPV = npv_results[[name]])
}), fill = TRUE)

combined_npvs <- combined_npvs[NPV != 0,]
combined_npvs_summary_1 <- combined_npvs[, .(
  mean_NPV = mean(NPV, na.rm = TRUE)/1e9,
  sd_NPV = sd(NPV, na.rm = TRUE)/1e9
), by = .(Pathway)] 


# Save combined NPV results to a single CSV file
write.csv(combined_npvs, file = file.path(output_path, "Imports.csv"), row.names = FALSE)
