# Load libraries
library(data.table)
library(readxl)
library(tidyverse)
library(fredr)


# FUNCTIONS
# NPV Calculator
calculate_npv <- function(dt, rate, base_year, col) {
  npv <- sum(dt[[col]] / (1 + rate)^(dt[["Year"]] - base_year))
  return(npv)
}

discount_rate <- 0.03
base_year <- 2024
n_hours_in_year <- 8760
hydro_CF <- 60.3/100

# API KEY 63522eae4ec927d6f1d9d86bf7826cc8
fredr_set_key("63522eae4ec927d6f1d9d86bf7826cc8") 
cpi_data <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"), observation_end = as.Date("2024-01-01"))

# Interpolation function for costs
interpolate_cost <- function(year, start_year, end_year, start_cost, end_cost) {
  return(start_cost + (end_cost - start_cost) * (year - start_year) / (end_year - start_year))
}

# Load Hydropower Capacity
# We calculate New Hydro power installed capacity
years <- 2024:2050
increment <- (9000 - 0) / (2045 - 2024)
values <- ifelse(years <= 2045, 0 + (years - 2024) * increment, 9000)
Hydropower <- data.table(
  Year = years,
  Pathway = "B3",
  Base_MW = values
)

# Load Capacity data
path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/Imports/Import Capacity.xlsx"

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
import_columns <- c("Imports QC") # we are only interested in imports from Quebec 
for (col in import_columns) {
  new_col_name <- paste0("new_capacity_", gsub(" ", "_", col))  # Create new column names
  decarbonization_pathways[, (new_col_name) := get(col) - shift(get(col), 1, type = "lag"), by = Pathway]
  decarbonization_pathways[is.na(get(new_col_name)), (new_col_name) := 0]
}

decarbonization_pathways[, new_capacity := round(new_capacity_Imports_QC, 2)]
Imports_Capacity <- decarbonization_pathways[, total_capacity := round(`Imports QC`, 2)]

# Pathways of interest [B1 is baseline for B3, so the excess is calculated based additions to B1]
pathway_B3 <- c("B3")

baseline_capacity_B1 <- Imports_Capacity[Pathway == "B1", ]
Imports_Capacity <- Imports_Capacity[Pathway %in% pathway_B3, ]

if (length(Imports_Capacity) == length(baseline_capacity_B1)) {
  # Calculate the difference
  Imports_Capacity$new_capacity <- Imports_Capacity$new_capacity - baseline_capacity_B1$new_capacity
} else {
  stop("Mismatch in number of rows between B3 and B1. Ensure data aligns correctly.")
}


# Initialize a variable to keep track of the last cumulative sum
last_cumsum <- 0

# Loop through the rows to calculate the cumulative sum with the desired behavior
for (i in 1:nrow(Imports_Capacity)) {
 
  # If the current difference is not zero and different from the previous one, add it to the cumulative sum
  if (Imports_Capacity$new_capacity[i] != 0 && Imports_Capacity$new_capacity[i] != Imports_Capacity$new_capacity[i-1]) {
    last_cumsum <- last_cumsum + Imports_Capacity$new_capacity[i]
  } 
  # Set the cumulative sum for the current row
  Imports_Capacity$cumsum_diff[i] <- last_cumsum
}

Imports_Capacity$QC_import_capacitydiff_MW <- Imports_Capacity$cumsum_diff
Imports_Capacity <- Imports_Capacity[, .(Year, Pathway, QC_import_capacitydiff_MW)]

# Add Imports_Difference to hydropower
Hydropower <- merge(Imports_Capacity, Hydropower, by = c("Pathway", "Year"), all.x = TRUE)
Hydropower <- Hydropower %>%
  mutate(Base_hydro_MW = QC_import_capacitydiff_MW/ (hydro_CF)) %>%
  mutate(Max_hydro_gen_MWh = Base_hydro_MW * n_hours_in_year * hydro_CF) %>%
 # mutate(Base_NE_ratio_MW = Base_NE_cost_ratio * Base_MW) %>%
  filter(!is.na(Base_hydro_MW))   # Remove rows with NA


# Load Imports data#left_join() Load Imports data
#-- Stepwise
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results Comperhensive"
#-- Rep Days
#file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/2 Representative Days Summary Results/Yearly_Results_rep_days.csv"
#output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results/2 Representative Days Costs/"

Yearly_Results <- as.data.table(fread(file_path))
Yearly_Results[, V1 := NULL]
Yearly_Results[, V1 := NULL]
Yearly_Results <- Yearly_Results[ Pathway == pathway_B3, ]


# Calculate mean, max, and min for Import.QC_hr_TWh by Year and Scenario
Yearly_imports <- Yearly_Results[, .(
  Mean_imports_QC_MWh = mean(Import.QC_hr_TWh, na.rm = TRUE) * 1e6, #TWh to MWh
  Max_imports_QC_MWh = max(Import.QC_hr_TWh, na.rm = TRUE) * 1e6,
  Min_imports_QC_MWh = min(Import.QC_hr_TWh, na.rm = TRUE) * 1e6
), by = .(Year, Pathway)]

# Evaluate how much of imports are from new transmission lines
Hydropower <- merge(Yearly_imports, Hydropower, by = c("Year", "Pathway"), all.x = TRUE)
Hydropower <- Hydropower %>%
  mutate(import_ratio = ifelse((Max_hydro_gen_MWh - Max_imports_QC_MWh) / Max_hydro_gen_MWh > 0,
                               (Max_hydro_gen_MWh - Max_imports_QC_MWh) / Max_hydro_gen_MWh,
                               0)) %>%
  mutate(import_ratio = 1 - import_ratio) %>%
  mutate(Base_hydro_MW = Base_hydro_MW * import_ratio)

setDT(Hydropower)
Hydropower[, Increase_hydro_MW := pmax(0, c(0, diff(Base_hydro_MW)))]


# CAPEX and FOM
# CAPEX: The investment costs of large (>10 MWe) hydropower plants range from $1750/kWe to $6250/kWe and are very site-sensitive, with an average figure of about $4000/kWe (US$ 2008).
# O&M costs are estimated between 1.5% and 2.5% of investment costs per year. Source: https://www.iea-etsap.org/E-TechDS/HIGHLIGHTS%20PDF/E06-hydropower-GS-gct_ADfina_gs%201.pdf
# ISO NE Reports HQ CAPEX Costs to be $5537/kW in 2016 USD

# Extracting CPI values for specific years
cpi_2016 <- filter(cpi_data, year(date) == 2016) %>% summarise(YearlyAvg = mean(value))
cpi_2024 <- filter(cpi_data, year(date) == 2024) %>% summarise(YearlyAvg = mean(value))

# Calculating conversion rate
conversion_rate_2016_24 <- cpi_2024$YearlyAvg / cpi_2016$YearlyAvg

CAPEX_mean <- 5537 * conversion_rate_2016_24 # Using ISO NE valuation $/KW
CAPEX_mean <- CAPEX_mean * 1000 # Using ISO NE valuation $/MW

#CAPEX_lower_factor <- 1750 / 4000
#CAPEX_upper_factor <- 6250 / 4000

#CAPEX_lower <- CAPEX_mean * CAPEX_lower_factor
#CAPEX_upper <- CAPEX_mean * CAPEX_upper_factor

CAPEX_lower <- 8307021 # $/MW, obtained from 12_Other_S3_CAN_Hydro_CostAssumptions.R code
CAPEX_upper <- 19688294 # $/MW, obtained from 12_Other_S3_CAN_Hydro_CostAssumptions.R code

FOM_lower <- CAPEX_lower * 1.5 /100
FOM_upper <- CAPEX_upper * 2.5 /100

direct_costs <- list(
 Hydropower_QC = list(Upfront = c(CAPEX_lower, CAPEX_upper), Fixed_OM = c(FOM_lower, FOM_upper), Variable_OM = 0, Capacity_Factor = 60.3)
  )

# Calculate CAPEX and FOM
BFL_costs_pathway_B3 <- data.table(Hydropower)
BFL_costs_pathway_B3$CAPEX_lower <- Hydropower[, "Increase_hydro_MW"] * direct_costs$Hydropower$Upfront[1]
BFL_costs_pathway_B3$CAPEX_upper <- Hydropower[, "Increase_hydro_MW"] * direct_costs$Hydropower$Upfront[2]
BFL_costs_pathway_B3$FOM_lower <- Hydropower[, "Base_hydro_MW"] * direct_costs$Hydropower$Fixed_OM[1]
BFL_costs_pathway_B3$FOM_upper <- Hydropower[, "Base_hydro_MW"] * direct_costs$Hydropower$Fixed_OM[2]

# Calculate NPV for CAPEX and FOM with lower costs
capex_npv_lower <- BFL_costs_pathway_B3[, .(NPV_CAPEX_Lower = calculate_npv(.SD, discount_rate, Year, "CAPEX_lower"))]
fom_npv_lower <- BFL_costs_pathway_B3[, .(NPV_FOM_Lower = calculate_npv(.SD, discount_rate, Year, "FOM_lower"))]

# Calculate NPV for CAPEX and FOM with upper costs
capex_npv_upper <- BFL_costs_pathway_B3[, .(NPV_CAPEX_Upper = calculate_npv(.SD, discount_rate, Year, "CAPEX_upper"))]
fom_npv_upper <- BFL_costs_pathway_B3[, .(NPV_FOM_Upper = calculate_npv(.SD, discount_rate, Year, "FOM_upper"))]

# For the lower NPV results
npv_results_lower <- cbind(capex_npv_lower, fom_npv_lower)

# For the upper NPV results
npv_results_upper <- cbind(capex_npv_upper, fom_npv_upper)

# Rename columns for clarity
setnames(npv_results_lower, c("NPV_CAPEX_Lower", "NPV_FOM_Lower"), c("NPV_CAPEX", "NPV_FOM"))
setnames(npv_results_upper, c("NPV_CAPEX_Upper", "NPV_FOM_Upper"), c("NPV_CAPEX", "NPV_FOM"))

# Add cost type information
npv_results_lower[, Cost_Type := "Lower"]
npv_results_upper[, Cost_Type := "Upper"]

# Combine all results
npv_results <- rbind(npv_results_lower, npv_results_upper)

# Save combined NPV results to a single CSV file
write.csv(npv_results, file = file.path(output_path, "CAPEX_FOM_CAN_Hydro.csv"), row.names = FALSE)

# CH4 Emissions

#Emissions using Delwiche et al, Eastmain Dam
#Delwiche, Eastmain 1 dam (768 MW installed capacity), Latitude: 51.5, Longitude: -74, unit Tg CH4 yr−1/ MW (Eastmain 1 capacity)
# 
# Define the parameters from the provided dam data
surface_area_km2 <- 602.9
mg_CH4_C_m2_d1 <- 9.43  # Methane emission rate in mg CH4-C per m² per day
factor <- 12/16 # Convert CH4-C to CH4, 12 g of CH4-C = 16 g CH4
mg_CH4_C_m2_d1 <- mg_CH4_C_m2_d1 * factor

# Convert surface area from km² to m²
surface_area_m2 <- surface_area_km2 * 1e6  # 1 km² = 1e6 m²

# Calculate annual emissions per m²
annual_emissions_per_m2 <- mg_CH4_C_m2_d1 * 365  # Convert daily rate to annual

# Calculate total annual emissions for the reservoir
total_annual_emissions_mg <- annual_emissions_per_m2 * surface_area_m2

# Convert from mg to kg for readability (1 kg = 1e6 mg)
total_annual_emissions_kg <- total_annual_emissions_mg / 1e6

# Output the total annual emissions in kg
total_annual_emissions_kg

# Base methane emission rate for Eastmain 1 (mg CH4-C m-2 d-1)
mg_CH4_C_m2_d1 <- 9.43

# Apply a 36% variability based on the uncertainty range from the paper
mg_CH4_C_m2_d1_lower <- mg_CH4_C_m2_d1 * (1 - 0.36)  # 36% lower
mg_CH4_C_m2_d1_upper <- mg_CH4_C_m2_d1 * (1 + 0.36)  # 36% higher

# Base dam capacity for estimation (Eastmain's capacity is 768 MW)
base_dam_capacity_MW <- 768
# Surface area to capacity ratio (Eastmain's surface area divided by its capacity)
surface_area_km2_per_MW <- 602.9 / base_dam_capacity_MW

# Calculate surface area for the dams based on the equivalent number of base dams
Hydropower[, Surface_Area_km2 := Base_hydro_MW * surface_area_km2_per_MW]

# Convert surface area from km² to m²
Hydropower[, Surface_Area_m2 := Surface_Area_km2 * 1e6]

# Calculate annual emissions per m² for lower and upper bounds
annual_emissions_per_m2_lower <- mg_CH4_C_m2_d1_lower * 365  # Lower bound
annual_emissions_per_m2_upper <- mg_CH4_C_m2_d1_upper * 365  # Upper bound

# Calculate total annual emissions for the new dams for lower and upper bounds
Hydropower[, Total_Annual_Emissions_mg_Lower := annual_emissions_per_m2_lower * Surface_Area_m2]
Hydropower[, Total_Annual_Emissions_mg_Upper := annual_emissions_per_m2_upper * Surface_Area_m2]

# Convert from mg to kg (1 kg = 1e6 mg)
Hydropower[, Total_Annual_Emissions_kg_Lower := Total_Annual_Emissions_mg_Lower / 1e6]
Hydropower[, Total_Annual_Emissions_kg_Upper := Total_Annual_Emissions_mg_Upper / 1e6]

# Convert from kg to tonnes (1 tonne = 1000 kg)
Hydropower[, Total_Annual_CH4_Emissions_tonnes_Lower := Total_Annual_Emissions_kg_Lower / 1000]
Hydropower[, Total_Annual_CH4_Emissions_tonnes_Upper := Total_Annual_Emissions_kg_Upper / 1000]

# Define the starting and ending costs for 2021 and 2050 - Source; Total Costs NY Paper
# Extracting CPI values for specific years
cpi_2019 <- filter(cpi_data, year(date) == 2019) %>% summarise(YearlyAvg = mean(value))
# Calculating conversion rate
conversion_rate_2019_24 <- cpi_2024$YearlyAvg / cpi_2019$YearlyAvg

start_year <- 2021
end_year <- 2050
costs_2021 <- list(CH4 = 2732 * conversion_rate_2019_24) # 2019 values, we convert to 2024
costs_2050 <- list(CH4 = 4684 * conversion_rate_2019_24)

# Interpolate costs for each year
GHG_Costs <- data.table(Year = start_year:end_year)
GHG_Costs[, `:=`(
  CH4_cost = interpolate_cost(Year, start_year, end_year, costs_2021$CH4, costs_2050$CH4)
)]

# Merge Costs with Emissions
selected_cols_lower <- Hydropower[, .(Year, Total_Annual_CH4_Emissions_tonnes_Lower)]
selected_cols_upper <- Hydropower[, .(Year, Total_Annual_CH4_Emissions_tonnes_Upper)]

BFL_costs_pathway_B3_CH4_Lower <- selected_cols_lower[GHG_Costs, on = "Year"]
BFL_costs_pathway_B3_CH4_Upper <- selected_cols_upper[GHG_Costs, on = "Year"]

BFL_costs_pathway_B3_CH4_Lower[, total_CH4_USD_Lower := CH4_cost * Total_Annual_CH4_Emissions_tonnes_Lower]
BFL_costs_pathway_B3_CH4_Upper[, total_CH4_USD_Upper := CH4_cost * Total_Annual_CH4_Emissions_tonnes_Upper]

BFL_costs_pathway_B3_CH4_Lower <- na.omit(BFL_costs_pathway_B3_CH4_Lower)
BFL_costs_pathway_B3_CH4_Upper <- na.omit(BFL_costs_pathway_B3_CH4_Upper)

# Calculate NPV for CH4 costs with lower and upper emissions
CH4_npv_lower <- BFL_costs_pathway_B3_CH4_Lower[, .(NPV_CH4_Lower = calculate_npv(.SD, discount_rate, Year, "total_CH4_USD_Lower"))]
CH4_npv_upper <- BFL_costs_pathway_B3_CH4_Upper[, .(NPV_CH4_Upper = calculate_npv(.SD, discount_rate, Year, "total_CH4_USD_Upper"))]

# Combine NPV results
CH4_npv <- cbind(CH4_npv_lower, CH4_npv_upper)

# Save combined NPV results to a single CSV file
write.csv(CH4_npv, file = file.path(output_path, "CH4_CAN_Hydro.csv"), row.names = FALSE)
