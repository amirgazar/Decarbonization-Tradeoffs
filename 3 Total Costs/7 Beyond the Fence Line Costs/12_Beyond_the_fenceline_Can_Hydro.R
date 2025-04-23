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

discount_rate <- 0.07
base_year <- 2024
n_hours_in_year <- 8760
hydro_CF <- 65/100 # From Hydro quebec

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
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"

Yearly_Results <- as.data.table(fread(file_path))
Yearly_Results[, V1 := NULL]
Yearly_Results[, V1 := NULL]
Yearly_Results <- Yearly_Results[ Pathway == pathway_B3, ]
# Compute base imports from each source (in TWh)
Yearly_Results[, Total_QC_import_TWh := Spot_Market_Imports_HQ_TWh + Long_Term_Imports_HQ_TWh]
Yearly_Results[, Total_base_import_TWh := Total_QC_import_TWh + Import_NYISO_TWh + Import_NBSO_TWh]

# Calculate fractional shares from the base values
Yearly_Results[, QC_import_share := Total_QC_import_TWh / Total_base_import_TWh]
Yearly_Results[, NYISO_import_share := Import_NYISO_TWh / Total_base_import_TWh]
Yearly_Results[, NBSO_import_share := Import_NBSO_TWh / Total_base_import_TWh]

# Calculate maximum import capacities from the installed capacity columns,
# converting from MWh to TWh (1 TWh = 1,000 MWh) using the factor 0.95 for the max CF.
imports_max_CF <- 0.95
Yearly_Results[, Total_QC_import_max_MWh := Imports_HQ_MW * imports_max_CF]
Yearly_Results[, Total_NYISO_import_max_MWh := Imports_NYISO_MW * imports_max_CF]
Yearly_Results[, Total_NBSO_import_max_MWh := Imports_NBSO_MW * imports_max_CF]

# Convert maximum capacities from MWh to TWh
Yearly_Results[, Total_QC_import_max_TWh := Total_QC_import_max_MWh / 1000]
Yearly_Results[, Total_NYISO_import_max_TWh := Total_NYISO_import_max_MWh / 1000]
Yearly_Results[, Total_NBSO_import_max_TWh := Total_NBSO_import_max_MWh / 1000]

# Compute the extra capacity available for each source (in TWh)
Yearly_Results[, extra_possible_QC := pmax(Total_QC_import_max_TWh - Total_QC_import_TWh, 0)]
Yearly_Results[, extra_possible_NYISO := pmax(Total_NYISO_import_max_TWh - Import_NYISO_TWh, 0)]
Yearly_Results[, extra_possible_NBSO := pmax(Total_NBSO_import_max_TWh - Import_NBSO_TWh, 0)]

Yearly_Results$Import_diff <- Yearly_Results$Calibrated_Total_import_net_TWh - Yearly_Results$Total_import_net_TWh
Yearly_Results[, c("Calibrated_QC_import_net_TWh", 
                   "Calibrated_NYISO_import_net_TWh", 
                   "Calibrated_NBSO_import_net_TWh") := {
                     
                     # Base import values (in TWh)
                     base_QC    = Total_QC_import_TWh
                     base_NYISO = Import_NYISO_TWh
                     base_NBSO  = Import_NBSO_TWh
                     
                     # Maximum available capacities (in TWh)
                     max_QC    = Total_QC_import_max_TWh
                     max_NYISO = Total_NYISO_import_max_TWh
                     max_NBSO  = Total_NBSO_import_max_TWh
                     
                     # Initial proportional allocation from Import_diff (in TWh)
                     alloc_QC    = QC_import_share * Import_diff
                     alloc_NYISO = NYISO_import_share * Import_diff
                     alloc_NBSO  = NBSO_import_share * Import_diff
                     
                     # Cap each allocation so that base + extra does not exceed maximum capacity
                     alloc_QC    = min(alloc_QC, max_QC - base_QC)
                     alloc_NYISO = min(alloc_NYISO, max_NYISO - base_NYISO)
                     alloc_NBSO  = min(alloc_NBSO, max_NBSO - base_NBSO)
                     
                     # Calculate the total allocated extra so far and remaining extra to allocate
                     allocated = alloc_QC + alloc_NYISO + alloc_NBSO
                     leftover = Import_diff - allocated
                     tol = 1e-6
                     
                     # Redistribute any leftover extra among sources with remaining capacity.
                     while(leftover > tol) {
                       avail_QC    = max_QC - (base_QC + alloc_QC)
                       avail_NYISO = max_NYISO - (base_NYISO + alloc_NYISO)
                       avail_NBSO  = max_NBSO - (base_NBSO + alloc_NBSO)
                       
                       total_avail = max(avail_QC + avail_NYISO + avail_NBSO, tol)
                       if(total_avail < tol) break  # No further capacity available
                       
                       # Determine additional shares based on available capacity
                       share_QC    = avail_QC / total_avail
                       share_NYISO = avail_NYISO / total_avail
                       share_NBSO  = avail_NBSO / total_avail
                       
                       # Additional allocation for each source is the minimum of the share and the available capacity.
                       add_QC    = min(leftover * share_QC, avail_QC)
                       add_NYISO = min(leftover * share_NYISO, avail_NYISO)
                       add_NBSO  = min(leftover * share_NBSO, avail_NBSO)
                       
                       add_total = add_QC + add_NYISO + add_NBSO
                       if(add_total < tol) break  # Nothing further can be allocated
                       
                       # Update allocations with the additional amounts and reduce the leftover accordingly.
                       alloc_QC    = alloc_QC + add_QC
                       alloc_NYISO = alloc_NYISO + add_NYISO
                       alloc_NBSO  = alloc_NBSO + add_NBSO
                       leftover = leftover - add_total
                     }
                     
                     # Final calibrated import for each source is the base plus its extra allocation.
                     list(base_QC + alloc_QC, base_NYISO + alloc_NYISO, base_NBSO + alloc_NBSO)
                   }, by = 1:nrow(Yearly_Results)]



# Calculate mean, max, and min for Calibrated_QC_import_net_TWh by Year and Scenario
Yearly_imports <- Yearly_Results[, .(
  Mean_imports_QC_MWh = mean(Calibrated_QC_import_net_TWh, na.rm = TRUE) * 1e6, #TWh to MWh
  Max_imports_QC_MWh = max(Calibrated_QC_import_net_TWh, na.rm = TRUE) * 1e6,
  Min_imports_QC_MWh = min(Calibrated_QC_import_net_TWh, na.rm = TRUE) * 1e6
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

# -------------------------------
# CH4 Emissions
#Emissions using Delwiche et al, 
# -------------------------------
# 1. Setup and Data Preparation
# -------------------------------
# Define the emission factors (kg CH4-C per MWh imported) from Delwiche et al. (2022)
min_emission_factor <- 0.16  # Lower bound
max_emission_factor <- 1.22  # Upper bound

# Convert imported energy from MWh to TWh (1 TWh = 1e6 MWh) 
Yearly_imports[, Generation_TWh_Mean := Mean_imports_QC_MWh / imports_max_CF]
Yearly_imports[, Generation_TWh_Max  := Max_imports_QC_MWh / imports_max_CF]
Yearly_imports[, Generation_TWh_Min  := Min_imports_QC_MWh / imports_max_CF]

# -------------------------------
# 2. Calculate Annual CH4-C and CH4 Emissions
# -------------------------------
# Scale the emission factors by the imported generation (in TWh)

# Annual CH4-C emissions (in kg) for the lower and upper bounds:
Yearly_imports[, Annual_CH4_C_emissions_kg_Lower := Generation_TWh_Min * min_emission_factor]
Yearly_imports[, Annual_CH4_C_emissions_kg_Upper := Generation_TWh_Max * max_emission_factor]

# Convert CH4-C to CH4 using the conversion factor (12 g CH4-C = 16 g CH4)
conversion_factor <- 16 / 12
Yearly_imports[, Annual_CH4_emissions_kg_Lower := Annual_CH4_C_emissions_kg_Lower * conversion_factor]
Yearly_imports[, Annual_CH4_emissions_kg_Upper := Annual_CH4_C_emissions_kg_Upper * conversion_factor]

# Convert emissions from kg to tonnes (1 tonne = 1000 kg)
Yearly_imports[, Annual_CH4_emissions_tonnes_Lower := Annual_CH4_emissions_kg_Lower / 1000]
Yearly_imports[, Annual_CH4_emissions_tonnes_Upper := Annual_CH4_emissions_kg_Upper / 1000]

# -------------------------------
# 3. Setup Cost Projections via CPI Adjustment
# -------------------------------
# Adjust CH4 cost values from 2019 to 2024 using CPI data.
cpi_2019 <- filter(cpi_data, year(date) == 2019) %>% summarise(YearlyAvg = mean(value))
conversion_rate_2019_24 <- cpi_2024$YearlyAvg / cpi_2019$YearlyAvg

# Set the projection period to match Yearly_imports data (2025 to 2050)
start_year <- 2025
end_year   <- 2050

# Define cost estimates for CH4 in 2021 and 2050 (adjusted to 2024 dollars)
# (Note: If needed, these base years could be aligned with the projection period.)
costs_2021 <- list(CH4 = 2732 * conversion_rate_2019_24)
costs_2050 <- list(CH4 = 4684 * conversion_rate_2019_24)

# Create a cost projection table using an interpolation function
GHG_Costs <- data.table(Year = start_year:end_year)
GHG_Costs[, CH4_cost := interpolate_cost(Year, start_year, end_year, costs_2021$CH4, costs_2050$CH4)]

# -------------------------------
# 4. Merge Emission Estimates with Cost Projections
# -------------------------------
# Select only the years available in Yearly_imports
emissions <- Yearly_imports[Year %in% (start_year:end_year), 
                            .(Year, Annual_CH4_emissions_tonnes_Lower, Annual_CH4_emissions_tonnes_Upper)]

# Merge the emissions estimates with the cost projections
BFL_costs_pathway_B3_CH4_Lower <- merge(emissions[, .(Year, Annual_CH4_emissions_tonnes_Lower)], 
                                        GHG_Costs, 
                                        by = "Year")
BFL_costs_pathway_B3_CH4_Lower[, total_CH4_USD_Lower := CH4_cost * Annual_CH4_emissions_tonnes_Lower]

BFL_costs_pathway_B3_CH4_Upper <- merge(emissions[, .(Year, Annual_CH4_emissions_tonnes_Upper)], 
                                        GHG_Costs, 
                                        by = "Year")
BFL_costs_pathway_B3_CH4_Upper[, total_CH4_USD_Upper := CH4_cost * Annual_CH4_emissions_tonnes_Upper]

# Remove any rows with missing values if present
BFL_costs_pathway_B3_CH4_Lower <- na.omit(BFL_costs_pathway_B3_CH4_Lower)
BFL_costs_pathway_B3_CH4_Upper <- na.omit(BFL_costs_pathway_B3_CH4_Upper)

# -------------------------------
# 5. Calculate Net Present Value (NPV) of CH4 Costs
# -------------------------------
# Use an existing function 'calculate_npv' that accepts the data table, discount rate, year column, and cost column name.
CH4_npv_lower <- BFL_costs_pathway_B3_CH4_Lower[, .(NPV_CH4_Lower = calculate_npv(.SD, discount_rate, Year, "total_CH4_USD_Lower"))]
CH4_npv_upper <- BFL_costs_pathway_B3_CH4_Upper[, .(NPV_CH4_Upper = calculate_npv(.SD, discount_rate, Year, "total_CH4_USD_Upper"))]

# Combine the lower and upper NPV estimates into a single data table
CH4_npv <- cbind(CH4_npv_lower, CH4_npv_upper)

# -------------------------------
# 6. Save the Results to CSV
# -------------------------------
write.csv(CH4_npv, file = file.path(output_path, "CH4_CAN_Hydro.csv"), row.names = FALSE)
