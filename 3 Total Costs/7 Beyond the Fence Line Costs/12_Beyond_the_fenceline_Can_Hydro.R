if (!isTRUE(getOption("phased.r1.cost_runner", FALSE))) stop("Source 1 Run full cost pipeline_R1.R, not individual cost components")
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

discount_rate <- 0.025 # Must adjust the SCC of Carbon
base_year <- 2024
n_hours_in_year <- 8760
hydro_CF <- 65/100 # From Hydro quebec

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
path <- "__PROJECT_ROOT__/Imports/Import Capacity.xlsx"

# Load Capacity data
file_path <- "__PROJECT_ROOT__/1 Decarbonization Pathways/Decarbonization_Pathways.xlsx"
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
Imports_Capacity <- Imports_Capacity[, .(Year, Pathway, QC_import_capacitydiff_MW, QC_import_cap_MW = total_capacity)]

# Add Imports_Difference to hydropower
Hydropower <- merge(Imports_Capacity, Hydropower, by = c("Pathway", "Year"), all.x = TRUE)
Hydropower <- Hydropower %>%
  mutate(Base_hydro_MW = QC_import_capacitydiff_MW/ (hydro_CF)) %>%
  mutate(Max_hydro_gen_MWh = Base_hydro_MW * n_hours_in_year * hydro_CF) %>%
 # mutate(Base_NE_ratio_MW = Base_NE_cost_ratio * Base_MW) %>%
  filter(!is.na(Base_hydro_MW))   # Remove rows with NA


# Load Imports data#left_join() Load Imports data
#-- Stepwise
file_path <- "__PROJECT_ROOT__/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "__PROJECT_ROOT__/3 Total Costs/9 Total Costs Results"

Yearly_Results <- as.data.table(fread(file_path))
Yearly_Results <- Yearly_Results[ Pathway == pathway_B3, ]

#: dispatch already records delivered energy on each intertie.
# Use those annual sums instead of reconstructing them from annual shares and
# hourly MW limits (which mixed annual energy and hourly capacity).
required_imports <- c("Calibrated_Long_Term_Imports_HQ_TWh","Calibrated_Spot_Market_Imports_HQ_TWh",
                      "Calibrated_Import_NYISO_TWh","Calibrated_Import_NBSO_TWh")
if (!all(required_imports %in% names(Yearly_Results))) stop("Calibrated link import totals required")
Yearly_Results[, `:=`(
  Calibrated_QC_import_net_TWh=Calibrated_Long_Term_Imports_HQ_TWh+Calibrated_Spot_Market_Imports_HQ_TWh,
  Calibrated_NYISO_import_net_TWh=Calibrated_Import_NYISO_TWh,
  Calibrated_NBSO_import_net_TWh=Calibrated_Import_NBSO_TWh)]

# Keep coefficient bounds within each simulation because they are not ensemble percentiles.
stopifnot(!anyDuplicated(Yearly_Results, by=c("Simulation","Pathway","Year")))
# Each annual simulation has one delivered-import quantity for both cost-bound cases.
Yearly_imports <- Yearly_Results[, .(
  Mean_imports_QC_MWh = mean(Calibrated_QC_import_net_TWh, na.rm = TRUE) * 1e6, #TWh to MWh
  Max_imports_QC_MWh = max(Calibrated_QC_import_net_TWh, na.rm = TRUE) * 1e6,
  Min_imports_QC_MWh = min(Calibrated_QC_import_net_TWh, na.rm = TRUE) * 1e6
), by = .(Simulation, Year, Pathway)]

# Evaluate how much of imports are from new transmission lines
Hydropower <- merge(Yearly_imports, Hydropower, by = c("Year", "Pathway"), all.x = TRUE)
Hydropower <- Hydropower %>%
  mutate(import_ratio = ifelse((Max_hydro_gen_MWh - Max_imports_QC_MWh) / Max_hydro_gen_MWh > 0,
                               (Max_hydro_gen_MWh - Max_imports_QC_MWh) / Max_hydro_gen_MWh,
                               0)) %>%
  mutate(import_ratio = 1 - import_ratio) %>%
  mutate(Base_hydro_MW = Base_hydro_MW * import_ratio)

setDT(Hydropower)
# Allocate capacity separately within each simulation so one draw cannot change another draw’s costs.
setorder(Hydropower, Simulation, Pathway, Year)
Hydropower[, Increase_hydro_MW := pmax(0, c(0, diff(Base_hydro_MW))), by=.(Simulation,Pathway)]
Hydropower[, total_hydro_increase := sum(Increase_hydro_MW), by=.(Simulation,Pathway)]
setkey(Hydropower, Simulation, Pathway, Year)
Hydropower[, Increase_hydro_MW := {
  pool <- total_hydro_increase[1]    # this simulation only
  draws <- numeric(.N)
  for(i in seq_len(.N)) {
    # draw no more than this year’s Base_MW, and no more than what's left
    draws[i] <- min(Base_MW[i], pool)
    pool <- pool - draws[i]
  }
  draws
}, by=.(Simulation,Pathway)]

#Hydropower[, Base_hydro_MW := cumsum(Increase_hydro_MW), by = Pathway]

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

VOM_mean <- 0.58 # $/MWh ATB, its same for upper and lower

direct_costs <- list(
 Hydropower_QC = list(Upfront = c(CAPEX_lower, CAPEX_upper), Fixed_OM = c(FOM_lower, FOM_upper), Variable_OM = 0, Capacity_Factor = 60.3)
  )

# Calculate CAPEX and FOM
BFL_costs_pathway_B3 <- data.table(Hydropower)
BFL_costs_pathway_B3$CAPEX_lower <- Hydropower[, "Increase_hydro_MW"] * direct_costs$Hydropower$Upfront[1]
BFL_costs_pathway_B3$CAPEX_upper <- Hydropower[, "Increase_hydro_MW"] * direct_costs$Hydropower$Upfront[2]
BFL_costs_pathway_B3$FOM_lower <- Hydropower[, "QC_import_cap_MW"] * direct_costs$Hydropower$Fixed_OM[1]/hydro_CF
BFL_costs_pathway_B3$FOM_upper <- Hydropower[, "QC_import_cap_MW"] * direct_costs$Hydropower$Fixed_OM[2]/hydro_CF
BFL_costs_pathway_B3$VOM_lower <- Hydropower[, "Min_imports_QC_MWh"] * VOM_mean # Multiply delivered MWh by the VOM rate because the rate is per MWh.
BFL_costs_pathway_B3$VOM_upper <- Hydropower[, "Max_imports_QC_MWh"] * VOM_mean # Use the same MWh basis for the upper coefficient case.

# Retain both coefficient cases within each simulation so cost bounds do not mix independent runs.
npv_results <- rbindlist(lapply(c("Lower","Upper"),function(case) {
 suffix <- tolower(case)
 z <- BFL_costs_pathway_B3[, .(
  NPV_CAPEX=calculate_npv(.SD,discount_rate,base_year,paste0("CAPEX_",suffix)),
  NPV_FOM=calculate_npv(.SD,discount_rate,base_year,paste0("FOM_",suffix)),
  NPV_VOM=calculate_npv(.SD,discount_rate,base_year,paste0("VOM_",suffix))
 ),by=.(Simulation,Pathway)]
 z[,Cost_Type:=case];z
}))
stopifnot(!anyDuplicated(npv_results,by=c("Simulation","Pathway","Cost_Type")))

# Save combined NPV results to a single CSV file
write.csv(npv_results, file = file.path(output_path, "CAPEX_FOM_CAN_Hydro.csv"), row.names = FALSE)
npv_results_CAPEX_FOM <- npv_results

# -------------------------------
# CH4 Emissions
#Emissions using Delwiche et al,
# -------------------------------
# 1. Setup and Data Preparation
# -------------------------------
# Define the emission factors (kg CH4-C per MWh imported) from Delwiche et al. (2022)
min_emission_factor <- 0.16  # Lower bound
max_emission_factor <- 1.22  # Upper bound

# Use delivered MWh directly because multiplying by hours again would double-count energy.
# No capacity-factor rescaling; multiply MWh by kg CH4-C/MWh.
Yearly_imports[, Generation_MWh_Mean := Mean_imports_QC_MWh]
Yearly_imports[, Generation_MWh_Max  := Max_imports_QC_MWh]
Yearly_imports[, Generation_MWh_Min  := Min_imports_QC_MWh]

# -------------------------------
# 2. Calculate Annual CH4-C and CH4 Emissions
# -------------------------------
# Scale the emission factors by the imported generation (in TWh)

# Annual CH4-C emissions (in kg) for the lower and upper bounds:
Yearly_imports[, Annual_CH4_C_emissions_kg_Lower := Generation_MWh_Min * min_emission_factor]
Yearly_imports[, Annual_CH4_C_emissions_kg_Upper := Generation_MWh_Max * max_emission_factor]

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
# Set the projection period to match Yearly_imports data (2025 to 2050)
start_year <- 2025
end_year   <- 2050

# Define cost estimates for CH4 in 2024 and 2050 (adjusted to 2024 dollars)
# (Note: If needed, these base years could be aligned with the projection period.)

cpi_data <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"), observation_end = as.Date("2024-01-01"))

# Extracting CPI values for specific years
cpi_2024 <- filter(cpi_data, year(date) == 2024) %>% summarise(YearlyAvg = mean(value))
cpi_2020 <- filter(cpi_data, year(date) == 2020) %>% summarise(YearlyAvg = mean(value))

conversion_rate_2020_24 <- cpi_2024$YearlyAvg / cpi_2020$YearlyAvg
# 2023 report 2%
#costs_2025 <- list(CO2 = 212 * conversion_rate_2020_24, CH4 = 2025 * conversion_rate_2020_24, N2O = 60267 * conversion_rate_2020_24)
#costs_2050 <- list(CO2 = 308 * conversion_rate_2020_24, CH4 = 4231 * conversion_rate_2020_24, N2O = 92996 * conversion_rate_2020_24)

# 2023 report 2.5%
costs_2025 <- list(CO2 = 130 * conversion_rate_2020_24, CH4 = 1590 * conversion_rate_2020_24, N2O = 39972 * conversion_rate_2020_24)
costs_2050 <- list(CO2 = 205 * conversion_rate_2020_24, CH4 = 3547 * conversion_rate_2020_24, N2O = 65635 * conversion_rate_2020_24)

# 2023 report 1.5%
#costs_2025 <- list(CO2 = 360 * conversion_rate_2020_24, CH4 = 2737 * conversion_rate_2020_24, N2O = 95210 * conversion_rate_2020_24)
#costs_2050 <- list(CO2 = 482 * conversion_rate_2020_24, CH4 = 5260 * conversion_rate_2020_24, N2O = 136799 * conversion_rate_2020_24)


# Create a cost projection table using an interpolation function
GHG_Costs <- data.table(Year = start_year:end_year)
GHG_Costs[, CH4_cost := interpolate_cost(Year, start_year, end_year, costs_2025$CH4, costs_2050$CH4)]

# -------------------------------
# 4. Merge Emission Estimates with Cost Projections
# -------------------------------
# Select only the years available in Yearly_imports
emissions <- Yearly_imports[Year %in% (start_year:end_year),
                            .(Simulation,Pathway,Year, Annual_CH4_emissions_tonnes_Lower, Annual_CH4_emissions_tonnes_Upper)]

# Merge the emissions estimates with the cost projections
BFL_costs_pathway_B3_CH4_Lower <- merge(emissions[, .(Simulation,Pathway,Year, Annual_CH4_emissions_tonnes_Lower)],
                                        GHG_Costs,
                                        by = "Year")
BFL_costs_pathway_B3_CH4_Lower[, total_CH4_USD_Lower := CH4_cost * Annual_CH4_emissions_tonnes_Lower]

BFL_costs_pathway_B3_CH4_Upper <- merge(emissions[, .(Simulation,Pathway,Year, Annual_CH4_emissions_tonnes_Upper)],
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
CH4_npv_lower <- BFL_costs_pathway_B3_CH4_Lower[, .(NPV_CH4_Lower = calculate_npv(.SD, discount_rate, base_year, "total_CH4_USD_Lower")),by=.(Simulation,Pathway)]
CH4_npv_upper <- BFL_costs_pathway_B3_CH4_Upper[, .(NPV_CH4_Upper = calculate_npv(.SD, discount_rate, base_year, "total_CH4_USD_Upper")),by=.(Simulation,Pathway)]

# Combine the lower and upper NPV estimates into a single data table
# Join bounds by simulation and pathway because row order is not a reliable identifier.
CH4_npv <- merge(CH4_npv_lower, CH4_npv_upper, by=c("Simulation","Pathway"),all=TRUE)
stopifnot(!anyDuplicated(CH4_npv,by=c("Simulation","Pathway")),!anyNA(CH4_npv))

# -------------------------------
# 6. Save the Results to CSV
# -------------------------------
write.csv(CH4_npv, file = file.path(output_path, "CH4_CAN_Hydro.csv"), row.names = FALSE)

