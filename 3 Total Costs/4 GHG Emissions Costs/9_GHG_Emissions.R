# Load libraries
library(data.table)
library(readxl)
library(sf)
library(fredr)
library(dplyr)
library(lubridate)


discount_rate <- 0.03
base_year <- 2024


### All emissions data is presented in units of metric tons of carbon dioxide equivalent using GWP's from IPCC's AR4
# Load Results - Facility level gen (old facilities)
#-- Stepwise
file_path_1 <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Facility_Level_Results.csv"
file_path_2 <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"

Yearly_Facility_Level_Results <- as.data.table(fread(file_path_1))
Yearly_Results <- as.data.table(fread(file_path_2))

# Load facilities data
# Old/existing fossil fuels
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
GHG_Fuels_NPC <- fread(file_path)
# New fossil fuels 
file_path <-"/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv"
GHG_Fuels_NPC_new <- fread(file_path)
GHG_Fuels_NPC_new <- GHG_Fuels_NPC_new[1,]

# NPV Calculator
calculate_npv <- function(dt, rate, base_year, col) {
  npv <- sum(dt[[col]] / (1 + rate)^(dt[["Year"]] - base_year))
  return(npv)
}


# API KEY 63522eae4ec927d6f1d9d86bf7826cc8
fredr_set_key("63522eae4ec927d6f1d9d86bf7826cc8") 
cpi_data <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"), observation_end = as.Date("2024-01-01"))

# Interpolation function for costs
interpolate_cost <- function(year, start_year, end_year, start_cost, end_cost) {
  return(start_cost + (end_cost - start_cost) * (year - start_year) / (end_year - start_year))
}

# Define the starting and ending costs for 2021 and 2050 - Source; Total Costs NY Paper
# Extracting CPI values for specific years
cpi_2024 <- filter(cpi_data, year(date) == 2024) %>% summarise(YearlyAvg = mean(value))
cpi_2019 <- filter(cpi_data, year(date) == 2019) %>% summarise(YearlyAvg = mean(value))

# Calculating conversion rate
conversion_rate_2019_24 <- cpi_2024$YearlyAvg / cpi_2019$YearlyAvg
# 2019 -> 2024
start_year <- 2021
end_year <- 2050
costs_2021 <- list(CO2 = 121 * conversion_rate_2019_24, CH4 = 2732 * conversion_rate_2019_24, N2O = 41956 * conversion_rate_2019_24)
costs_2050 <- list(CO2 = 167 * conversion_rate_2019_24, CH4 = 4684 * conversion_rate_2019_24, N2O = 64399 * conversion_rate_2019_24)

# Interpolate costs for each year
GHG_Costs <- data.table(Year = start_year:end_year)
GHG_Costs[, `:=`(
  CO2_cost = interpolate_cost(Year, start_year, end_year, costs_2021$CO2, costs_2050$CO2),
  CH4_cost = interpolate_cost(Year, start_year, end_year, costs_2021$CH4, costs_2050$CH4),
  N2O_cost = interpolate_cost(Year, start_year, end_year, costs_2021$N2O, costs_2050$N2O)
)]

# Other GHG emissions ratios from EPA Greenhouse Gas Reporting Program for 2011-2022
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EPA GHGRP/emissions_by_unit_and_fuel_type_c_d_aa_09_2023.xlsx"
sheet_name <- "UNIT_DATA"
EPA_GHG_2022 <- read_excel(file_path, sheet = sheet_name, skip = 6)
setDT(EPA_GHG_2022)

new_england_states <- c("CT", "ME", "MA", "NH", "RI", "VT")

EPA_GHG_2022 <- EPA_GHG_2022[
  `Industry Type (sectors)` == "Power Plants" & 
    State %in% new_england_states & 
    `Unit Type` == "Electricity Generator" &
    `Reporting Year` == 2022
]

EPA_GHG_2022[, `:=`(
  CH4_CO2_ratio_non_biogenic = `Unit Methane (CH4) emissions` / `Unit CO2 emissions (non-biogenic)`,
  N2O_CO2_ratio_non_biogenic = `Unit Nitrous Oxide (N2O) emissions` / `Unit CO2 emissions (non-biogenic)`,
  CH4_CO2_ratio_biogenic = ifelse(`Unit Biogenic CO2 emissions (metric tons)` > 0,
                                  `Unit Methane (CH4) emissions` / `Unit Biogenic CO2 emissions (metric tons)`, NA),
  N2O_CO2_ratio_biogenic = ifelse(`Unit Biogenic CO2 emissions (metric tons)` > 0,
                                  `Unit Nitrous Oxide (N2O) emissions` / `Unit Biogenic CO2 emissions (metric tons)`, NA)
)]

# Merge NPC with GHG ratios
selected_cols <- EPA_GHG_2022[, .(Facility_Name = `Facility Name`, Unit_ID = `Unit Name`, CH4_CO2_ratio_non_biogenic, N2O_CO2_ratio_non_biogenic)]
GHG_Fuels_NPC <- selected_cols[GHG_Fuels_NPC, on = .(Facility_Name, Unit_ID), allow.cartesian = TRUE]

# Add in fuel category
GHG_Fuels_NPC[, Fuel_Category := fcase(
  grepl("oil", Primary_Fuel_Type, ignore.case = TRUE), "Oil",
  grepl("coal", Primary_Fuel_Type, ignore.case = TRUE), "Coal",
  grepl("wood", Primary_Fuel_Type, ignore.case = TRUE), "Wood",
  grepl("gas", Primary_Fuel_Type, ignore.case = TRUE) & grepl("Combustion Turbine", Unit_Type, ignore.case = TRUE), "Gas_CT",
  grepl("gas", Primary_Fuel_Type, ignore.case = TRUE), "Gas_CC",
  default = "Other"
)]

mean_ratios <- GHG_Fuels_NPC[, .(
  mean_CH4_CO2_ratio_non_biogenic = mean(CH4_CO2_ratio_non_biogenic, na.rm = TRUE),
  mean_N2O_CO2_ratio_non_biogenic = mean(N2O_CO2_ratio_non_biogenic, na.rm = TRUE)
), by = Fuel_Category]

# Replace NaN means with zero
mean_ratios[is.nan(mean_CH4_CO2_ratio_non_biogenic), mean_CH4_CO2_ratio_non_biogenic := 0]
mean_ratios[is.nan(mean_N2O_CO2_ratio_non_biogenic), mean_N2O_CO2_ratio_non_biogenic := 0]

# Merge the mean ratios back to the main data
GHG_Fuels_NPC <- merge(GHG_Fuels_NPC, mean_ratios, by = "Fuel_Category", all.x = TRUE)

# Replace NA values with the mean ratios
GHG_Fuels_NPC[is.na(CH4_CO2_ratio_non_biogenic), CH4_CO2_ratio_non_biogenic := mean_CH4_CO2_ratio_non_biogenic]
GHG_Fuels_NPC[is.na(N2O_CO2_ratio_non_biogenic), N2O_CO2_ratio_non_biogenic := mean_N2O_CO2_ratio_non_biogenic]

# Drop the mean ratio columns as they are no longer needed
GHG_Fuels_NPC[, c("mean_CH4_CO2_ratio_non_biogenic", "mean_N2O_CO2_ratio_non_biogenic") := NULL]

# Merge Facilities with yearly results
selected_cols <- GHG_Fuels_NPC[, .(Facility_Unit.ID, Unit_Type, Fuel_Category, CH4_CO2_ratio_non_biogenic, N2O_CO2_ratio_non_biogenic, mean_CO2_tons_MW, mean_CO2_tons_MW_estimate)]
Yearly_Facility_Level_Results <- selected_cols[Yearly_Facility_Level_Results, on = "Facility_Unit.ID"]

# Calculate CO2 emissions
Yearly_Facility_Level_Results$total_CO2_tons <- Yearly_Facility_Level_Results$total_generation_GWh * 1e3 * Yearly_Facility_Level_Results$mean_CO2_tons_MW
Yearly_Facility_Level_Results$total_CO2_tons <- ifelse(
  is.na(Yearly_Facility_Level_Results$total_CO2_tons),
  Yearly_Facility_Level_Results$total_generation_GWh * 1e3 *
    Yearly_Facility_Level_Results$mean_CO2_tons_MW_estimate,
  Yearly_Facility_Level_Results$total_CO2_tons
)

# Calculate CH4 and N2O emissions
Yearly_Facility_Level_Results[, total_CH4_tons_eq := CH4_CO2_ratio_non_biogenic * total_CO2_tons]
Yearly_Facility_Level_Results[, total_N2O_tons_eq := N2O_CO2_ratio_non_biogenic * total_CO2_tons]

# Summarize based on Fuel Type, sim, year and Pathway
Yearly_Facility_Level_Results <- Yearly_Facility_Level_Results[, .(
  total_CO2_tons = sum(total_CO2_tons, na.rm = TRUE),
  total_CH4_tons_eq = sum(total_CH4_tons_eq, na.rm = TRUE),
  total_N2O_tons_eq = sum(total_N2O_tons_eq, na.rm = TRUE)
), by = .(Year, Simulation, Pathway)]

# Total GHG CO2 equivalaent
Yearly_Facility_Level_Results <- Yearly_Facility_Level_Results[, .(
  total_GHG_CO2_eq = total_CO2_tons + total_CH4_tons_eq + total_N2O_tons_eq
), by = .(Year, Simulation, Pathway)]

# Calculate the total cost of CO2 equivalent
#Yearly_Facility_Level_Results[, total_cost_CO2_eq := total_CO2_tons * CO2_cost + total_CH4_tons_eq * CH4_cost + total_N2O_tons_eq * N2O_cost]
Yearly_Facility_Level_Results <- merge(Yearly_Facility_Level_Results, GHG_Costs, by = "Year", all.x = TRUE)
Yearly_Facility_Level_Results[, total_cost_CO2_eq := total_GHG_CO2_eq * CO2_cost]

# Define a function to process GHG Costs
process_GHG <- function(sim, scen, data) {
  filtered_data <- data[Simulation == sim & Pathway == scen]
  npv <- calculate_npv(filtered_data, discount_rate, base_year, "total_cost_CO2_eq")
  return(npv)
}

# NPV Calculation
simulations <- unique(Yearly_Facility_Level_Results$Simulation)
Pathways <- unique(Yearly_Facility_Level_Results$Pathway)
npv_results <- list()

for (sim in simulations) {
  for (scen in Pathways) {
    npv <- process_GHG(sim, scen, Yearly_Facility_Level_Results)
    npv_results[[paste0(sim, "_", scen, "_Total_GHG_NPV")]] <- npv
  }
}

# Combine NPV results into a single data.table
combined_npvs <- rbindlist(lapply(names(npv_results), function(name) {
  parts <- strsplit(name, "_")[[1]]
  data.table(Simulation = parts[1], Pathway = parts[2], NPV = npv_results[[name]])
}), fill = TRUE)

combined_npvs <- combined_npvs[NPV != 0,]
combined_npvs <- combined_npvs %>%
  group_by(Pathway) %>%
  summarise(
    NPV_max = max(NPV),
    NPV_mean = mean(NPV),
    NPV_min = min(NPV)
  ) 


# New fossil fuel emissions
Yearly_Results$Fossil_new.CO2_tons <- GHG_Fuels_NPC_new$mean_CO2_tons_MW * Yearly_Results$New_Fossil_Fuel_TWh * 1e6
Yearly_Results$Fossil_new.CH4_tons_eq <- Yearly_Results$Fossil_new.CO2_tons * mean_ratios$mean_CH4_CO2_ratio_non_biogenic[mean_ratios$Fuel_Category == "Gas_CC"] 
Yearly_Results$Fossil_new.N2O_tons_eq <- Yearly_Results$Fossil_new.CO2_tons * mean_ratios$mean_N2O_CO2_ratio_non_biogenic[mean_ratios$Fuel_Category == "Gas_CC"] 
Yearly_Results$Fossil_new.CO2_tons_eq <- Yearly_Results$Fossil_new.CO2_tons + Yearly_Results$Fossil_new.CH4_tons_eq + Yearly_Results$Fossil_new.N2O_tons_eq 

Yearly_Results_Summary <- Yearly_Results[, .(
  total_CO2_tons = sum(Fossil_new.CO2_tons, na.rm = TRUE),
  total_CH4_tons_eq = sum(Fossil_new.CH4_tons_eq, na.rm = TRUE),
  total_N2O_tons_eq = sum(Fossil_new.N2O_tons_eq, na.rm = TRUE),
  total_GHG_tons_CO2_eq = sum(Fossil_new.CO2_tons_eq, na.rm = TRUE)
), by = .(Year, Simulation, Pathway)]


Yearly_Results_Summary <- merge(Yearly_Results_Summary, GHG_Costs, by = "Year", all.x = TRUE)
Yearly_Results_Summary[, total_cost_CO2_eq := total_GHG_tons_CO2_eq * CO2_cost]

# Set up correct loop inputs
simulations_summary <- unique(as.character(Yearly_Results_Summary$Simulation))
pathways_summary <- unique(as.character(Yearly_Results_Summary$Pathway))

# Recalculate
npv_results_hourly <- list()

for (sim in simulations_summary) {
  for (scen in pathways_summary) {
    npv <- process_GHG(sim, scen, Yearly_Results_Summary)
    npv_results_hourly[[paste0(sim, "_", scen, "_Total_GHG_NPV")]] <- npv
  }
}


# Combine NPV results into a single data.table
combined_npvs_hourly <- rbindlist(lapply(names(npv_results_hourly), function(name) {
  parts <- strsplit(name, "_")[[1]]
  data.table(Simulation = parts[1], Pathway = parts[2], NPV_newNG = npv_results_hourly[[name]])
}), fill = TRUE)


combined_npvs_hourly <- combined_npvs_hourly[!(combined_npvs_hourly$Pathway == "D" & combined_npvs_hourly$NPV_newNG == 0), ]
combined_npvs_hourly <- combined_npvs_hourly %>%
  group_by(Pathway) %>%
  summarise(
    NPV_newNG_max = max(NPV_newNG),
    NPV_newNG_mean = mean(NPV_newNG),
    NPV_newNG_min = min(NPV_newNG)
  ) 


combined_npvs <- merge(combined_npvs, combined_npvs_hourly, by = c("Pathway"))
combined_npvs$NPV_mean <- combined_npvs$NPV_mean + combined_npvs$NPV_newNG_mean
combined_npvs$NPV_max <- combined_npvs$NPV_max + combined_npvs$NPV_newNG_max
combined_npvs$NPV_min <- combined_npvs$NPV_min + combined_npvs$NPV_newNG_min
combined_npvs <- as.data.table(combined_npvs)
combined_npvs[, NPV_newNG_mean := NULL ]
combined_npvs[, NPV_newNG_max := NULL ]
combined_npvs[, NPV_newNG_min := NULL ]

# Save combined NPV results to a single CSV file
write.csv(combined_npvs, file = file.path(output_path, "GHG_Emissions.csv"), row.names = FALSE)
