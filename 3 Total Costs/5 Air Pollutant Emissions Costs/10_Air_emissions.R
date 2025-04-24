# Load libraries
library(data.table)
library(readxl)
library(tidyverse)
library(fredr)
library(sf)
library(ggplot2)

# FUNCTIONS
# NPV Calculator
calculate_npv <- function(dt, rate, base_year, col) {
  npv <- sum(dt[[col]] / (1 + rate)^(dt[["Year"]] - base_year))
  return(npv)
}

discount_rate <- 0.03
base_year <- 2024

lbs_tons_conversion<- 1 / 2204.62 # lbs to tons
ton_conversion <- 0.907185 # Conversion factor from US tons to metric tons

# Load Results
#-- Stepwise
file_path_1 <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Facility_Level_Results_County_added_in.csv"
file_path_2 <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"

Facility_Level_Results <- fread(file_path_1)

Yearly_Results <- as.data.table(fread(file_path_2))
Yearly_Results <- Yearly_Results[, .(Simulation, Year, Pathway, New_Fossil_Fuel_TWh)]

# Nameplate Capacity
# Load facilities data
# Old/existing fossil fuels
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
Facilities_Data <- fread(file_path)
# New fossil fuels 
file_path <-"/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC_new <- fread(file_path) # Note stack height is assumed 150 feet for this facility
setkey(Fossil_Fuels_NPC_new, "Unit_ID")

# API KEY 63522eae4ec927d6f1d9d86bf7826cc8
fredr_set_key("63522eae4ec927d6f1d9d86bf7826cc8") 
cpi_data <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"), observation_end = as.Date("2024-01-01"))

# Extracting CPI values for specific years
cpi_2000 <- filter(cpi_data, year(date) == 2000) %>% summarise(YearlyAvg = mean(value))
cpi_2024 <- filter(cpi_data, year(date) == 2024) %>% summarise(YearlyAvg = mean(value))

# Calculating conversion rate
conversion_rate <- cpi_2024$YearlyAvg / cpi_2000$YearlyAvg

# NE FIPS
# Define New England FIPS codes
ne_fips <- c("90", "23", "25", "33", "44", "50")

# Load air emissions data
path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/AP3 Model/air_emissions_below250.csv"
air_emissions_below250 <- read.csv(path, check.names = TRUE)
air_emissions_below250$fips <- as.character(air_emissions_below250$fips)
air_emissions_below250 <- air_emissions_below250 %>%
  filter(substr(fips, 1, 2) %in% ne_fips)

air_emissions_below250 <- air_emissions_below250 %>%
  select(-County) %>%
  mutate(across(NH3:PM10, ~ (.x * conversion_rate / ton_conversion)))

path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/AP3 Model/air_emissions_above500.csv"
air_emissions_above500 <- read.csv(path, check.names = TRUE)
air_emissions_above500$fips <- as.character(air_emissions_above500$fips)
air_emissions_above500 <- air_emissions_above500 %>%
  filter(substr(fips, 1, 2) %in% ne_fips)
air_emissions_above500 <- air_emissions_above500 %>%
  mutate(across(NH3:PM10, ~ (.x * conversion_rate / ton_conversion)))

path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/AP3 Model/air_emissions_above250_below500.csv"
air_emissions_between250_500 <- read.csv(path, check.names = TRUE)
air_emissions_between250_500$fips <- as.character(air_emissions_between250_500$fips)
air_emissions_between250_500 <- air_emissions_between250_500 %>%
  filter(substr(fips, 1, 2) %in% ne_fips)
air_emissions_between250_500 <- air_emissions_between250_500 %>%
  select(-County) %>%
  mutate(across(NH3:PM10, ~ (.x * conversion_rate / ton_conversion)))


# Stack heights
# Load stack heights
new_england_states <- c("CT", "ME", "MA", "NH", "RI", "VT")
## Integrating stack height using EIA 860 
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EIA 860/eia8602023/6_2_EnviroEquip_Y2023.xlsx"
sheet_name <- "Stack Flue"
# Read the Excel file with column names in the second row
eia_860_2023_stack <- read_excel(file_path, sheet = sheet_name, skip = 1)
eia_860_2023_stack<- eia_860_2023_stack[eia_860_2023_stack$State %in% new_england_states, ]
setDT(eia_860_2023_stack)

# Perform the left join using EIA 860 
Facilities_Data_updated <- eia_860_2023_stack[Facilities_Data, 
                                              on = c("Plant Code" = "Facility_ID"), 
                                              .(Facility_ID = Facility_ID, Unit_ID = Unit_ID, Stack_Height_ft = `Stack Height (Feet)`), 
                                              nomatch = 0L]

Facilities_Data_updated <- Facilities_Data_updated[, .SD[1], by = Facility_ID]

Facilities_Data <- merge(Facilities_Data, Facilities_Data_updated, by = "Facility_ID", all.x = TRUE)

# We manually match facilities with certain heights and tabulate
# matches from stack heights dataset
manual_matches <- data.frame(
  Stack_Height_ft = c(210, 320, 320, 320, 425, 500, 305, 305, 305, 305, 498, 498, 498, 500, 223, 187, 187, 445, 445, 226, 227, 226, 269, 362, 340, 340, 249, 390, 185, 185, 255, 255, 150, 150, 150, 150, 266, 266, 498, 215, 215, 298, 251, 498),
  Facility_Unit.ID = c("10726_1", "1507_1", "1507_2", "1507_3", "1507_4", "1588_7", "1588_81", "1588_82", "1588_93", "1588_94", "1599_1", "1599_2", "1599_3", "1626_4", "1642_3", "1682_8", "1682_9", "2364_1", "2364_2", "2367_4", "2367_5", "2367_6", "50243_6", "50243_7", "544_7", "544_8", "546_5", "546_6", "55107_1", "55107_2", "55317_11", "55317_12", "55661_1", "55661_2", "56047_1", "56047_2", "562_2", "562_3", "562_4", "56798_1", "56798_2", "568_501", "568_2", "568_3")
)
# Adding above 500 ft matches
additional_matches <- data.frame(
  Stack_Height_ft = rep(600, 23),
  Facility_Unit.ID = c(
    "1507_1", "1507_2", "1507_3", "1507_4", 
    "1599_1", "1599_2", "1599_3", 
    "1619_1", "1619_2", "1619_3", "1619_4", 
    "1626_1", "1626_2", "1626_3", "1626_4", 
    "1588_7", "1588_81", "1588_82", "1588_93", "1588_94", "1588_MJ-1",
    "55661_1", "55661_2"),
  NH3 = c(422, 422, 422, 422, 1192, 1192, 1192, 2970, 2970, 2970, 2970, 2360, 2360, 2360, 2360, 3256, 3256, 3256, 3256, 3256, 3256, 790, 790),
  PM2.5 = c(467, 467, 467, 467, 1153, 1153, 1153, 2100, 2100, 2100, 2100, 1791, 1791, 1791, 1791, 2345, 2345, 2345, 2345, 2345, 2345, 725, 725),
  NOx = c(94, 94, 94, 94, 103, 103, 103, 114, 114, 114, 114, 109, 109, 109, 109, 112, 112, 112, 112, 112, 112, 98, 98),
  SO2 = c(406, 406, 406, 406, 738, 738, 738, 972, 972, 972, 972, 960, 960, 960, 960, 1142, 1142, 1142, 1142, 1142, 1142, 484, 484),
  VOC = c(115, 115, 115, 115, 190, 190, 190, 294, 294, 294, 294, 260, 260, 260, 260, 321, 321, 321, 321, 321, 321, 144, 144),
  PM10 = c(71, 71, 71, 71, 171, 171, 171, 317, 317, 317, 317, 274, 274, 274, 274, 362, 362, 362, 362, 362, 362, 109, 109)
)

# Combining manual matches and additional matches, giving priority to additional matches
combined_matches <- bind_rows(additional_matches, manual_matches) %>%
  distinct(Facility_Unit.ID, .keep_all = TRUE)

# Updating Facilities_Data with the combined matches
Facilities_Data <- Facilities_Data %>%
  left_join(combined_matches, by = c("Facility_Unit.ID" = "Facility_Unit.ID")) %>%
  mutate(Stack_Height_ft = coalesce(Stack_Height_ft.x, Stack_Height_ft.y)) %>%
  select(-Stack_Height_ft.x, -Stack_Height_ft.y)

# Add stack height to results
selected_facilities_data <- Facilities_Data[, .(Facility_Unit.ID, Stack_Height_ft)]
selected_facilities_data[is.na(Stack_Height_ft), Stack_Height_ft := 120]
Facility_Level_Results <- selected_facilities_data[Facility_Level_Results, on = .(Facility_Unit.ID)]

# Cost benefit calculations
# Social costs
# merging air_emissions and facility level results
# Convert `GEOID` in `Facility_Level_Results` to integer
Facility_Level_Results$GEOID <- as.integer(Facility_Level_Results$GEOID)
air_emissions_below250$fips <- as.integer(air_emissions_below250$fips)
air_emissions_between250_500$fips <- as.integer(air_emissions_between250_500$fips)

# Filter rows with stack height below 250
# Filtering the data to include facilities with stack heights below 250 feet and retaining NA values
facilities_below_250 <- Facility_Level_Results[is.na(Facility_Level_Results$Stack_Height_ft) | Facility_Level_Results$Stack_Height_ft < 250, ]
facilities_between_250_500 <- Facility_Level_Results[Stack_Height_ft >= 250 & Stack_Height_ft <= 500]
facilities_above_500 <- Facility_Level_Results[Stack_Height_ft > 500]

# Merge the filtered facilities with air_emissions_below250 based on fips and GEOID
facilities_below_250 <- left_join(facilities_below_250, air_emissions_below250, by = c("GEOID" = "fips"))
facilities_between_250_500 <- left_join(facilities_between_250_500, air_emissions_between250_500, by = c("GEOID" = "fips"))
facilities_above_500 <- left_join(facilities_above_500, additional_matches, by = "Facility_Unit.ID")

create_empty_dt <- function(template) {
  empty_dt <- template[0]
  return(empty_dt)
}

# Ensure each data.table is not empty by creating an empty one with the same structure if needed
if (nrow(facilities_below_250) == 0) {
  facilities_below_250 <- create_empty_dt(Facility_Level_Results)
}
if (nrow(facilities_between_250_500) == 0) {
  facilities_between_250_500 <- create_empty_dt(Facility_Level_Results)
}
if (nrow(facilities_above_500) == 0) {
  facilities_above_500 <- create_empty_dt(Facility_Level_Results)
}

# Now the results contain emission rates except CO
Facility_Level_Results <- rbindlist(list(facilities_below_250, facilities_between_250_500, facilities_above_500), use.names = TRUE, fill = TRUE)

# Check datasets
# Before merge
print(summary(facilities_below_250$NOx))
print(summary(facilities_between_250_500$NOx))
print(summary(facilities_above_500$NOx))

# After merge
print(summary(Facility_Level_Results$NOx))

# add in CO costs 
# Define the CO cost range (2019-USD) from Total Costs paper
co_cost_min_2019 <- 2
co_cost_max_2019 <- 1982

# Extracting CPI values for specific years
cpi_2019 <- filter(cpi_data, year(date) == 2019) %>% summarise(YearlyAvg = mean(value))

# Calculating conversion rate
conversion_rate <- cpi_2024$YearlyAvg / cpi_2019$YearlyAvg

# Adjust CO cost range to 2024 USD using the conversion rate
co_cost_min_2024 <- co_cost_min_2019 * conversion_rate
co_cost_max_2024 <- co_cost_max_2019 * conversion_rate

# Calculate the relative SO2 emissions across all facilities
Facility_Level_Results[, SO2_ratio := SO2 / max(SO2, na.rm = TRUE)]

# Assign CO cost based on SO2 ratio
Facility_Level_Results[, CO := co_cost_min_2024 + (co_cost_max_2024 - co_cost_min_2024) * SO2_ratio]

## Load emission ratio data
# Other Air_Pollutant emissions ratios from EPA Greenhouse Gas Reporting Program for 2011-2022
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EIA 923/f923_2023/EIA923_Schedule_8_Annual_Envir_Infor_2023_Final.xlsx"
sheet_name <- "8C Air Emissions Control Info"
EPA_f923_2023 <- read_excel(file_path, sheet = sheet_name, skip = 4)
setDT(EPA_f923_2023)
# ALL US Facilities data
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EPA CAMPD/States Historical and Simulated Data/USA/Facilities_Data_USA.csv"
Fossil_Fuels_NPC_USA <- fread(file_path)

# Merge NPC with Air_Pollutant ratios
selected_cols <- EPA_f923_2023[, .(Facility_ID = `Plant ID`, PM_lbs_mmBTU = `PM Emissions Rate \r\n(lbs/MMBtu)`, PM_eff = `PM Removal Efficiency Rate\r\n at Annual Operating Factor`)]
selected_cols$PM_lbs_mmBTU <- as.numeric(selected_cols$PM_lbs_mmBTU)
selected_cols$PM_eff <- as.numeric(selected_cols$PM_eff)
selected_cols <- selected_cols[!is.na(selected_cols$PM_lbs_mmBTU) & !is.na(selected_cols$PM_eff)]
selected_cols <- selected_cols[, .(Facility_ID, PM_lbs_mmBTU = PM_lbs_mmBTU * PM_eff)]
selected_cols <- selected_cols[, .(PM_lbs_mmBTU = mean(PM_lbs_mmBTU, na.rm = TRUE)), by = Facility_ID]

Air_Pollutant_Fuels_NPC_USA <- selected_cols[Fossil_Fuels_NPC_USA, on = .(Facility_ID), allow.cartesian = TRUE]
Air_Pollutant_Fuels_NPC_USA[, Fuel_Category := fcase(
  grepl("oil", Primary_Fuel_Type, ignore.case = TRUE), "Oil",
  grepl("coal", Primary_Fuel_Type, ignore.case = TRUE), "Coal",
  grepl("wood", Primary_Fuel_Type, ignore.case = TRUE), "Wood",
  grepl("gas", Primary_Fuel_Type, ignore.case = TRUE) & grepl("Combustion Turbine", Unit_Type, ignore.case = TRUE), "Gas_CT",
  grepl("gas", Primary_Fuel_Type, ignore.case = TRUE), "Gas_CC",
  default = "Other"
)]
mean_ratios_USA <- Air_Pollutant_Fuels_NPC_USA[, .(
  mean_PM_lbs_mmBTU = mean(PM_lbs_mmBTU, na.rm = TRUE)
), by = Fuel_Category]

# filter for NE
Facility_ID_NE <- unique(Facilities_Data$Facility_ID)
EPA_f923_2023 <- EPA_f923_2023[`Plant ID` %in% Facility_ID_NE]

# Add in fuel category
Facilities_Data[, Fuel_Category := fcase(
  grepl("oil", Primary_Fuel_Type, ignore.case = TRUE), "Oil",
  grepl("coal", Primary_Fuel_Type, ignore.case = TRUE), "Coal",
  grepl("wood", Primary_Fuel_Type, ignore.case = TRUE), "Wood",
  grepl("gas", Primary_Fuel_Type, ignore.case = TRUE) & grepl("Combustion Turbine", Unit_Type, ignore.case = TRUE), "Gas_CT",
  grepl("gas", Primary_Fuel_Type, ignore.case = TRUE), "Gas_CC",
  default = "Other"
)]

Air_Pollutant_Fuels_NPC <- selected_cols[Facilities_Data, on = .(Facility_ID), allow.cartesian = TRUE]

mean_ratios <- Air_Pollutant_Fuels_NPC[, .(
  mean_PM_lbs_mmBTU = mean(PM_lbs_mmBTU, na.rm = TRUE)
), by = Fuel_Category]

# Merge the mean ratios back to the main data
Air_Pollutant_Fuels_NPC <- merge(Air_Pollutant_Fuels_NPC, mean_ratios, by = "Fuel_Category", all.x = TRUE)

# Replace NA values with the mean ratios
Air_Pollutant_Fuels_NPC[is.na(PM_lbs_mmBTU), PM_lbs_mmBTU := mean_PM_lbs_mmBTU]

# For coal it is still NaN we use the same value as oil
Air_Pollutant_Fuels_NPC[is.na(PM_lbs_mmBTU), PM_lbs_mmBTU := mean_ratios_USA$mean_PM_lbs_mmBTU[1]] # add in Coal

# Drop the mean ratio columns as they are no longer needed
Air_Pollutant_Fuels_NPC[, c("mean_PM_lbs_mmBTU") := NULL]

# Merge Facilities with yearly results
selected_cols <- Air_Pollutant_Fuels_NPC[, .(Facility_Unit.ID, Facility_ID, Unit_ID.x, Primary_Fuel_Type, Facility_Name,PM_lbs_mmBTU, mean_NOx_lbs_MW, 
                                             mean_SO2_lbs_MW, mean_HI_mmBtu_per_MW = mean_Heat_Input_mmBtu/Estimated_NameplateCapacity_MW, mean_NOx_lbs_MW_estimate, 
                                             mean_SO2_lbs_MW_estimate, mean_HI_mmBtu_per_MW_estimate = mean_HI_mmBtu_per_MW)]
Facility_Level_Results <- selected_cols[Facility_Level_Results, on = "Facility_Unit.ID"]

Facility_Level_Results_unique <- unique(Facility_Level_Results, by = "Facility_Unit.ID")
fwrite(Facility_Level_Results_unique, file = pasteoutput_path)
fwrite(Facility_Level_Results_unique, file = file.path(output_path, "Facility_Level_airpollutant_costs.csv"), row.names = FALSE)

# Calculate NOx
Facility_Level_Results$total_NOx_lbs <- Facility_Level_Results$total_generation_GWh * 1e3 * Facility_Level_Results$mean_SO2_lbs_MW
Facility_Level_Results$total_NOx_lbs <- ifelse(
  is.na(Facility_Level_Results$total_NOx_lbs),
  Facility_Level_Results$total_generation_GWh * 1e3 *
    Facility_Level_Results$mean_NOx_lbs_MW_estimate,
  Facility_Level_Results$total_NOx_lbs
)

# Calculate SO2
Facility_Level_Results$total_SO2_lbs <- Facility_Level_Results$total_generation_GWh * 1e3 * Facility_Level_Results$mean_SO2_lbs_MW
Facility_Level_Results$total_SO2_lbs <- ifelse(
  is.na(Facility_Level_Results$total_SO2_lbs),
  Facility_Level_Results$total_generation_GWh * 1e3 *
    Facility_Level_Results$mean_SO2_lbs_MW_estimate,
  Facility_Level_Results$total_SO2_lbs
)

# Calculate HI
Facility_Level_Results$total_HI_mmBtu <- Facility_Level_Results$total_generation_GWh * 1e3 * Facility_Level_Results$mean_HI_mmBtu_per_MW
Facility_Level_Results$total_HI_mmBtu <- ifelse(
  is.na(Facility_Level_Results$total_HI_mmBtu),
  Facility_Level_Results$total_generation_GWh * 1e3 *
    Facility_Level_Results$mean_HI_mmBtu_per_MW_estimate,
  Facility_Level_Results$total_HI_mmBtu
)

# Calculate PM.total
Facility_Level_Results[, PM.total_tons := total_HI_mmBtu * PM_lbs_mmBTU * lbs_tons_conversion]

# Calculate PM2.5 and PM10 using total costs NY Paper
Facility_Level_Results[, PM2.5_tons := fcase(
  grepl("oil", Fuel_type_1, ignore.case = TRUE), PM.total_tons * 69/100,
  grepl("coal", Fuel_type_1, ignore.case = TRUE), PM.total_tons * 32.5/100,
  grepl("wood", Fuel_type_1, ignore.case = TRUE), PM.total_tons,
  grepl("gas", Fuel_type_1, ignore.case = TRUE), PM.total_tons
)]

Facility_Level_Results[, PM10_tons := fcase(
  grepl("oil", Fuel_type_1, ignore.case = TRUE), PM.total_tons - PM2.5_tons,
  grepl("coal", Fuel_type_1, ignore.case = TRUE), PM.total_tons - PM2.5_tons,
  grepl("wood", Fuel_type_1, ignore.case = TRUE), PM.total_tons - PM2.5_tons,
  grepl("gas", Fuel_type_1, ignore.case = TRUE), PM.total_tons - PM2.5_tons
)]

# NOx and SOx lbs to tons
Facility_Level_Results[, total_NOx_tons := total_NOx_lbs * lbs_tons_conversion]
Facility_Level_Results[, total_SO2_tons := total_SO2_lbs * lbs_tons_conversion]

# CO calculation
# Data from EPA AP 42 
# Calculate total CO emissions using conditional logic with fcase
Facility_Level_Results[, CO_tons := fcase(
  grepl("oil", Fuel_type_1, ignore.case = TRUE), total_HI_mmBtu * (5/150) * lbs_tons_conversion,  # Oil (Distillate), 150 MMBtu/103gal
  grepl("coal", Fuel_type_1, ignore.case = TRUE), total_HI_mmBtu * (0.5/26) * lbs_tons_conversion,     # Coal lbs/tons/26(mmBtu/tons) as per AP42
  grepl("wood", Fuel_type_1, ignore.case = TRUE), total_HI_mmBtu * 0.6 * lbs_tons_conversion,     # Wood, directly from AP42 table
  grepl("gas", Fuel_type_1, ignore.case = TRUE), total_HI_mmBtu * 3.0e-02 * lbs_tons_conversion  # Natural Gas, directly from AP42 table
)]

# Calculate total VOC emissions using conditional logic with fcase
Facility_Level_Results[, VOC_tons := fcase(
  grepl("oil", Fuel_type_1, ignore.case = TRUE), PM.total_tons * 0.35,  # Oil (Distillate) Organic matter is 35% of total PM
  grepl("coal", Fuel_type_1, ignore.case = TRUE), PM.total_tons * 0.2,  # Coal Organic matter is 20% of total PM
  grepl("wood", Fuel_type_1, ignore.case = TRUE), total_HI_mmBtu * 0.017 * lbs_tons_conversion,    # Wood, directly from AP42 table
  grepl("gas", Fuel_type_1, ignore.case = TRUE), total_HI_mmBtu * 2.1e-03 * lbs_tons_conversion  # Natural Gas, directly from AP42 table
)]

# Calculate costs
Facility_Level_Results[, total_NOx_USD := total_NOx_tons * NOx]
Facility_Level_Results[, total_SO2_USD := total_SO2_tons * SO2]
Facility_Level_Results[, total_PM2.5_USD := PM2.5_tons * PM2.5]
Facility_Level_Results[, total_PM10_USD := PM10_tons * PM10]
Facility_Level_Results[, total_VOC_USD := VOC_tons * VOC]
Facility_Level_Results[, total_CO_USD := CO_tons * CO]

# Summarize data for Sim, Scenario, year
Yearly_Facility_Level_Results <- Facility_Level_Results[, .(
  total_NOx_USD = sum(total_NOx_USD, na.rm = TRUE),
  total_SO2_USD = sum(total_SO2_USD, na.rm = TRUE),
  total_PM2.5_USD = sum(total_PM2.5_USD, na.rm = TRUE),
  total_PM10_USD = sum(total_PM10_USD, na.rm = TRUE),
  total_CO_USD = sum(total_CO_USD, na.rm = TRUE),
  total_VOC_USD = sum(total_VOC_USD, na.rm = TRUE)
), by = .(Year, Simulation, Pathway)]

# Total air pollutants
Yearly_Facility_Level_Results[, total_air_emission_USD := total_NOx_USD + total_SO2_USD + total_PM2.5_USD + 
                                total_PM10_USD + total_CO_USD + total_VOC_USD]

# Save the results
fwrite(Facility_Level_Results, file = file.path(output_path, "Facility_Level_Results_Existing.csv"), row.names = FALSE)
fwrite(Yearly_Facility_Level_Results, file = file.path(output_path, "Yearly_Facility_Level_Results_Existing.csv"), row.names = FALSE)

# Define a function to process Air_Pollutant Costs
process_Air_Pollutant <- function(sim, path, data) {
  filtered_data <- data[Simulation == sim & Pathway == path]
  npv <- calculate_npv(filtered_data, discount_rate, base_year, "total_air_emission_USD")
  return(npv)
}

# NPV Calculation
simulations <- unique(Yearly_Facility_Level_Results$Simulation)
Pathways <- unique(Yearly_Facility_Level_Results$Pathway)
npv_results <- list()

for (sim in simulations) {
  for (path in Pathways) {
    npv <- process_Air_Pollutant(sim, path, Yearly_Facility_Level_Results)
    npv_results[[paste0(sim, "_", path, "_Total_Air_Pollutant_NPV")]] <- npv
  }
}

# Combine NPV results into a single data.table
combined_npvs <- rbindlist(lapply(names(npv_results), function(name) {
  parts <- strsplit(name, "_")[[1]]
  data.table(Simulation = parts[1], Pathway = parts[2], NPV = npv_results[[name]])
}), fill = TRUE)
combined_npvs <- combined_npvs[NPV != 0]
# Save combined NPV results to a single CSV file
fwrite(combined_npvs, file = file.path(output_path, "Air_Emissions_Existing.csv"), row.names = FALSE)

# Plots
Yearly_Facility_Level_Results_long <- melt(Yearly_Facility_Level_Results, 
                                           id.vars = c("Year", "Simulation", "Pathway"), 
                                           measure.vars = c("total_NOx_USD", "total_SO2_USD", "total_PM2.5_USD", "total_PM10_USD", "total_CO_USD", "total_VOC_USD", "total_air_emission_USD"),
                                           variable.name = "Pollutant", 
                                           value.name = "Value")
setDT(Yearly_Facility_Level_Results_long)
# Convert costs to billions
Yearly_Facility_Level_Results_long[, Value := Value / 1e6]

# Plotting
ggplot(Yearly_Facility_Level_Results_long, aes(x = Year, y = Value, color = Pathway)) +
  geom_line() +
  facet_wrap(~ Pollutant, scales = "free_y") +
  labs(title = "Temporal Evolution of Emissions by Pathway",
       x = "Year",
       y = "Emission Cost (mUSD)") +
  theme_minimal()


# Avoided mortality for each Pathway
# Cost of mortality: $7.4 million in 2006-USD per avoided mortality
mortality_million_USD_2006 <- 7.4

# Extracting CPI values for specific years
cpi_2006 <- filter(cpi_data, year(date) == 2006) %>% summarise(YearlyAvg = mean(value))

# Calculating conversion rate
conversion_rate <- cpi_2024$YearlyAvg / cpi_2006$YearlyAvg

# 2024 mortality cost
mortality_million_USD_2024 <- mortality_million_USD_2006 * conversion_rate

# How many moralities 
combined_npvs$NPV_millions <- combined_npvs$NPV /1e6
combined_npvs$mortality <- combined_npvs$NPV_millions / mortality_million_USD_2024

# County level results
# Summarize data for Sim, Pathway, year and County
Yearly_County_Level_Results <- Facility_Level_Results[, .(
  total_NOx_USD = sum(total_NOx_USD, na.rm = TRUE),
  total_SO2_USD = sum(total_SO2_USD, na.rm = TRUE),
  total_PM2.5_USD = sum(total_PM2.5_USD, na.rm = TRUE),
  total_PM10_USD = sum(total_PM10_USD, na.rm = TRUE),
  total_CO_USD = sum(total_CO_USD, na.rm = TRUE),
  total_VOC_USD = sum(total_VOC_USD, na.rm = TRUE)
), by = .(Year, Simulation, Pathway, County, State)]

# Total air pollutants
Yearly_County_Level_Results[, total_air_emission_USD := total_NOx_USD + total_SO2_USD + total_PM2.5_USD + 
                                total_PM10_USD + total_CO_USD + total_VOC_USD]
# Annual Mortality
Yearly_County_Level_Results[, mortality_annual_person := total_air_emission_USD/ mortality_million_USD_2024/ 1e6]

# Summarize the data by County, State, Simulation, and Pathway, and calculate NPV
Yearly_County_Level_Results[, Year := as.numeric(as.character(Year))]

County_Level_NPV <- Yearly_County_Level_Results[, .(
  npv_NOx_USD = calculate_npv(.SD, discount_rate, base_year, "total_NOx_USD"),
  npv_SO2_USD = calculate_npv(.SD, discount_rate, base_year, "total_SO2_USD"),
  npv_PM2.5_USD = calculate_npv(.SD, discount_rate, base_year, "total_PM2.5_USD"),
  npv_PM10_USD = calculate_npv(.SD, discount_rate, base_year, "total_PM10_USD"),
  npv_CO_USD = calculate_npv(.SD, discount_rate, base_year, "total_CO_USD"),
  npv_VOC_USD = calculate_npv(.SD, discount_rate, base_year, "total_VOC_USD"),
  npv_total_air_emission_USD = calculate_npv(.SD, discount_rate, base_year, "total_air_emission_USD"), 
  mortality_person = sum(mortality_annual_person)
), by = .(County, State, Simulation, Pathway)]


# Save combined NPV results to a single CSV file
fwrite(County_Level_NPV, file = file.path(output_path, "Air_Emissions_County_Level_Existing.csv"), row.names = FALSE)

#------
## Cost of New Fossil Fuels Generation
# Annual Fossil_Fuels_NPC_new
Fossil_Fuels_NPC_new[, Facility_Unit.ID := NULL]
Fossil_Fuels_NPC_new[, Year := year(Commercial_Operation_Date)]
Fossil_Fuels_NPC_new <- Fossil_Fuels_NPC_new[!duplicated(Facility_ID)]
columns_to_multiply <- c("Estimated_NameplateCapacity_MW", "mean_Heat_Input_mmBtu")  
Fossil_Fuels_NPC_new[, (columns_to_multiply) := lapply(.SD, function(x) x * 2), .SDcols = columns_to_multiply]

# Facility details (randomly assigned location)
Facilities_Data_filtered <- Air_Pollutant_Fuels_NPC[Primary_Fuel_Type == "Pipeline Natural Gas" & Unit_Type == "Combined cycle",]
set.seed(1) # For reproducibility
random_selection <- Facilities_Data_filtered[sample(1:nrow(Facilities_Data_filtered), 
                                                    min(18, nrow(Facilities_Data_filtered))), ]
random_selection <- random_selection[, .(Facility_Unit.ID, Latitude, Longitude, State, County)]
Fossil_Fuels_NPC_new[, index := .I]
random_selection[, index := .I]
Fossil_Fuels_NPC_new <- merge(Fossil_Fuels_NPC_new, random_selection, by = "index", all.x = TRUE)

selected_cols <- Facility_Level_Results[, .(Facility_Unit.ID, PM_lbs_mmBTU, CO, NH3, PM2.5, NOx, SO2, VOC, PM10)]
selected_cols <- unique(selected_cols, by = "Facility_Unit.ID")
Fossil_Fuels_NPC_new <- merge(selected_cols, Fossil_Fuels_NPC_new, by = "Facility_Unit.ID")

# Add in parameters to generation
Yearly_Results <- merge(Yearly_Results, Fossil_Fuels_NPC_new, by = "Year", all.x = TRUE)
Yearly_Results <- Yearly_Results[New_Fossil_Fuel_TWh > 0, ]

# Calculate emissions and costs
# Calculate emissions and costs for Gas_CC facilities
Yearly_Results[, total_HI_mmBtu := New_Fossil_Fuel_TWh * 1e6 * mean_Heat_Input_mmBtu / Estimated_NameplateCapacity_MW]
Yearly_Results[, PM.total_tons := total_HI_mmBtu * PM_lbs_mmBTU * lbs_tons_conversion]

# Calculate PM2.5 and PM10 specifically for Gas_CC
Yearly_Results[, PM2.5_tons := PM.total_tons] 
Yearly_Results[, PM10_tons := 0]  

# NOx and SO2 emissions (convert lbs to tons)
Yearly_Results[, total_NOx_tons := New_Fossil_Fuel_TWh * 1e6 * mean_NOx_lbs_MW * lbs_tons_conversion]
Yearly_Results[, total_SO2_tons := New_Fossil_Fuel_TWh * 1e6 * mean_SO2_lbs_MW * lbs_tons_conversion]

# CO emissions for Gas_CC
Yearly_Results[, CO_tons := total_HI_mmBtu * 3.0e-02 * lbs_tons_conversion]  # Emission factor for Gas_CC

# VOC emissions for Gas_CC
Yearly_Results[, VOC_tons := total_HI_mmBtu * 2.1e-03 * lbs_tons_conversion]  # Emission factor for Gas_CC

# Calculate costs
Yearly_Results[, total_NOx_USD := total_NOx_tons * NOx]
Yearly_Results[, total_SO2_USD := total_SO2_tons * SO2]
Yearly_Results[, total_PM2.5_USD := PM2.5_tons * PM2.5]
Yearly_Results[, total_PM10_USD := PM10_tons * PM10]
Yearly_Results[, total_CO_USD := CO_tons * CO]
Yearly_Results[, total_VOC_USD := VOC_tons * VOC]

# Summarize data for Sim, Scenario, year
Yearly_Facility_Level_Results <- Yearly_Results[, .(
  total_NOx_USD = sum(total_NOx_USD, na.rm = TRUE),
  total_SO2_USD = sum(total_SO2_USD, na.rm = TRUE),
  total_PM2.5_USD = sum(total_PM2.5_USD, na.rm = TRUE),
  total_PM10_USD = sum(total_PM10_USD, na.rm = TRUE),
  total_CO_USD = sum(total_CO_USD, na.rm = TRUE),
  total_VOC_USD = sum(total_VOC_USD, na.rm = TRUE)
), by = .(Year, Simulation, Pathway)]

# Total air pollutants
Yearly_Facility_Level_Results[, total_air_emission_USD := total_NOx_USD + total_SO2_USD + total_PM2.5_USD + 
                                total_PM10_USD + total_CO_USD + total_VOC_USD]

# Adjust County names
Yearly_Results$County <- gsub(" County", "", Yearly_Results$County)
Yearly_Facility_Level_Results$County <- gsub(" County", "", Yearly_Facility_Level_Results$County)

# Save the results
fwrite(Yearly_Results, file = file.path(output_path, "Facility_Level_Results_New.csv"), row.names = FALSE)
fwrite(Yearly_Facility_Level_Results, file = file.path(output_path, "Yearly_Facility_Level_Results_New.csv"), row.names = FALSE)

# Define a function to process Air_Pollutant Costs
process_Air_Pollutant <- function(sim, path, data) {
  filtered_data <- data[Simulation == sim & Pathway == path]
  npv <- calculate_npv(filtered_data, discount_rate, base_year, "total_air_emission_USD")
  return(npv)
}

# NPV Calculation
simulations <- unique(Yearly_Facility_Level_Results$Simulation)
Pathways <- unique(Yearly_Facility_Level_Results$Pathway)
npv_results <- list()

for (sim in simulations) {
  for (path in Pathways) {
    npv <- process_Air_Pollutant(sim, path, Yearly_Facility_Level_Results)
    npv_results[[paste0(sim, "_", path, "_Total_Air_Pollutant_NPV")]] <- npv
  }
}

# Combine NPV results into a single data.table
combined_npvs_new <- rbindlist(lapply(names(npv_results), function(name) {
  parts <- strsplit(name, "_")[[1]]
  data.table(Simulation = parts[1], Pathway = parts[2], NPV = npv_results[[name]])
}), fill = TRUE)


# Save combined NPV results to a single CSV file
fwrite(combined_npvs_new, file = file.path(output_path, "Air_Emissions_New.csv"), row.names = FALSE)

# Avoided mortality for each Pathway
# Cost of mortality: $7.4 million in 2006-USD per avoided mortality
mortality_million_USD_2006 <- 7.4

# Extracting CPI values for specific years
cpi_2006 <- filter(cpi_data, year(date) == 2006) %>% summarise(YearlyAvg = mean(value))

# Calculating conversion rate
conversion_rate <- cpi_2024$YearlyAvg / cpi_2006$YearlyAvg

# 2024 mortality cost
mortality_million_USD_2024 <- mortality_million_USD_2006 * conversion_rate

# How many moralities 
combined_npvs_new$NPV_millions <- combined_npvs_new$NPV /1e6
combined_npvs_new$mortality <- combined_npvs_new$NPV_millions / mortality_million_USD_2024

# County level results
# Summarize data for Sim, Pathway, year and County
Yearly_County_Level_Results <- Yearly_Results[, .(
  total_NOx_USD = sum(total_NOx_USD, na.rm = TRUE),
  total_SO2_USD = sum(total_SO2_USD, na.rm = TRUE),
  total_PM2.5_USD = sum(total_PM2.5_USD, na.rm = TRUE),
  total_PM10_USD = sum(total_PM10_USD, na.rm = TRUE),
  total_CO_USD = sum(total_CO_USD, na.rm = TRUE),
  total_VOC_USD = sum(total_VOC_USD, na.rm = TRUE)
), by = .(Year, Simulation, Pathway, County, State)]

# Total air pollutants
Yearly_County_Level_Results[, total_air_emission_USD := total_NOx_USD + total_SO2_USD + total_PM2.5_USD + 
                              total_PM10_USD + total_CO_USD + total_VOC_USD]
# Annual Mortality
Yearly_County_Level_Results[, mortality_annual_person := total_air_emission_USD/ mortality_million_USD_2024/ 1e6]

# Summarize the data by County, State, Simulation, and Pathway, and calculate NPV
Yearly_County_Level_Results[, Year := as.numeric(as.character(Year))]

County_Level_NPV <- Yearly_County_Level_Results[, .(
  npv_NOx_USD = calculate_npv(.SD, discount_rate, base_year, "total_NOx_USD"),
  npv_SO2_USD = calculate_npv(.SD, discount_rate, base_year, "total_SO2_USD"),
  npv_PM2.5_USD = calculate_npv(.SD, discount_rate, base_year, "total_PM2.5_USD"),
  npv_PM10_USD = calculate_npv(.SD, discount_rate, base_year, "total_PM10_USD"),
  npv_CO_USD = calculate_npv(.SD, discount_rate, base_year, "total_CO_USD"),
  npv_VOC_USD = calculate_npv(.SD, discount_rate, base_year, "total_VOC_USD"),
  npv_total_air_emission_USD = calculate_npv(.SD, discount_rate, base_year, "total_air_emission_USD"), 
  mortality_person = sum(mortality_annual_person)
), by = .(County, State, Simulation, Pathway)]


# Save combined NPV results to a single CSV file
fwrite(County_Level_NPV, file = file.path(output_path, "Air_Emissions_County_Level_New.csv"), row.names = FALSE)

## Total NPVs
# Assuming combined_npvs and combined_npvs_new are already data.tables
# Remove zeros from both datasets
combined_npvs_filtered <- combined_npvs[NPV > 0,]
combined_npvs_new_filtered <- combined_npvs_new[NPV > 0,]

# Calculate the mean for each pathway in combined_npvs (in billions $)
mean_npvs_1 <- combined_npvs_filtered[, .(
  NPV_mean_1 = mean(NPV, na.rm = TRUE),
  NPV_max_1 = max(NPV, na.rm = TRUE),
  NPV_min_1 = min(NPV, na.rm = TRUE),
  mortality_mean_1 = mean(mortality, na.rm = TRUE),
  mortality_max_1 = max(mortality, na.rm = TRUE),
  mortality_min_1 = min(mortality, na.rm = TRUE)
), by = Pathway]

# Calculate the mean for each pathway in combined_npvs_new (in billions $)
mean_npvs_2 <- combined_npvs_new_filtered[, .(
  NPV_mean_2 = mean(NPV, na.rm = TRUE),
  NPV_max_2 = max(NPV, na.rm = TRUE),
  NPV_min_2 = min(NPV, na.rm = TRUE),
  mortality_mean_2 = mean(mortality, na.rm = TRUE),
  mortality_max_2 = max(mortality, na.rm = TRUE),
  mortality_min_2 = min(mortality, na.rm = TRUE)
), by = Pathway]

# Combine the pathway means
combined_pathway_means <- merge(mean_npvs_1, mean_npvs_2, by = "Pathway", all = TRUE)

# Calculate the sum of the means for each pathway 
summary_npvs <- combined_pathway_means[, .(
  mean_NPV = sum(c(NPV_mean_1, NPV_mean_2), na.rm = TRUE),
  max_NPV = sum(c(NPV_max_1, NPV_max_2), na.rm = TRUE),
  min_NPV = sum(c(NPV_min_1, NPV_min_2), na.rm = TRUE),
  mean_mortality = sum(c(mortality_mean_1, mortality_mean_2), na.rm = TRUE),
  max_mortality = sum(c(mortality_max_1, mortality_max_2), na.rm = TRUE),
  min_mortality = sum(c(mortality_min_1, mortality_min_2), na.rm = TRUE)
), by = Pathway]


fwrite(summary_npvs, file = file.path(output_path, "Air_Emissions_ALL.csv"), row.names = FALSE)

#---- Emissions per county per pathway
# ------------------------------
# Summarize Mean, Min, and Max Emissions (NPV) for Each County
# for Existing and New Facilities, then save as CSV

# Read in the previously saved county-level results for existing and new facilities
existing_emissions <- fread(file.path(output_path, "Air_Emissions_County_Level_Existing.csv"))
new_emissions      <- fread(file.path(output_path, "Air_Emissions_County_Level_New.csv"))

# For each county, compute summary statistics using the NPV of total air emissions (USD)
summary_existing <- existing_emissions[, .(
  mean_emission = mean(npv_total_air_emission_USD, na.rm = TRUE),
  min_emission  = min(npv_total_air_emission_USD, na.rm = TRUE),
  max_emission  = max(npv_total_air_emission_USD, na.rm = TRUE)
), by = .(County, State, Pathway)]
summary_existing[, Facility_Type := "Existing"]

summary_new <- new_emissions[, .(
  mean_emission = mean(npv_total_air_emission_USD, na.rm = TRUE),
  min_emission  = min(npv_total_air_emission_USD, na.rm = TRUE),
  max_emission  = max(npv_total_air_emission_USD, na.rm = TRUE)
), by = .(County, State, Pathway)]
summary_new[, Facility_Type := "New"]

# Combine the summaries for both facility types
combined_summary <- rbind(summary_existing, summary_new)
# Group by County, State, and Pathway and sum the emission columns across Facility_Type
final_county_summary <- combined_summary[, .(
  total_mean_emission = sum(mean_emission, na.rm = TRUE)/1e6,
  total_min_emission = sum(min_emission, na.rm = TRUE)/1e6,
  total_max_emission = sum(max_emission, na.rm = TRUE)/1e6
), by = .(County, State, Pathway)]

final_county_summary <- final_county_summary[
  total_mean_emission != 0 & total_min_emission != 0 & total_max_emission != 0
]
# Save the aggregated summary to a CSV file
fwrite(final_county_summary, file = file.path(output_path, "County_Level_Emissions_Summary_Total.csv"), row.names = FALSE)


