# Load libraries
library(data.table)
library(readxl)
library(fredr)
library(dplyr)
library(stringr)

# NPV Calculator
calculate_npv <- function(dt, rate, base_year, col) {
  npv <- sum(dt[[col]] / (1 + rate)^(dt[["Year"]] - base_year))
  return(npv)
}

discount_rate <- 0.025
base_year <- 2024
new_england_states <- c("CT", "ME", "MA", "NH", "RI", "VT")

# API KEY 63522eae4ec927d6f1d9d86bf7826cc8
fredr_set_key("63522eae4ec927d6f1d9d86bf7826cc8") 
cpi_data <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"), observation_end = as.Date("2024-01-01"))

# Extracting CPI values for specific years
cpi_2021 <- filter(cpi_data, year(date) == 2021) %>% summarise(YearlyAvg = mean(value))
cpi_2024 <- filter(cpi_data, year(date) == 2024) %>% summarise(YearlyAvg = mean(value))

# Calculating conversion rate
conversion_rate <- cpi_2024$YearlyAvg / cpi_2021$YearlyAvg

# Load costs ATB 2021
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/NREL ATB/ATB_2021_Fuel_Costs_Fossil.xlsx"
sheet_names <- excel_sheets(file_path)
# Read all sheets into a list of data frames
all_sheets <- lapply(sheet_names, function(sheet) read_excel(file_path, sheet = sheet))
names(all_sheets) <- sheet_names

# Combine all data frames into one
ATB_2021 <- as.data.table(do.call(rbind, all_sheets))
ATB_2024 <- ATB_2021 %>%
  mutate(across(-1, ~ . * conversion_rate))

# Reshape the datatable
ATB_2024 <- melt(
  ATB_2024,
  id.vars = "Fuel Costs ($/MWh)",
  variable.name = "Year",
  value.name = "Cost_USD_per_MWh"
)
setnames(ATB_2024, "Fuel Costs ($/MWh)", "Technology")

# Add Fuel_Category column
ATB_2024 <- ATB_2024 %>%
  mutate(Fuel_Category = case_when(
    grepl("^Dedicated - ", Technology) ~ "Wood",
    grepl("^Coal-IGCC-AvgCF-", Technology) ~ "Coal",
    grepl("^Gas-CC-AvgCF - ", Technology) ~ "Gas_CC",
    grepl("^Gas-CT-AvgCF - ", Technology) ~ "Gas_CT"
  )) %>%
  filter(!is.na(Fuel_Category))

ATB_2024 <- ATB_2024 %>%
  distinct(Year, Fuel_Category, Cost_USD_per_MWh, .keep_all = TRUE)
setDT(ATB_2024)

# Load Results
#-- Stepwise
file_path_1 <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Facility_Level_Results.csv"
file_path_2 <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"

Facility_Level_Results <- as.data.table(fread(file_path_1))
Facility_Level_Results <- Facility_Level_Results[Pathway %in% c("A", "D", "B1", "B2", "B3", "C1", "C2", "C3")]


Yearly_Results <- as.data.table(fread(file_path_2))

# Load facilities data
# Old/existing fossil fuels
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(file_path)
# New fossil fuels 
file_path <-"/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC_new <- fread(file_path)
Fossil_Fuels_NPC_new <- Fossil_Fuels_NPC_new[1,]

# Add the Fuel_Category column based on the specified conditions
Facilities_Data <- Fossil_Fuels_NPC %>%
  mutate(Fuel_Category = case_when(
    Primary_Fuel_Type == "Wood" ~ "Wood",
    Primary_Fuel_Type == "Coal" ~ "Coal",
    Primary_Fuel_Type == "Pipeline Natural Gas" & Unit_Type == "Combined cycle" ~ "Gas_CC",
    Primary_Fuel_Type == "Pipeline Natural Gas" & Unit_Type != "Combined cycle" ~ "Gas_CT",
    TRUE ~ "Gas_CT"
  ))

# Add Fuel_Category to Facility_Level_Results
Facility_Level_Results <- merge(Facility_Level_Results, Facilities_Data[, .(Facility_Unit.ID, Fuel_Category)], by = "Facility_Unit.ID", all.x = TRUE)

# Cross join Facility_Level_Results with cost_categories
Facility_Level_Results[, Year := as.character(Year)]

# Perform the merge 
Facility_Level_Results <- merge(Facility_Level_Results, ATB_2024[, .(Fuel_Category, Year, Cost_USD_per_MWh)], 
           by = c("Fuel_Category", "Year"), all.x = TRUE)

# Fuel costs calculation
Facility_Level_Results[, Total_Cost_USD := total_generation_GWh * Cost_USD_per_MWh * 1000] # GWh to MWh

# Summarize based on Fuel Type, sim, year and Pathway
Facility_Level_Results <- Facility_Level_Results[, .(
  total_Fuel_costs_USD = sum(Total_Cost_USD, na.rm = TRUE),
  total_Fuel_used_MWh = sum(total_generation_GWh, na.rm = TRUE) * 1000
), by = .(Year, Simulation, Pathway, Fuel_Category)]

Facility_Level_Results_sim <- copy(Facility_Level_Results)
Facility_Level_Results$Year <- as.numeric(as.character(Facility_Level_Results$Year))

Facility_Level_Results <- Facility_Level_Results[, .(
  total_Fuel_costs_USD_mean = mean(total_Fuel_costs_USD, na.rm = TRUE),
  total_Fuel_costs_USD_max = max(total_Fuel_costs_USD, na.rm = TRUE),
  total_Fuel_costs_USD_min = min(total_Fuel_costs_USD, na.rm = TRUE)
), by = .(Year, Pathway, Fuel_Category)]

# Add new natural gas costs
Fossil_new_gen <- Yearly_Results[, .(Year, Simulation, Pathway, Fuel_Category = as.character("Gas_CC"), total_generation_GWh = New_Fossil_Fuel_TWh * 1e3)]
ATB_2024[, Year := as.integer(as.character(Year))]
Fossil_new_gen <- merge(Fossil_new_gen, 
                        ATB_2024[, .(Fuel_Category, Year, Cost_USD_per_MWh)], 
                        by = c("Fuel_Category", "Year"), all.x = TRUE)

Fossil_new_gen[, Total_Cost_USD := total_generation_GWh * Cost_USD_per_MWh * 1000] # GWh to MWh
Fossil_new_gen <- Fossil_new_gen[, .(
  total_Fuel_costs_USD = sum(Total_Cost_USD, na.rm = TRUE),
  total_Fuel_used_MWh = sum(total_generation_GWh, na.rm = TRUE) * 1000
), by = .(Year, Simulation, Pathway, Fuel_Category)]
Fossil_new_gen_sim <- copy(Fossil_new_gen)

Fossil_new_gen <- Fossil_new_gen[, .(
  total_Fuel_costs_USD_mean = mean(total_Fuel_costs_USD, na.rm = TRUE),
  total_Fuel_costs_USD_max = max(total_Fuel_costs_USD, na.rm = TRUE),
  total_Fuel_costs_USD_min = min(total_Fuel_costs_USD, na.rm = TRUE)
), by = .(Year, Pathway, Fuel_Category)]


# ALL costs merged
Facility_Level_Results <- rbind(Facility_Level_Results, Fossil_new_gen)
Facility_Level_Results <- Facility_Level_Results[, .(
  total_Fuel_costs_USD_mean = sum(total_Fuel_costs_USD_mean, na.rm = TRUE),
  total_Fuel_costs_USD_max = sum(total_Fuel_costs_USD_max, na.rm = TRUE),
  total_Fuel_costs_USD_min = sum(total_Fuel_costs_USD_min, na.rm = TRUE)
), by = .(Year, Pathway, Fuel_Category)]

#  process Fuel costs NPV
pathways <- unique(Facility_Level_Results$Pathway)
fossil_fuels <- unique(Facility_Level_Results$Fuel_Category)
npv_results_fossil <- list()

for (path in pathways) {
  for (fossil_info in fossil_fuels) {
    fossil_data <- Facility_Level_Results[Pathway == path & Fuel_Category == fossil_info]
    result <- calculate_npv(fossil_data, discount_rate, base_year, "total_Fuel_costs_USD_mean")
    npv_results_fossil[[paste0(path, "_", fossil_info, "_fuel_NPV_")]] <- result
  }
}


# Combine NPV results into a single data.table
combined_npvs_fuel <- rbindlist(lapply(names(npv_results_fossil), function(name) {
  parts <- strsplit(name, "_")[[1]]
  # Ensure the technology name captures both parts if it contains 'Gas_CC' or 'Gas_CT'
  data.table(Pathway = parts[1], NPV = npv_results_fossil[[name]], Technology = parts[2])
}), fill = TRUE)

combined_npvs_fuel <- combined_npvs_fuel[NPV != 0,]
combined_npvs_summary_1 <- combined_npvs_fuel[, .(
  sum_NPV = sum(NPV, na.rm = TRUE)/1e9
), by = .(Pathway)] 

# Save combined NPV results to a single CSV file
write.csv(combined_npvs_fuel, file = file.path(output_path, "Fuel_Fossil.csv"), row.names = FALSE)

# Per Simulation 
Facility_Level_Results_sim <- rbind(Facility_Level_Results_sim, Fossil_new_gen_sim)
Facility_Level_Results_sim$Year <- as.numeric(as.character(Facility_Level_Results_sim$Year))
npv_per_sim <- Facility_Level_Results_sim[, .(
  NPV = calculate_npv(.SD, discount_rate, base_year, "total_Fuel_costs_USD")
), by = .(Simulation, Pathway, Fuel_Category)]

npv_per_sim <- npv_per_sim[NPV != 0,]
npv_per_sim <- npv_per_sim[, .(
  sum_NPV = sum(NPV, na.rm = TRUE)/1e9
), by = .(Pathway, Simulation)] 
combined_npvs_summary_2 <- npv_per_sim[, .(
  mean_NPV = mean(sum_NPV, na.rm = TRUE),
  sd_NPV = sd(sum_NPV, na.rm = TRUE)
), by = .(Pathway)] 


write.csv(
  npv_per_sim,
  file.path(output_path, "Fuel_Fossil_per_Simulation.csv"),
  row.names = FALSE
)

