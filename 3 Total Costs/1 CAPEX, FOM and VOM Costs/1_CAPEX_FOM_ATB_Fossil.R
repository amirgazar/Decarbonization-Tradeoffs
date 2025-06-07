# Load libraries
library(data.table)
library(readxl)
library(dplyr)
library(lubridate)

# NPV Calculator
calculate_npv <- function(dt, rate, base_year) {
  npv <- sum(dt[,2] / (1 + rate)^(dt[,1] - base_year))
  return(npv)
}

discount_rate <- 0.025
base_year <- 2024

# Load ATB Costs
ATBe <- fread("/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/NREL ATB/ATBe_2024.csv")
ATB_scenarios <- c("Advanced", "Moderate", "Conservative")

# Load Fossil Fuels data
# Old fossil
Fossil_Fuels_NPC <- fread("/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv")

Coal_NPC <- Fossil_Fuels_NPC[grepl("Coal", Primary_Fuel_Type, ignore.case = TRUE)]
Wood_NPC <- Fossil_Fuels_NPC[grepl("Wood", Primary_Fuel_Type, ignore.case = TRUE)]
Oil_NPC <- Fossil_Fuels_NPC[grepl("Oil", Primary_Fuel_Type, ignore.case = TRUE)]
Gas_CC_NPC <- Fossil_Fuels_NPC[grepl("Gas", Fossil_Fuels_NPC$Primary_Fuel_Type, ignore.case = TRUE) & 
                                 grepl("Combined Cycle", Fossil_Fuels_NPC$Unit_Type, ignore.case = TRUE), ]
Gas_CT_NPC <-  Fossil_Fuels_NPC[grepl("Gas", Fossil_Fuels_NPC$Primary_Fuel_Type, ignore.case = TRUE) & 
                                  !grepl("Combined Cycle", Fossil_Fuels_NPC$Unit_Type, ignore.case = TRUE), ]

# New fossil (Note this is all new NG CC)
New_Fossil_Fuels_NPC <- fread("/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv")

# Create annual data tables for each fuel type
create_annual_data_table <- function(start_year, end_year) {
  data.table(Year = start_year:end_year, Estimated_NameplateCapacity_MW = 0)
}

Coal_NPC_annual <- create_annual_data_table(2025, 2050)
Wood_NPC_annual <- create_annual_data_table(2025, 2050)
Gas_CC_NPC_annual <- create_annual_data_table(2025, 2050)
Gas_CT_NPC_annual <- create_annual_data_table(2025, 2050)
Oil_NPC_annual <- create_annual_data_table(2025, 2050)

# Function to calculate total capacity considering retirement for each year
calculate_capacity <- function(fuel_subset, result_table) {
  for (year in start_year:end_year) {
    # Calculate total capacity without considering retirements
    total_capacity_no_retirements <- sum(fuel_subset$Estimated_NameplateCapacity_MW, na.rm = TRUE)
    
    # Calculate total capacity considering retirements for the current year
    total_capacity <- sum(fuel_subset[Retirement_year >= year, ]$Estimated_NameplateCapacity_MW, na.rm = TRUE)
    
    # Update the result_table for the current year
    result_table[Year == as.integer(year), `:=`(
      Estimated_NameplateCapacity_MW = total_capacity,
      Estimated_NameplateCapacity_MW_no_retirements = total_capacity_no_retirements
    )]
  }
}


start_year <- 2025
end_year <- 2050

calculate_capacity(Coal_NPC, Coal_NPC_annual)
calculate_capacity(Wood_NPC, Wood_NPC_annual)
calculate_capacity(Gas_CC_NPC, Gas_CC_NPC_annual)
calculate_capacity(Gas_CT_NPC, Gas_CT_NPC_annual)
calculate_capacity(Oil_NPC, Oil_NPC_annual)

# New natural gas capacity, based on operating start years
New_Fossil_Fuels_NPC[, Year := as.integer(format(Commercial_Operation_Date, "%Y"))]
New_Fossil_Fuels_NPC <- New_Fossil_Fuels_NPC[, .(NewNGCC_NameplateCapacity_MW = sum(Estimated_NameplateCapacity_MW, na.rm = TRUE)), by = Year]
New_Fossil_Fuels_NPC[, Total.NewNGCC_NameplateCapacity_MW := (cumsum(NewNGCC_NameplateCapacity_MW))]

# Add in to existing natural gas Gas_CC_NPC
Gas_CC_NPC_annual <- merge(Gas_CC_NPC_annual, New_Fossil_Fuels_NPC, by = "Year", all.x = TRUE)

# Integrating pathways retirement schedule
Gas_CC_NPC_annual <- Gas_CC_NPC_annual %>%
  dplyr::select(Year, 
                Estimated_NameplateCapacity_MW, 
                Estimated_NameplateCapacity_MW_no_retirements,
                Total.NewNGCC_NameplateCapacity_MW, 
                NewNGCC_NameplateCapacity_MW) %>%
  mutate(
    B_C_FOM = Estimated_NameplateCapacity_MW,
    A_FOM = Estimated_NameplateCapacity_MW_no_retirements,
    D_FOM = rowSums(across(c(Estimated_NameplateCapacity_MW_no_retirements, Total.NewNGCC_NameplateCapacity_MW)), na.rm = TRUE),
    D_CAPEX = NewNGCC_NameplateCapacity_MW
  )

# Calculate new installed capacities for new Natural Gas
Gas_CC_NPC_annual <- Gas_CC_NPC_annual %>%
  # Keep only 'Year' and the specified columns
  dplyr::select(Year, B_C_FOM, A_FOM, D_FOM, D_CAPEX) %>%
  # Replace NA in D_CAPEX with 0
  mutate(D_CAPEX = if_else(is.na(D_CAPEX), 0, D_CAPEX))

# Calculate CAPEX, Fixed O&M and NPV for each fuel type and scenario
calculate_fixed_om_npv <- function(fuel_data, atb_data, atb_scenarios, discount_rate, base_year) {
  # Initialize an empty data table to store results
  fixed_npv <- data.table(Scenario = character(), Pathway = character(), NPV = numeric())
  
  for (i in seq_along(atb_scenarios)) {
    # Merge data for the current scenario
    scenario_data <- merge(atb_data[scenario == atb_scenarios[i]], fuel_data, by.x = "core_metric_variable", by.y = "Year")
    
    # Calculate fixed O&M costs
    scenario_data[, B_C_Fixed_OM := Estimated_NameplateCapacity_MW * value * 1000] # KW to MW
    scenario_data[, A_D_Fixed_OM := Estimated_NameplateCapacity_MW_no_retirements * value * 1000] # KW to MW
    
    # Calculate NPV for each pathway
    B_C_npv <- calculate_npv(scenario_data[, .(core_metric_variable, B_C_Fixed_OM)], discount_rate, base_year)
    A_D_npv <- calculate_npv(scenario_data[, .(core_metric_variable, A_D_Fixed_OM)], discount_rate, base_year)
    
    # Append results for each pathway
    fixed_npv <- rbind(fixed_npv, 
                       data.table(Scenario = atb_scenarios[i], Pathway = "B_C", NPV = B_C_npv))
    fixed_npv <- rbind(fixed_npv, 
                       data.table(Scenario = atb_scenarios[i], Pathway = "A_D", NPV = A_D_npv))
  }
  
  # Return the results
  return(fixed_npv)
}


# Every fuel except NGCC FOM
fuel_types <- c("Coal", "Wood", "Gas_CT", "Oil")
npv_results <- list()
for (fuel in fuel_types) {
  # Define the filtering criteria based on the switch cases
  technology_filter <- switch(fuel, 
                              Coal = "Coal_FE", 
                              Wood = "Biopower", 
                              Gas_CT = "NaturalGas_FE", 
                              Oil = "NaturalGas_FE")
  
  techdetail_filter <- switch(fuel, 
                               Coal = "Coal-IGCC", 
                               Wood = "Dedicated", 
                               Gas_CT = "NG Combustion Turbine (F-Frame)", 
                               Oil = "NG Combustion Turbine (F-Frame)")
  
  core_metric_case_filter <- "Market"
  crpyears_filter <- 30
  core_metric_variable_filter <- 2025
  maturity_filter <- "Y"
  core_metric_parameter_filter <- "Fixed O&M"
  
  # Apply all filters simultaneously
  atb_data_FOM <- ATBe[
    technology == technology_filter & 
      techdetail == techdetail_filter & 
      core_metric_case == core_metric_case_filter & 
      crpyears == crpyears_filter & 
      maturity == maturity_filter &
      core_metric_variable >= core_metric_variable_filter & 
      core_metric_parameter == core_metric_parameter_filter
  ]
  
  fixed_npv <- calculate_fixed_om_npv(get(paste0(fuel, "_NPC_annual")), atb_data_FOM, 
                                      ATB_scenarios, discount_rate, base_year)
  npv_results[[paste0(fuel, "_fixed_NPV")]] <- fixed_npv
}

# NGCC FOM and CAPEX
core_metric_case_filter <- "Market"
crpyears_filter <- 30
core_metric_variable_filter <- 2025
maturity_filter <- "Y"
core_metric_parameter_filter <- "Fixed O&M"
technology_filter <- "NaturalGas_FE"
techdetail_filter <- "NG 1-on-1 Combined Cycle (H-Frame)"

# Apply all filters simultaneously
atb_data_FOM <- ATBe[
  technology == technology_filter & 
    techdetail == techdetail_filter & 
    core_metric_case == core_metric_case_filter & 
    crpyears == crpyears_filter & 
    maturity == maturity_filter &
    core_metric_variable >= core_metric_variable_filter & 
    core_metric_parameter == core_metric_parameter_filter
]
  
core_metric_parameter_filter <- "CAPEX"
atb_data_CAPEX <- ATBe[
  technology == technology_filter & 
    techdetail == techdetail_filter & 
    core_metric_case == core_metric_case_filter & 
    crpyears == crpyears_filter & 
    core_metric_variable >= core_metric_variable_filter & 
    core_metric_parameter == core_metric_parameter_filter
]  

# Initialize an empty data frame to store results
gas_CC_npv <- data.table(Scenario = character(), Cost_Type =  character(), Pathway = character(), Fuel = character(), NPV = numeric())

for (i in seq_along(ATB_scenarios)) {
  # Merge data for the current scenario
  scenario_data <- merge(atb_data_FOM[scenario == ATB_scenarios[i]], Gas_CC_NPC_annual, by.x = "core_metric_variable", by.y = "Year")
  
  # Calculate fixed O&M costs
  scenario_data[, B_C_Fixed_OM := B_C_FOM * value * 1000] # KW to MW
  scenario_data[, A_Fixed_OM := A_FOM * value * 1000]     # KW to MW
  scenario_data[, D_Fixed_OM := D_FOM * value * 1000]     # KW to MW
  

  # Calculate FOM NPV for each pathway
  B_C_npv <- calculate_npv(scenario_data[, .(core_metric_variable, B_C_Fixed_OM)], discount_rate, base_year)
  A_npv <- calculate_npv(scenario_data[, .(core_metric_variable, A_Fixed_OM)], discount_rate, base_year)
  D_npv <- calculate_npv(scenario_data[, .(core_metric_variable, D_Fixed_OM)], discount_rate, base_year)
  
  # CAPEX NPV
  scenario_data <- merge(atb_data_CAPEX[scenario == ATB_scenarios[i]], Gas_CC_NPC_annual, by.x = "core_metric_variable", by.y = "Year")
  scenario_data[, D_CAPEX := D_CAPEX * value * 1000]     # KW to MW
  D_CAPEX_npv <- calculate_npv(scenario_data[, .(core_metric_variable, D_CAPEX)], discount_rate, base_year)
  
  # Append results for each pathway
  gas_CC_npv <- rbind(gas_CC_npv, 
                          data.table(Scenario = ATB_scenarios[i], Pathway = "B_C", Cost_Type = "FOM", Fuel = "Gas_CC", NPV = B_C_npv))
  gas_CC_npv <- rbind(gas_CC_npv, 
                          data.table(Scenario = ATB_scenarios[i], Pathway = "A", Cost_Type = "FOM", Fuel = "Gas_CC", NPV = A_npv))
  gas_CC_npv <- rbind(gas_CC_npv, 
                          data.table(Scenario = ATB_scenarios[i], Pathway = "D", Cost_Type = "FOM", Fuel = "Gas_CC", NPV = D_npv))
  gas_CC_npv <- rbind(gas_CC_npv, 
                      data.table(Scenario = ATB_scenarios[i], Pathway = "D", Cost_Type = "CAPEX", Fuel = "Gas_CC", NPV = D_CAPEX_npv))
}

# Define the mapping of pathway expansions
pathway_expansion <- list(
  "A" = c("A"),
  "D" = c("D"),
  "B_C" = c("B1", "B2", "B3", "C1", "C2", "C3")
)

# Expand the pathways
expanded_gas_CC_npv <- gas_CC_npv[, .(
  Pathway = unlist(pathway_expansion[Pathway]), 
  Scenario, 
  NPV, 
  Cost_Type, 
  Fuel
), by = .(rowid = .I)]

# Remove the temporary rowid column
expanded_gas_CC_npv[, I := NULL]

# Process the NPV results
combined_npvs <- rbindlist(lapply(names(npv_results), function(fuel) {
  base_fuel <- sub("_.*$", "", fuel)
  # Conditionally change "Gas" to "Gas_CT"
  base_fuel <- ifelse(base_fuel == "Gas", "Gas_CT", base_fuel)
  
  # Create a data table with additional columns
  data.table(
    npv_results[[fuel]], 
    Cost_Type = "FOM", 
    Fuel = base_fuel
  )
}), fill = TRUE)

# Define the mapping of pathway expansions
pathway_expansion <- list(
  "A_D" = c("A", "D"),
  "B_C" = c("B1", "B2", "B3", "C1", "C2", "C3")
)

# Expand the pathways
expanded_combined_npvs <- combined_npvs[, .(
  Pathway = unlist(pathway_expansion[Pathway]), 
  Scenario, 
  NPV, 
  Cost_Type, 
  Fuel
), by = .(rowid = .I)]

# Remove the temporary rowid column
expanded_combined_npvs[, I := NULL]

# Combine everything
combined_npvs_all <- rbind(expanded_gas_CC_npv, expanded_combined_npvs)

combined_npvs_summary <- combined_npvs_all[NPV != 0, .(
  mean_NPV = mean(NPV, na.rm = TRUE)/1e9,
  sd_NPV = sd(NPV, na.rm = TRUE)/1e9
), by = .(Pathway, Cost_Type, Fuel)] 

# Save combined NPV results to a single CSV file
write.csv(combined_npvs_all, file = "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results/CAPEX_Fixed_Fossil.csv", row.names = FALSE)


# Tax revenue
# Monte Carlo placement of new NGCC plants across New England counties
# and calculation of annual property-tax revenue
# =============== Monte Carlo tax revenue using CAPEX ===============

# 1) Extract CAPEX $/kW by year from ATBe (average across scenarios)
capex_dt <- ATBe[
  technology          == "NaturalGas_FE" &
    techdetail          == "NG 1-on-1 Combined Cycle (H-Frame)" &
    core_metric_case    == "Market" &
    crpyears            == 30 &
    maturity            == "Y" &
    core_metric_parameter == "CAPEX",
  .(Year = core_metric_variable, capex_per_kW = value)
]
capex_dt <- capex_dt[, .(capex_per_kW = mean(capex_per_kW, na.rm = TRUE)), by = Year]

# 2) Prepare plants table: add year, capacity_kW, and CAPEX per kW
plants_dt <- fread(
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv"
) %>%
  mutate(
    Year    = year(as.Date(Commercial_Operation_Date)),
    cap_kW  = Estimated_NameplateCapacity_MW * 1000
  ) %>%
  select(Facility_Unit.ID, Year, cap_kW) %>%
  as.data.table()

plants_dt <- merge(plants_dt, capex_dt, by = "Year", all.x = TRUE)

# 3) Load & clean property‐tax rates for New England
tax_raw <- fread(
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/Tax Foundation/Property_taxes.csv",
  header = TRUE
)
tax <- tax_raw %>%
  rename(
    state_full   = State,
    county       = County,
    eff_rate_str = `Effective Property Tax Rate (2023)`
  ) %>%
  mutate(
    effective_rate = as.numeric(gsub("%", "", eff_rate_str)) / 100,
    state_abbr     = state.abb[match(state_full, state.name)]
  ) %>%
  filter(state_abbr %in% c("ME","NH","VT","MA","RI","CT")) %>%
  select(county, state_abbr, effective_rate)

# 4) Monte Carlo setup
set.seed(42)
n_sim    <- 1000L
n_plants <- nrow(plants_dt)

sim_list <- vector("list", n_sim)
for (i in seq_len(n_sim)) {
  # assign each plant a random county
  assigned_counties <- sample(tax$county, size = n_plants, replace = TRUE)
  
  sim_dt <- copy(plants_dt)[
    , county := assigned_counties
  ][
    tax, on = .(county), nomatch = 0
  ][
    # compute total CAPEX per plant
    , capex_total := cap_kW * capex_per_kW
  ][
    # tax revenue = tax rate * total CAPEX
    , tax_rev := effective_rate * capex_total
  ][
    , sim := i
  ]
  
  sim_list[[i]] <- sim_dt
}

# 5) Aggregate & save
all_sims    <- rbindlist(sim_list)
sim_summary <- all_sims[, .(total_tax_rev = sum(tax_rev, na.rm = TRUE)), by = sim]

# 6) Compute overall summary statistics
tax_stats <- sim_summary[, .(
  mean_tax_rev = mean(total_tax_rev, na.rm = TRUE)/1e9,
  min_tax_rev  = min(total_tax_rev, na.rm = TRUE)/1e9,
  max_tax_rev  = max(total_tax_rev, na.rm = TRUE)/1e9
)]

# 7) Save only the summary statistics
fwrite(
  tax_stats,
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results/NG_Tax_Revenue.csv"
)
