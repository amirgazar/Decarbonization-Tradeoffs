# Load libraries
library(data.table)
library(readxl)
library(tidyr)
library(dplyr)

# NPV Calculator
calculate_npv <- function(dt, rate, base_year) {
  npv <- sum(dt[, 2] / (1 + rate)^(dt[, 1] - base_year))
  return(npv)
}

discount_rate <- 0.025
base_year <- 2024

# Load ATB Costs
ATBe <- fread("/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/NREL ATB/ATBe_2024.csv")
ATB_scenarios <- c("Advanced", "Moderate", "Conservative")

# Load Capacity data
# Define the file path
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

# Define a function to process non-fossil CAPEX and O&M
process_non_fossil <- function(tech_info, dataset) {
  # Define the variables for filtering
  technology_filter <- tech_info[[1]]
  techdetail_filter <- tech_info[[2]]
  column_name <- tech_info[[3]]
  core_metric_case_filter <- "Market"
  crpyears_filter <- 30
  core_metric_variable_filter <- 2025
  
  # Apply all filters using data.table syntax
  tech_data <- ATBe[
    technology == technology_filter & 
      techdetail == techdetail_filter & 
      core_metric_case == core_metric_case_filter & 
      crpyears == crpyears_filter & 
      core_metric_variable >= core_metric_variable_filter
  ]
  
  capex_data <- tech_data[core_metric_parameter == "CAPEX"]
  fixed_om_data <- tech_data[core_metric_parameter == "Fixed O&M"]
  
  dataset[, new_capacity := get(column_name) - shift(get(column_name), 1, type = "lag")] 
  dataset[1, new_capacity := 0]
  
  capex_data <- merge(capex_data, dataset, by.x = "core_metric_variable", by.y = "Year")
  capex_data[, CAPEX := new_capacity * value * 1000] # KW to MW
  
  fixed_om_data <- merge(fixed_om_data, dataset, by.x = "core_metric_variable", by.y = "Year")
  fixed_om_data[, Fixed_OM := get(column_name) * value * 1000] # KW to MW
  
  capex_npv <- numeric(length(ATB_scenarios))
  fixed_om_npv <- numeric(length(ATB_scenarios))
  
  for (i in seq_along(ATB_scenarios)) {
    capex_scenario_data <- capex_data[scenario == ATB_scenarios[i], .(core_metric_variable, CAPEX)]
    capex_npv[i] <- calculate_npv(capex_scenario_data, discount_rate, base_year)
    
    fixed_om_scenario_data <- fixed_om_data[scenario == ATB_scenarios[i], .(core_metric_variable, Fixed_OM)]
    fixed_om_npv[i] <- calculate_npv(fixed_om_scenario_data, discount_rate, base_year)
  }
  
  names(capex_npv) <- ATB_scenarios
  names(fixed_om_npv) <- ATB_scenarios
  
  list(CAPEX_NPV = capex_npv, Fixed_OM_NPV = fixed_om_npv)
}

# Process NPVs
# Process  technologies 
technologies <- list(
  list(tech = "UtilityPV", detail = "Class5", column_name = "Solar"),
  list(tech = "LandbasedWind", detail = "Class4", column_name = "Onshore Wind"),
  list(tech = "OffShoreWind", detail = "Class4", column_name = "Offshore Wind"),
  list(tech = "Commercial Battery Storage", detail = "8Hr Battery Storage", column_name = "Storage"),
  list(tech = "Nuclear", detail = "Nuclear - Large", column_name = "Nuclear"),
  list(tech = "Nuclear", detail = "Nuclear - Small", column_name = "SMR"),
  list(tech = "Hydropower", detail = "NSD1",  column_name = "Hydropower"),
  list(tech = "Biopower", detail = "Dedicated", column_name = "Biomass")
)

pathways <- unique(decarbonization_pathways$Pathway)
results <- list()
for (pathway in pathways) {
  for (tech_info in technologies) {
    filtered_data <- decarbonization_pathways[Pathway == pathway, ]
    result <- process_non_fossil(tech_info, filtered_data)
    results[[paste0(pathway, "_CAPEX_", tech_info$column_name)]] <- result$CAPEX_NPV
    results[[paste0(pathway, "_FOM_", tech_info$column_name)]] <- result$Fixed_OM_NPV
  }
}

# Function to extract Scenario, NPV_Type, and Technology from the name
extract_info <- function(name) {
  parts <- unlist(strsplit(name, "_"))
  scenario <- ifelse(parts[length(parts)] %in% c("S1", "S2", "S3"), parts[length(parts)], "S1-3")
  technology <- name
  return(list(Scenario = scenario, Technology = technology, Pathway = pathway))
}


# Combine NPV results into a single data.table
combined_npvs <- rbindlist(lapply(names(results), function(name) {
  parts <- unlist(strsplit(name, "_"))
  data.table(Pathway = parts[1], Cost_Type = parts[2], Technology = parts[3], ATB_Scenario = names(results[[name]]), NPV = results[[name]])
}), fill = TRUE)

combined_npvs_summary <- combined_npvs[NPV != 0, .(
  mean_NPV = mean(NPV, na.rm = TRUE)/1e9,
  sd_NPV = sd(NPV, na.rm = TRUE)/1e9
), by = .(Pathway, Cost_Type, Technology)] 

combined_npvs_summary_more <- combined_npvs_summary[, .(
  sum_NPV = sum(mean_NPV, na.rm = TRUE)
), by = .(Pathway, Cost_Type)] 

# Save combined NPV results to a single CSV file
write.csv(combined_npvs, file = "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results/CAPEX_Fixed_Non_Fossil.csv", row.names = FALSE)


# Tax revenue estimates
# ========== Monte Carlo tax revenue for non-fossil CAPEX (Solar, Onshore Wind, Storage, SMR) ==========

# 1) Extract CAPEX per kW for each non-fossil technology
non_fossil_techs <- list(
  Solar   = list(technology="UtilityPV",               techdetail="Class5"),
  Onshore = list(technology="LandbasedWind",           techdetail="Class4"),
  SMR     = list(technology="Nuclear",                 techdetail="Nuclear - Small")
)


capex_list <- lapply(names(non_fossil_techs), function(tech) {
  f <- non_fossil_techs[[tech]]
  dt <- ATBe[
    technology    == f$technology &
      techdetail    == f$techdetail &
      core_metric_case    == "Market" &
      scenario == "Moderate" &
      crpyears      == 30 &
      core_metric_parameter == "CAPEX" &
      core_metric_variable == 2030,
    .(Year = core_metric_variable, capex_per_kW = mean(value, na.rm=TRUE))
  ]
  dt[, Technology := tech]
  dt
})
capex_non_fossil_dt <- rbindlist(capex_list)
capex_non_fossil_dt[, Year := NULL]

# 2) Compute incremental capacity by pathway-year-tech
incap_dt <- decarbonization_pathways[
  , .(Pathway, Year,
      Solar,
      Onshore = `Onshore Wind`,
      SMR)
][order(Pathway, Year)]
incap_dt <- incap_dt[
  , `:=`(
    new_Solar   = Solar  - shift(Solar,  type="lag", fill=0),
    new_Onshore = Onshore - shift(Onshore, type="lag", fill=0),
    new_SMR     = SMR     - shift(SMR,     type="lag", fill=0)
  ),
  by = Pathway
]

# 3) Melt to event-level rows
events <- melt(
  incap_dt,
  id.vars     = c("Pathway","Year"),
  measure.vars= patterns("^new_"),
  value.name  = "new_capacity_MW"
)
events[, Technology := fifelse(variable=="new_Solar",   "Solar",
                               fifelse(variable=="new_Onshore", "Onshore",
                                               fifelse(variable=="new_SMR",     "SMR", NA_character_))) ]
events[, variable := NULL]
events <- events[new_capacity_MW > 0]
events <- events[Year != 2024]

# 4) Join CAPEX rates and compute total CAPEX per event
events <- merge(
  events, capex_non_fossil_dt,
  by = c("Technology"), all.x = TRUE
)
events[, capex_total := new_capacity_MW * 1000 * capex_per_kW]

# 5) Load & clean county tax rates for New England
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
    effective_rate = as.numeric(gsub("%","", eff_rate_str)) / 100,
    state_abbr     = state.abb[match(state_full, state.name)]
  ) %>%
  filter(state_abbr %in% c("ME","NH","VT","MA","RI","CT")) %>%
  select(county, effective_rate)

# 6) Monte Carlo: assign each event to a random county and sum tax revenue by pathway
# 6) Monte Carlo: sample tax rows by index, attach rates directly, and summarize
set.seed(42)
n_sim    <- 1000L
n_events <- nrow(events)
sim_list <- vector("list", n_sim)

for (i in seq_len(n_sim)) {
  # draw row-indices from tax (one row per NE county)
  idx       <- sample(nrow(tax), size = n_events, replace = TRUE)
  assigned  <- tax[idx]          # data.table of length n_events
  
  # copy events and tack on the sampled tax info
  sim_dt <- copy(events)[
    , `:=`(
      county         = assigned$county,
      state_abbr     = assigned$state_abbr,
      effective_rate = assigned$effective_rate
    )
  ]
  
  # compute tax revenue = CAPEX_total * rate
  sim_dt[, tax_rev := capex_total * effective_rate]
  
  # sum up by pathway
  summary_dt <- sim_dt[, .(tax_rev = sum(tax_rev, na.rm = TRUE)), by = Pathway]
  summary_dt[, sim := i]
  
  sim_list[[i]] <- summary_dt
}

sim_dt      <- rbindlist(sim_list)
nonfossil_stats <- sim_dt[
  , .(
    mean_tax_rev = mean(tax_rev)/1e9,
    min_tax_rev  = min(tax_rev)/1e9,
    max_tax_rev  = max(tax_rev)/1e9
  ),
  by = Pathway
]

# 7) Save only the summary statistics
fwrite(
  nonfossil_stats,
  "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results/Non_Fossil_Tax_Revenue.csv"
)

