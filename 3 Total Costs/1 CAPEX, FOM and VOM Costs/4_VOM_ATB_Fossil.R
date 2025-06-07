# Load libraries
library(data.table)
library(readxl)

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

# Load Results - Facility level gen (old facilities)
#-- Stepwise
file_path_1 <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Facility_Level_Results.csv"
file_path_2 <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"

Yearly_Facility_Level_Results <- as.data.table(fread(file_path_1))
Yearly_Facility_Level_Results <- Yearly_Facility_Level_Results[Pathway %in% c("A", "D", "B1", "B2", "B3", "C1", "C2", "C3")]


Yearly_Results <- as.data.table(fread(file_path_2))

# Load facilities data
# Old/existing fossil fuels
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(file_path)
# New fossil fuels 
file_path <-"/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/2 New Fossil Fuels/1 New Fossil Fuels Facilities Data/New_Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC_new <- fread(file_path)
Fossil_Fuels_NPC_new <- Fossil_Fuels_NPC_new[1,]

# Merge
selected_cols <- Fossil_Fuels_NPC[, .(Facility_Unit.ID, Unit_Type)]
Yearly_Facility_Level_Results <- selected_cols[Yearly_Facility_Level_Results, on = "Facility_Unit.ID"]
# Create Fuel_Category column
Yearly_Facility_Level_Results[, Fuel_Category := fcase(
  grepl("oil", Fuel_type_1, ignore.case = TRUE), "Oil",
  grepl("coal", Fuel_type_1, ignore.case = TRUE), "Coal",
  grepl("wood", Fuel_type_1, ignore.case = TRUE), "Wood",
  grepl("gas", Fuel_type_1, ignore.case = TRUE) & grepl("Combustion Turbine", Unit_Type, ignore.case = TRUE), "Gas_CT",
  grepl("gas", Fuel_type_1, ignore.case = TRUE), "Gas_CC",
  default = "Other"
)]

# Summarize based on Fuel Type, year and pathway
Yearly_Facility_Level_Results <- Yearly_Facility_Level_Results[, .(
  Fossil_gen_MWh = sum(total_generation_GWh, na.rm = TRUE) * 1e3
), by = .(Year, Simulation, Pathway, Fuel_Category)]

Yearly_Facility_Level_Results <- Yearly_Facility_Level_Results[, .(
  Fossil_gen_MWh_mean = mean(Fossil_gen_MWh, na.rm = TRUE),
  Fossil_gen_MWh_max = max(Fossil_gen_MWh, na.rm = TRUE),
  Fossil_gen_MWh_min = min(Fossil_gen_MWh, na.rm = TRUE)
), by = .(Year, Pathway, Fuel_Category)]


# Summarize Hourly results for New Gas generation
Yearly_Results_summary <- Yearly_Results[, .(
  Fossil_gen_MWh = sum(New_Fossil_Fuel_TWh, na.rm = TRUE) * 1e6,
  Fuel_Category = "Gas_CC"
), by = .(Year, Simulation, Pathway)]

Yearly_Results_summary <- Yearly_Results_summary[, .(
  Fossil_gen_MWh_mean = mean(Fossil_gen_MWh, na.rm = TRUE),
  Fossil_gen_MWh_max = max(Fossil_gen_MWh, na.rm = TRUE),
  Fossil_gen_MWh_min = min(Fossil_gen_MWh, na.rm = TRUE)
), by = .(Year, Pathway, Fuel_Category)]

# Combine all data
Yearly_Facility_Level_Results <- rbind(Yearly_Facility_Level_Results, Yearly_Results_summary)
Yearly_Facility_Level_Results <- Yearly_Facility_Level_Results[, .(
  Fossil_gen_MWh_mean = sum(Fossil_gen_MWh_mean, na.rm = TRUE),
  Fossil_gen_MWh_max = sum(Fossil_gen_MWh_max, na.rm = TRUE),
  Fossil_gen_MWh_min = sum(Fossil_gen_MWh_min, na.rm = TRUE)
), by = .(Year, Pathway, Fuel_Category)]

  
# Define a function to process fossil VOM
process_fossil <- function(technology, techdetail, dataset, column_name, pathway) {
  # Define the variables for filtering
  technology_filter <- technology
  techdetail_filter <- techdetail
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
  
  var_om_data <- tech_data[core_metric_parameter == "Variable O&M"]

  filtered_dataset <- dataset[Pathway == pathway]
  var_om_data <- merge(var_om_data, filtered_dataset, by.x = "core_metric_variable", by.y = "Year")
  var_om_data[, Var_OM := get(column_name) * value] # MWh 
  
  Var_om_npv <- numeric(length(ATB_scenarios))
  
  for (i in seq_along(ATB_scenarios)) {
    Var_om_scenario_data <- var_om_data[scenario == ATB_scenarios[i], .(core_metric_variable, Var_OM)]
    Var_om_npv[i] <- calculate_npv(Var_om_scenario_data, discount_rate, base_year)
  }
  
  names(Var_om_npv) <- ATB_scenarios
  
  list(Var_OM_NPV = Var_om_npv)
}

pathways <- unique(Yearly_Facility_Level_Results$Pathway)

# Process each fossil fuel for each combination of simulation and Pathway
fossil_fuels <- list(
  list(tech = "Coal_FE", detail = "Coal-IGCC-90%-CCS", column_name = "Fossil_gen_MWh_mean", fuel_type = "Coal"),
  list(tech = "Biopower", detail = "Dedicated", column_name = "Fossil_gen_MWh_mean", fuel_type = "Wood"),
  list(tech = "NaturalGas_FE", detail = "NG 1-on-1 Combined Cycle (H-Frame)", column_name = "Fossil_gen_MWh_mean", fuel_type = "Gas_CC"),
  list(tech = "NaturalGas_FE", detail = "NG Combustion Turbine (F-Frame)", column_name = "Fossil_gen_MWh_mean", fuel_type = "Gas_CT"),
  list(tech = "NaturalGas_FE", detail = "NG Combustion Turbine (F-Frame)", column_name = "Fossil_gen_MWh_mean", fuel_type = "Oil")
)

npv_results_fossil <- list()

for (scen in pathways) {
  for (fossil_info in fossil_fuels) {
    fossil_data <- Yearly_Facility_Level_Results[Fuel_Category == fossil_info$fuel_type]
    result <- process_fossil(fossil_info$tech, fossil_info$detail, fossil_data, fossil_info$column_name, scen)
    npv_results_fossil[[paste0(scen, "_", fossil_info$fuel_type, "_VOM_NPV")]] <- result$Var_OM_NPV
  }
}

# Combine NPV results into a single data.table
combined_npvs_fossil <- rbindlist(lapply(names(npv_results_fossil), function(name) {
  parts <- strsplit(name, "_")[[1]]
  # Ensure the technology name captures both parts if it contains 'Gas_CC' or 'Gas_CT'
  technology <- ifelse(parts[2] == "Gas", paste(parts[2], parts[3], sep = "_"), parts[2])
  atb_scenario <- ifelse(technology == "Gas_CC" | technology == "Gas_CT", parts[4], parts[3])
  data.table(Pathway = parts[1], ATB_Scenario = ATB_scenarios, NPV = npv_results_fossil[[name]], Technology = technology)
}), fill = TRUE)

combined_npvs_fossil <- combined_npvs_fossil[NPV != 0,]

combined_npvs_summary_2 <- combined_npvs_fossil[NPV != 0, .(
  mean_NPV = mean(NPV, na.rm = TRUE)/1e9,
  sd_NPV = sd(NPV, na.rm = TRUE)/1e9
), by = .(Pathway, Technology)] 

combined_npvs_summary_again_2 <- combined_npvs_summary_2[, .(
  sum_NPV = sum(mean_NPV, na.rm = TRUE)
), by = .(Pathway)] 

# Save combined NPV results to a single CSV file
fwrite(combined_npvs_fossil, file = file.path(output_path, "VOM_Fossil.csv"), row.names = FALSE)

# --------------------------------------------------------------
# 1) Build a per-simulation generation table
# --------------------------------------------------------------
# (re-read raw facility-level results)
rawFLLR <- as.data.table(fread(file_path_1))
rawFLLR <- selected_cols[rawFLLR, on = "Facility_Unit.ID"]
rawFLLR[, Fuel_Category := fcase(
  grepl("oil", Fuel_type_1, ignore.case = TRUE), "Oil",
  grepl("coal", Fuel_type_1, ignore.case = TRUE), "Coal",
  grepl("wood", Fuel_type_1, ignore.case = TRUE), "Wood",
  grepl("gas", Fuel_type_1, ignore.case = TRUE) & grepl("Combustion Turbine", Unit_Type, ignore.case = TRUE), "Gas_CT",
  grepl("gas", Fuel_type_1, ignore.case = TRUE), "Gas_CC",
  default = "Other"
)]
# facility gen per simulation
gen_old <- rawFLLR[, .(
  Fossil_gen_MWh = sum(total_generation_GWh, na.rm = TRUE) * 1e3
), by = .(Year, Simulation, Pathway, Fuel_Category)]

# hourly‐based new gas gen per simulation
gen_new <- Yearly_Results[, .(
  Fossil_gen_MWh = sum(New_Fossil_Fuel_TWh, na.rm = TRUE) * 1e6
), by = .(Year, Simulation, Pathway)]
gen_new[, Fuel_Category := "Gas_CC"]

# combine them
gen_by_sim_all <- rbindlist(list(gen_old, gen_new), use.names = TRUE, fill = TRUE)

# --------------------------------------------------------------
# 2) Compute VOM‐NPV per Simulation & ATB scenario
# --------------------------------------------------------------
npv_results_by_sim <- list()

for (scen in pathways) {
  for (fossil_info in fossil_fuels) {
    # filter generation for this pathway & fuel
    fossil_data_sim <- gen_by_sim_all[
      Pathway == scen & Fuel_Category == fossil_info$fuel_type
    ]
    if (nrow(fossil_data_sim)==0) next
    
    # pull VOM rates from ATBe
    tech_data <- ATBe[
      technology       == fossil_info$tech &
        techdetail       == fossil_info$detail &
        core_metric_case == "Market" &
        crpyears         == 30 &
        core_metric_variable >= base_year
    ]
    var_om_param <- tech_data[core_metric_parameter == "Variable O&M"]
    
    # merge on year
    merged_sim <- merge(
      var_om_param,
      fossil_data_sim,
      by.x = "core_metric_variable",
      by.y = "Year",
      allow.cartesian = TRUE
    )
    merged_sim[, Var_OM := Fossil_gen_MWh * value]  # $ cost = MWh × $/MWh
    
    # discounted sum per ATB scenario & Simulation
    npv_sim <- merged_sim[
      , .(NPV = sum(
        Var_OM / (1 + discount_rate)^(core_metric_variable - base_year),
        na.rm = TRUE
      )
      ), by = .(scenario, Simulation)
    ]
    npv_sim[, `:=`(
      Pathway    = scen,
      Technology = fossil_info$fuel_type
    )]
    
    npv_results_by_sim[[paste0(scen, "_", fossil_info$fuel_type)]] <- npv_sim
  }
}

# combine and save
combined_npvs_by_sim <- rbindlist(npv_results_by_sim, use.names = TRUE, fill = TRUE)
fwrite(
  combined_npvs_by_sim,
  file = file.path(output_path, "VOM_Fossil_by_Simulation.csv"),
  row.names = FALSE
)

