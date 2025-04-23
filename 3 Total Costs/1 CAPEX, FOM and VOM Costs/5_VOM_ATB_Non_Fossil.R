# Load libraries
library(data.table)
library(readxl)

# NPV Calculator
calculate_npv <- function(dt, rate, base_year) {
  npv <- sum(dt[, 2] / (1 + rate)^(dt[, 1] - base_year))
  return(npv)
}

discount_rate <- 0.07
base_year <- 2024

# Load ATB Costs (Note ATB 2023 used)
ATBe <- fread("/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/NREL ATB/ATBe_2024.csv")
ATB_scenarios <- c("Advanced", "Moderate", "Conservative")
ATBe[, V1 := NULL]

# Load Generation data
#-- Stepwise
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"

Yearly_Results <- as.data.table(fread(file_path))
Yearly_Results[, V1 := NULL]
Yearly_Results[, V1 := NULL]

# Define a function to process non-fossil VOM
process_non_fossil <- function(technology, techdetail, dataset, column_name, simulation, pathway) {
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
  
  filtered_dataset <- dataset[Simulation == simulation & Pathway == pathway]
  var_om_data <- merge(var_om_data, filtered_dataset, by.x = "core_metric_variable", by.y = "Year")
  var_om_data[, Var_OM := get(column_name) * value * 1e6] # MWh to TWh
  
  Var_om_npv <- numeric(length(ATB_scenarios))
  
  for (i in seq_along(ATB_scenarios)) {
    Var_om_scenario_data <- var_om_data[scenario == ATB_scenarios[i], .(core_metric_variable, Var_OM)]
    Var_om_npv[i] <- calculate_npv(Var_om_scenario_data, discount_rate, base_year)
  }
  
  names(Var_om_npv) <- ATB_scenarios
  
  list(Var_OM_NPV = Var_om_npv)
}

simulations <- unique(Yearly_Results$Simulation)
pathways <- unique(Yearly_Results$Pathway)

# Process each tech for each combination of simulation and Pathway
technologies <- list(
  list(tech = "Nuclear", detail = "Nuclear - Large", column_name = "Nuclear_TWh"),
  list(tech = "Nuclear", detail = "Nuclear - Small", column_name = "SMR_TWh"),
  list(tech = "Biopower", detail = "Dedicated", column_name = "Biomass_TWh")
)

npv_results <- list()
library(parallel)

# Function to process in parallel
process_parallel <- function(tech_info, Yearly_Results, sim, path) {
  process_non_fossil(tech_info$tech, tech_info$detail, Yearly_Results, tech_info$column_name, sim, path)
}

# Pre-allocate npv_results list
npv_results <- list()

# Number of cores available
num_cores <- detectCores() - 1

# Outer loops remain as they are
for (sim in simulations) {
  for (path in pathways) {
    # Run the innermost loop in parallel
    result_list <- mclapply(technologies, process_parallel, Yearly_Results = Yearly_Results, sim = sim, path = path, mc.cores = num_cores)
    
    # Combine the results into npv_results
    for (i in seq_along(result_list)) {
      npv_results[[paste0(sim, "_", path, "_VOM_", technologies[[i]]$column_name)]] <- result_list[[i]]$Var_OM_NPV
    }
  }
}



for (sim in simulations) {
  for (path in pathways) {
    for (tech_info in technologies) {
      result <- process_non_fossil(tech_info$tech, tech_info$detail, Yearly_Results, tech_info$column_name, sim, path)
      npv_results[[paste0(sim, "_", path, "_VOM_", tech_info$column_name)]] <- result$Var_OM_NPV
    }
  }
}

# Combine NPV results into a single data.table
combined_npvs <- rbindlist(lapply(names(npv_results), function(name) {
  parts <- strsplit(name, "_")[[1]]
  data.table(Simulation = parts[1], Pathway = parts[2], ATB_Scenario = names(npv_results[[name]]), NPV = npv_results[[name]], Technology = parts[4])
}), fill = TRUE)

combined_npvs <- combined_npvs[NPV != 0,]

combined_npvs_summary_2 <- combined_npvs[NPV != 0, .(
  mean_NPV = mean(NPV, na.rm = TRUE)/1e9,
  sd_NPV = sd(NPV, na.rm = TRUE)/1e9
), by = .(Pathway, Technology)] 

combined_npvs_summary_again_2 <- combined_npvs_summary_2[, .(
  sum_NPV = sum(mean_NPV, na.rm = TRUE)
), by = .(Pathway)] 

# Save combined NPV results to a single CSV file
write.csv(combined_npvs, file = file.path(output_path, "VOM_Non_Fossil.csv"), row.names = FALSE)
