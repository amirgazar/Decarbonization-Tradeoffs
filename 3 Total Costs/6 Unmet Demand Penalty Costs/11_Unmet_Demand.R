# Load libraries
library(data.table)
library(readxl)
library(tidyverse)


# FUNCTIONS
# NPV Calculator
calculate_npv <- function(dt, rate, base_year, col) {
  npv <- sum(dt[[col]] / (1 + rate)^(dt[["Year"]] - base_year))
  return(npv)
}

discount_rate <- 0.025
base_year <- 2024


# 2025 EVOLL Cost ISO NE : https://www.iso-ne.com/static-assets/documents/100012/iso-ne-2023-emm-report-final.pdf
EVOLL_2025 <- 9337 # $/MWh 2024 USD effective in 2025
EVOLL_2025 <- 3500 # $/MWh 2024 USD current

# Load Results
#-- Stepwise
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results_Shortages.csv"
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"

Yearly_Results <- fread(file_path)

Yearly_Results$Unmet_Demand_USD_total <- Yearly_Results$Unmet_Demand_total_MWh * EVOLL_2025

# Define a function to process Costs
process_unmet_demand <- function(sim, path, data, col) {
  filtered_data <- data[Simulation == sim & Pathway == path]
  npv <- calculate_npv(filtered_data, discount_rate, base_year, col)
  return(npv)
}

# NPV Calculation
simulations <- unique(Yearly_Results$Simulation)
scenarios <- unique(Yearly_Results$Pathway)
cost_cols <- c("Unmet_Demand_USD_total")
npv_results <- list()

for (sim in simulations) {
  for (scen in scenarios) {
    for (col in cost_cols) {
      npv <- process_unmet_demand(sim, scen, Yearly_Results, col)
      npv_results[[paste0(sim, "_", scen,"_", col)]] <- npv
    }
  }
}

combined_npvs <- rbindlist(
  lapply(names(npv_results), function(name) {
    parts <- strsplit(name, "_")[[1]]
    npv_value <- npv_results[[name]]
    if (npv_value != 0) {
      data.table(Simulation = parts[1], Pathway = parts[2], NPV = npv_value)
    } else {
      NULL  # Exclude rows with NPV = 0
    }
  }),
  fill = TRUE
)


combined_npvs_summary_2 <- combined_npvs[NPV != 0, .(
  mean_NPV = mean(NPV, na.rm = TRUE)/1e9,
  sd_NPV = sd(NPV, na.rm = TRUE)/1e9
), by = .(Pathway)] 


# Save combined NPV results to a single CSV file
write.csv(combined_npvs, file = file.path(output_path, "Unmet_demand.csv"), row.names = FALSE)


