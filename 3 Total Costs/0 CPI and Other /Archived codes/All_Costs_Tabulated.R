# Load libraries
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(scales)
library(future)
library(future.apply)
library(tools)

# helper to format billions‐USD with commas (and one decimal)
format_cost <- function(x) {
  scales::comma(x, accuracy = 0.1)
}


# Typical Cost Items
#CAPEX_FOM: Fossil, Non Fossil, Imports
#VOM: Fossil, Non Fossil, Imports
#Fuels: Fossil, Non Fossil
#GHG: CO2, N2O, CH4
#Air_emissions: NOx, SO2, PM2.5, PM10, CO
#Unmet demand
# Other Costs:
#Beyond the fenceline costs Canadian Side costs; New Hydro CAPEX, FOM, CH4 Emissions

# Run all files in the background and simulatniously assign discount rate
# plan(multisession)
# base_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/"
# folders <- c("1 CAPEX, FOM and VOM Costs", "2 Fuel Costs", "3 Imports Costs",
#              "4 GHG Emissions Costs", "5 Air Pollutant Emissions Costs", "6 Unmet Demand Penalty Costs",
#              "7 Beyond the Fence Line Costs")
# 
# files <- unlist(lapply(folders, function(subfolder) {
#   list.files(
#     path = file.path(base_path, subfolder),
#     pattern = "\\.R$",
#     full.names = TRUE,
#     recursive = TRUE  # Remove or set to FALSE if you don't need to search subdirectories
#   )
# }))
# 
# futures <- lapply(files, function(file) future({discount_rate <- 0.03; source(file) }))
# all_done <- all(sapply(futures, FUN = resolved))
# if (all_done) {
#   message("All files have been executed.")
# } else {
#   message("Some files are still running.")
# }

# Listing all files in the folder and subfolders
#--- Stepwise
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/10 Total Costs Results Final"
folder_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/3 Total Costs/9 Total Costs Results"

# list only *.csv files in that folder, not subfolders
csv_files <- list.files(
  path       = folder_path,
  pattern    = "\\.csv$",
  full.names = TRUE,
  recursive  = FALSE
)

# read each one and assign to global env
data_tables <- lapply(csv_files, function(file) {
  dt   <- fread(file)
  name <- file_path_sans_ext(basename(file))
  assign(name, dt, envir = .GlobalEnv)
  dt
})


# $ to b$
billion <- 1e9

# CAPEX and FOM
# Fossil
total_CAPEX_Fixed_Fossil <- CAPEX_Fixed_Fossil %>%
  rename(Cost_Category = Scenario) %>%  # Rename Cost_Type to Cost_Category
  group_by(Cost_Category, Pathway) %>%  # Use Cost_Category in grouping
  summarise(
    FOM = sum(ifelse(Cost_Type == "FOM", NPV, 0)), 
    CAPEX = sum(ifelse(Cost_Type == "CAPEX", NPV, 0)), 
    .groups = 'drop'
  ) %>%
  mutate(Technology = "Fossil Fuels")

# Imports
total_CAPEX_FOM_Imports <- CAPEX_FOM_Imports %>%
  group_by(Pathway, Cost_Type) %>%
  summarise(FOM = sum(NPV_FOM), CAPEX = sum(NPV_CAPEX), .groups = 'drop') %>%
  rename(Cost_Category = Cost_Type)

mean_values <- total_CAPEX_FOM_Imports %>%
  group_by(Pathway) %>%
  summarise(FOM = mean(FOM), CAPEX = mean(CAPEX))

total_CAPEX_FOM_Imports <- total_CAPEX_FOM_Imports %>%
  bind_rows(mean_values %>% mutate(Cost_Category = "Mean")) %>%
  mutate(Technology = "Imports") 

total_CAPEX_FOM_Imports <- total_CAPEX_FOM_Imports %>%
  mutate(Cost_Category = case_when(
    Cost_Category == "Lower" ~ "Advanced",
    Cost_Category == "Mean" ~ "Moderate",
    Cost_Category == "Upper" ~ "Conservative",
    TRUE ~ Cost_Category
  ))

# Non Fossil
total_CAPEX_Fixed_Non_Fossil <- CAPEX_Fixed_Non_Fossil %>%
  group_by(Pathway, ATB_Scenario, Technology) %>%
  summarise(
    FOM = sum(ifelse(Cost_Type == "FOM", NPV, 0)), 
    CAPEX = sum(ifelse(Cost_Type == "CAPEX", NPV, 0)), 
    .groups = 'drop'
  ) %>%
  rename(Cost_Category = ATB_Scenario)

# Combine
total_CAPEX_FOM <- bind_rows(total_CAPEX_Fixed_Fossil, total_CAPEX_Fixed_Non_Fossil, total_CAPEX_FOM_Imports)

total_CAPEX_FOM <- total_CAPEX_FOM %>%
  group_by(Pathway, Cost_Category) %>%
  summarize(
    FOM_bUSD = sum(FOM)/billion,
    CAPEX_bUSD = sum(CAPEX)/billion,
    .groups = 'drop'
  )

# Variable Costs
# Fossil 
total_VOM_Fossil <- VOM_Fossil %>%
  group_by(Pathway, ATB_Scenario) %>%
  summarise(VOM = sum(NPV), .groups = 'drop') %>%
  rename(Cost_Category = ATB_Scenario)
total_VOM_Fossil <- total_VOM_Fossil %>%
  mutate(Technology = "Fossil Fuels")
total_VOM_Fossil <- total_VOM_Fossil %>%
  group_by(Pathway, Cost_Category, Technology) %>%
  summarise(
    VOM_max  = max(VOM),
    VOM_mean = mean(VOM),
    VOM_min  = min(VOM),
    .groups  = 'drop'
  )

# Non Fossil
total_VOM_Non_Fossil <- VOM_Non_Fossil %>%
  group_by(Simulation, Pathway, ATB_Scenario, Technology) %>%
  summarise(VOM = sum(NPV), .groups = 'drop') %>%
  rename(Cost_Category = ATB_Scenario) %>%
  mutate(Technology = case_when(
    Technology == "Bio.gen" ~ "Biopower",
    Technology == "SMR.gen" ~ "SMR",
    Technology == "Nuclear.gen" ~ "Nuclear",
    TRUE ~ Technology
  ))
total_VOM_Non_Fossil <- total_VOM_Non_Fossil %>%
  group_by(Pathway, Cost_Category, Technology) %>%
  summarise(
    VOM_max  = max(VOM),
    VOM_mean = mean(VOM),
    VOM_min  = min(VOM),
    VOM_sd   = sd(VOM),
    .groups  = 'drop'
  )

# Combine
total_VOM <- bind_rows(total_VOM_Fossil, total_VOM_Non_Fossil)
total_VOM <- total_VOM %>%
  group_by(Pathway, Cost_Category) %>%
  summarize(
    VOM_max_bUSD = sum(VOM_max)/billion,
    VOM_mean_bUSD = sum(VOM_mean)/billion,
    VOM_min_bUSD = sum(VOM_min)/billion,
    VOM_sd_bUSD = sum(VOM_sd)/billion,
    .groups = 'drop'
  )

# Imports Costs
# Imports
total_Imports <- Imports %>%
  group_by(Simulation, Pathway, Cost_Type, Jurisdiction) %>%
  summarise(Imports = sum(NPV), .groups = 'drop') %>%
  rename(Cost_Category = Cost_Type)

mean_values <- total_Imports %>%
  group_by(Simulation, Pathway, Jurisdiction) %>%
  summarise(Imports = mean(Imports))
total_Imports <- total_Imports %>%
  bind_rows(mean_values %>% mutate(Cost_Category = "Mean")) %>%
  mutate(Technology = "Imports") 
total_Imports <- total_Imports %>%
  mutate(Cost_Category = case_when(
    Cost_Category == "Lower" ~ "Advanced",
    Cost_Category == "Mean" ~ "Moderate",
    Cost_Category == "Upper" ~ "Conservative",
    TRUE ~ Cost_Category
  ))

total_Imports <- total_Imports %>%
  group_by(Simulation, Pathway, Cost_Category, Jurisdiction) %>%
  summarize(
    Imports_bUSD = sum(Imports)/billion,
    .groups = 'drop'
  )

# Fuel Costs
# Fossil
total_Fuel_Fossil <- Fuel_Fossil %>%
  group_by(Pathway, Technology) %>%
  summarise(Fuel_mean = mean(NPV), 
            Fuel_max = max(NPV), 
            Fuel_min = min(NPV), 
            .groups = 'drop') 

total_Fuel_Fossil <- total_Fuel_Fossil %>%
  group_by(Pathway) %>%
  summarize(
    Fuel_mean_bUSD = sum(Fuel_mean)/billion,
    Fuel_max_bUSD = sum(Fuel_max)/billion,
    Fuel_min_bUSD = sum(Fuel_min)/billion,
    .groups = 'drop'
  )

# Non Fossil
total_Fuel_Non_Fossil <- Fuel_Non_Fossil %>%
  group_by(Pathway, ATB_Scenario, Technology) %>%
  summarise(Fuel_mean = mean(NPV), 
            Fuel_max = max(NPV), 
            Fuel_min = min(NPV), 
            .groups = 'drop') %>%
  rename(Cost_Scenario = ATB_Scenario)

total_Fuel_Non_Fossil <- total_Fuel_Non_Fossil %>%
  group_by(Pathway, Cost_Scenario) %>%
  summarize(
    Fuel_mean_bUSD = sum(Fuel_mean)/billion,
    Fuel_max_bUSD = sum(Fuel_max)/billion,
    Fuel_min_bUSD = sum(Fuel_min)/billion,
    .groups = 'drop'
  )

# GHG Emissions
total_GHG_Emissions <- GHG_Emissions %>%
  group_by(Pathway) %>%
  summarize(
    GHG_mean_bUSD = NPV_mean/billion,
    GHG_max_bUSD = NPV_max/billion,
    GHG_min_bUSD = NPV_min/billion,
    .groups = 'drop'
  )


# Air Emissions
total_Air_Emissions <- Air_Emissions_ALL %>%
  group_by(Pathway) %>%
  summarize(
    Air_emissions_mean_bUSD = mean_NPV/billion,
    Air_emissions_max_bUSD = max_NPV/billion,
    Air_emissions_min_bUSD = min_NPV/billion,
    .groups = 'drop'
  )

# Unmet Demand
total_unmet_demand <- transform(Unmet_demand, Unmet_demand_bUSD = NPV / billion)

## Analysis
# Calculate range (max, mean, min) for each total cost and scenario

# CAPEX and FOM
range_CAPEX_FOM <- total_CAPEX_FOM %>%
  group_by(Pathway) %>%
  summarise(
    FOM_max = max(FOM_bUSD),
    FOM_mean = mean(FOM_bUSD),
    FOM_min = min(FOM_bUSD),
    CAPEX_max = max(CAPEX_bUSD),
    CAPEX_mean = mean(CAPEX_bUSD),
    CAPEX_min = min(CAPEX_bUSD)
  ) %>%
  gather(key = "Metric", value = "Cost_bUSD", FOM_max:FOM_min, CAPEX_max:CAPEX_min) %>%
  separate(Metric, into = c("Cost_Type", "Statistic"), sep = "_") %>%
  mutate(Category = "CAPEX and FOM")

# Variable O&M
range_VOM <- total_VOM %>%
  group_by(Pathway) %>%
  summarise(
    max_val  = max(VOM_max_bUSD, na.rm = TRUE),
    mean_val = mean(VOM_mean_bUSD, na.rm = TRUE),
    min_val  = min(VOM_min_bUSD, na.rm = TRUE)
  ) %>%
  # Pivot the summary to long format with one row per statistic.
  pivot_longer(
    cols = c(max_val, mean_val, min_val),
    names_to = "Statistic",
    values_to = "Cost_bUSD"
  ) %>%
  # Adjust the Statistic names and add constant columns.
  mutate(
    Statistic = recode(Statistic, 
                       max_val  = "max", 
                       mean_val = "mean", 
                       min_val  = "min"),
    Cost_Type = "VOM",
    Category  = "VOM"  # Change this label if needed.
  ) %>%
  select(Pathway, Cost_Type, Statistic, Cost_bUSD, Category)


# Imports
range_Imports <- total_Imports %>%
  group_by(Pathway, Jurisdiction) %>%
  summarise(
    Imports_max = max(Imports_bUSD),
    Imports_mean = mean(Imports_bUSD),
    Imports_min = min(Imports_bUSD)
  ) %>%
  gather(key = "Metric", value = "Cost_bUSD", Imports_max:Imports_min) %>%
  separate(Metric, into = c("Cost_Type", "Statistic"), sep = "_") %>%
  mutate(Category = "Imports")

# Filter rows where Pathway is 'B3'
B3_rows <- range_Imports %>%
  filter(Pathway == "B3")

# Create a new data frame for B3-A by replacing the 'Pathway' column with 'B3-A'
# and set the Cost_bUSD to zero where Jurisdiction is 'QC'
B3_A_rows <- B3_rows %>%
  mutate(Pathway = "B3-A",
         Cost_bUSD = ifelse(Jurisdiction == "QC", 0, Cost_bUSD))

# Bind the new rows to the original data frame
range_Imports <- bind_rows(range_Imports, B3_A_rows)

# Summarize the data by Pathway, Cost_Type, Statistic, and Category
range_Imports <- range_Imports %>%
  group_by(Pathway, Cost_Type, Statistic, Category) %>%
  summarize(Cost_bUSD = sum(Cost_bUSD, na.rm = TRUE), .groups = "drop")

# Fuel Costs
range_Fuel_Non_Fossil <- total_Fuel_Non_Fossil %>%
  group_by(Pathway) %>%
  summarise(
    Fuel_max = max(Fuel_max_bUSD),
    Fuel_mean = mean(Fuel_mean_bUSD),
    Fuel_min = min(Fuel_min_bUSD)
  ) %>%
  gather(key = "Metric", value = "Cost_bUSD", Fuel_max:Fuel_min) %>%
  separate(Metric, into = c("Cost_Type", "Statistic"), sep = "_") %>%
  mutate(Category = "Non_Fossil")

range_Fuel_Fossil <- total_Fuel_Fossil %>%
  group_by(Pathway) %>%
  summarise(
    Fuel_max = max(Fuel_max_bUSD),
    Fuel_mean = mean(Fuel_mean_bUSD),
    Fuel_min = min(Fuel_min_bUSD)
  ) %>%
  gather(key = "Metric", value = "Cost_bUSD", Fuel_max:Fuel_min) %>%
  separate(Metric, into = c("Cost_Type", "Statistic"), sep = "_") %>%
  mutate(Category = "Fossil")

range_Fuel <- bind_rows(range_Fuel_Non_Fossil, range_Fuel_Fossil)

range_Fuel <- range_Fuel %>%
  group_by(Pathway, Cost_Type, Statistic) %>%
  summarize(Cost_bUSD = sum(Cost_bUSD), .groups = 'drop')

# GHG Emissions
range_GHG <- total_GHG_Emissions %>%
  group_by(Pathway) %>%
  summarise(
    GHG_max = max(GHG_max_bUSD),
    GHG_mean = mean(GHG_mean_bUSD),
    GHG_min = min(GHG_min_bUSD)
  ) %>%
  gather(key = "Metric", value = "Cost_bUSD", GHG_max:GHG_min) %>%
  separate(Metric, into = c("Cost_Type", "Statistic"), sep = "_") %>%
  mutate(Category = "GHG Emissions")

# Air Emissions
range_Air <- total_Air_Emissions %>%
  group_by(Pathway) %>%
  summarise(
    Air_max = max(Air_emissions_max_bUSD),
    Air_mean = mean(Air_emissions_mean_bUSD),
    Air_min = min(Air_emissions_min_bUSD)
  ) %>%
  gather(key = "Metric", value = "Cost_bUSD", Air_max:Air_min) %>%
  separate(Metric, into = c("Cost_Type", "Statistic"), sep = "_") %>%
  mutate(Category = "Air Emissions")

# Unmet Demand
range_Unmet_demand <- total_unmet_demand %>%
  group_by(Pathway) %>%
  summarise(
    UD_max = max(Unmet_demand_bUSD),
    UD_mean = mean(Unmet_demand_bUSD),
    UD_min = min(Unmet_demand_bUSD)
  ) %>%
  gather(key = "Metric", value = "Cost_bUSD", UD_max:UD_min) %>%
  separate(Metric, into = c("Cost_Type", "Statistic"), sep = "_") %>%
  mutate(Category = "Unmet Demand")

# Canadian Side costs
# Outside the fence line
# Calculate mean, min, and max for NPV_CAPEX
capex_stats <- CAPEX_FOM_CAN_Hydro %>%
  reframe(
    Statistic = c("min", "mean", "max"),
    Cost_bUSD = c(min(NPV_CAPEX), mean(NPV_CAPEX), max(NPV_CAPEX)) / 1e9,
    Cost_Type = "CAPEX",
    Category = "CAN Hydro CAPEX"
  )

# Calculate mean, min, and max for NPV_FOM using reframe()
fom_stats <- CAPEX_FOM_CAN_Hydro %>%
  reframe(
    Statistic = c("min", "mean", "max"),
    Cost_bUSD = c(min(NPV_FOM), mean(NPV_FOM), max(NPV_FOM)) / 1e9,
    Cost_Type = "FOM",
    Category = "CAN Hydro FOM"
  )


# Calculate mean, min, and max for NPV_CH4
ch4_stats <- CH4_CAN_Hydro %>%
  reframe(
    Statistic = c("min", "mean", "max"),
    Cost_bUSD = c(NPV_CH4_Lower, mean(c(NPV_CH4_Lower, NPV_CH4_Upper)), NPV_CH4_Upper) / 1e9,
    Cost_Type = "GHG",
    Category = "CAN Hydro CH4"
  )

# Combine CAPEX and FOM statistics
range_other_costs <- bind_rows(capex_stats, fom_stats, ch4_stats) %>%
  mutate(Pathway = "B3-A")

# Combine all ranges into a single data frame
combined_ranges <- bind_rows(
  range_CAPEX_FOM,
  range_VOM,
  range_Imports,
  range_Fuel,
  range_GHG,
  range_Air,
  range_Unmet_demand,
  range_other_costs
)

# Filter rows corresponding to B3
B3_rows <- combined_ranges %>%
  filter(Pathway == "B3", Cost_Type != "Imports")

# Duplicate the B3 rows and change the Pathway to B3-A
B3_A_rows <- B3_rows %>% mutate(Pathway = "B3-A")

# Combine the original data with the new B3-A rows
combined_ranges <- bind_rows(combined_ranges, B3_A_rows)


# Rename scenarios
# Reorder the Cost_Type factor levels and rename them
combined_ranges$Cost_Type <- factor(combined_ranges$Cost_Type, 
                                    levels = c("UD", "Air", "GHG", "Fuel","Imports", "VOM", "FOM", "CAPEX"),
                                    labels = c("Unmet Demand Penalty", "Air Emissions", "GHG Emissions", "Fuel", "Imports", "Variable O&M", "Fixed O&M", "CAPEX"))

# Add BTF costs to each category
combined_ranges <- combined_ranges %>%
  group_by(Pathway, Statistic, Cost_Type) %>%
  summarise(Cost_bUSD = sum(Cost_bUSD), .groups = 'drop')

#--- Plotting ---
# Calculate total costs (max, mean, min) across all categories for each scenario
total_costs <- combined_ranges %>%
  group_by(Pathway, Statistic) %>%
  summarise(Total_Cost_bUSD = sum(Cost_bUSD), .groups = 'drop')

# Extract mean, min, and max separately for plotting
mean_costs <- total_costs %>% filter(Statistic == "mean")
min_costs <- total_costs %>% filter(Statistic == "min")
max_costs <- total_costs %>% filter(Statistic == "max")

# Create the stacked bar plot with range bars for the total cost
plot_total_costs <- ggplot() +
  geom_bar(data = combined_ranges %>% filter(Statistic == "mean"), 
           aes(x = Pathway, y = Cost_bUSD, fill = Cost_Type), 
           stat = "identity", position = "stack") +
  geom_errorbar(data = mean_costs, 
                aes(x = Pathway, ymin = min_costs$Total_Cost_bUSD, ymax = max_costs$Total_Cost_bUSD), 
                width = 0.2, color = "black") +
  geom_text(data = mean_costs, 
            aes(x = Pathway, y = max_costs$Total_Cost_bUSD, 
                label = format_cost(max_costs$Total_Cost_bUSD)), 
            vjust = -0.5, color = "black", size = 5) +  # Increased text size
  geom_text(data = mean_costs, 
            aes(x = Pathway, y = min_costs$Total_Cost_bUSD, 
                label = format_cost(min_costs$Total_Cost_bUSD)), 
            vjust = 1.5, color = "black", size = 5) +  # Increased text size
  labs(title = "Total 2024-NPV for Decarbonization Pathways in New England [2025-2050]", 
       y = "NPV (billions-USD 2024)", 
       x = "Pathway") +
  theme_minimal() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = comma) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),  # Increased plot title size
    axis.title.x = element_text(size = 16, face = "bold"),  # Increased x-axis title size
    axis.title.y = element_text(size = 18, face = "bold"),  # Increased y-axis title size
    axis.text.x = element_text(size = 12),  # Increased x-axis text size
    axis.text.y = element_text(size = 14),  # Increased y-axis text size
    legend.title = element_text(size = 14),  # Increased legend title size
    legend.text = element_text(size = 12)  # Increased legend text size
  )

plot_total_costs

# Save the final data set
#write.csv(combined_ranges,  file = file.path(output_path, "All_Costs.csv"), row.names = FALSE)

# ─── Compare results with ISO-NE Findings
# ─── 1. ISO-NE DATA & INTERPOLATION ────────────────────────────────────────────────
known_years     <- c(2025.05, 2030.11, 2035.47, 2040.03, 2045.17, 2050.04)
known_usd_2018  <- c(  20.48,   20.72,   19.98,   19.28,   19.29,   19.77)
target_years    <- 2025:2050

# piece‐wise linear (end‐extrapolate)
iso_interp <- approx(x = known_years,
                     y = known_usd_2018,
                     xout = target_years,
                     rule = 2)

iso_df <- data.frame(
  Year      = iso_interp$x,
  USD_2018  = iso_interp$y
)

# ─── 2. INFLATE TO 2024$ & DISCOUNT TO NPV ────────────────────────────────────────
cpi_factor     <- 1.23375    # 2018→2024 CPI
discount_rate  <- 0.03

iso_df <- iso_df %>%
  mutate(
    USD_2024       = USD_2018 * cpi_factor,
    DiscountFact   = (1 + discount_rate)^(Year - 2024),
    DiscountedUSD  = USD_2024 / DiscountFact
  )

npv_iso_ne <- sum(iso_df$DiscountedUSD)

# ─── 3. PULL PATHWAY B1 NPV ───────────────────────────────────────
# Compute total NPVs for B1 (mean, max, min)
npv_B1_list <- combined_ranges %>%
  filter(Pathway == "B1", Statistic %in% c("mean", "max", "min")) %>%
  group_by(Statistic) %>%
  summarise(Total_NPV = sum(Cost_bUSD), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = Statistic, values_from = Total_NPV) %>%
  as.list()


## ---- Cost per household ----
n_households_NE  <- 6153123  # Source: https://censusreporter.org/profiles/03000US1-new-england-division/
discount_rate     <- 0.03     # 3%
n_years           <- 2050 - 2025 + 1        # levelize over n years

# annuity factor: A = PV × [r / (1 – (1 + r)^−n)]
annuity_factor <- discount_rate / (1 - (1 + discount_rate)^(-n_years))

annual_costs <- total_costs %>%
  # 1) PV per household at 2025 dollars
  mutate(cost_per_household = Total_Cost_bUSD * 1e9 / n_households_NE,
         # 2) levelized annual cost (2025$) over n years
         annual_cost_per_household = cost_per_household * annuity_factor,
         # 3) levelized monthly cost (2025$)
         monthly_cost_per_household = annual_cost_per_household / 12)