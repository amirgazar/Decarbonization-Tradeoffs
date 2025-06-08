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
library(stringr)


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

# Grab all sims and paths
sims <- Imports %>% 
  distinct(Simulation)
paths <- Imports %>% 
  distinct(Pathway)

# CAPEX and FOM
# Fossil
total_CAPEX_FOM_Fossil <- CAPEX_Fixed_Fossil %>%
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
total_CAPEX_FOM <- bind_rows(total_CAPEX_FOM_Fossil, total_CAPEX_Fixed_Non_Fossil, total_CAPEX_FOM_Imports)

total_CAPEX_FOM <- total_CAPEX_FOM %>%
  group_by(Pathway, Cost_Category) %>%
  summarize(
    FOM_bUSD = sum(FOM)/billion,
    CAPEX_bUSD = sum(CAPEX)/billion,
    .groups = 'drop'
  )

total_CAPEX_FOM <- total_CAPEX_FOM %>%
  group_by(Pathway) %>%
  summarize(
    FOM_mean_bUSD = mean(FOM_bUSD),
    FOM_max_bUSD = max(FOM_bUSD),
    FOM_min_bUSD = min(FOM_bUSD),
    CAPEX_mean_bUSD = mean(CAPEX_bUSD),
    CAPEX_max_bUSD = max(CAPEX_bUSD),
    CAPEX_min_bUSD = min(CAPEX_bUSD),
    .groups = 'drop'
  ) 


# Expand CAPEX and FOM for every simulation
total_CAPEX_FOM <- total_CAPEX_FOM %>%
  tidyr::crossing(sims) %>%
  select(Simulation, everything())

total_CAPEX <- total_CAPEX_FOM %>%
  select(Simulation, Pathway, 
         CAPEX_mean_bUSD,
         CAPEX_max_bUSD,
         CAPEX_min_bUSD)

# FOM‐only summary
total_FOM  <- total_CAPEX_FOM %>%
  select(Simulation, Pathway, 
         FOM_mean_bUSD,
         FOM_max_bUSD,
         FOM_min_bUSD)

# Variable Costs
# Fossil 
total_VOM_Fossil <- VOM_Fossil_by_Simulation %>%
  group_by(Simulation, Pathway, scenario) %>%
  summarise(VOM = sum(NPV), .groups = 'drop') %>%
  rename(Cost_Category = scenario)
total_VOM_Fossil <- total_VOM_Fossil %>%
  mutate(Technology = "Fossil Fuels")

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

# Combine
total_VOM <- bind_rows(total_VOM_Fossil, total_VOM_Non_Fossil)
total_VOM <- total_VOM %>%
  group_by(Simulation, Pathway, Cost_Category) %>%
  summarize(
    VOM_bUSD = sum(VOM),
    .groups = 'drop'
  )

total_VOM <- total_VOM %>%
  group_by(Simulation, Pathway) %>%
  summarize(
    VOM_max_bUSD = max(VOM_bUSD)/billion,
    VOM_mean_bUSD = mean(VOM_bUSD)/billion,
    VOM_min_bUSD = min(VOM_bUSD)/billion,
    .groups = 'drop'
  )


# Imports Costs
# Imports
total_Imports <- Imports %>%
  group_by(Simulation, Pathway, Cost_Type) %>%
  summarise(Imports = sum(NPV), .groups = 'drop') %>%
  rename(Cost_Category = Cost_Type)
mean_values <- total_Imports %>%
  group_by(Simulation, Pathway) %>%
  summarise(Imports = mean(Imports))
total_Imports <- total_Imports %>%
  bind_rows(mean_values %>% mutate(Cost_Category = "Mean"))

total_Imports <- total_Imports %>%
  group_by(Simulation, Pathway) %>%
  summarize(
    Imports_max_bUSD = max(Imports)/billion,
    Imports_mean_bUSD = mean(Imports)/billion,
    Imports_min_bUSD = min(Imports)/billion,
    .groups = 'drop'
  )

# Imports for pathway B3
B3_rows <- Imports %>%
  filter(Pathway == "B3")
B3_2_Imports <- B3_rows %>%
  mutate(Pathway = "B3",
         NPV = ifelse(Jurisdiction == "QC", 0, NPV))
B3_2_Imports <- B3_2_Imports %>%
  group_by(Simulation, Pathway, Cost_Type) %>%
  summarise(Imports = sum(NPV), .groups = 'drop') %>%
  rename(Cost_Category = Cost_Type)
mean_values <- B3_2_Imports %>%
  group_by(Simulation, Pathway) %>%
  summarise(Imports = mean(Imports))
B3_2_Imports <- B3_2_Imports %>%
  bind_rows(mean_values %>% mutate(Cost_Category = "Mean"))
B3_2_Imports <- B3_2_Imports %>%
  group_by(Simulation, Pathway) %>%
  summarize(
    CAN_Imports_adj_max_bUSD = max(Imports)/billion,
    CAN_Imports_adj_mean_bUSD = mean(Imports)/billion,
    CAN_Imports_adj_min_bUSD = min(Imports)/billion,
    .groups = 'drop'
  )

# Fuel Costs
# Fossil
scenarios <- c("Advanced", "Conservative", "Moderate")
total_Fuel_Fossil <- Fuel_Fossil_per_Simulation %>%
  group_by(Simulation, Pathway) %>%
  summarise(
    Fuel_bUSD = sum_NPV,
    .groups = "drop"
  ) %>%
  crossing(Cost_Scenario = scenarios)

# Non Fossil
total_Fuel_Non_Fossil <- Fuel_Non_Fossil %>%
  group_by(Simulation, Pathway, ATB_Scenario) %>%
  summarise(Fuel_bUSD = sum(NPV)/billion, .groups = "drop") %>%
  rename(Cost_Scenario = ATB_Scenario)

total_Fuel <- bind_rows(total_Fuel_Non_Fossil, total_Fuel_Fossil)
total_Fuel <- total_Fuel %>%
  group_by(Simulation, Pathway, Cost_Scenario) %>%
  summarise(Fuel_bUSD = sum(Fuel_bUSD), 
            .groups = "drop")

total_Fuel <- total_Fuel %>%
  group_by(Simulation, Pathway,) %>%
  summarize(
    Fuel_mean_bUSD = mean(Fuel_bUSD),
    Fuel_max_bUSD = max(Fuel_bUSD),
    Fuel_min_bUSD = min(Fuel_bUSD),
    .groups = 'drop'
  )

# GHG Emissions
total_GHG_Emissions <- GHG_Emissions_per_simulation %>%
  group_by(Simulation, Pathway) %>%
  summarize(
    GHG_mean_bUSD = mean(NPV)/billion,
    GHG_max_bUSD = max(NPV)/billion,
    GHG_min_bUSD = min(NPV)/billion,
    .groups = 'drop'
  )

# Air Emissions
total_Air_Emissions <- Air_Emissions_Existing %>%
  group_by(Simulation, Pathway) %>%
  summarize(
    Air_emissions_mean_bUSD = mean(NPV)/billion,
    Air_emissions_max_bUSD = max(NPV)/billion,
    Air_emissions_min_bUSD = min(NPV)/billion,
    .groups = 'drop'
  ) 

# Unmet Demand
total_Unmet_demand <- Unmet_demand %>%
  group_by(Simulation, Pathway) %>%
  summarize(
    Unmet_demand_mean_bUSD = mean(NPV)/billion,
    Unmet_demand_max_bUSD = max(NPV)/billion,
    Unmet_demand_min_bUSD = min(NPV)/billion,
    .groups = 'drop'
  ) 


# Canadian Side costs
# Outside the fence line, Pathway B3(2)
total_CAN_range <- CAPEX_FOM_CAN_Hydro %>%
  summarise(
    CAN_CAPEX_max_bUSD = max(NPV_CAPEX) / billion,
    CAN_CAPEX_mean_bUSD = mean(NPV_CAPEX) / billion,
    CAN_CAPEX_min_bUSD = min(NPV_CAPEX) / billion,
    CAN_FOM_max_bUSD   = max(NPV_FOM)   / billion,
    CAN_FOM_mean_bUSD  = mean(NPV_FOM)   / billion,
    CAN_FOM_min_bUSD   = min(NPV_FOM)   / billion,
    .groups = "drop"
  ) %>%
  mutate(Pathway = "B3")  
total_CAN_CH4_range <- CH4_CAN_Hydro %>%
  summarise(
    CAN_CH4_max_bUSD  = NPV_CH4_Upper / billion,
    CAN_CH4_mean_bUSD = (NPV_CH4_Lower + NPV_CH4_Upper) / 2 / billion,
    CAN_CH4_min_bUSD  = NPV_CH4_Lower / billion,
    .groups = "drop"
  ) %>%
  mutate(Pathway = "B3")


# All Canada side costs + adjusted Imports
total_CAN <- dplyr::left_join(
  total_CAN_range,
  total_CAN_CH4_range,
  by = c("Pathway")
)
total_CAN <- total_CAN %>%
  tidyr::crossing(sims)

total_CAN <- merge(B3_2_Imports, total_CAN, by = c("Simulation","Pathway"))


full_grid <- expand_grid(
  Simulation = sims$Simulation,
  Pathway    = paths$Pathway
)
total_CAN_full <- full_grid %>%
  left_join(total_CAN, by = c("Simulation","Pathway"))
total_CAN_full <- total_CAN_full %>%
  mutate(
    across(
      starts_with("CAN_"),
      ~ replace_na(.x, 0)    
    )
  )


# -- Merging all datasets ---
sim_lists <- list(
  CAPEX   = total_CAPEX,
  FOM     = total_FOM,
  VOM     = total_VOM,
  Fuel    = total_Fuel,
  Imports = total_Imports,
  GHG     = total_GHG_Emissions,
  Air     = total_Air_Emissions,
  Unmet   = total_Unmet_demand,
  CAN     = total_CAN_full
)

# full‐join all the sim‐tables together on Simulation + Pathway
All_Costs <- purrr::reduce(sim_lists, full_join, by = c("Simulation","Pathway"))
All_Costs <- All_Costs[ complete.cases(All_Costs), ]


# Adjusting B3
# pull out the original B3 rows
B3_base <- All_Costs %>% filter(Pathway == "B3")

# make B3(1) with CAN_ cols zeroed
B3_1_rows <- B3_base %>%
  mutate(
    Pathway = "B3(1)",
    across(starts_with("CAN_"), ~ 0)
  )

# make B3(2) with Imports cols zeroed
B3_2_rows <- B3_base %>%
  mutate(
    Pathway = "B3(2)",
    across(starts_with("Imports"), ~ 0)
  )

# combine and replace in the full table
All_Costs_final <- All_Costs %>%
  filter(Pathway != "B3") %>%           # drop the old B3 rows
  bind_rows(B3_1_rows, B3_2_rows) %>%   # add the two new ones
  arrange(Simulation, Pathway)

#--- Total costs calculation ----
All_Costs_final <- All_Costs_final %>%
  mutate(
    Total_Costs_mean_bUSD = rowSums(select(., ends_with("_mean_bUSD")), na.rm = TRUE),
    Total_Costs_max_bUSD  = rowSums(select(., ends_with("_max_bUSD")),  na.rm = TRUE),
    Total_Costs_min_bUSD  = rowSums(select(., ends_with("_min_bUSD")),  na.rm = TRUE)
  )
# Eliminate any Simulation that we dont have all pathway results for 
all_paths <- All_Costs_final %>% 
  distinct(Pathway) %>% 
  nrow()
All_Costs_final <- All_Costs_final %>%
  group_by(Simulation) %>%
  filter(n_distinct(Pathway) == all_paths) %>%
  ungroup()
# add in difference between B1 and other pathways
All_Costs_final <- All_Costs_final %>%
  group_by(Simulation) %>%
  mutate(
    # grab B1 for this simulation
    B1_mean = Total_Costs_mean_bUSD[Pathway == "B1"],
    B1_max  = Total_Costs_max_bUSD[Pathway == "B1"],
    B1_min  = Total_Costs_min_bUSD[Pathway == "B1"],
    # subtract to get diffs
    B1_Diff_mean_bUSD = Total_Costs_mean_bUSD - B1_mean,
    B1_Diff_max_bUSD  = Total_Costs_max_bUSD  - B1_max,
    B1_Diff_min_bUSD  = Total_Costs_min_bUSD  - B1_min
  ) %>%
  ungroup() %>%
  # drop the temporary B1_* columns if you like
  select(-B1_mean, -B1_max, -B1_min)

# Save results
write.csv(All_Costs_final,  file = file.path(output_path, "All_Costs_per_Simulation.csv"), row.names = FALSE)

#── Building combined_ranges from All_Costs_final ───────────────────────────
# 1) pivot-long, fold Canadian pieces, drop B1_Diff
long_data <- All_Costs_final %>%
  # 1) grab only the *_bUSD columns
  select(Pathway, Simulation, ends_with("_bUSD")) %>%
  # 2) pivot into long form
  pivot_longer(
    cols          = -c(Pathway, Simulation),
    names_to      = c("Cost_Type", "Statistic"),
    names_pattern = "(.*)_(mean|min|max)_bUSD",
    values_to     = "Cost_bUSD"
  ) %>%
  # 3) fold Canadian pieces into base Cost_Type
  mutate(
    Cost_Type = case_when(
      Cost_Type == "Air_emissions"           ~ "Air Emissions",
      Cost_Type == "Unmet_demand"            ~ "Unmet Demand Penalty",
      Cost_Type == "VOM"                     ~ "Variable O&M",
      Cost_Type == "FOM"                     ~ "Fixed O&M",
      Cost_Type == "CAPEX"                   ~ "CAPEX",
      Cost_Type == "Fuel"                    ~ "Fuel",
      Cost_Type == "Imports"                 ~ "Imports",
      Cost_Type == "GHG"                     ~ "GHG Emissions",
      str_detect(Cost_Type, "CAN_CAPEX")     ~ "CAPEX",
      str_detect(Cost_Type, "CAN_FOM")       ~ "Fixed O&M",
      str_detect(Cost_Type, "CAN_CH4")       ~ "GHG Emissions",
      str_detect(Cost_Type, "CAN_Imports_adj") ~ "Imports",
      TRUE                                   ~ Cost_Type
    )
  ) %>%
  # 4) drop any unwanted types
  filter(Cost_Type != "B1_Diff") %>%
  # 5) **collapse** CAN + original into one row (sum their costs)
  group_by(Pathway, Simulation, Cost_Type, Statistic) %>%
  summarise(
    Cost_bUSD = sum(Cost_bUSD, na.rm = TRUE),
    .groups   = "drop"
  )

# 2) compute total cost per sim, detect outliers by IQR per Pathway
outlier_sims <- long_data %>%
  group_by(Pathway, Simulation) %>%
  summarise(total_cost = sum(Cost_bUSD, na.rm = TRUE), .groups = "drop") %>%
  group_by(Pathway) %>%
  mutate(
    Q1    = quantile(total_cost, 0.25),
    Q3    = quantile(total_cost, 0.75),
    IQR   = Q3 - Q1,
    lower = Q1 - 1.5 * IQR,
    upper = Q3 + 1.5 * IQR
  ) %>%
  filter(total_cost < lower | total_cost > upper) %>%
  distinct(Simulation) %>%
  pull(Simulation)


# 3) drop all rows for those sims, then re-aggregate
combined_ranges <- long_data %>%
  filter(!Simulation %in% outlier_sims) %>%
  group_by(Pathway, Cost_Type, Statistic) %>%
  summarise(
    Cost_bUSD = mean(Cost_bUSD, na.rm = TRUE),
    .groups   = "drop"
  )

write.csv(combined_ranges,  file = file.path(output_path, "All_Costs.csv"), row.names = FALSE)

## --- Additional analysis/plots ----

All_Costs_final_summary <- All_Costs_final %>%
  group_by(Pathway) %>%
  summarize(
    B1_Diff_mean_bUSD = mean(B1_Diff_mean_bUSD),
    B1_Diff_max_bUSD = mean(B1_Diff_max_bUSD),
    B1_Diff_min_bUSD = mean(B1_Diff_min_bUSD),
    .groups = 'drop'
  ) 

# ---- Plot 1
# --- Plotting Total Costs ---
stack_data <- All_Costs_final %>%
  group_by(Pathway) %>%
  summarise(
    CAPEX      = mean(CAPEX_mean_bUSD,        na.rm = TRUE),
    FOM        = mean(FOM_mean_bUSD,          na.rm = TRUE),
    VOM        = mean(VOM_mean_bUSD,          na.rm = TRUE),
    Fuel       = mean(Fuel_mean_bUSD,         na.rm = TRUE),
    Imports    = mean(Imports_mean_bUSD,      na.rm = TRUE),
    GHG        = mean(GHG_mean_bUSD,          na.rm = TRUE),
    Air        = mean(Air_emissions_mean_bUSD, na.rm = TRUE),
    Unmet      = mean(Unmet_demand_mean_bUSD, na.rm = TRUE),
    CAN_Cost   = mean(CAN_CAPEX_mean_bUSD + CAN_FOM_mean_bUSD + CAN_CH4_mean_bUSD, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols      = -Pathway,
    names_to  = "Cost_Type",
    values_to = "Cost_bUSD"
  ) %>%
  mutate(Statistic = "mean")

cost_stats <- All_Costs_final %>%
  group_by(Pathway) %>%
  summarise(
    min_cost  = min(Total_Costs_min_bUSD, na.rm = TRUE),
    mean_cost = mean(Total_Costs_mean_bUSD, na.rm = TRUE),
    max_cost  = max(Total_Costs_max_bUSD, na.rm = TRUE)
  )

format_cost <- function(x) {
  paste0("$", format(round(x,1), big.mark = ","), " B")
}

plot_total_costs <- ggplot() +
  # stacked bars of mean component costs
  geom_bar(
    data     = stack_data %>% filter(Statistic == "mean"),
    aes(x     = Pathway,
        y     = Cost_bUSD,
        fill  = Cost_Type),
    stat     = "identity",
    position = "stack"
  ) +
  # min/max error bars around total cost
  geom_errorbar(
    data = cost_stats,
    aes(
      x    = Pathway,
      ymin = min_cost,
      ymax = max_cost
    ),
    width = 0.2,
    color = "black"
  ) +
  # annotate max
  geom_text(
    data = cost_stats,
    aes(
      x     = Pathway,
      y     = max_cost,
      label = format_cost(max_cost)
    ),
    vjust = -0.5,
    size  = 5
  ) +
  # annotate min
  geom_text(
    data = cost_stats,
    aes(
      x     = Pathway,
      y     = min_cost,
      label = format_cost(min_cost)
    ),
    vjust =  1.5,
    size  = 5
  ) +
  # labels & theme
  labs(
    title = "Total 2024-NPV for Decarbonization Pathways in New England [2025–2050]",
    x     = "Pathway",
    y     = "NPV (billions-USD 2024)"
  ) +
  theme_minimal() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = comma) +
  theme(
    plot.title     = element_text(size = 18, face = "bold"),
    axis.title.x   = element_text(size = 16, face = "bold"),
    axis.title.y   = element_text(size = 18, face = "bold"),
    axis.text.x    = element_text(size = 12),
    axis.text.y    = element_text(size = 14),
    legend.title   = element_text(size = 14),
    legend.text    = element_text(size = 12)
  )

print(plot_total_costs)

# Direct costs per household in New England
# 1. Number of households in New England (ACS 2023 1-yr estimate)
households_NE <- 6153123  # from Census Reporter :contentReference[oaicite:0]{index=0}
# 1. Compute direct costs per row
direct_diffs <- All_Costs_final %>%
  mutate(
    Direct_mean_bUSD = CAPEX_mean_bUSD 
    + FOM_mean_bUSD 
    + VOM_mean_bUSD 
    + Fuel_mean_bUSD 
    + Imports_mean_bUSD,
    Direct_max_bUSD  = CAPEX_max_bUSD 
    + FOM_max_bUSD 
    + VOM_max_bUSD 
    + Fuel_max_bUSD 
    + Imports_max_bUSD,
    Direct_min_bUSD  = CAPEX_min_bUSD 
    + FOM_min_bUSD 
    + VOM_min_bUSD 
    + Fuel_min_bUSD 
    + Imports_min_bUSD
  ) %>%
  # grab A’s direct costs within each simulation
  group_by(Simulation) %>%
  mutate(
    A_Direct_mean = Direct_mean_bUSD[Pathway == "A"],
    A_Direct_max  = Direct_max_bUSD[Pathway == "A"],
    A_Direct_min  = Direct_min_bUSD[Pathway == "A"]
  ) %>%
  ungroup() %>%
  # compute differences vs. A
  mutate(
    Diff_direct_mean = Direct_mean_bUSD - A_Direct_mean,
    Diff_direct_max  = Direct_max_bUSD  - A_Direct_max,
    Diff_direct_min  = Direct_min_bUSD  - A_Direct_min
  ) %>%
  select(Simulation, Pathway,
         Diff_direct_mean, Diff_direct_min, Diff_direct_max)

# 2. Summarize across all simulations by pathway
summary_direct <- direct_diffs %>%
  group_by(Pathway) %>%
  summarise(
    Mean_Diff_direct = mean(Diff_direct_mean),
    Min_Diff_direct  = mean(Diff_direct_min),
    Max_Diff_direct  = mean(Diff_direct_max),
    .groups = "drop"
  )

# 2. Start from your summary_direct (which has Pathway, 
#    Mean_Diff_direct, Min_Diff_direct, Max_Diff_direct in bUSD)
annual_per_hh <- summary_direct %>%
  mutate(
    # convert bUSD → USD
    Annual_Mean_USD         = Mean_Diff_direct * 1e9,
    Annual_Min_USD          = Min_Diff_direct  * 1e9,
    Annual_Max_USD          = Max_Diff_direct  * 1e9,
    # per-household, per-month
    Mean_per_HH_per_Month   = Annual_Mean_USD / households_NE / 12 /25,
    Min_per_HH_per_Month    = Annual_Min_USD  / households_NE / 12 /25,
    Max_per_HH_per_Month    = Annual_Max_USD  / households_NE / 12 /25
  ) %>%
  select(
    Pathway,
    Mean_Diff_direct_bUSD   = Mean_Diff_direct,
    Min_Diff_direct_bUSD    = Min_Diff_direct,
    Max_Diff_direct_bUSD    = Max_Diff_direct,
    Mean_per_HH_per_Month,
    Min_per_HH_per_Month,
    Max_per_HH_per_Month
  )

