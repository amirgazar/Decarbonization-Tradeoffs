# Install required libraries
library(dplyr)
library(readxl)
library(data.table)
library(fredr)
library(ggplot2)

# Define the start and end year
start_year <- 2025
end_year <- 2050

years <- start_year:end_year
year_data <- data.frame(Year = years)

# Set the Path
path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/AEO 2023/population_GDP_growth.xlsx"

# Demand Data
household_data <- read_excel(path)
household_data <- household_data %>%
  filter(Year >= start_year & Year <= end_year)
colnames(household_data) <- c("Year", "n_households_millions", "GDP_bUSD_2012")
# GDP Per Capita convert to 2023
# API KEY 63522eae4ec927d6f1d9d86bf7826cc8
fredr_set_key("63522eae4ec927d6f1d9d86bf7826cc8") 
cpi_data <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"), observation_end = as.Date("2024-01-01"))

# Extracting CPI values for specific years
cpi_2012 <- filter(cpi_data, year(date) == 2012) %>% summarise(YearlyAvg = mean(value))
cpi_2023 <- filter(cpi_data, year(date) == 2023) %>% summarise(YearlyAvg = mean(value))

# Calculating conversion rate
conversion_rate <- cpi_2023$YearlyAvg / cpi_2012$YearlyAvg

household_data <- household_data %>%
  mutate(GDP_bUSD_2023 = GDP_bUSD_2012 * conversion_rate)

# Load the combined NPV from the saved CSV file
combined_NPV <- read.csv("/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/R Files/NPV Results/All_Costs/All_Costs.csv")

# Calculate the number of years
n_years <- end_year - start_year + 1

# Define the discount rate
discount_rate <- 0.02
combined_NPV <- combined_NPV %>%
  mutate(Annual_Cost_bUSD = (Cost_bUSD * discount_rate) / (1 - (1 + discount_rate)^(-n_years)))


expanded_npv_data <- combined_NPV %>%
  select(Cost_Type, Statistic, Scenario, Annual_Cost_bUSD) %>%
  slice(rep(1:n(), each = length(years))) %>%
  mutate(Year = rep(years, times = nrow(combined_NPV)))

# Reorder the columns if necessary
expanded_npv_data <- expanded_npv_data %>%
  select(Year, everything())

expanded_npv_data <- expanded_npv_data %>%
  left_join(household_data, by = "Year")

# Calculate the cost per household and add it as a new column
expanded_npv_data <- expanded_npv_data %>%
  mutate(Cost_USD_per_person = (Annual_Cost_bUSD / (n_households_millions * 1e6)) * 1e9) %>%
  mutate(NPV_GDP_ratio = Annual_Cost_bUSD / GDP_bUSD_2023)

# Calculate the mean annual value for each scenario, cost type, and statistic
mean_annual_data <- expanded_npv_data %>%
  group_by(Cost_Type, Statistic, Scenario) %>%
  summarize(Cost_USD_per_person = mean(Cost_USD_per_person), .groups = "drop")

# Calculate min, mean, and max annual costs for the combined data
min_costs_all <- mean_annual_data %>%
  group_by(Scenario) %>%
  summarize(Total_Cost_USD_min = sum(Cost_USD_per_person[Statistic == "min"]), .groups = 'drop')

max_costs_all <- mean_annual_data %>%
  group_by(Scenario) %>%
  summarize(Total_Cost_USD_max = sum(Cost_USD_per_person[Statistic == "max"]), .groups = 'drop')

mean_costs_all <- mean_annual_data %>%
  group_by(Scenario) %>%
  summarize(Total_Cost_USD_mean = sum(Cost_USD_per_person[Statistic == "mean"]), .groups = 'drop')

# Reverse the order to have CAPEX at the top
mean_annual_data <- mean_annual_data %>%
  mutate(Cost_Type = factor(Cost_Type, levels = rev(c("CAPEX", "Fixed O&M", "Variable O&M", "Imports", "Fuel", "GHG Emissions", "Air Emissions", "Unmet Demand Penalty"))))

# Create the plot with all scenarios
plot_combined_all <- ggplot() +
  geom_bar(data = mean_annual_data %>% filter(Statistic == "mean"), 
           aes(x = Scenario, y = Cost_USD_per_person, fill = Cost_Type), 
           stat = "identity", position = "stack") +
  geom_errorbar(data = mean_costs_all, 
                aes(x = Scenario, ymin = min_costs_all$Total_Cost_USD_min, ymax = max_costs_all$Total_Cost_USD_max), 
                width = 0.2, color = "black") +
  geom_text(data = max_costs_all, 
            aes(x = Scenario, y = Total_Cost_USD_max, 
                label = scales::comma(Total_Cost_USD_max)), 
            vjust = -0.5, color = "black", size = 4) +
  geom_text(data = min_costs_all, 
            aes(x = Scenario, y = Total_Cost_USD_min, 
                label = scales::comma(Total_Cost_USD_min)), 
            vjust = 1.5, color = "black", size = 4) +
  labs(title = "Average Annual Costs (2023-USD) per Person for New England's Decarbonization Pathways", 
       y = "Cost per Person (2023-USD/year)", x = "Pathway") +
  theme_minimal() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma)

# Display the plot
plot_combined_all

# Save the final plot
path = "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/R Files/Plots/Costs/cost_per_person.png"
ggsave(path, plot = plot_combined_all, width = 14, height = 7, dpi = 300)


# Demand versus costs
# Demand
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/R Files/RDS Files Scenarios/demand_data.rds"
demand_data <- readRDS(file_path)
setDT(demand_data)
demand_data[, Year := year(Date)]
annual_demand <- demand_data[, .(Demand_kWh = sum(Demand)*1e3), by = Year]

expanded_npv_data <- expanded_npv_data %>%
  left_join(annual_demand, by = "Year")

# Calculate the cost per kWh and add it as a new column
expanded_npv_data <- expanded_npv_data %>%
  mutate(Cost_cUSD_per_kWh = (Annual_Cost_bUSD / (Demand_kWh)) * 1e9 *1e2) 

# Calculate the mean annual value for each scenario, cost type, and statistic
mean_annual_data_kWh <- expanded_npv_data %>%
  group_by(Cost_Type, Statistic, Scenario) %>%
  summarize(Cost_cUSD_per_kWh = mean(Cost_cUSD_per_kWh), .groups = "drop")

# Calculate min, mean, and max annual costs for the combined data
min_costs_all_kWh <- mean_annual_data_kWh %>%
  group_by(Scenario) %>%
  summarize(Total_Cost_USD_min = sum(Cost_cUSD_per_kWh[Statistic == "min"]), .groups = 'drop')

max_costs_all_kWh <- mean_annual_data_kWh %>%
  group_by(Scenario) %>%
  summarize(Total_Cost_USD_max = sum(Cost_cUSD_per_kWh[Statistic == "max"]), .groups = 'drop')

mean_costs_all_kWh <- mean_annual_data_kWh %>%
  group_by(Scenario) %>%
  summarize(Total_Cost_USD_mean = sum(Cost_cUSD_per_kWh[Statistic == "mean"]), .groups = 'drop')

# Reverse the order to have CAPEX at the top
mean_annual_data_kWh <- mean_annual_data_kWh %>%
  mutate(Cost_Type = factor(Cost_Type, levels = rev(c("CAPEX", "Fixed O&M", "Variable O&M", "Imports", "Fuel", "GHG Emissions", "Air Emissions", "Unmet Demand Penalty"))))

# Create the plot with all scenarios
plot_combined_all <- ggplot() +
  geom_bar(data = mean_annual_data_kWh %>% filter(Statistic == "mean"), 
           aes(x = Scenario, y = Cost_cUSD_per_kWh, fill = Cost_Type), 
           stat = "identity", position = "stack") +
  geom_errorbar(data = mean_costs_all_kWh, 
                aes(x = Scenario, ymin = min_costs_all_kWh$Total_Cost_USD_min, ymax = max_costs_all_kWh$Total_Cost_USD_max), 
                width = 0.2, color = "black") +
  geom_text(data = max_costs_all_kWh, 
            aes(x = Scenario, y = Total_Cost_USD_max, 
                label = scales::comma(Total_Cost_USD_max)), 
            vjust = -0.5, color = "black", size = 4) +
  geom_text(data = min_costs_all_kWh, 
            aes(x = Scenario, y = Total_Cost_USD_min, 
                label = scales::comma(Total_Cost_USD_min)), 
            vjust = 1.5, color = "black", size = 4) +
  labs(title = "Average Annual Costs (2023-¢USD) per kWh for New England's Decarbonization Pathways", 
       y = "Cost per kWh (2023-¢USD/year)", x = "Pathway") +
  theme_minimal() +
  scale_fill_viridis_d() +
  scale_y_continuous(labels = scales::comma)

# Display the plot
plot_combined_all

# Save the final plot
path = "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/R Files/Plots/Costs/cost_per_kWh.png"
ggsave(path, plot = plot_combined_all, width = 14, height = 7, dpi = 300)

# Calculate the annual value for each scenario, cost type, and statistic and year
annual_data_kWh <- expanded_npv_data %>%
  group_by(Statistic, Scenario, Year) %>%
  summarize(Cost_cUSD_per_kWh = sum(Cost_cUSD_per_kWh), .groups = "drop")
