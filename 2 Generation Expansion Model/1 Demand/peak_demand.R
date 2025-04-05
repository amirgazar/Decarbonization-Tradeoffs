# Install required libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)
library(data.table)

# MODEL SOURCE: https://www.mapc.org/our-work/expertise/clean-energy/peak-demand/
# SAMPLE: https://peak-alerts.herokuapp.com/


# Load demand data
# Demand
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/demand_data.csv"
demand_data <- read.csv(file_path)
setDT(demand_data)

# Calculate dynamic thresholds for peak demand based on annual peak demand
calculate_dynamic_thresholds <- function(yearly_peak_demand) {
  thresholds <- yearly_peak_demand %>%
    mutate(
      Unlikely_Threshold = Peak_Demand * 0.85,
      Possible_Threshold = Peak_Demand * 0.95,
      Likely_Threshold = Peak_Demand 
    )
  return(thresholds)
}

# Function to calculate peak 
calculate_peak <- function(data, thresholds) {
  data <- data %>%
    left_join(thresholds, by = "Year") %>%
    mutate(
      Peak = case_when(
        Demand < Unlikely_Threshold ~ "UNLIKELY",
        Demand >= Unlikely_Threshold & Demand < Possible_Threshold ~ "POSSIBLE",
        Demand >= Possible_Threshold ~ "LIKELY"
      )
    )
  return(data)
}

# Extract Year from Date
demand_data <- demand_data %>%
  mutate(Year = year(Date))

# Calculate yearly peak demand
yearly_peak_demand <- demand_data %>%
  group_by(Year) %>%
  summarize(Peak_Demand = max(Demand))

# Calculate dynamic thresholds
thresholds <- calculate_dynamic_thresholds(yearly_peak_demand)

# Calculate peak 
demand_data_peak <- calculate_peak(demand_data, thresholds)

# Function to calculate daily peak demand 
calculate_daily_peak <- function(data) {
  daily_summary <- data %>%
    group_by(Date) %>%
    summarize(
      Peak_Demand = max(Demand),
      Peak_Hour = Hour[which.max(Demand)],
      Peak = ifelse(Peak_Demand >= max(Possible_Threshold), "LIKELY", ifelse(Peak_Demand >= min(Unlikely_Threshold), "POSSIBLE", "UNLIKELY"))
    )
  return(daily_summary)
}

# Calculate daily peak demand and for the entire dataset
daily_summary <- calculate_daily_peak(demand_data_peak)

# Format the date
daily_summary <- daily_summary %>%
  mutate(
    Date = as.Date(Date), # Convert the Date column to Date class
    Day = weekdays(Date),
    Formatted_Date = format(Date, "%a %d %b")
  )


# Print the summary table for every day
print(daily_summary)

# Save daily_summary
# Save as CSV
csv_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/peak_demand_data.csv"
write.csv(daily_summary, file = csv_path, row.names = FALSE)


# Create a visualization for a specific week as an example 
weekly_data <- daily_summary %>%
  filter(Date >= as.Date("2050-01-23") & Date <= as.Date("2050-01-30"))

# Add year to the formatted date
weekly_data <- weekly_data %>%
  mutate(Formatted_Date = factor(paste0(format(Date, "%Y-%m-%d"), " (", weekdays(Date), ")"),
                                 levels = unique(paste0(format(Date, "%Y-%m-%d"), " (", weekdays(Date), ")")[order(Date)])))

# Generate the plot
plot <- ggplot(weekly_data, aes(x = Formatted_Date, y = Peak_Demand)) +
  geom_bar(stat = "identity", fill = ifelse(weekly_data$Peak == "LIKELY", "#FF0000", 
                                            ifelse(weekly_data$Peak == "POSSIBLE", "#FFA500", "#228B22")), alpha = 1) +
  geom_text(aes(label = paste0("Peak load\n", round(Peak_Demand / 1000, 1), " GW\nat ", Peak_Hour, ":00 - ", Peak_Hour + 1, ":00 PM\n", Peak)), 
            vjust = -0.5, size = 3, fontface = "bold") +
  theme_minimal() +
  labs(
    title = "Peak demand for January 23-30, 2050",
    x = "Date",
    y = "Peak Load (GW)"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(0, 80000)

print(plot)
