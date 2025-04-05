# Install required libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(zoo)
library(data.table)

# Function to fill leap year data
fill_leap_year_data <- function(data) {
  for (year in unique(data$Year)) {
    if (year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0)) {  # Check if leap year
      for (hour in 0:23) {
        feb_28_data <- data[Year == year & Month == 2 & Day == 28 & Hour == hour, Demand]
        mar_1_data <- data[Year == year & Month == 3 & Day == 1 & Hour == hour, Demand]
        
        if (length(feb_28_data) == 1 && length(mar_1_data) == 1) {
          leap_day_demand <- mean(c(feb_28_data, mar_1_data), na.rm = TRUE)
          data <- rbind(data, data.table(
            Year = year,
            Month = 2,
            Day = 29,
            Hour = hour,
            Demand = leap_day_demand
          ))
        }
      }
    }
  }
  return(data)
}

# Set the File Path
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/Massachusetts 2050 Decarbonization Roadmap Study/Massachusetts Workbook of Energy Modeling Results 2024.xlsx"

# Demand Data
sheet_name <- "11. Hourly Electric Load"
demand_data <- read_excel(file_path, sheet = sheet_name, skip = 19)
setDT(demand_data)

demand_data[, Demand := `Coincident Inflexible Load...17`]

demand_data <- demand_data[, .(Year, Month, Day, Hour = Hour, Demand)]
unique_years <- unique(demand_data$Year)
unique_hours <- unique(demand_data$Hour)

# We now know hourly demand for 2020, 2025,.... till 2050 (5-year), we will interpolate other years
setorder(demand_data, Year, Month, Day, Hour)
demand_data <- demand_data[, .(Hour = unique_hours, Demand), by = .(Year, Month, Day)]

# Generate a complete sequence of datetime for interpolation
complete_dt <- seq(ymd("2020-01-01", tz = "America/New_York"), ymd("2050-12-31", tz = "America/New_York"), by = "day")
complete_dt <- data.table(
  Date = complete_dt,
  Year = year(complete_dt),
  Month = month(complete_dt),
  Day = day(complete_dt)
)
complete_dt <- complete_dt[, .(Hour = unique_hours), by = .(Year, Month, Day)]

# Merge with the original data to get missing time points
demand_data <- merge(complete_dt, demand_data, by = c("Year", "Month", "Day", "Hour"), all.x = TRUE)

# Check the number of hours for each day
Count_daylight_savings<- demand_data[, .N, by = .(Year, Month, Day)]

# Interpolate missing Demand values using SPLINE interpolation
full_years <- seq(min(demand_data$Year), max(demand_data$Year))
interpolated_data <- data.table()

for (hour in unique(demand_data$Hour)) {
  for (month in unique(demand_data$Month)) {
    for (day in unique(demand_data$Day)) {
      subset_data <- demand_data[Month == month & Day == day & Hour == hour]
      
      # This is to fix the leap year issues as we dont have leap year data
      # Check if there are enough unique years with non-NA demand values
      if (length(unique(subset_data$Year[!is.na(subset_data$Demand)])) > 1) {
        spline_result <- spline(subset_data$Year, subset_data$Demand, xout = full_years)
        interpolated <- data.table(
          Year = spline_result$x,
          Month = month,
          Day = day,
          Hour = hour,
          Demand = spline_result$y
        )
        interpolated_data <- rbind(interpolated_data, interpolated)
      }
    }
  }
}

demand_data <- interpolated_data

# Interpolate Leap year hours
demand_data <- fill_leap_year_data(demand_data)

# Filtering years from 2024 to 2050
demand_data <- demand_data[Year >= 2024 & Year <= 2050]
demand_data[, Date := as.Date(ISOdate(Year, Month, Day, tz = "America/New_York"))]
demand_data <- demand_data[, .(Date, Hour = Hour + 1, Demand = Demand * 1000)]# Demand in MW

demand_data <- unique(demand_data, by = c("Date", "Hour", "Demand"))

Count_daylight_savings<- demand_data[, .N, by = .(Date)]

# Save as CSV
csv_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/demand_data.csv"
write.csv(demand_data, file = csv_path, row.names = FALSE)

# Plotting March 1st Data
march_1st_data <- demand_data %>%
  filter(format(Date, "%m-%d") == "03-01")

p<-ggplot(march_1st_data, aes(x = Hour, y = Demand, group = format(Date, "%Y"), color = format(Date, "%Y"))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Electricity Demand in New England on March 1st Across 2024-2050",
       x = "Hour of the Day",
       y = "Demand (MW)",
       color = "Year") +
  scale_x_continuous(breaks = 1:24)
print(p)

# Plotting July 1st Data
july_1st_data <- demand_data %>%
  filter(format(Date, "%m-%d") == "07-01")

p<-ggplot(july_1st_data, aes(x = Hour, y = Demand, group = format(Date, "%Y"), color = format(Date, "%Y"))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Electricity Demand in New England on July 1st Across 2024-2050",
       x = "Hour of the Day",
       y = "Demand (MW)",
       color = "Year") +
  scale_x_continuous(breaks = 1:24)
print(p)


# Plotting November 1st Data
nov_1st_data <- demand_data %>%
  filter(format(Date, "%m-%d") == "11-02")

p<-ggplot(nov_1st_data, aes(x = Hour, y = Demand, group = format(Date, "%Y"), color = format(Date, "%Y"))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Electricity Demand in New England on November 1st Across 2024-2050",
       x = "Hour of the Day",
       y = "Demand",
       color = "Year") +
  scale_x_continuous(breaks = 1:24)
print(p)

# Plotting Feb 29th Data Leap years
nov_1st_data <- demand_data %>%
  filter(format(Date, "%m-%d") == "02-29")

p<-ggplot(nov_1st_data, aes(x = Hour, y = Demand, group = format(Date, "%Y"), color = format(Date, "%Y"))) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Electricity Demand in New England on Feb 29th Across 2024-2050",
       x = "Hour of the Day",
       y = "Demand",
       color = "Year") +
  scale_x_continuous(breaks = 1:24)
print(p)

# Plot annual demand
annual_demand <- demand_data %>%
  mutate(Year = year(ymd(Date))) %>% 
  filter(Year >= 2024 & Year <= 2050) %>%
  group_by(Year) %>%
  summarize(mean_demand_GW = mean(Demand, na.rm = TRUE)/1000) %>%
  ungroup() 
p<-ggplot(annual_demand, aes(x = Year, y = mean_demand_GW)) +
  geom_bar(stat = "identity", fill = "black") +
  theme_minimal() +
  labs(title = "ISO NE Annual Mean Electricity Demand Predictions from 2024 to 2050",
       x = "Year",
       y = "Mean Demand (GW)")
print(p)

annual_load <- demand_data %>%
  mutate(Year = year(ymd(Date))) %>% 
  filter(Year >= 2024 & Year <= 2050) %>%
  group_by(Year) %>%
  summarize(load_TWh = sum(Demand, na.rm = TRUE)/1000000) %>%
  ungroup() 

p<-ggplot(annual_load, aes(x = Year, y = load_TWh)) +
  geom_line(color = "steelblue", size = 1) +  
  geom_ribbon(aes(ymin = 0, ymax = load_TWh), fill = "steelblue", alpha = 0.3) +  
  theme_minimal() +
  labs(title = "ISO NE Electricity Load Predictions from 2024 to 2050",
       x = "Year",
       y = "Load (TWh)")
print(p)

# Calculate daily mean demand for 2050
daily_demand <- demand_data %>%
  mutate(Date = ymd(Date),
         Year = year(Date),
         Month = month(Date),
         Day = day(Date)) %>%
  filter(Year == 2050) %>%
  group_by(Date) %>%
  summarize(mean_demand_GW = mean(Demand, na.rm = TRUE) / 1000, .groups = 'drop')

# Plot the daily mean demand for 2050
p <- ggplot(daily_demand, aes(x = Date, y = mean_demand_GW)) +
  geom_line(color = "blue") +
  theme_minimal() +
  labs(title = "ISO NE Daily Mean Electricity Demand for 2050",
       x = "Date",
       y = "Mean Demand (GW)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
print(p)
