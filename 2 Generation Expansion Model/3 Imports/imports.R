# Load required libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(purrr)
library(data.table)

# Defining the start and end date
start_date <- as.Date("2011-01-01")
end_date <- as.Date("2023-12-31")

max_CF_transmission_lines <- 0.95 # According to NREL ATB

# Function to calculate percentiles
calculate_percentiles <- function(data, column) {
  quantiles <- quantile(data[[column]], probs = seq(0.01, 0.99, by = 0.01), na.rm = TRUE)
  tibble(Percentile = seq(1, 99), Value = quantiles)
}

# Imports Data
file <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/ISO-NE Daily Imports/daily_capacity_status.xlsx"
imports_data <- data.frame()
years <- as.character(2011:2023)
sheet_names <- excel_sheets(file)
sheet_names_of_interest <- sheet_names[sapply(sheet_names, function(name) name %in% years)]

for (sheet_name in sheet_names_of_interest) {
  sheet_data <- read_excel(file, sheet = sheet_name)
  
  sheet_data$Year <- sheet_name
  sheet_data$Month <- match(sheet_data$Month, month.name)
  sheet_data$Date <- make_date(year = sheet_data$Year, month = sheet_data$Month, day = sheet_data$Day)
  sheet_data <- sheet_data %>%
    dplyr::select(
      Date, 
      Hour = `Hour`, 
      Peak_load = `Actual Peak Load`, 
      NYPP = 'NYPP',
      NNC = 'NNC',
      CSC = 'CSC',
      NB = 'NB',
      Phase_II = `Phase II`, 
      Highgate = `Highgate`
    ) %>%
    mutate(
      Hour = hour(lubridate::ymd_hms(Hour)), # Extract hour part 
      Peak_load = as.numeric(Peak_load)
    )
  # Combine with the existing data frame
  imports_data <- rbind(imports_data, sheet_data)
}

# Prepare data frames for dispatch curve 
imports_data <- imports_data %>%
  mutate(imports_QC = ifelse(-Phase_II - Highgate < 0, 0, -Phase_II - Highgate)) %>%
  mutate(imports_NYISO = ifelse(-NYPP - NNC - CSC < 0, 0, -NYPP - NNC - CSC)) %>%
  mutate(imports_NBSO = ifelse(-NB < 0, 0, -NB)) %>%
  dplyr::select(Date, Hour, imports_QC, imports_NYISO, imports_NBSO)

# Add year and month columns for grouping
imports_data <- imports_data %>%
  mutate(
    Year = year(Date),
    Month = month(Date)
  )

# Find the max for each variable by month/year and normalize
imports_data <- imports_data %>%
  group_by(Year, Month) %>%
  mutate(
    max_imports_QC = max(imports_QC, na.rm = TRUE),
    max_imports_NYISO = max(imports_NYISO, na.rm = TRUE),
    max_imports_NBSO = max(imports_NBSO, na.rm = TRUE),
    imports_QC_norm = imports_QC / max_imports_QC,
    imports_NYISO_norm = imports_NYISO / max_imports_NYISO,
    imports_NBSO_norm = imports_NBSO / max_imports_NBSO
  ) %>%
  ungroup() %>%
  dplyr::select(-max_imports_QC, -max_imports_NYISO, -max_imports_NBSO) # Remove intermediate columns if not needed

# Creating an hourly profile for each day in the year
imports_data <- imports_data %>%
  mutate(day_of_year = yday(Date)) # labeling each day

# Calculate hourly distribution parameters for each day of the year
hourly_stats_by_day <- imports_data %>%
  group_by(day_of_year) %>%
  group_split() %>%
  map_dfr(function(day_data) {
    tibble(
      DayLabel = day_data$day_of_year[1],
      Percentile = seq(1, 99),
      imports_QC = calculate_percentiles(day_data, "imports_QC_norm")$Value,
      imports_NYISO = calculate_percentiles(day_data, "imports_NYISO_norm")$Value,
      imports_NBSO = calculate_percentiles(day_data, "imports_NBSO_norm")$Value
    )
  })

# Save the CSV files
fwrite(hourly_stats_by_day, "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/3 Imports/1 Imports CF/Imports_CF.csv")
