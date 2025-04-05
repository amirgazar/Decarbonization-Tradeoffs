# Install required libraries
library(dplyr)
library(readxl)
library(ggplot2)
library(lubridate)
library(tidyr)
library(data.table)
library(forecast)
library(multcomp)
library(zoo)

# Function to check for leap year
is_leap_year <- function(year) {
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}

# Function to process each data file
data_processing_func <- function(file, columns_to_keep) {
  data <- readr::read_csv(file, show_col_types = FALSE)
  data <- data %>% dplyr::select(all_of(columns_to_keep))
  data$Date <- as.POSIXct(data$Date, format = "%m/%d/%Y %I:%M:%S %p", tz = "UTC")
  
  # Subtract 5 hours to convert UTC to EST
  data$Date <- data$Date - hours(5)
  
  # Add DayLabel and Hour columns
  data <- data %>%
    mutate(DayLabel = yday(Date), 
           Hour = hour(Date) + 1) 
  
  # Adjust DayLabel for non-leap years
  if (!is_leap_year(year(data$Date[365]))) {
    data <- data %>% mutate(DayLabel = ifelse(DayLabel == 366, 365, DayLabel))
  }
  
  # Adjust DayLabel for leap years
  if (is_leap_year(year(data$Date[365]))) {
    data <- data %>%
      mutate(DayLabel = ifelse(DayLabel == 365 & !is_leap_year(year(Date)), 366, DayLabel))
  }
  
  # Add P column
  p_part <- sub(".*_(P[0-9]+)_.*", "\\1", basename(file))
  p_num <- as.integer(sub("P", "", p_part)) 
  formatted_p_part <- sprintf("P%02d", p_num) 
  data$P = formatted_p_part
  
  return(data)
}

# Capacity Factor Labels: ISONE_pv_pwr, ISONE_ofsw_net_pwr, ISONE_onsw_net_pwr
# Solar data
# Defining the path and file pattern
path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/ISO-NE Wind and Solar CF/Stochastic Wind and Solar/Solar"

file_pattern <- "ISONE_SolarGeneration_P[0-9]+_[0-9]+_PUBLIC\\.csv$"
files <- list.files(path = path, pattern = file_pattern, full.names = TRUE)

# Columns to keep
columns_to_keep <- c("Date", "Realization", "ISONE_wnd_spd", "ISONE_wnd_dir", "ISONE_grs_ld",
                     "ISONE_net_ld", "ISONE_ofsw_grs_pwr", "ISONE_ofsw_net_pwr", "ISONE_onsw_grs_pwr",
                     "ISONE_onsw_net_pwr", "ISONE_totw_grs_pwr", "ISONE_totw_net_pwr", "ISONE_pv_pwr",
                     "ISONE_ghi", "ISONE_temp", "ISONE_rh", "ISONE_pres")

# Process each file using the function
data_list <- lapply(files, data_processing_func, columns_to_keep)
solar_data <- bind_rows(data_list)

p_counts <- table(solar_data$P)
print(p_counts)

# Onshore wind Data
path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/ISO-NE Wind and Solar CF/Stochastic Wind and Solar/Onshore Wind"

file_pattern <- "ISONE_OnshoreGrossWindGeneration_P[0-9]+_[0-9]+_PUBLIC\\.csv$"
files <- list.files(path = path, pattern = file_pattern, full.names = TRUE)

# Process each file using the function
data_list <- lapply(files, data_processing_func, columns_to_keep)
onwind_data <- bind_rows(data_list)

p_counts <- table(onwind_data$P)
print(p_counts)

# Offshore wind Data
path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/ISO-NE Wind and Solar CF/Stochastic Wind and Solar/Offshore Wind"

file_pattern <- "ISONE_OffshoreContractedGrossWindGeneration_P[0-9]+_[0-9]+_PUBLIC\\.csv$"
files <- list.files(path = path, pattern = file_pattern, full.names = TRUE)

# Process each file using the function
data_list <- lapply(files, data_processing_func, columns_to_keep)
offwind_data <- bind_rows(data_list)

p_counts <- table(offwind_data$P)
print(p_counts)


# ---Distribution analysis
# Define the fill_na_with_mean function with day wrapping
fill_na_with_mean <- function(df, P_col) {
  max_day <- max(df$DayLabel)
  
  na_indices <- which(is.na(df[[P_col]]))
  
  for (idx in na_indices) {
    day <- df$DayLabel[idx]
    hour <- df$Hour[idx]
    
    # Calculate surrounding days with wrapping
    prev_day <- ifelse(day - 1 < 1, max_day, day - 1)
    next_day <- ifelse(day + 1 > max_day, 1, day + 1)
    
    # Get the values for DayLabel ± 1 and the same Hour
    surrounding_days <- df[(df$DayLabel %in% c(prev_day, next_day)) & df$Hour == hour, P_col, drop = TRUE]

    # Calculate the mean of these values, ignoring NA
    replacement_value <- mean(surrounding_days, na.rm = TRUE)
    
    # Replace the NA value with the calculated mean
    df[idx, P_col] <- replacement_value
  }
  
  return(df)
}

# Prepare data frames for dispatch curve 
# Generation data frames for solar, wind and offshore wind
# IMPORTANT Note: We only use files for specific facilities
near_zero_threshold <- 0.001 
p_order <- c("P01", "P05", "P10", "P50", "P90", "P95", "P99")

solar_CF <- solar_data %>%
  mutate(Date = as.Date("2048-01-01") + DayLabel - 1) %>%
  dplyr::select(DayLabel, Hour, P, ISONE_pv_pwr) %>%
  pivot_wider(names_from = P, values_from = ISONE_pv_pwr) %>%
  mutate(across(starts_with("P"), ~ifelse(. < near_zero_threshold, 0, .))) %>%
  dplyr::select(DayLabel, Hour, all_of(p_order)) # Ensure the columns are in the correct order

onwind_CF <- onwind_data %>%
  mutate(Date = as.Date("2048-01-01") + DayLabel - 1) %>%
  dplyr::select(DayLabel, Hour, P, ISONE_onsw_net_pwr) %>%
  pivot_wider(names_from = P, values_from = ISONE_onsw_net_pwr) %>%
  mutate(across(starts_with("P"), ~ifelse(. < near_zero_threshold, 0, .))) %>%
  dplyr::select(DayLabel, Hour, all_of(p_order)) # Ensure the columns are in the correct order

offwind_CF <- offwind_data %>%
  mutate(Date = as.Date("2050-01-01") + DayLabel - 1) %>%
  dplyr::select(DayLabel, Hour, P, ISONE_ofsw_net_pwr) %>%
  pivot_wider(names_from = P, values_from = ISONE_ofsw_net_pwr) %>%
  mutate(across(starts_with("P"), ~ifelse(. < near_zero_threshold, 0, .))) %>%
  dplyr::select(DayLabel, Hour, all_of(p_order)) # Ensure the columns are in the correct order

# Fill NA values for each P column using the defined function
for (P_col in p_order) {
  solar_CF <- fill_na_with_mean(solar_CF, P_col)
  onwind_CF <- fill_na_with_mean(onwind_CF, P_col)
  offwind_CF <- fill_na_with_mean(offwind_CF, P_col)
}
setorder(solar_CF, DayLabel, Hour)
setorder(onwind_CF, DayLabel, Hour)
setorder(offwind_CF, DayLabel, Hour)

# Expand the CF using linear regression
# Function to interpolate percentiles
interpolate_percentiles <- function(hourly_data) {
  # Percentiles available in the data
  percentiles <- c(1, 5, 10, 50, 90, 95, 99)
  
  # Data frame to store interpolated values
  interpolated_data <- data.frame(DayLabel = integer(), Hour = integer(), Percentile = integer(), CF = numeric())
  
  for (i in 1:nrow(hourly_data)) {
    # Interpolate using spline for smooth curve or approx for linear
    interpolated <- approx(x = percentiles, 
                           y = as.numeric(hourly_data[i, 3:9]), 
                           xout = 1:99, 
                           method = "linear")

    
    # Create a data frame for this hour
    hour_df <- data.frame(DayLabel = hourly_data$DayLabel[i], 
                          Hour = hourly_data$Hour[i], 
                          Percentile = interpolated$x, 
                          CF = interpolated$y)
    
    # Bind the data frame to the results
    interpolated_data <- rbind(interpolated_data, hour_df)
  }
  
  return(interpolated_data)
}

# Interpolate the data
solar_CF_expanded <- interpolate_percentiles(solar_CF)
onwind_CF_expanded <- interpolate_percentiles(onwind_CF)
offwind_CF_expanded <- interpolate_percentiles(offwind_CF)

# Save as CSV files
write.csv(solar_CF_expanded, "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/5 Intermediatary Data/1 Wind and Solar CF/solar_CF.csv", row.names = FALSE)
write.csv(onwind_CF_expanded, "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/5 Intermediatary Data/1 Wind and Solar CF/onwind_CF.csv", row.names = FALSE)
write.csv(offwind_CF_expanded, "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/5 Intermediatary Data/1 Wind and Solar CF/offwind_CF.csv", row.names = FALSE)
