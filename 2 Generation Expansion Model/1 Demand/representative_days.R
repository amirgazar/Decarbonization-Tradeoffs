# Load Libraries
library(data.table)
library(dplyr)
library(lubridate)

# Load Demand Data
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/demand_data.csv"
Demand_data <- fread(file_path)
start_date <- as.Date("2025-01-01")
end_date <- as.Date("2050-12-31")
Demand_data <- Demand_data[Date >= start_date & Date <= end_date]

# Load CF Data
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/1 Wind and Solar/1 Wind and Solar CF/solar_CF.csv"
Solar_CF <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/1 Wind and Solar/1 Wind and Solar CF/onwind_CF.csv"
Onwind_CF <- fread(file_path)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/1 Wind and Solar/1 Wind and Solar CF/offwind_CF.csv"
Offwind_CF <- fread(file_path)

# Ensure Demand Data has DayLabel and Hour columns
Demand_data[, `:=`(
  Date = as.Date(Date),
  DayLabel = yday(Date),
  Hour = Hour  # Ensure Hour is numeric
)]

# Reshape CF Data for Solar
Solar_CF_wide <- dcast(Solar_CF, DayLabel + Hour ~ Percentile, value.var = "CF", fun.aggregate = mean)
setnames(Solar_CF_wide, old = names(Solar_CF_wide)[3:ncol(Solar_CF_wide)], new = paste0("Solar_CF_", names(Solar_CF_wide)[3:ncol(Solar_CF_wide)]))

# Reshape CF Data for Onwind
Onwind_CF_wide <- dcast(Onwind_CF, DayLabel + Hour ~ Percentile, value.var = "CF", fun.aggregate = mean)
setnames(Onwind_CF_wide, old = names(Onwind_CF_wide)[3:ncol(Onwind_CF_wide)], new = paste0("Onwind_CF_", names(Onwind_CF_wide)[3:ncol(Onwind_CF_wide)]))

# Reshape CF Data for Offwind
Offwind_CF_wide <- dcast(Offwind_CF, DayLabel + Hour ~ Percentile, value.var = "CF", fun.aggregate = mean)
setnames(Offwind_CF_wide, old = names(Offwind_CF_wide)[3:ncol(Offwind_CF_wide)], new = paste0("Offwind_CF_", names(Offwind_CF_wide)[3:ncol(Offwind_CF_wide)]))

# Calculate features for Demand Data grouped by Date and Hour
demand_features <- Demand_data[, .(
  Mean_Demand = mean(Demand, na.rm = TRUE),
  Max_Demand = max(Demand, na.rm = TRUE),
  Min_Demand = min(Demand, na.rm = TRUE),
  SD_Demand = sd(Demand, na.rm = TRUE)
), by = .(Date)]

# Add DayLabel to demand_features Data
demand_features <- demand_features %>%
  mutate(
    DayLabel = yday(Date)                    # Calculate day of the year (1–365/366)
  )

# CF features
solar_features <- Solar_CF_wide[, c(
  setNames(lapply(.SD, function(col) mean(col, na.rm = TRUE)), paste0(names(.SD), "_Mean")),
  setNames(lapply(.SD, function(col) max(col, na.rm = TRUE)), paste0(names(.SD), "_Max")),
  setNames(lapply(.SD, function(col) min(col, na.rm = TRUE)), paste0(names(.SD), "_Min")),
  setNames(lapply(.SD, function(col) sd(col, na.rm = TRUE)), paste0(names(.SD), "_SD"))
), by = .(DayLabel), .SDcols = patterns("^Solar_CF_")]

onwind_features <- Onwind_CF_wide[, c(
  setNames(lapply(.SD, function(col) mean(col, na.rm = TRUE)), paste0(names(.SD), "_Mean")),
  setNames(lapply(.SD, function(col) max(col, na.rm = TRUE)), paste0(names(.SD), "_Max")),
  setNames(lapply(.SD, function(col) min(col, na.rm = TRUE)), paste0(names(.SD), "_Min")),
  setNames(lapply(.SD, function(col) sd(col, na.rm = TRUE)), paste0(names(.SD), "_SD"))
), by = .(DayLabel), .SDcols = patterns("^Onwind_CF_")]

offwind_features <- Offwind_CF_wide[, c(
  setNames(lapply(.SD, function(col) mean(col, na.rm = TRUE)), paste0(names(.SD), "_Mean")),
  setNames(lapply(.SD, function(col) max(col, na.rm = TRUE)), paste0(names(.SD), "_Max")),
  setNames(lapply(.SD, function(col) min(col, na.rm = TRUE)), paste0(names(.SD), "_Min")),
  setNames(lapply(.SD, function(col) sd(col, na.rm = TRUE)), paste0(names(.SD), "_SD"))
), by = .(DayLabel), .SDcols = patterns("^Offwind_CF_")]



# All data added to demand dataset
feature_data <- merge(demand_features, solar_features, by = c("DayLabel"), all.x = FALSE)
feature_data <- merge(feature_data, onwind_features, by = c("DayLabel"), all.x = FALSE)
feature_data <- merge(feature_data, offwind_features, by = c("DayLabel"), all.x = FALSE)

# Step 2: Normalize Features
normalized_features <- feature_data[, lapply(.SD, scale), .SDcols = -c("DayLabel", "Date")]

# Add Date back for Reference
normalized_data <- cbind(feature_data[, .(Date)], normalized_features)

# clean data
anyNA(normalized_data)
normalized_data <- normalized_data[, lapply(.SD, function(x) {
  if (is.numeric(x)) x[is.na(x) | is.nan(x) | is.infinite(x)] <- 0
  return(x)
})]

# Step 3: Apply k-means Clustering
set.seed(42)
num_clusters <- 45  # Adjust as needed
kmeans_result <- kmeans(as.matrix(normalized_data[, -c("Date")]), centers = num_clusters)

# Add Cluster Assignments
normalized_data[, Cluster := kmeans_result$cluster]

# Step 4: Compute Distances to Centroids for Each Data Point
# Ensure centroids are available and correctly indexed
centroids <- kmeans_result$centers

# Calculate distances to centroids
normalized_data[, Distance_to_Centroid := sapply(1:.N, function(i) {
  cluster_id <- Cluster[i]  # Access the cluster assignment for the row
  sum((as.numeric(.SD[i, ]) - centroids[cluster_id, ])^2)
}), .SDcols = -c("Date", "Cluster")]


# Step 5: Select Representative Days
representative_days <- normalized_data[, .SD[which.min(Distance_to_Centroid)], by = Cluster]

# Step 6: Add Two Days Before and After Each Representative Date
extended_days <- representative_days[, {
  seq_start <- Date - 3
  seq_end <- Date + 3
  data.table(Date = seq(seq_start, seq_end, by = 1), Cluster)
}, by = Cluster]

# Step 7: Count Representative Days Per Year
rep_days_per_year <- extended_days[, .N, by = year(Date)]
day_label_count <- extended_days[, .N, by = yday(Date)]
  
# Save the Date column as a CSV file
output_file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/representative_days.csv"
fwrite(extended_days[, .(Date)], file = output_file_path)

# Assign representative days to demand data
cluster_to_rep_day <- representative_days[, .(Cluster, Representative_Date = Date)]

demand_with_rep_days <- merge(normalized_data, cluster_to_rep_day, by = "Cluster", all.x = TRUE)
demand_final <- merge(Demand_data, demand_with_rep_days[, .(Date, Representative_Date)], by = "Date", all.x = TRUE)

# Save the Date column as a CSV file
output_file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/1 Demand/1 Hourly Demand/demand_data_and_rep_days.csv"
fwrite(demand_final, file = output_file_path)

