# Load the packages from the specified library
library("httr")
library("htmltools")
library("jsonlite")
library("data.table")
library("lubridate")
library(rmarkdown)
library(dplyr)

# Define state codes
stateCodes <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", 
                "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", 
                "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", 
                "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", 
                "WA", "WI", "WV", "WY")

state <- c("MA", "RI", "CT", "VT", "ME", "NH")
state <- "NH"

# Define the base directory
base_dir <- "/Users/amirgazar/Documents/GitHub/States Historical Data"

# Define the state directory dynamically
state_dir <- file.path(base_dir, state)

# Define the path to read the facilities data
facilities_data_path <- file.path(state_dir, paste0("Facilities_Data_", state, "_Clean.csv"))
emissions_data_path <- file.path(state_dir, paste0("Hourly_Stochastic_Gen_", state, ".csv"))

# Read the CSV file into a data.table
emissions_data <- fread(emissions_data_path)
facilities_data <- fread(facilities_data_path)

# Select the most recent year for each Facility_Unit.ID
facilities_data <- facilities_data[ , .SD[which.max(Year)], by = "Facility_Unit.ID"]

# Identify Facility_Unit.IDs that are "Retired" in facilities_data
retired_ids <- facilities_data[grepl("Retired", Operating_Status), Facility_Unit.ID]

# Exclude those Facility_Unit.IDs from emissions_data
emissions_data_operating <- emissions_data[!Facility_Unit.ID %in% retired_ids]

# Define a function to calculate mean of sums for a given column prefix
calculate_sum <- function(column_prefix) {
  # Create an empty data.table to store results for each i
  result_dt <- data.table()
  
  # Loop through 1 to 99 to calculate sums for each column
  for (i in 1:99) {
    column_name <- paste0(column_prefix, "_", i)  # Generate the column name
    column_gen <- paste0("Gen_", i)  # Generate the column name
    # Calculate sum for each Facility_Unit.ID, then take the mean of the sums
    sum_value <- emissions_data_operating[
      is.finite(get(column_gen)) & is.finite(get(column_name)),  # Filter finite values for column_gen and current column
      .(sum_value = sum(get(column_gen) * get(column_name) / 1e3, na.rm = TRUE))
    ]
    
    # Add the sum_value to the data.table as a new column
    result_dt[, (paste0(column_prefix, "_", i)) := sum_value$sum_value]
  }
  
  return(result_dt)  # Return the data.table with all sums for this prefix
}

# List of column prefixes to apply the function
column_prefixes <- c("CO2", "NOx", "SO2")

# Create a data.table to store results for all column prefixes
sums_results <- data.table()

# Loop through each prefix and bind the results into the sums_results data.table
for (prefix in column_prefixes) {
  result_dt <- calculate_sum(prefix)
  sums_results <- cbind(sums_results, result_dt)  # Combine results from all prefixes
}

# Create the Reports folder
report_dir <- file.path(state_dir, "Reports")
if (!dir.exists(report_dir)) {
  dir.create(report_dir)
}

# Loop through each Facility_Unit.ID
unique_facilities <- unique(facilities_data$Facility_Unit.ID)
#unique_facilities<-"54605_001"

for (facility in unique_facilities) {
  facility_name <- facilities_data %>%
    filter(Facility_Unit.ID == facility) %>%
    pull(Facility_Name) %>%
    unique()
  facility_name <- gsub("&", "\\\\&", facility_name)
  
  facility_data <- emissions_data %>%
    filter(Facility_Unit.ID == facility)
  
  # Skip the facility if no data is available or if max_gen is zero
  if (nrow(facility_data) == 0 || facilities_data$max_gen_MW[facilities_data$Facility_Unit.ID == facility][1] == 0) {
    next
  }
  
  report_path <- file.path(report_dir, paste0("Report_", facility, ".pdf"))
  
  rmarkdown::render(
    input = file.path("/Users/amirgazar/Documents/GitHub/EPA_Debarbonization/ARC SSH Fossil Fuels USA/Automation", "facility_report_template.Rmd"),
    output_file = report_path,
    params = list(
      state = facilities_data$State[facilities_data$Facility_Unit.ID == facility][1],
      operating_status = facilities_data$Operating_Status[facilities_data$Facility_Unit.ID == facility][1],
      facility_name = facility_name,
      facility_id = facilities_data$Facility_ID[facilities_data$Facility_Unit.ID == facility][1],
      unit_id = facilities_data$Unit_ID[facilities_data$Facility_Unit.ID == facility][1],
      county = facilities_data$County[facilities_data$Facility_Unit.ID == facility][1],
      primary_fuel = facilities_data$Primary_Fuel_Type[facilities_data$Facility_Unit.ID == facility][1],
      npc = facilities_data$Associated_Generators_Capacity[facilities_data$Facility_Unit.ID == facility][1],
      min_gen = round(facilities_data$min_gen_MW[facilities_data$Facility_Unit.ID == facility][1]),
      mean_gen = round(facilities_data$mean_gen_MW[facilities_data$Facility_Unit.ID == facility][1]),
      max_gen = round(facilities_data$max_gen_MW[facilities_data$Facility_Unit.ID == facility][1]),
      retired_ids = retired_ids,
      emissions_data_facility = facility_data,
      historical_data_availability = facilities_data$Reliability_Label[facilities_data$Facility_Unit.ID == facility][1],
      emissions_sum = sums_results
    ),
    envir = new.env(parent = globalenv())
  )
  gc()
}

