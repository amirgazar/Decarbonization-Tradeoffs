# Load necessary libraries
library(readxl)
library(data.table)
library(lubridate) # For date handling

# Define the file path
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/1 Decarbonization Pathways/Decarbonization_Pathways.xlsx"

# Get all sheet names
sheet_names <- excel_sheets(file_path)

# Initialize an empty list to store data tables
data_tables <- list()

# Loop through each sheet, read it into a data table, and add the Pathway column
for (sheet in sheet_names) {
  # Read the sheet
  sheet_data <- as.data.table(read_excel(file_path, sheet = sheet))
  
  # Add the Pathway column
  sheet_data[, Pathway := sheet]
  
  # Store in the list
  data_tables[[sheet]] <- sheet_data
}

# Combine all data tables into one
combined_data <- rbindlist(data_tables, fill = TRUE)

# Extract the range of years from the combined data
years <- unique(as.numeric(combined_data$Year)) # Assuming 'Year' column exists in the combined data
start_year <- min(years, na.rm = TRUE)
end_year <- max(years, na.rm = TRUE)

# Generate a complete data.table with all days, hours, and years
# Create a sequence of dates
date_seq <- seq.Date(
  from = as.Date(paste0(start_year, "-01-01")),
  to = as.Date(paste0(end_year, "-12-31")),
  by = "day"
)

# Create a data.table with every hour for every day
base_table <- data.table(
  Date = rep(date_seq, each = 24),
  Hour = rep(1:24, times = length(date_seq))
)

# Add DayLabel (Day of the year: 1 to 366) and Year columns
base_table[, DayLabel := yday(Date)]
base_table[, Year := year(Date)]

# Perform a left join, mapping annual data to each hourly record
final_table <- merge(
  base_table, 
  combined_data, 
  by = "Year", 
  all.x = TRUE, 
  allow.cartesian = TRUE
)

# Extract the spot market benchmark from 2024
spot_capacity <- final_table[Year == 2024, unique(`Imports QC`)]
# In case there are multiple values, take the first one:
spot_capacity <- spot_capacity[1]

# For each row, if the QC imports exceed the 2024 level, assign:
# - Spot market imports: the 2024 benchmark (or the available amount if below that)
# - Long-term imports: the excess capacity above the 2024 level
final_table[, Spot_Market_QC_Imports := ifelse(`Imports QC` < spot_capacity, `Imports QC`, spot_capacity)]
final_table[, Long_Term_QC_Imports := ifelse(`Imports QC` > spot_capacity, `Imports QC` - spot_capacity, 0)]

# Rename capacity-related columns to include the _MW suffix
setnames(final_table,
         old = c("Nuclear", "Hydropower", "Biomass", "Solar", "Onshore Wind", "Offshore Wind", 
                 "SMR", "New NG", "Imports QC", "Imports NBSO", "Imports NYISO", "Storage",
                 "Spot_Market_QC_Imports", "Long_Term_QC_Imports"),
         new = c("Nuclear_MW", "Hydropower_MW", "Biomass_MW", "Solar_MW", "Onshore_Wind_MW", "Offshore_Wind_MW", 
                 "SMR_MW", "New_NG_MW", "Imports_HQ_MW", "Imports_NBSO_MW", "Imports_NYISO_MW", "Storage_MW",
                 "Spot_Market_Imports_HQ_MW", "Long_Term_Imports_HQ_MW"))


# Save the final table to the output file
output_file_path <- file.path(dirname(file_path), "Hourly_Installed_Capacity.csv")
fwrite(final_table, output_file_path)

