# Load necessary libraries
library(sf)
library(ggplot2)
library(dplyr)
library(readr)
library(tigris)
library(data.table)

# Load the GeoJSON files
us_boundary <- st_read('/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. Census Geo Data/US.json')
county_boundaries <- st_read('/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. Census Geo Data/New_England_county_boundaries.json')

# Set CRS for county_boundaries if missing
if (is.na(st_crs(county_boundaries))) {
  county_boundaries <- st_set_crs(county_boundaries, 4326)
}

# Load Results
#-- Stepwise
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Facility_Level_Results.csv"
output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/"

Yearly_Facility_Level_Results <- fread(file_path)
Yearly_Facility_Level_Results[, V1 := NULL]
Yearly_Facility_Level_Results[, V1 := NULL]

# Remove rows with missing latitude or longitude values
facilities_clean <- Yearly_Facility_Level_Results %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Convert lat/long data to an sf object
facilities_sf <- st_as_sf(facilities_clean, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

# Get county boundaries using tigris
counties_data <- counties(cb = TRUE, resolution = "20m", class = "sf")
counties_data <- st_transform(counties_data, 4326)  # Transform counties data to WGS 84

# Perform spatial join to get county information
facilities_sf <- st_join(facilities_sf, counties_data, join = st_within)

# Fix the issue with CT

# Mapping of planning regions to counties
planning_to_county <- c(
  "South Central Connecticut" = "New Haven",
  "Lower Connecticut River Valley" = "Middlesex",
  "Greater Bridgeport" = "Fairfield",
  "Naugatuck Valley" = "New Haven",
  "Southeastern Connecticut" = "New London",
  "Capitol" = "Hartford",
  "Northeastern Connecticut" = "Windham",
  "Western Connecticut" = "Litchfield",
  "Northwest Hills" = "Litchfield"
)

# Mapping of county names to GEOIDs
county_to_geoid <- c(
  "Fairfield" = "9001",
  "Hartford" = "9003",
  "Litchfield" = "9005",
  "Middlesex" = "9007",
  "New Haven" = "9009",
  "New London" = "9011",
  "Tolland" = "9013",
  "Windham" = "9015"
)

# Convert data to data.table if not already
if (!is.data.table(facilities_sf)) {
  facilities_sf <- as.data.table(facilities_sf)
}

# Replace planning regions with county names
facilities_sf[STUSPS == "CT", NAME := planning_to_county[NAME]]

# Update the GEOID based on the county name
facilities_sf[STUSPS == "CT", GEOID := county_to_geoid[NAME]]

# Add state abbreviation to county name
facilities_sf <- facilities_sf %>%
  mutate(County_State = paste(NAME, STUSPS, sep = ", "))

# Address NA issues
# Create a data table with only rows where NOx is NA
na_rows <- facilities_sf[is.na(GEOID), ]
na_facilities <- unique(na_rows$Facility_Unit.ID)

# Manually assign County_State if necessary
facilities_sf[Facility_Unit.ID == "10823_S42", `:=`(
  GEOID = "25025",
  NAME = "Suffolk",
  STATEFP = "25",
  COUNTYFP = "025",
  STATE_NAME = "Massachusetts",
  STUSPS = "MA",
  County_State = "Suffolk, MA"
)]

facilities_sf[Facility_Unit.ID == "10823_S43", `:=`(
  GEOID = "25025",
  NAME = "Suffolk",
  STATEFP = "25",
  COUNTYFP = "025",
  STATE_NAME = "Massachusetts",
  STUSPS = "MA",
  County_State = "Suffolk, MA"
)]

setnames(facilities_sf, "NAME", "County")

##---SAVE RESULTS 
# Save the dataframe as an RDS file
fwrite(facilities_sf, file = file.path(output_path, "Yearly_Facility_Level_Results_County_added_in.csv"))
