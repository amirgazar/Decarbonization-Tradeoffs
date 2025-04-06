# Load the required packages
library(httr)
library(htmltools)
library(jsonlite)
library(data.table)
library(lubridate)
library(arrow)

# Function to calculate hourly percentiles
calculate_hourly_percentiles <- function(dt) {
  p_low <- 50
  p_up <- 51
  percentiles <- seq(p_low/100, p_up/100, by = 0.01)
  quantile_names_Gen <- paste0("Gen_", p_low:p_up)
  quantile_names_CO2 <- paste0("CO2_", p_low:p_up)
  quantile_names_NOx <- paste0("NOx_", p_low:p_up)
  quantile_names_SO2 <- paste0("SO2_", p_low:p_up)
  quantile_names_HI <- paste0("HI_", p_low:p_up)
  
  dt[, c(setNames(lapply(percentiles, function(p) quantile(Gen_MW, probs = p, na.rm = TRUE)), quantile_names_Gen),
         setNames(lapply(percentiles, function(p) quantile(CO2_tons_per_MW, probs = p, na.rm = TRUE)), quantile_names_CO2),
         setNames(lapply(percentiles, function(p) quantile(NOx_lbs_per_MW, probs = p, na.rm = TRUE)), quantile_names_NOx),
         setNames(lapply(percentiles, function(p) quantile(SO2_lbs_per_MW, probs = p, na.rm = TRUE)), quantile_names_SO2),
         setNames(lapply(percentiles, function(p) quantile(Heat_Input_mmBtu, probs = p, na.rm = TRUE)), quantile_names_HI)),
     by = .(Season, DayLabel, Hour, Facility_Unit.ID)]
}

# Define state codes
stateCodes <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI", 
                "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", 
                "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", 
                "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT", 
                "WA", "WI", "WV", "WY")

# For this example, we'll process only state "AL"
stateCodes <- "AL"
state <- "AL"

# Define the base directory (adjust this to your folder location)
base_dir <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EPA CAMPD Data"

# Loop over each state in stateCodes
for (state in stateCodes) {
  
  # Define the state directory dynamically
  state_dir <- file.path(base_dir, state)
  
  # Define the path to read the emissions data (Parquet file)
  emissions_data_path <- file.path(state_dir, paste0("Hourly_Emissions_", state, "_Clean.parquet"))
  
  # Load the emissions data if it exists
  if (file.exists(emissions_data_path)) {
    emissions_data <- arrow::read_parquet(emissions_data_path)
    setDT(emissions_data)
    
    # Prepare the Fossil_Fuels_Gen dataset
    Fossil_Fuels_Gen <- emissions_data[, .(
      Date = Date,
      DayLabel = DayLabel, 
      Hour = Hour,
      Facility_Unit.ID = Facility_Unit.ID,
      Gen_MW = Gross_Load_MW,
      CO2_tons_per_MW = CO2_Mass_short_tons / Gross_Load_MW,
      NOx_lbs_per_MW = NOx_Mass_lbs / Gross_Load_MW,
      SO2_lbs_per_MW = SO2_Mass_lbs / Gross_Load_MW,
      Heat_Input_mmBtu = Heat_Input_mmBtu
    )]
    
    # Create lookup tables for assigning Season based on DayLabel
    non_leap_day_month_season <- data.table(
      DayLabel = 1:365,
      Season = rep(c("Winter", "Spring", "Summer", "Fall"), each = 91, length.out = 365)
    )
    
    leap_day_month_season <- data.table(
      DayLabel = 1:366,
      Season = rep(c("Winter", "Spring", "Summer", "Fall"), each = 91, length.out = 366)
    )
    
    # Identify leap years
    Fossil_Fuels_Gen[, Leap_Year := year(Date) %% 4 == 0 & (year(Date) %% 100 != 0 | year(Date) %% 400 == 0)]
    
    # Assign Season based on DayLabel (using fifelse for fast if-else)
    Fossil_Fuels_Gen[, Season := fifelse(Leap_Year, 
                                         leap_day_month_season[DayLabel, Season], 
                                         non_leap_day_month_season[DayLabel, Season])]
    
    # Calculate hourly percentiles
    Fossil_Fuels <- calculate_hourly_percentiles(Fossil_Fuels_Gen)
    setDT(Fossil_Fuels)
    Fossil_Fuels[, `:=`(Season = NULL)]
    
    # Define the new path to save the CSV file
    save_path <- file.path(state_dir, paste0("Hourly_Stochastic_Gen_", state, ".csv"))
    
    # Save the results as a CSV file
    fwrite(Fossil_Fuels, save_path)
    gc()
    
    print(paste("Processed and saved data for", state))
  } else {
    print(paste("No data found for", state))
  }
}
