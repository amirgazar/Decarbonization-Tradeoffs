# Specify the existing library path
lib_path <- "/home/amirgazar/Fossil_Fuels/env"

# Function to load packages
load_packages <- function(package, lib) {
  if (!require(package, character.only = TRUE, lib.loc = lib)) {
    stop(paste("Package", package, "is not available in the library path:", lib))
  }
}

# Load the packages from the specified library
load_packages("httr", lib_path)
load_packages("htmltools", lib_path)
load_packages("jsonlite", lib_path)
load_packages("data.table", lib_path)
load_packages("lubridate", lib_path)


# Preparing for analysis
# Function
calculate_hourly_percentiles <- function(dt) {
  p_low <- 1
  p_up <- 99
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


#batch processing
stateCodes <- "TX"

print(stateCodes)

# Define the base directory
base_dir <- "/home/amirgazar/EPAdecarbonization/Dataset/States Historical Data"

# Loop over each state
for (state in stateCodes) {
  
  # Define the state directory dynamically
  state_dir <- file.path(base_dir, state)
  
  # Define the path to read the emissions data
  emissions_data_path <- file.path(state_dir, paste0("Hourly_Emissions_", state, "_Clean.rds"))
  
  # Load the emissions data
  if (file.exists(emissions_data_path)) {
    emissions_data <- readRDS(emissions_data_path)
    setDT(emissions_data)
    print(state)
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
    
    # Create a lookup table for DayLabel, Month, and Season for both leap years and non-leap years
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
    
    # Assign Season based on DayLabel
    Fossil_Fuels_Gen[, Season := fifelse(Leap_Year, leap_day_month_season[DayLabel, Season], non_leap_day_month_season[DayLabel, Season])]
    
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

# Pushover alert - Pushover credentials
pushover_user <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST("https://api.pushover.net/1/messages.json",
                 body = list(
                   token = pushover_token,
                   user = pushover_user,
                   message = "The calculate_hourly_percentiles function has finished running."
                 ),
                 encode = "form")

