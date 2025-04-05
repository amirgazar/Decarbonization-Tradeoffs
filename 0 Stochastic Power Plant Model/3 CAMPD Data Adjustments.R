# Required libraries
library(dplyr)
library(lubridate)
library(purrr)
library(stringr)
library(arrow)
library(data.table)

# Define state codes
stateCodes <- c("CT", "ME", "MA", "NH", "RI", "VT")

# Define the base directory
base_dir <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EPA CAMPD"

process_state_data <- function(state) {
  # Define directories and file paths
  emissions_dir <- file.path(base_dir, state, "Yearly")
  facilities_data_path <- file.path(base_dir, state, paste0("Facilities_Data_", state, ".parquet"))
  
  # Initialize list to store processed data
  result <- list(facilities_data = NULL, emissions_data = NULL)
  
  # -------------------------------------------------------------------------
  # Part A: Load Emissions Data from Parquet files in the "Yearly" folder
  # -------------------------------------------------------------------------
  parquet_files <- list.files(
    path       = emissions_dir,
    pattern    = paste0("Hourly_Emissions_", state, "_\\d{4}\\.parquet$"),
    full.names = TRUE
  )
  
  if (length(parquet_files) > 0) {
    emissions_data <- purrr::map_dfr(parquet_files, function(file) {
      df <- arrow::read_parquet(file)
      # Convert Unit.ID to character to avoid type conflicts
      if ("Unit.ID" %in% names(df)) {
        df$`Unit.ID` <- as.character(df$`Unit.ID`)
      }
      return(df)
    })
    
    # Standardize column names for emissions data
    emissions_data <- emissions_data %>%
      rename(
        State                = State,
        Facility_Name        = Facility.Name,
        Facility_ID          = Facility.ID,
        Unit_ID              = Unit.ID,
        Associated_Stacks    = Associated.Stacks,
        Date                 = Date,
        Hour                 = Hour,
        Operating_Time       = Operating.Time,
        Gross_Load_MW        = Gross.Load..MW.,
        SO2_Mass_lbs         = SO2.Mass..lbs.,
        CO2_Mass_short_tons  = CO2.Mass..short.tons.,
        NOx_Mass_lbs         = NOx.Mass..lbs.,
        Heat_Input_mmBtu     = Heat.Input..mmBtu.,
        Primary_Fuel_Type    = Primary.Fuel.Type,
        Secondary_Fuel_Type  = Secondary.Fuel.Type,
        Unit_Type            = Unit.Type
      )
    
    # Ensure 'Date' is formatted correctly
    if ("Date" %in% names(emissions_data) && !inherits(emissions_data$Date, "Date")) {
      emissions_data$Date <- ymd(emissions_data$Date)
    }
    
    # Add a column for the day of the year
    emissions_data <- emissions_data %>% mutate(DayLabel = yday(Date))
    
    # Increment 'Hour' by 1 (if needed)
    if ("Hour" %in% names(emissions_data)) {
      emissions_data$Hour <- emissions_data$Hour + 1
    }
    
    # Create a combined Facility_Unit.ID
    emissions_data <- emissions_data %>% mutate(Facility_Unit.ID = paste(Facility_ID, Unit_ID, sep = "_"))
    
    result$emissions_data <- emissions_data
    
    # ---------------------------------------------------------------------
    # Save the processed hourly emissions data as a Parquet file
    # ---------------------------------------------------------------------
    emissions_clean_parquet_path <- file.path(base_dir, state, paste0("Hourly_Emissions_", state, "_Clean.parquet"))
    arrow::write_parquet(emissions_data, emissions_clean_parquet_path)
    
    # ---- Calculate Reliability Metrics ----
    valid_points <- emissions_data %>%
      group_by(Facility_Unit.ID, DayLabel, Hour) %>%
      summarise(Valid_Data_Points = sum(!is.na(Gross_Load_MW), na.rm = TRUE), .groups = "drop") %>%
      mutate(Threshold_3 = Valid_Data_Points >= 3)
    
    sufficient_points <- valid_points %>%
      group_by(Facility_Unit.ID) %>%
      summarise(Time_Slots_Above_3 = sum(Threshold_3), .groups = "drop")
    
    total_possible_points <- emissions_data %>%
      group_by(Facility_Unit.ID) %>%
      summarise(Total_Expected_Points = n_distinct(DayLabel) * 24, .groups = "drop")
    
    reliability_data <- total_possible_points %>%
      left_join(sufficient_points, by = "Facility_Unit.ID") %>%
      mutate(
        Time_Slots_Above_3 = ifelse(is.na(Time_Slots_Above_3), 0, Time_Slots_Above_3),
        Reliability_Score_3 = (Time_Slots_Above_3 / Total_Expected_Points) * 100,
        Reliability_Label = case_when(
          Reliability_Score_3 >= 90 ~ "High",
          Reliability_Score_3 >= 70 & Reliability_Score_3 < 90 ~ "Medium",
          TRUE ~ "Low"
        )
      )
    
    summary_data <- emissions_data %>%
      group_by(Facility_Unit.ID) %>%
      summarise(
        min_gen_MW            = ifelse(all(is.na(Gross_Load_MW)), NA, min(Gross_Load_MW, na.rm = TRUE)),
        mean_gen_MW           = mean(Gross_Load_MW, na.rm = TRUE),
        max_gen_MW            = ifelse(all(is.na(Gross_Load_MW)), NA, max(Gross_Load_MW, na.rm = TRUE)),
        avg_operating_time    = mean(Operating_Time, na.rm = TRUE),
        non_zero_count        = sum(!is.na(Gross_Load_MW) & Gross_Load_MW != 0),
        unique_day_hours      = n_distinct(DayLabel, Hour),
        start_date            = min(Date, na.rm = TRUE),
        end_date              = max(Date, na.rm = TRUE),
        non_zero_count_CO2    = sum(!is.na(CO2_Mass_short_tons) & CO2_Mass_short_tons != 0),
        mean_CO2_tons_MW      = mean(CO2_Mass_short_tons, na.rm = TRUE) / mean_gen_MW,
        mean_NOx_lbs_MW       = mean(NOx_Mass_lbs, na.rm = TRUE) / mean_gen_MW,
        mean_SO2_lbs_MW       = mean(SO2_Mass_lbs, na.rm = TRUE) / mean_gen_MW,
        mean_Heat_Input_mmBtu  = mean(Heat_Input_mmBtu, na.rm = TRUE),
        .groups = "drop"
      )
    
  } else {
    warning(paste("No emissions data found for state:", state))
  }
  
  # -------------------------------------------------------------------------
  # Part B: Load Facilities Data from a top-level Parquet file
  # -------------------------------------------------------------------------
  if (file.exists(facilities_data_path)) {
    facilities_data <- arrow::read_parquet(facilities_data_path)
    
    facilities_data <- facilities_data %>%
      rename(
        State                          = stateCode,
        Facility_Name                  = facilityName,
        Facility_ID                    = facilityId,
        Unit_ID                        = unitId,
        Associated_Stacks              = associatedStacks,
        Year                           = year,
        Program_Code                   = programCodeInfo,
        Primary_Rep_Info               = primaryRepInfo,
        EPA_Region                     = epaRegion,
        NERC_Region                    = nercRegion,
        County                         = county,
        County_Code                    = countyCode,
        FIPS_Code                      = fipsCode,
        Source_Category                = sourceCategory,
        Latitude                       = latitude,
        Longitude                      = longitude,
        Owner_Operator                 = ownerOperator,
        SO2_Phase                      = so2Phase,
        NOx_Phase                      = noxPhase,
        Unit_Type                      = unitType,
        Primary_Fuel_Type              = primaryFuelInfo,
        Secondary_Fuel_Type            = secondaryFuelInfo,
        SO2_Controls                   = so2ControlInfo,
        NOx_Controls                   = noxControlInfo,
        PM_Controls                    = pmControlInfo,
        Hg_Controls                    = hgControlInfo,
        Commercial_Operation_Date      = commercialOperationDate,
        Operating_Status               = operatingStatus,
        Max_Hourly_HI_Rate             = maxHourlyHIRate,
        Associated_Generators_Capacity = associatedGeneratorsAndNameplateCapacity
      )
    
    facilities_data <- facilities_data %>% 
      mutate(Facility_Unit.ID = paste(Facility_ID, Unit_ID, sep = "_"))
    
    facilities_data <- facilities_data %>%
      distinct(Facility_Unit.ID, Operating_Status, Associated_Generators_Capacity, .keep_all = TRUE)
    
    # Estimate nameplate capacity from the text field
    nameplateData <- data.frame(facilities_data$Associated_Generators_Capacity, stringsAsFactors = FALSE)
    nameplateData <- nameplateData %>%
      mutate(
        extracted_values = sapply(
          str_extract_all(facilities_data$Associated_Generators_Capacity, "(?<=\\()\\d+\\.?\\d*"),
          function(x) paste(unlist(x), collapse = ", ")
        ),
        Estimated_NameplateCapacity_MW = sapply(
          str_extract_all(facilities_data$Associated_Generators_Capacity, "(?<=\\()\\d+\\.?\\d*"),
          function(x) sum(as.numeric(unlist(x)))
        )
      )
    facilities_data$Estimated_NameplateCapacity_MW <- nameplateData$Estimated_NameplateCapacity_MW
    
    # Join with emissions summary and reliability data if available
    if (exists("summary_data")) {
      facilities_data <- facilities_data %>%
        left_join(summary_data, by = "Facility_Unit.ID") %>%
        left_join(reliability_data, by = "Facility_Unit.ID")
    } else {
      message("summary_data not available, skipping join.")
    }
    
    result$facilities_data <- facilities_data
    
    # Save cleaned facilities data as a Parquet file
    facilities_clean_parquet_path <- file.path(base_dir, state, paste0("Facilities_Data_", state, "_Clean.parquet"))
    arrow::write_parquet(facilities_data, facilities_clean_parquet_path)
    
  } else {
    warning(paste("Facilities data file not found for state:", state))
  }
  
  gc()  # Invoke garbage collection
  return(result)
}

# Loop over states
for (state in stateCodes) {
  process_state_data(state)
  gc()
}
