library(ggplot2)
library(data.table)
library(dplyr)
library(tidyr)
library(stringdist)

stateCodes <- c(
  "AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "HI",
  "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN",
  "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH",
  "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VT",
  "WA", "WI", "WV", "WY"
)

# Only New England state codes
#stateCodes <- c("CT", "ME", "MA", "NH", "RI", "VT")


base_dir <- "/Users/amirgazar/Documents/GitHub/States Historical Data"
all_facilities_data <- list()

for (state in stateCodes) {
  state_dir <- file.path(base_dir, state)
  facilities_data_path <- file.path(state_dir, paste0("Facilities_Data_", state, "_Clean.csv"))
  
  if (file.exists(facilities_data_path)) {
    facilities_data <- fread(facilities_data_path)
    
    if (state == "DC" && !is.null(all_facilities_data[["AK"]])) {
      example_state <- all_facilities_data[["AK"]]
      class_38 <- class(example_state[[38]])[1]
      class_39 <- class(example_state[[39]])[1]
      
      if (class_38 == "IDate") {
        facilities_data[[38]] <- as.IDate(facilities_data[[38]], origin = "1970-01-01")
      }
      if (class_39 == "IDate") {
        facilities_data[[39]] <- as.IDate(facilities_data[[39]], origin = "1970-01-01")
      } else if (class_39 == "numeric") {
        facilities_data[[39]] <- as.numeric(facilities_data[[39]])
      } else if (class_39 == "character") {
        facilities_data[[39]] <- as.character(facilities_data[[39]])
      }
      facilities_data <- facilities_data[, 1:38]
    }
    
    facilities_data <- facilities_data[, .SD[which.max(Year)], by = "Facility_Unit.ID"]
    all_facilities_data[[state]] <- facilities_data
  }
}

combined_facilities_data <- rbindlist(all_facilities_data, fill = TRUE)

if ("start_date" %in% names(combined_facilities_data) & "end_date" %in% names(combined_facilities_data)) {
  combined_facilities_data[, start_date := as.IDate(start_date)]
  combined_facilities_data[, end_date := as.IDate(end_date)]
  combined_facilities_data[, date_range := as.numeric(end_date - start_date)]
}

ggplot(combined_facilities_data, aes(x = date_range)) +
  geom_histogram(bins = 30, fill = "red", color = "black", alpha = 0.7, center = 0) +
  scale_x_continuous(breaks = seq(0, max(combined_facilities_data$non_zero_count, na.rm = TRUE), by = 1000)) +
  labs(title = "Histogram of non_zero_count", x = "Non-Zero Count", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

base_dir <- "/Users/amirgazar/Documents/GitHub/States Historical Data"
usa_dir <- file.path(base_dir, "USA")

facilities_na_reliability <- combined_facilities_data[
  !grepl("retired", Operating_Status, ignore.case = TRUE) & 
    (is.na(Reliability_Score_3) | is.na(Reliability_Label) |
       is.na(mean_CO2_tons_MW) | is.na(mean_NOx_lbs_MW) |
       is.na(mean_SO2_lbs_MW) |
       Estimated_NameplateCapacity_MW == 0 | max_gen_MW == 0)
]
#facilities_na_reliability <- combined_facilities_data
cat("Number of facilities with NA reliability or zero max_gen_MW:", nrow(facilities_na_reliability), "\n")

facilities_na_reliability[
  !is.na(Reliability_Label), 
  Reliability_Label := NA_character_
]
cat("Replaced Reliability_Label with NA for unmatched facilities.\n")

facilities_high_reliability <- combined_facilities_data[Reliability_Label == "High"]
# CF factor for high reliablity
facilities_high_reliability$CF <- facilities_high_reliability$mean_gen_MW /facilities_high_reliability$Estimated_NameplateCapacity_MW
facilities_high_reliability <- facilities_high_reliability[facilities_high_reliability$CF <= 1, ]
facilities_high_reliability[, mean_HI_mmBtu_per_MW := mean_Heat_Input_mmBtu / mean_gen_MW]
facilities_high_reliability <- facilities_high_reliability[!is.na(mean_CO2_tons_MW) & 
                                                             !is.na(mean_SO2_lbs_MW) & 
                                                             !is.na(mean_NOx_lbs_MW) & 
                                                             !is.na(mean_HI_mmBtu_per_MW)]


cat("Number of high-reliability facilities:", nrow(facilities_high_reliability), "\n")

required_columns <- c("Unit_Type", "Primary_Fuel_Type", "State", "Estimated_NameplateCapacity_MW", "Facility_Unit.ID")

unit_type_mapping <- list(
  "boiler" = c("Dry bottom wall-fired boiler", "Other boiler", "Cyclone boiler", 
               "Dry bottom vertically-fired boiler", "Circulating fluidized bed boiler"),
  "tangentially-fired" = "Tangentially-fired",
  "combustion turbine" = "Combustion turbine",
  "process heater" = "Process Heater",
  "stoker" = "Stoker",
  "cement kiln" = "Cement Kiln",
  "combined cycle" = "Combined cycle"
)

map_unit_type <- function(unit_type) {
  for (standard_type in names(unit_type_mapping)) {
    if (unit_type %in% unit_type_mapping[[standard_type]]) {
      return(standard_type)
    }
  }
  return(tolower(trimws(unit_type)))
}

facilities_na_reliability[, Unit_Type_Standard := sapply(Unit_Type, map_unit_type)]
facilities_high_reliability[, Unit_Type_Standard := sapply(Unit_Type, map_unit_type)]
facilities_na_reliability[, Unit_Type_Standard := tolower(Unit_Type_Standard)]
facilities_high_reliability[, Unit_Type_Standard := tolower(Unit_Type_Standard)]
cat("Standardized Unit_Type entries.\n")

facilities_na_reliability_expanded <- facilities_na_reliability

facilities_na_reliability_expanded[, Similar_Facility_Unit_ID := NA_character_]

find_similar_unit_exact <- function(target, high_rel_data) {
  target_unit_type <- target$Unit_Type_Standard
  target_primary_fuel <- target$Primary_Fuel_Type
  target_capacity <- target$Estimated_NameplateCapacity_MW
  
  if (!is.na(target_capacity) && target_capacity > 0) {
    matched <- high_rel_data[
      Unit_Type_Standard == target_unit_type &
        Estimated_NameplateCapacity_MW >= (target_capacity - 20) &
        Estimated_NameplateCapacity_MW <= (target_capacity + 20)
    ]
    if (nrow(matched) > 0) {
      nrow(matched)
      matched[, capacity_diff := abs(Estimated_NameplateCapacity_MW - target_capacity)]
      return(matched[which.min(capacity_diff)]$Facility_Unit.ID)
    }
  }

  matched_no_capacity <- high_rel_data[
    Unit_Type_Standard == target_unit_type &
      Primary_Fuel_Type == target_primary_fuel
  ]
  if (nrow(matched_no_capacity) > 0) {
    nrow(matched_no_capacity)
    return(matched_no_capacity$Facility_Unit.ID[1])
  }

  matched_unit <- high_rel_data[
    Unit_Type_Standard == target_unit_type
  ]
  if (nrow(matched_unit) > 0) {
    nrow(matched_unit)
    return(matched_unit$Facility_Unit.ID[1])
  }
  return(NA_character_)
}

cat("Starting assignment of Similar_Facility_Unit_ID...\n")
for (i in 1:nrow(facilities_na_reliability_expanded)) {
  target <- facilities_na_reliability_expanded[i]
  similar_unit <- find_similar_unit_exact(target, facilities_high_reliability)
  facilities_na_reliability_expanded$Similar_Facility_Unit_ID[i] <- similar_unit
  if (i %% 100 == 0) {
    cat("Processed", i, "of", nrow(facilities_na_reliability_expanded), "facilities\n")
  }
}
cat("Assignment of Similar_Facility_Unit_ID completed.\n")

similar_unit_mapping <- facilities_na_reliability_expanded[, .(Facility_Unit.ID, Similar_Facility_Unit_ID)]

if (!"Similar_Facility_Unit_ID" %in% names(combined_facilities_data)) {
  combined_facilities_data[, Similar_Facility_Unit_ID := NA_character_]
  cat("Initialized Similar_Facility_Unit_ID column in combined_facilities_data.\n")
} else {
  cat("Similar_Facility_Unit_ID column already exists in combined_facilities_data.\n")
}

combined_facilities_data <- merge(
  combined_facilities_data,
  similar_unit_mapping,
  by = "Facility_Unit.ID",
  all.x = TRUE,
  suffixes = c("", "_new")
)

combined_facilities_data[!is.na(Similar_Facility_Unit_ID_new), Similar_Facility_Unit_ID := Similar_Facility_Unit_ID_new]
combined_facilities_data[, Similar_Facility_Unit_ID_new := NULL]
cat("Merged Similar_Facility_Unit_ID back into combined_facilities_data.\n")

setnames(facilities_high_reliability, "Facility_Unit.ID", "Similar_Facility_Unit_ID")
combined_facilities_data <- merge(
  combined_facilities_data,
  facilities_high_reliability[, .(
    Similar_Facility_Unit_ID, 
    CF, 
    mean_CO2_tons_MW, 
    mean_NOx_lbs_MW, 
    mean_SO2_lbs_MW, 
    mean_HI_mmBtu_per_MW
  )],
  by = "Similar_Facility_Unit_ID",
  all.x = TRUE,
  suffixes = c("", "_estimate")
)

# Convert 'Commercial_Operation_Date' to Date format if not already
combined_facilities_data$Commercial_Operation_Date <- as.Date(combined_facilities_data$Commercial_Operation_Date)

# Extract the year from the 'Commercial_Operation_Date'
combined_facilities_data$Operation_Year <- format(combined_facilities_data$Commercial_Operation_Date, "%Y")

# Calculate the age of the facility
current_year <- 2024
combined_facilities_data$Age_of_Facility <- current_year - as.numeric(combined_facilities_data$Operation_Year)


output_path <- file.path(usa_dir, "Facilities_Data_USA_Clean.csv")
fwrite(combined_facilities_data, output_path)
cat("Updated Facilities_Data_USA_Clean.csv has been saved successfully at:", output_path, "\n")