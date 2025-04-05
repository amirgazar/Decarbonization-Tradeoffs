# Loading libraries
library(data.table)
library(lubridate)

# Ramp Function
convert_ramp_to_numeric <- function(ramp) {
  if (is.na(ramp)) {
    return(24) # Assuming NA is 24 hours
  } else if (ramp == "10M") {
    return(0.1666667)  
  } else if (ramp == "1H") {
    return(1)  
  } else if (ramp == "12H") {
    return(12)  
  } else if (ramp == "OVER") {
    return(24)  # Assuming Over is 24 hours
  } else {
    return(24) # Assuming else is 24 hours
  }
}

# New England States
new_england_states <- c("CT", "ME", "MA", "NH", "RI", "VT")

# Load the data
usa_dir <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EPA CAMPD/USA/Facilities_Data_USA.csv"
facilities_data <- read.csv(usa_dir)
setDT(facilities_data)
facilities_data_NE <- facilities_data[facilities_data$State %in% new_england_states, ]
facilities_data_NE <- facilities_data_NE[!grepl("retired", facilities_data_NE$Operating_Status, ignore.case = TRUE), ]
facilities_data_NE <- facilities_data_NE[grepl("Utility", facilities_data_NE$Source_Category, ignore.case = TRUE), ]
facilities_data_NE_sum <- facilities_data_NE[, .(Total_NPC_MW = sum(Estimated_NameplateCapacity_MW, na.rm = TRUE)), by = Facility_ID]

## Integrating nameplate capacity from EIA 860 
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. EIA 860/eia8602023/3_1_Generator_Y2023.xlsx"
sheet_name <- "Operable"
eia_860_2023_NPC <- read_excel(file_path, sheet = sheet_name, skip = 1)
eia_860_2023_NPC<- eia_860_2023_NPC[eia_860_2023_NPC$State %in% new_england_states, ]
setDT(eia_860_2023_NPC)
eia_860_2023_NPC[, Facility_Unit.ID := paste0(`Plant Code`, "_", `Generator ID`)]
carbon_free_sources <- c("WAT", "SUN", "WND", "GEO", "WAS", "LFG", "AB", "MSW", "OB", "WDS", "BLQ", "HY")
eia_860_2023_NPC_fossil <- eia_860_2023_NPC[!(`Energy Source 1` %in% carbon_free_sources)]

eia_860_2023_NPC_fossil_sum <- eia_860_2023_NPC_fossil[, .(Total_NPC_MW_eia_850_2023 = sum(`Nameplate Capacity (MW)`, na.rm = TRUE)), by = `Plant Code`]

compare_dt <- facilities_data_NE_sum[eia_860_2023_NPC_fossil_sum,
                                    on = c("Facility_ID" = "Plant Code"), 
                                    .(Facility_ID = Facility_ID,  Total_NPC_MW, Total_NPC_MW_eia_850_2023), nomatch = 0L]

compare_dt <- compare_dt[, diff := Total_NPC_MW - Total_NPC_MW_eia_850_2023]

eia_860_2023_NPC_fossil_Ramp <- unique(eia_860_2023_NPC_fossil, by = "Plant Code")
eia_860_2023_NPC_fossil_Ramp <- eia_860_2023_NPC_fossil_Ramp[, .(Facility_ID = `Plant Code`, Ramp_category = `Time from Cold Shutdown to Full Load`, eia860_Retirement = `Planned Retirement Year`)]  


# Apply the conversion function
eia_860_2023_NPC_fossil_Ramp[, Ramp := sapply(Ramp_category, convert_ramp_to_numeric)]

facilities_data_NE <- merge(facilities_data_NE, eia_860_2023_NPC_fossil_Ramp, by = "Facility_ID", all.x = TRUE)

facilities_data_NE[, Ramp := nafill(Ramp, type = "locf"), by = Facility_ID]
# Replace remaining NAs with 24
facilities_data_NE[is.na(Ramp), Ramp := 24]

facilities_data_NE[, eia860_Retirement := nafill(eia860_Retirement, type = "locf"), by = Facility_ID]

# Manually entering some facilities nameplate capacity
facilities_data_NE[Facility_ID == 59882 & Unit_ID == "J4", Estimated_NameplateCapacity_MW := 131.8] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 59882 & Unit_ID == "J5", Estimated_NameplateCapacity_MW := 131.8] # source: eia 860 2023 & egrid 2022

facilities_data_NE[Facility_ID == 544 & Unit_ID == "10", Estimated_NameplateCapacity_MW := 18.6] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 544 & Unit_ID == "15", Estimated_NameplateCapacity_MW := 51.0] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 544 & Unit_ID == "16", Estimated_NameplateCapacity_MW := 51.0] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 544 & Unit_ID == "17", Estimated_NameplateCapacity_MW := 51.0] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 544 & Unit_ID == "18", Estimated_NameplateCapacity_MW := 51.0] # source: eia 860 2023 & egrid 2022

facilities_data_NE[Facility_ID == 565 & Unit_ID == 10, Estimated_NameplateCapacity_MW := 21.8] # source: eia 860 2023 & egrid 2022

facilities_data_NE[Facility_ID == 1507 & Unit_ID == 3, Estimated_NameplateCapacity_MW := 213.6] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 1507 & Unit_ID == 4, Estimated_NameplateCapacity_MW := 649.1] # source: eia 860 2023 & egrid 2022

facilities_data_NE[Facility_ID == 55026 & Unit_ID == 1, Estimated_NameplateCapacity_MW := 200] # source: eia 860 2023 & egrid 2022

facilities_data_NE[Facility_ID == 542 & Unit_ID == 10, Estimated_NameplateCapacity_MW := 25] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 542 & Unit_ID == 11, Estimated_NameplateCapacity_MW := 25] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 542 & Unit_ID == 12, Estimated_NameplateCapacity_MW := 25] # source: eia 860 2023 & egrid 2022

facilities_data_NE[Facility_ID == 1599 & Unit_ID == 1, Estimated_NameplateCapacity_MW := 585] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 1599 & Unit_ID == 2, Estimated_NameplateCapacity_MW := 580] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 1599 & Unit_ID == 3, Estimated_NameplateCapacity_MW := 367.7] # source: eia 860 2023 [WINTER NPC]

facilities_data_NE[Facility_ID == 55517 & Unit_ID == "CT01", Estimated_NameplateCapacity_MW := 50] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 55517 & Unit_ID == "CT02", Estimated_NameplateCapacity_MW := 50] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 55517 & Unit_ID == "CT03", Estimated_NameplateCapacity_MW := 50] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 55517 & Unit_ID == "CT04", Estimated_NameplateCapacity_MW := 50] # source: eia 860 2023 & egrid 2022
facilities_data_NE[Facility_ID == 55517 & Unit_ID == "CT05", Estimated_NameplateCapacity_MW := 50] # source: eia 860 2023 & egrid 2022

facilities_data_NE[Facility_ID == 	
                    589 & Unit_ID == "1", Estimated_NameplateCapacity_MW := 59.5] # source:egrid 2022

facilities_data_NE[Facility_ID == 54805 & Unit_ID == "1", Estimated_NameplateCapacity_MW := 249.3] # source: eia 860 2023 & egrid 2022

facilities_data_NE[Facility_ID == 546 & Unit_ID == "5", Estimated_NameplateCapacity_MW := 80.4] # source: eia 860 2023 & egrid 2022


facilities_data_NE[Facility_ID == 562 & Unit_ID == "12", Estimated_NameplateCapacity_MW := 60.5] # source: egrid 2022
facilities_data_NE[Facility_ID == 562 & Unit_ID == "13", Estimated_NameplateCapacity_MW := 60.5] # source: egrid 2022
facilities_data_NE[Facility_ID == 562 & Unit_ID == "14", Estimated_NameplateCapacity_MW := 60.5] # source: egrid 2022
facilities_data_NE[Facility_ID == 562 & Unit_ID == "15", Estimated_NameplateCapacity_MW := 60.5] # source: egrid 2022

facilities_data_NE$CF <- ifelse(
  !is.na(facilities_data_NE$mean_gen_MW / facilities_data_NE$Estimated_NameplateCapacity_MW),
  facilities_data_NE$mean_gen_MW / facilities_data_NE$Estimated_NameplateCapacity_MW,
  facilities_data_NE$CF
)


# Adding retirement year from eGrid Data
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. eGrid 2022/eGRID2022_Data.xlsx" 
eGrid_data2022 <- read_excel(file_path, sheet = "GEN22")
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. eGrid 2022/eGRID2021_Data.xlsx"  
eGrid_data2021 <- read_excel(file_path, sheet = "GEN21")
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. eGrid 2022/eGRID2020_Data.xlsx"  
eGrid_data2020 <- read_excel(file_path, sheet = "GEN20")
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. eGrid 2022/eGRID2019_Data.xlsx"  
eGrid_data2019 <- read_excel(file_path, sheet = "GEN19")
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/U.S. eGrid 2022/eGRID2018_Data.xlsx"  
eGrid_data2018 <- read_excel(file_path, sheet = "GEN18")
eGrid_data <- rbind(eGrid_data2022, eGrid_data2021, eGrid_data2020, eGrid_data2019, eGrid_data2018)
eGrid_data<- eGrid_data[eGrid_data$State %in% new_england_states, ]

unique_fuels <- unique(eGrid_data$`Fuel`)

eGrid_data <- eGrid_data %>%
  filter(`Fuel` != "WND" & `Fuel` != "SUN" & `Fuel` != "WAT" & `Fuel` != "OBG")

setDT(eGrid_data)
eGrid_data[, Facility_Unit.ID := paste(Facility.ID, Unit.ID, sep = "_")]
eGrid_mean_data <- eGrid_data %>%
  filter(Year %in% c(2018, 2019, 2020, 2021, 2022)) %>%
  group_by(Facility_Unit.ID)
setDT(eGrid_mean_data)
unique_generator_statuses <- unique(eGrid_mean_data$`Generator status`)

eGrid_mean_data <- eGrid_mean_data %>%
  filter(`Generator status` == 'OP' | `Generator status` == 'SB' | `Generator status` == 'OS')

eGrid_mean_data <- eGrid_mean_data %>%
  ungroup() %>%
  mutate(modified_capacity = ifelse(eGrid_mean_data$`Number of associated boilers` > 0, 
                                    `Generator nameplate capacity (MW)` * eGrid_mean_data$`Number of associated boilers`, 
                                    `Generator nameplate capacity (MW)`))

eGrid_mean_data <- eGrid_mean_data[!is.na(eGrid_mean_data$`Generator planned or actual retirement year`), ]
eGrid_mean_data <- unique(eGrid_mean_data, by = "Facility_Unit.ID")

eGrid_mean_data <- eGrid_mean_data[, .(Facility_Unit.ID, eGrid_Retirement = `Generator planned or actual retirement year`)]  
facilities_data_NE <- merge(facilities_data_NE, eGrid_mean_data, by = "Facility_Unit.ID", all.x = TRUE)

# Set Retirement_year to 2030 for facilities with primary fuel type of coal
facilities_data_NE$Planned_retirement[grepl("Coal", facilities_data_NE$Primary_Fuel_Type)] <- 2030

# Filter facilities with Primary_Fuel_Type as "Oil"
oil_facilities <- facilities_data_NE[grepl("Oil", Primary_Fuel_Type, ignore.case = TRUE)]
# Calculate the 75th percentile CF threshold
cf_threshold <- quantile(oil_facilities$CF, probs = 0.75, na.rm = TRUE)
# Identify facilities with CF <= 75th percentile
oil_facilities_low_cf <- oil_facilities[CF <= cf_threshold]
# Assign Retirement_year to 2030 for these facilities
facilities_data_NE[
  Facility_ID %in% oil_facilities_low_cf$Facility_ID,
  Planned_retirement := 2030
]

# Set retirement for Coal at 75 years and oil/NG 55 years
# Define the lifespan for each fuel type
coal_lifespan <- 75
oil_ng_lifespan <- 55
wood_lifespan <- 45

# Calculate the retirement year based on lifespan
facilities_data_NE[, Lifespan_Retirement := ifelse(
  grepl("Coal", Primary_Fuel_Type, ignore.case = TRUE),
  Operation_Year + coal_lifespan,
  ifelse(
    grepl("Wood", Primary_Fuel_Type, ignore.case = TRUE),
    Operation_Year + wood_lifespan,
    ifelse(
      grepl("Oil|Gas", Primary_Fuel_Type, ignore.case = TRUE),
      Operation_Year + oil_ng_lifespan,
      2100
    )
  )
)]

# Any facility that is already retired
facilities_data_NE$Retirement_year[grepl("Retired", facilities_data_NE$operatingStatus)] <- 2000

# Update Retirement_year with available values from eia860_Retirement and eGrid_Retirement
facilities_data_NE[, Retirement_year := ifelse(
  !is.na(eia860_Retirement) | !is.na(eGrid_Retirement) | !is.na(Lifespan_Retirement) | !is.na(Planned_retirement),
  pmin(fcoalesce(eia860_Retirement, Inf), fcoalesce(eGrid_Retirement, Inf), fcoalesce(Lifespan_Retirement, Inf), fcoalesce(Planned_retirement, Inf), na.rm = TRUE),
  Retirement_year
)]

# Set retirement_year for the rest to 2100
facilities_data_NE[, Retirement_year := fifelse(is.na(Retirement_year), 2100, Retirement_year)]

# Save the data table as a CSV file
path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"
fwrite(facilities_data_NE, file = path)


