if (!isTRUE(getOption("phased.r1.cost_runner", FALSE))) stop("Source 1 Run full cost pipeline_R1.R, not individual cost components")
# Load libraries
library(data.table)
library(readxl)
library(fredr)

# NPV Calculator
calculate_npv <- function(dt, rate, base_year, col) {
  npv <- sum(dt[[col]] / (1 + rate)^(dt[['Year']] - base_year))
  return(npv)
}

discount_rate <- 0.025
base_year <- 2024

cpi_data <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"), observation_end = as.Date("2024-01-01"))

# Extracting CPI values for specific years
cpi_2021 <- filter(cpi_data, year(date) == 2021) %>% summarise(YearlyAvg = mean(value))
cpi_2022 <- filter(cpi_data, year(date) == 2022) %>% summarise(YearlyAvg = mean(value))
cpi_2024 <- filter(cpi_data, year(date) == 2024) %>% summarise(YearlyAvg = mean(value))


# Calculating conversion rate
conversion_rate_2021 <- cpi_2024$YearlyAvg / cpi_2021$YearlyAvg
conversion_rate_2022 <- cpi_2024$YearlyAvg / cpi_2022$YearlyAvg

# Load Capacity data
file_path <- "__PROJECT_ROOT__/1 Decarbonization Pathways/Decarbonization_Pathways.xlsx"
sheet_names <- excel_sheets(file_path)
data_tables <- list()
# Loop through each sheet, read it into a data table, and add the Pathway column
for (sheet in sheet_names) {
  sheet_data <- as.data.table(read_excel(file_path, sheet = sheet))
  sheet_data[, Pathway := sheet]
  data_tables[[sheet]] <- sheet_data
}
decarbonization_pathways <- rbindlist(data_tables, fill = TRUE)
setorder(decarbonization_pathways, Pathway, Year)

# Calculate new capacity built each year
import_columns <- c("Imports QC", "Imports NYISO", "Imports NBSO")
for (col in import_columns) {
  new_col_name <- paste0("new_capacity_", gsub(" ", "_", col))  # Create new column names
  decarbonization_pathways[, (new_col_name) := get(col) - shift(get(col), 1, type = "lag"), by = Pathway]
  decarbonization_pathways[is.na(get(new_col_name)), (new_col_name) := 0]
}

decarbonization_pathways[, new_capacity := round(new_capacity_Imports_QC + new_capacity_Imports_NYISO + new_capacity_Imports_NBSO, 2)]
Imports_Capacity <- decarbonization_pathways[, total_capacity := round(`Imports QC` + `Imports NYISO` + `Imports NBSO`, 2)]
Imports_Capacity <- Imports_Capacity[, .(Year, Pathway, Imports_QC = `Imports QC`, Imports_NYISO = `Imports NYISO`, Imports_NB = `Imports NBSO`, total_capacity)]


# Load Imports Costs (i.e cost of market rate) US Market - 2021
# Source: https://www.researchgate.net/publication/356444411_Cost_of_Long-Distance_Energy_Transmission_by_Different_Carriers
lower_Imports <- 20.8 * conversion_rate_2021 # $/MWh/1000 miles
upper_Imports <- 62.5 * conversion_rate_2021 # $/MWh/1000 miles

# Costs for CHPE - 2022
# 0.5 to 2 cents per KWh or 5-20 dollars per MWh
CHPE_cost <- 2 * conversion_rate_2022
CHPE_cost <- CHPE_cost*100/(1000/339)
# CHPE Length 339 miles
# Source Ryans NY Paper
lower_Imports_QC <- CHPE_cost - (upper_Imports-lower_Imports)  # $/MWh/1000 miles
upper_Imports_QC <- CHPE_cost # $/MWh/1000 miles

# Load Generation data
#-- Stepwise
file_path <- "__PROJECT_ROOT__/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "__PROJECT_ROOT__/3 Total Costs/9 Total Costs Results"

Yearly_Results <- as.data.table(fread(file_path))

# Assume every 1200 MW = 340 Miles
# Calculate total miles for each jurisdiction
conversion_factor <- 340 / 1200 # Miles per MW or thousand miles per GW

# Calculate total miles and costs for each jurisdiction
Imports_Capacity[, QC_Miles := Imports_QC * conversion_factor ]
Imports_Capacity[, NB_Miles := Imports_NB * conversion_factor ]
Imports_Capacity[, NYISO_Miles := Imports_NYISO * conversion_factor ]

Imports_Capacity[, QC_USD_TWh_lower := lower_Imports_QC * QC_Miles * 1e6/1e3] # USD/ TWh/1000miles
Imports_Capacity[, NB_USD_TWh_lower := lower_Imports_QC * NB_Miles  * 1e3]
Imports_Capacity[, NYISO_USD_TWh_lower := lower_Imports * NYISO_Miles  * 1e3]

Imports_Capacity[, QC_USD_TWh_upper := upper_Imports_QC * QC_Miles * 1e3]
Imports_Capacity[, NB_USD_TWh_upper := upper_Imports_QC * NB_Miles * 1e3]
Imports_Capacity[, NYISO_USD_TWh_upper := upper_Imports * NYISO_Miles * 1e3]

# join capacity-based rates once because repeated full-table filtering scales poorly at 1000 simulations.
keys <- c("Year","Pathway")
stopifnot(!anyDuplicated(Imports_Capacity,by=keys),!anyDuplicated(Yearly_Results,by=c("Simulation",keys)))
joined <- merge(Yearly_Results,Imports_Capacity,by=keys,all.x=TRUE)
joined[,QC_TWh:=Calibrated_Spot_Market_Imports_HQ_TWh+Calibrated_Long_Term_Imports_HQ_TWh]
energy_cols <- c(QC="QC_TWh",NYISO="Calibrated_Import_NYISO_TWh",NB="Calibrated_Import_NBSO_TWh")
rows <- list()
for(jurisdiction in names(energy_cols))for(cost_type in c("Lower","Upper")) {
 rate_col<-paste0(jurisdiction,"_USD_TWh_",tolower(cost_type));energy_col<-energy_cols[[jurisdiction]]
 if(any(!is.finite(joined[[rate_col]]))||any(!is.finite(joined[[energy_col]])))stop("Missing import quantities or coefficients")
 part<-joined[,.(NPV=sum(get(energy_col)*get(rate_col)/(1+discount_rate)^(Year-base_year))),by=.(Simulation,Pathway)]
 part[,`:=`(Jurisdiction=jurisdiction,Cost_Type=cost_type)]
 rows[[length(rows)+1L]]<-part
}
# retain zero costs so every intertie has both endpoint cases in the sampled publication tables.
combined_npvs<-rbindlist(rows,use.names=TRUE)
setcolorder(combined_npvs,c("Simulation","Pathway","Jurisdiction","Cost_Type","NPV"))
fwrite(combined_npvs,file.path(output_path,"Imports.csv"))
