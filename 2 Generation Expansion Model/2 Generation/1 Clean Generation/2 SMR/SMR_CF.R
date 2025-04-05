
# We use the following data provided by StarCube
# Create the data frame
data <- data.frame(
  Label = "SMR300",
  Category = "SMR",
  Nameplate_Capacity_MW = 300,
  CF = 0.9,
  Ramp = 1,
  Minimum_Power_Output_MW = 60,
  Enrichment = "4.95%",
  Fuel = "UO2",
  Life_of_the_Core_Fuel = "4 years",
  Burnup_GWd_t = 50,
  Reactor_Lifetime = 60
)

file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/2 SMR/1 SMR Facility Data/SMR_Facility_Data.csv"
write.csv(data, file_path, row.names = FALSE)


