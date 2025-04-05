library(data.table)
file_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/4 External Data/NREL ATB/ATBe_2024.csv"
ATB_data <- fread(file_path)

# Filter the data for the specified technologies and metric
ATB_data_1 <- ATB_data[
  technology %in% c("Hydropower", "Biopower", "Nuclear", "Commercial Battery Storage") & 
  techdetail %in% c("NPD5", "Dedicated", "Nuclear - Large", "8Hr Battery Storage") & 
  core_metric_variable == "2030" &
  core_metric_parameter == "CF" &
  core_metric_case == "Market" & 
  scenario == "Moderate" &
  crpyears == "20"
]

output_path <- "/Users/amirgazar/Documents/GitHub/Decarbonization-Tradeoffs/2 Generation Expansion Model/2 Generation/1 Clean Generation/3 Large nuclear hydro and bio/1 Clean Baseload Facility Data/CleanBaseload_Facility_Data.csv"
write.csv(ATB_data_1, output_path)
