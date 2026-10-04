if(!isTRUE(getOption('phased.r1.cost_runner',FALSE)))stop('Source an R1 cost runner')
library(data.table)
discount_rate <- 0.025
base_year <- 2024
# retain the effective original $3500/MWh value explicitly; the preceding overwritten $9337 assignment was unused.
EVOLL_2025 <- 3500
file_path <- "__PROJECT_ROOT__/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results_Shortages.csv"
output_path <- "__PROJECT_ROOT__/3 Total Costs/9 Total Costs Results"
y <- fread(file_path)
if(any(!is.finite(y$Unmet_Demand_total_MWh))||any(y$Unmet_Demand_total_MWh<0))stop('Invalid unserved demand')
# group annual costs directly and retain zero penalties because a complete pathway cannot disappear when no shortage occurs.
combined_npvs <- y[,.(NPV=sum(Unmet_Demand_total_MWh*EVOLL_2025/(1+discount_rate)^(Year-base_year))),by=.(Simulation,Pathway)]
fwrite(combined_npvs,file.path(output_path,'Unmet_demand.csv'))
