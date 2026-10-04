if(!isTRUE(getOption('phased.r1.cost_runner',FALSE)))stop('Source an R1 cost runner')
library(data.table)
discount_rate <- 0.025
base_year <- 2024
ATBe <- fread("__PROJECT_ROOT__/4 External Data/NREL ATB/ATBe_2024.csv")
file_path <- "__PROJECT_ROOT__/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Results.csv"
output_path <- "__PROJECT_ROOT__/3 Total Costs/9 Total Costs Results"
y <- fread(file_path)
# join coefficient years once instead of running the same nested calculation twice for every simulation.
techs <- data.table(technology=c('Nuclear','Nuclear','Biopower'),techdetail=c('Nuclear - Large','Nuclear - Small','Dedicated'),Column=c('Nuclear_TWh','SMR_TWh','Biomass_TWh'),Technology=c('Nuclear','SMR','Biomass'))
rates <- merge(ATBe[core_metric_case=='Market' & crpyears==30 & core_metric_parameter=='Variable O&M' & core_metric_variable %in% 2025:2050 & scenario %in% c('Advanced','Moderate','Conservative')],techs,by=c('technology','techdetail'))
rates <- rates[,.(Year=core_metric_variable,Technology,ATB_Scenario=scenario,Rate=as.numeric(value))]
if(!setequal(rates$Technology,techs$Technology)||!setequal(rates$ATB_Scenario,c('Advanced','Moderate','Conservative'))||anyDuplicated(rates,by=c('Year','Technology','ATB_Scenario'))||any(!is.finite(rates$Rate)))stop('Incomplete or duplicate non-fossil VOM coefficients')
# record uncovered coefficient years because the original inner join omits them; no extrapolated rates are invented here.
expected <- CJ(Year=2025:2050,Technology=techs$Technology,ATB_Scenario=c('Advanced','Moderate','Conservative'))
missing <- fsetdiff(expected,rates[,.(Year,Technology,ATB_Scenario)])
fwrite(missing,file.path(output_path,'VOM_coefficient_years_missing_R1.csv'))
if(nrow(missing))warning('Non-fossil VOM retains original coefficient-year coverage; see VOM_coefficient_years_missing_R1.csv')
gen <- melt(y,id.vars=c('Simulation','Pathway','Year'),measure.vars=techs$Column,variable.name='Column',value.name='Generation_TWh')
gen <- merge(gen,techs[,.(Column,Technology)],by='Column')
cost <- merge(gen,rates,by=c('Year','Technology'),allow.cartesian=TRUE)
# retain zero-generation cases so every expected technology/pathway/simulation key remains explicit.
combined_npvs <- cost[,.(NPV=sum(Generation_TWh*1e6*Rate/(1+discount_rate)^(Year-base_year))),by=.(Simulation,Pathway,ATB_Scenario,Technology)]
if(any(!is.finite(combined_npvs$NPV)))stop('Nonfinite non-fossil VOM costs')
fwrite(combined_npvs,file.path(output_path,'VOM_Non_Fossil.csv'))
