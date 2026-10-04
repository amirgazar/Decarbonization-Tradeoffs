# Purpose: Prepare indexed inputs, run bounded partitions and check final accounting; supports reproducibility of the operational method.
# Independent physical and accounting checks for local partitions.
validate_local_R1 <- function(run) {
 suppressPackageStartupMessages(library(data.table))
 parts<-list.files(file.path(run,'partitions R1'),pattern='[.]rds$',full.names=TRUE)
 if(!length(parts))stop('No local partitions in ',run)
 reports<-lapply(parts,function(p){
  x<-readRDS(p);h<-as.data.table(x$hourly);f<-as.data.table(x$facility)
  stopifnot(!anyDuplicated(h,by=c('Date','Hour')), !anyDuplicated(f,by=c('Facility_Unit.ID','Date','Hour')))
  u<-f[,.(Gen=sum(Gen_MWh_adj),CO2=sum(CO2_tons),NOx=sum(NOx_lbs),SO2=sum(SO2_lbs),HI=sum(HI_mmBtu)),by=.(Date,Hour)]
  z<-merge(h,u,by=c('Date','Hour'));stopifnot(nrow(z)==nrow(h))
  errors<-c(Generation=max(abs(z$Gen-z$Old_Fossil_Fuels_adj_MWh)),CO2=max(abs(z$CO2-z$CO2_tons)),NOx=max(abs(z$NOx-z$NOx_lbs)),SO2=max(abs(z$SO2-z$SO2_lbs)),HI=max(abs(z$HI-z$HI_mmBtu)))
  stopifnot(all(errors<1e-6))
  stopifnot(all(is.finite(as.matrix(f[,.(Gen_MWh_adj,CO2_tons,NOx_lbs,SO2_lbs,HI_mmBtu)]))),all(as.matrix(f[,.(Gen_MWh_adj,CO2_tons,NOx_lbs,SO2_lbs,HI_mmBtu)])>=0))
  off<-f[Gen_MWh_adj==0];stopifnot(all(as.matrix(off[,.(CO2_tons,NOx_lbs,SO2_lbs,HI_mmBtu)])==0))
  # Check final, rather than preliminary, storage and import outputs.
  stopifnot(all(h$Calibrated_Storage_status>=-1e-6),all(h$Calibrated_Storage_status<=h$Storage_MW+1e-6))
  stopifnot(all(h$Calibrated_Battery_charge_grid>=0),all(h$Calibrated_Battery_discharge_grid>=0),all(h$Calibrated_Battery_charge_grid<=h$battery_power_limit+1e-6),all(h$Calibrated_Battery_discharge_grid<=h$battery_power_limit+1e-6))
  balance<-with(h,Clean_MWh+Old_Fossil_Fuels_adj_MWh+New_Fossil_Fuel_MWh+Calibrated_Total_import_net_MWh+Calibrated_Battery_discharge_grid-Calibrated_Battery_charge_grid+Calibrated_Shortage_MWh-Calibrated_Curtailments_MWh-Demand)
  stopifnot(max(abs(balance))<=0.005001)
  data.table(Pathway=unique(h$Pathway),Hours=nrow(h),Facility_rows=nrow(f),Units=uniqueN(f$Facility_Unit.ID),Max_unit_regional_error=max(errors),Max_balance_MWh=max(abs(balance)),Checks_passed=TRUE)
 })
 result<-rbindlist(reports);fwrite(result,file.path(run,'Independent_checks_R1.csv'));print(result);invisible(result)
}
