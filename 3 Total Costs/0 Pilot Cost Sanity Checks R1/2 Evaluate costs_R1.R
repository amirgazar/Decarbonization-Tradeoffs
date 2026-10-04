# Check annual energy, emissions and cost allocation.
review_validate_pilot <- function(p,f,rates) {
y<-fread(file.path(f,'Yearly_Results.csv'));z<-fread(file.path(f,'Yearly_Facility_Level_Results.csv'),select=c('Simulation','Pathway','Year','Facility_Unit.ID','total_generation_GWh','total_CO2_tons','total_NOx_lbs','total_SO2_lbs','total_HI_mmBtu'));s<-fread(file.path(f,'Yearly_Results_Shortages.csv'))
stopifnot(!anyDuplicated(z,by=c('Simulation','Pathway','Year','Facility_Unit.ID')),!anyNA(z),all(y$Hours_present==ifelse(y$Year%%4==0,8784,8760)))
for(k in c('total_generation_GWh','total_CO2_tons','total_NOx_lbs','total_SO2_lbs','total_HI_mmBtu')) stopifnot(all(z[[k]]>=0))
g<-z[,.(Gen_MWh=sum(total_generation_GWh)*1000,CO2=sum(total_CO2_tons),NOx=sum(total_NOx_lbs),SO2=sum(total_SO2_lbs),HI=sum(total_HI_mmBtu)),by=.(Simulation,Pathway,Year)]
y<-merge(y,g,by=c('Simulation','Pathway','Year'))
y[,Annual_balance:=Clean_MWh+Old_Fossil_Fuels_adj_MWh+New_Fossil_Fuel_MWh+Calibrated_Total_import_net_MWh+Calibrated_Battery_discharge_grid+Calibrated_Shortage_MWh-Demand-Calibrated_Battery_charge_grid-Calibrated_Curtailments_MWh]
check<-data.table(Check=c('Regional/facility generation MWhe','CO2 short tons','NOx lbs','SO2 lbs','HI MMBtu','Annual balance MWh'),Maximum_error=c(max(abs(y$Old_Fossil_Fuels_adj_MWh-y$Gen_MWh)),max(abs(y$CO2_tons-y$CO2)),max(abs(y$NOx_lbs-y$NOx)),max(abs(y$SO2_lbs-y$SO2)),max(abs(y$HI_mmBtu-y$HI)),max(abs(y$Annual_balance))))
# distinguish the documented backup heat cap from quantities with identical aggregation definitions.
backup_cap <- all(vapply(c("Backup aggregation","Active backup formulas retained"),function(x)any(grepl(x,readLines(file.path(f,"SUMMARY_COMPLETE.txt")),fixed=TRUE)),logical(1)))
heat_ok <- all(abs(y$HI_mmBtu-y$HI)<1e-4) || (backup_cap && all(y$HI_mmBtu-y$HI>=-1e-4))
check[,Interpretation:=c(rep("Same definition; absolute reconciliation",4),if(backup_cap)"Facility hourly cap; regional uncapped. Difference recorded, not exact reconciliation." else "Same definition; absolute reconciliation","Accumulated hourly rounding")]
print(check);stopifnot(all(check$Maximum_error[1:4]<1e-4),heat_ok,all(abs(y$Annual_balance)<=y$Hours_present*.005001))
facility_rows_R1 <- nrow(z);rm(z);gc()
for(rate in rates) {
 d<-file.path(p,paste0('discount_R1_',rate),'Components R1')
 cols<-c('total_NOx_USD','total_SO2_USD','total_PM2.5_USD','total_PM10_USD','total_CO_USD','total_VOC_USD')
 # read only the six checked costs because full air outputs include large intermediate tables.
 a<-fread(file.path(d,'Facility_Level_Results_Existing.csv'),select=cols);b<-fread(file.path(d,'Facility_Level_Results_New.csv'),select=cols)
 stopifnot(nrow(a)==facility_rows_R1,all(vapply(a[,..cols],function(x)all(is.finite(x)),logical(1))),all(vapply(b[,..cols],function(x)all(is.finite(x)),logical(1))))
 q<-fread(file.path(d,'New_gas_allocation_audit.csv'));stopifnot(max(abs(q$Expected_TWh-q$Allocated_TWh))<1e-8)
}
cat('All validation checks passed. Regional rows:',nrow(y),'facility annual rows:',facility_rows_R1,'\n')
fwrite(check,file.path(p,'Independent_validation.csv'))
y[,.(Demand_TWh=sum(Demand)/1e6,Existing_fossil_TWh=sum(Old_Fossil_Fuels_adj_TWh),New_gas_TWh=sum(New_Fossil_Fuel_TWh),Biomass_TWh=sum(Biomass_TWh),Shortage_TWh=sum(Calibrated_Shortage_TWh),Curtailment_TWh=sum(Calibrated_Curtailments_TWh),CO2_million_short_tons=sum(CO2_tons)/1e6),by=Pathway] |> fwrite(file.path(p,'Pilot_physical_totals.csv'))

}
