# Purpose: Export realized import draws and figure metadata so tables and figures match the sampled totals and AP4 valuation.
# split import links in publication tables because B3's adjusted-import column hides their origins.
r1_cost_components <- function(totals, folder) {
 imports <- fread(file.path(folder,"Components R1/Imports.csv"))
 imports[,Simulation:=as.character(Simulation)]
 ids <- as.character(sort(unique(totals$Simulation)))
 paths <- c("A","B1","B2","B3","C1","C2","C3","D")
 keys <- c("Simulation","Pathway","Jurisdiction","Cost_Type")
 grid <- CJ(Simulation=ids,Pathway=paths,Jurisdiction=c("NYISO","QC","NB"),Cost_Type=c("Lower","Upper"))
 if(anyDuplicated(imports,by=keys)||anyNA(imports)||any(!is.finite(imports$NPV))||
    nrow(imports)!=nrow(grid)||nrow(fsetdiff(imports[,..keys],grid)))stop("Incomplete import-link coefficient cases")
 link <- imports[,.(Cost=mean(NPV)/1e9),by=.(Simulation,Pathway,Jurisdiction)]
 # export realized import draws because case averages no longer reproduce sampled totals.
 settings_file <- file.path(folder,"Totals R1/Cost_sampling_settings_R1.csv")
 if(file.exists(settings_file)) {
  link <- fread(file.path(folder,"Totals R1/Import_link_draws_R1.csv"))
  link[,Simulation:=as.character(Simulation)]
  expected <- unique(grid[,.(Simulation,Pathway,Jurisdiction)])
  if(anyNA(link)||any(!is.finite(link$Cost))||anyDuplicated(link,by=c("Simulation","Pathway","Jurisdiction"))||
     nrow(link)!=nrow(expected)||nrow(fsetdiff(link[,.(Simulation,Pathway,Jurisdiction)],expected)))stop("Incomplete sampled import links")
 }

 link <- dcast(link,Simulation+Pathway~Jurisdiction,value.var="Cost")
 setnames(link,c("Pathway","NYISO","QC","NB"),c("Dispatch_Pathway","Imports_NYISO","Imports_QC","Imports_NBSO"))
 other <- c("CAPEX","FOM","VOM","Fuel","GHG","Air_emissions","Unmet_demand","CAN_CAPEX","CAN_FOM","CAN_VOM","CAN_CH4")
 old_cols <- paste0(other,"_mean_bUSD")
 parts <- copy(totals[,c("Simulation","Pathway",old_cols),with=FALSE])
 setnames(parts,old_cols,other)
 parts[,`:=`(Simulation=as.character(Simulation),Dispatch_Pathway=sub("\\(.*","",Pathway))]
 parts <- merge(parts,link,by=c("Simulation","Dispatch_Pathway"),all.x=TRUE)
 if(anyNA(parts))stop("Missing import link in publication components")
 # zero only Quebec purchases in B3(2), because that boundary substitutes Canadian hydro costs.
 parts[Pathway=="B3(2)",Imports_QC:=0]
 check <- copy(totals[,.(Simulation=as.character(Simulation),Pathway,Expected=Total_Costs_mean_bUSD,
  Expected_imports=Imports_mean_bUSD+CAN_Imports_adj_mean_bUSD)])
 check <- merge(check,parts,by=c("Simulation","Pathway"),all=TRUE)
 components <- c(other,"Imports_NYISO","Imports_QC","Imports_NBSO")
 if(anyNA(check)||max(abs(rowSums(check[,..components])-check$Expected))>1e-8||
    max(abs(check$Imports_NYISO+check$Imports_QC+check$Imports_NBSO-check$Expected_imports))>1e-8)
  stop("Publication components do not reconcile to original totals")
 parts[,Dispatch_Pathway:=NULL]
 parts[]
}

# export tables from the same cost run so figures, intervals and manuscript values use identical draws.
r1_export_tables <- function(output) {
 dest <- file.path(output,"Publication tables R1");dir.create(dest,showWarnings=FALSE)
 # physical totals retain simulation identity before across-run summaries.
 settings <- fread(file.path(output,"Run_settings_R1.csv"))
 annual <- fread(file.path(settings$Final[1],"Yearly_Results.csv"))
 physical <- annual[,.(Demand_TWh=sum(Demand)/1e6,Existing_fossil_TWh=sum(Old_Fossil_Fuels_adj_TWh),
 New_gas_TWh=sum(New_Fossil_Fuel_TWh),Biomass_TWh=sum(Biomass_TWh),
 Shortage_TWh=sum(Calibrated_Shortage_TWh),Surplus_TWh=sum(Calibrated_Curtailments_TWh),
 CO2_million_short_tons=sum(CO2_tons)/1e6),by=.(Simulation,Pathway)]
# use a distinct filename because the annual review exports a broader physical table with different columns.
 fwrite(physical,file.path(dest,"Cost_physical_totals_per_simulation_R1.csv"))

 for(rate in c(.015,.02,.025)) {
  folder <- file.path(output,paste0("discount_R1_",rate))
  if(!dir.exists(folder))next
  d <- fread(file.path(folder,"Totals R1/All_Costs_per_Simulation.csv"))
  # reject missing draw records in new runs because reverting to case means would understate uncertainty.
  if("Cost_sampling" %in% names(settings) && settings$Cost_sampling[1]=="bounded_coefficients_v1" &&
     !file.exists(file.path(folder,"Totals R1/Cost_sampling_settings_R1.csv")))stop("Missing cost sampling records")
  display <- r1_cost_components(d,folder)
  components <- setdiff(grep("_mean_bUSD$",names(d),value=TRUE),c("Total_Costs_mean_bUSD","B1_Diff_mean_bUSD"))
  long <- melt(d,id.vars=c("Simulation","Pathway"),measure.vars=c(components,"Total_Costs_mean_bUSD"),variable.name="Component",value.name="Cost_bUSD")
  summary <- long[,.(N=.N,Mean=mean(Cost_bUSD),SD=sd(Cost_bUSD),P05=quantile(Cost_bUSD,.05),P95=quantile(Cost_bUSD,.95)),by=.(Pathway,Component)]
  num <- switch(as.character(rate),"0.015"=10,"0.02"=11,"0.025"=12)
  fwrite(summary,file.path(dest,paste0("Table_S",num,"_R1.csv")))
  county <- fread(file.path(folder,"Components R1/County_costs_per_simulation_R1.csv"))
  # county outputs must reconcile to the exact same total-cost ensemble.
  total_air <- d[,.(Simulation,Dispatch_Pathway=sub("\\(.*","",Pathway),Air=Air_emissions_mean_bUSD)]
  total_air <- unique(total_air)
  sum_air <- county[,.(County_air=sum(npv_total_air_emission_USD)/1e9),by=.(Simulation,Dispatch_Pathway=Pathway)]
  check_air <- merge(total_air,sum_air,by=c("Simulation","Dispatch_Pathway"),all=TRUE)
  if(anyNA(check_air)||max(abs(check_air$Air-check_air$County_air))>1e-8)stop("County and total air costs differ")
  if(any(county[,.(N=uniqueN(Simulation)),by=.(County,State,Pathway)]$N!=uniqueN(d$Simulation)))stop("Incomplete county simulation group")
  cs <- county[,.(N=.N,Mean_mUSD=mean(npv_total_air_emission_USD)/1e6,P05_mUSD=quantile(npv_total_air_emission_USD,.05)/1e6,P95_mUSD=quantile(npv_total_air_emission_USD,.95)/1e6),by=.(County,State,Pathway)]
  fwrite(cs,file.path(dest,paste0("Table_S",num+4,"_R1.csv")))
  if(rate==.02) {
   s8 <- fread(file.path(folder,"Components R1/AP4_source_mapping_R1.csv"))
   # name the PM10 and CO sources explicitly because CO is not an AP3 coefficient.
   supplementary <- fread(file.path(folder,"Components R1/AP3_reference_coefficients_R1.csv"))[,.(Facility_Unit.ID,AP3_PM10=PM10,Published_CO=CO)]
   s8 <- merge(s8,supplementary,by="Facility_Unit.ID",all.x=TRUE)
   fwrite(s8,file.path(dest,"Table_S8_existing_AP4_coefficients_R1.csv"))
   # Hypothetical sources carry actual donor IDs, never label them exact new-plant AP4 estimates.
   new <- fread(file.path(folder,"Components R1/Facility_Level_Results_New.csv"))
   donor <- unique(new[,.(Facility_ID,Donor_unit=Facility_Unit.ID,State,County,NOx,SO2,PM2.5,VOC,PM10,CO)])
   donor[,Mapping:="Existing-location donor proxy for hypothetical new plant"]
   fwrite(donor,file.path(dest,"Table_S8_new_source_proxies_R1.csv"))
   display_long <- melt(display,id.vars=c("Simulation","Pathway"),variable.name="Component",value.name="Cost_bUSD")
   s18 <- display_long[,.(N=.N,Mean=mean(Cost_bUSD),SD=sd(Cost_bUSD),P05=quantile(Cost_bUSD,.05),P95=quantile(Cost_bUSD,.95)),by=.(Pathway,Component)]
   fwrite(s18,file.path(dest,"Table_S18_R1.csv"))
   pairs <- fread(file.path(output,"Paired_B1_summary_R1.csv"))[Rate==.02]
   fwrite(pairs,file.path(dest,"Table_S19_R1.csv"))
   # exclude the shortage penalty from financial costs because Eq. 48 treats it as a separate social cost.
   direct <- c("CAPEX_mean_bUSD","FOM_mean_bUSD","VOM_mean_bUSD","Fuel_mean_bUSD","Imports_mean_bUSD")
   t13 <- summary[Pathway=="B1"&Component %in% direct]
   t13[,Roadmap_mapping:="Pending category, price-year and cash-flow harmonization"]
   fwrite(t13,file.path(dest,"Table_S13_PHased_categories_PENDING_crosswalk_R1.csv"))
   share_data <- merge(display,d[,.(Simulation=as.character(Simulation),Pathway,Total=Total_Costs_mean_bUSD)],by=c("Simulation","Pathway"))
   display_cols <- setdiff(names(display),c("Simulation","Pathway"))
   # use the displayed components and percentages so the table reproduces Figure S6 exactly.
   shares <- share_data[,lapply(.SD,function(v)100*cov(v,Total)/var(Total)),by=Pathway,.SDcols=display_cols]
   if(uniqueN(d$Simulation)>1 && any(abs(rowSums(shares[,-1])-100)>1e-8))stop("R1 cost covariance identity failed")
   fwrite(shares,file.path(dest,"Figure_S6_covariance_shares_R1.csv"))
   fig <- file.path(output,"Figure inputs R1");dir.create(fig,showWarnings=FALSE)
   # record valuation and sampling scope with the figure inputs so captions cannot silently use older assumptions.
   meta <- list(n_simulations=uniqueN(d$Simulation),air_model=settings$Air_model[1],
    cost_sampling=if(file.exists(file.path(folder,"Totals R1/Cost_sampling_settings_R1.csv")))"bounded_coefficients_v1" else "generation_CAPEX_FOM_only",
    data_status=if(uniqueN(d$Simulation)==1000L)"Final cost run; scientific review remains separate" else "Interim cost run")
   jsonlite::write_json(meta,file.path(fig,"Figure_input_metadata_R1.json"),auto_unbox=TRUE,pretty=TRUE)
   fwrite(d,file.path(fig,"All_Costs_per_Simulation.csv"))
   fwrite(display,file.path(fig,"Cost_components_R1.csv"))
   fwrite(county,file.path(fig,"County_costs_per_simulation.csv"))
  }
 }
 writeLines(c("R1 generated tables use the same cost run and simulation IDs.",
 "S8: native AP4 EGU rates; unmatched counties and new-plant donors explicitly labeled.",
 "PM10 uses AP3 because it is unavailable in the AP4 tables used here. CO uses the separate published cost range. NH3 mass is not modeled.",
 "S13 is PHASED-side evidence only; Roadmap comparability remains open.",
 "Intervals are empirical P05/P95, not confidence intervals.",
 "Source-county air damages do not identify affected residents."),file.path(dest,"README_R1.txt"))
}
