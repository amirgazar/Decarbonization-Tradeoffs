# Validate every partition and aggregate paired results, including missing-profile, ramp and surplus diagnostics.
#: validate exact scope, stream one partition at a time, preserve legacy HI cap with audit.
suppressPackageStartupMessages(library(data.table))
setDTthreads(as.integer(Sys.getenv("SLURM_CPUS_PER_TASK","4")))
a<-commandArgs(TRUE);stopifnot(length(a)==2L);source(a[1]);mode<-a[2];stopifnot(mode%in%c("pilot_R1","ensemble_R1"))
source_root<-dirname(normalizePath(a[1]));jobs<-fread(file.path(source_root,"Jobs.csv"))[Mode==mode]
expected_ids<-if(mode=="pilot_R1")arc$simulations[1] else arc$simulations
if(!setequal(jobs$Simulation,expected_ids)||anyDuplicated(jobs$Simulation))stop("Job manifest differs from intended simulations")
final<-file.path(arc$results_root,mode,"Final R1");if(dir.exists(final)||dir.exists(paste0(final,".pending")))stop("Summary directory already exists; preserve it and choose a fresh result root")
work<-paste0(final,".pending");dir.create(work,recursive=TRUE)
input<-function(rel){path<-file.path(arc$data_root,if(arc$flat_data)basename(rel) else rel);if(!file.exists(path))stop("Missing input ",path);path}
demand<-fread(input("2 Generation Expansion Model/1 Demand/1 Hourly Demand/demand_data.csv"));demand[,Date:=as.Date(Date)]
demand<-demand[as.integer(format(Date,"%Y"))>=arc$first_year & as.integer(format(Date,"%Y"))<=arc$last_year];setorder(demand,Date,Hour)
if(arc$hours>0)demand<-head(demand[as.integer(format(Date,"%Y"))==arc$start_year],arc$hours)
if(!nrow(demand)||anyNA(demand[,.(Date,Hour)])||anyDuplicated(demand[,.(Date,Hour)]))stop("Invalid expected hourly grid")
if(arc$hours==0){
 grid<-CJ(Date=seq(as.Date(paste0(arc$first_year,"-01-01")),as.Date(paste0(arc$last_year,"-12-31")),by="day"),Hour=1:24)
 if(!identical(paste(demand$Date,demand$Hour),paste(grid$Date,grid$Hour)))stop("Demand does not cover every intended flat-calendar hour; resolve calendar inputs before ensemble")
}
expected_keys<-paste(demand$Date,demand$Hour)
meta<-fread(input("2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"))
if(anyDuplicated(meta$Facility_Unit.ID))stop("Duplicate facility metadata")
curve<-fread(arc$evoll);stopifnot(all(c("Percentage","Cost_per_MWh")%in%names(curve)))
setorder(curve,Percentage);if(anyDuplicated(curve$Percentage)||any(!is.finite(unlist(curve[,.(Percentage,Cost_per_MWh)]))))stop("Invalid EVOLL curve")
append_csv<-function(x,name){dest<-file.path(work,name);fwrite(x,dest,append=file.exists(dest))}
audit<-list();idx<-0L
for(i in seq_len(nrow(jobs))){
 sim<-jobs$Simulation[i];run<-file.path(arc$results_root,mode,jobs$Directory[i])
 if(!file.exists(file.path(run,"DISPATCH_COMPLETE.txt")))stop("Incomplete dispatch process: ",run)
 if(unname(tools::md5sum(file.path(run,"dispatch_code_used_R1.R")))!=unname(tools::md5sum(file.path(source_root,"dispatch_curve_base_v2.R"))))stop("Dispatch code differs from this bundle")
 for(path in arc$pathways){
  filename<-file.path(run,"partitions R1",sprintf("sim_%04d_%s.rds",sim,path));if(!file.exists(filename))stop("Missing partition: ",filename)
  x<-readRDS(filename);h<-as.data.table(x$hourly);f<-as.data.table(x$facility);h[,Date:=as.Date(Date)];f[,Date:=as.Date(Date)];setorder(h,Date,Hour)
  if(!identical(paste(h$Date,h$Hour),expected_keys)||anyDuplicated(h[,.(Date,Hour)]))stop("Hourly coverage differs: ",filename)
  if(any(h$Simulation!=sim|h$Pathway!=path)||any(f$Simulation!=sim|f$Pathway!=path)||anyDuplicated(f[,.(Facility_Unit.ID,Date,Hour)]))stop("Partition keys invalid")
  if(anyNA(f[,.(Date,Hour,Facility_Unit.ID)])||any(!paste(f$Date,f$Hour)%in%expected_keys))stop("Facility keys outside expected horizon")
  physical<-c("Gen_MWh_adj","CO2_tons","NOx_lbs","SO2_lbs","HI_mmBtu")
  if(any(!is.finite(unlist(f[,.SD,.SDcols=physical]))))stop("Nonfinite facility results")
  sums<-f[,lapply(.SD,sum),by=.(Date,Hour),.SDcols=physical];chk<-merge(h,sums,by=c("Date","Hour"),suffixes=c("","_unit"),all.x=TRUE)
  for(pair in list(c("Old_Fossil_Fuels_adj_MWh","Gen_MWh_adj"),c("CO2_tons","CO2_tons_unit"),c("NOx_lbs","NOx_lbs_unit"),c("SO2_lbs","SO2_lbs_unit"),c("HI_mmBtu","HI_mmBtu_unit"))){
   u<-chk[[pair[2]]];u[is.na(u)]<-0;v<-chk[[pair[1]]]
   if(any(!is.finite(v))||any(abs(v-u)>1e-6+pmax(abs(v),abs(u))*1e-10))stop("Unit/regional mismatch: ",pair[1])
  }
  balance<-with(h,Clean_MWh+Calibrated_Total_import_net_MWh+Old_Fossil_Fuels_adj_MWh+New_Fossil_Fuel_MWh+Calibrated_Battery_discharge_grid+Calibrated_Shortage_MWh-Demand-Calibrated_Battery_charge_grid-Calibrated_Curtailments_MWh)
  if(any(!is.finite(balance))||max(abs(balance))>.03)stop("Energy balance failed")
  link<-with(h,Calibrated_Long_Term_Imports_HQ_MWh+Calibrated_Spot_Market_Imports_HQ_MWh+Calibrated_Import_NYISO_MWh+Calibrated_Import_NBSO_MWh)
  if(any(!is.finite(link))||any(abs(link-h$Calibrated_Total_import_net_MWh)>1e-6))stop("Delivered link sum mismatch")
  if(any(h$Calibrated_Import_NYISO_MWh>0.95*h$Imports_NYISO_MW+1e-6)|any(h$Calibrated_Import_NBSO_MWh>0.95*h$Imports_NBSO_MW+1e-6)|any(h$Calibrated_Long_Term_Imports_HQ_MWh+h$Calibrated_Spot_Market_Imports_HQ_MWh>0.95*h$Imports_HQ_MW+1e-6))stop("Intertie limit exceeded")
  if(any(h$Calibrated_Storage_status< -1e-6|h$Calibrated_Storage_status>h$Storage_MW+1e-6)|any(h$Calibrated_Battery_charge_grid>1e-6 & h$Calibrated_Battery_discharge_grid>1e-6))stop("Storage bounds or simultaneous flows failed")
  flags<-c("Historical_profile_available","Ramp_supported_missing_hour","Profile_or_ramp_eligible")
  if(!all(flags%in%names(f))||anyNA(f[,..flags]))stop("Missing profile-eligibility diagnostics")
  if(any(f$Profile_or_ramp_eligible!=(f$Historical_profile_available|f$Ramp_supported_missing_hour)) ||
     any(!f$Profile_or_ramp_eligible & f$Gen_MWh_adj!=0))stop("Unsupported missing-hour generation")
  limits<-merge(f[,.(Facility_Unit.ID,Date,Hour,Gen_MWh_adj,Profile_or_ramp_eligible)],
    meta[,.(Facility_Unit.ID,min_gen_MW,max_gen_MW,Ramp_MWh=Estimated_NameplateCapacity_MW/ceiling(Ramp))],by="Facility_Unit.ID",all.x=TRUE)
  if(any(!is.finite(unlist(limits[,.(min_gen_MW,max_gen_MW,Ramp_MWh)]))) ||
     any(limits$Gen_MWh_adj<0 | limits$Gen_MWh_adj>limits$max_gen_MW+1e-8 |
         (limits$Profile_or_ramp_eligible & limits$Gen_MWh_adj<limits$min_gen_MW-1e-8)))stop("Operating generation bounds failed")
  setorder(limits,Facility_Unit.ID,Date,Hour)
  ramp_check<-limits[,.(bad=any(abs(diff(Gen_MWh_adj))>Ramp_MWh[1]+0.010001)),by=Facility_Unit.ID]
  if(any(ramp_check$bad))stop("Hourly ramp limits failed")
  if(any(h$Calibrated_Battery_discharge_grid>1e-8 & h$Calibrated_Curtailments_MWh>0.01))stop("Battery discharge during surplus")
  eta<-sqrt(0.85)
  if(max(abs(h$Calibrated_Storage_status-shift(h$Calibrated_Storage_status,fill=0)-eta*h$Calibrated_Battery_charge_grid+h$Calibrated_Battery_discharge_grid/eta))>1e-6)stop("Storage recurrence failed for configured 85% efficiency and carry-over")
  rm(limits,ramp_check)
  h[,Year:=as.integer(format(Date,"%Y"))];f[,Year:=as.integer(format(Date,"%Y"))]
  cols<-grep("_MWh$|_grid$|^CO2_tons$|^NOx_lbs$|^SO2_lbs$|^HI_mmBtu$",names(h),value=TRUE)
  if(any(!is.finite(unlist(h[,.SD,.SDcols=cols]))))stop("Nonfinite hourly totals")
  yr<-h[,lapply(.SD,sum),by=.(Simulation,Pathway,Year),.SDcols=cols]
  yr<-merge(yr,h[,.(Demand=sum(Demand),Hours_present=.N),by=.(Simulation,Pathway,Year)],by=c("Simulation","Pathway","Year"))
  for(n in grep("_MWh$",names(yr),value=TRUE))set(yr,j=sub("_MWh$","_TWh",n),value=yr[[n]]/1e6)
  caps<-grep("_MW$",names(h),value=TRUE)
  cap<-h[,lapply(.SD,function(v){if(uniqueN(v)!=1L)stop("Capacity changes within year");v[1]}),by=.(Simulation,Pathway,Year),.SDcols=caps]
  append_csv(merge(yr,cap,by=c("Simulation","Pathway","Year")),"Yearly_Results.csv")
  ratio<-h$Calibrated_Shortage_MWh/h$Demand;positive<-h$Calibrated_Shortage_MWh>0
  if(any(!is.finite(ratio))||any(ratio<0)||any(ratio[positive]<min(curve$Percentage)|ratio[positive]>max(curve$Percentage)))stop("Shortage ratio outside EVOLL curve; no silent zero cost")
  price<-numeric(nrow(h));price[positive]<-approx(curve$Percentage,curve$Cost_per_MWh,ratio[positive])$y
  h[,Unmet_Demand_USD:=Calibrated_Shortage_MWh*price]
  append_csv(h[,.(Unmet_Demand_total_MWh=sum(Calibrated_Shortage_MWh),Unmet_Demand_USD_total=sum(Unmet_Demand_USD)),by=.(Simulation,Pathway,Year)],"Yearly_Results_Shortages.csv")
  f<-merge(f,meta[,.(Facility_Unit.ID,Max_Hourly_HI_Rate)],by="Facility_Unit.ID",all.x=TRUE)
  f[,Raw_HI_mmBtu:=HI_mmBtu];f[,Legacy_HI_cap_reduction:=fifelse(is.finite(Max_Hourly_HI_Rate),pmax(0,HI_mmBtu-Max_Hourly_HI_Rate),0)]
  unit<-f[,.(total_generation_GWh=sum(Gen_MWh_adj)/1000,total_CO2_tons=sum(CO2_tons),total_NOx_lbs=sum(NOx_lbs),total_SO2_lbs=sum(SO2_lbs),total_HI_mmBtu=sum(HI_mmBtu),raw_HI_mmBtu=sum(Raw_HI_mmBtu),Legacy_HI_cap_reduction_mmBtu=sum(Legacy_HI_cap_reduction),Hours_present=.N),by=.(Facility_Unit.ID,Simulation,Pathway,Year)]
  static<-meta[,.(Facility_Unit.ID,State,Fuel_type_1=Primary_Fuel_Type,Fuel_type_2=Secondary_Fuel_Type,latitude=Latitude,longitude=Longitude,Ramp_hr=ceiling(Ramp),Fossil.NPC_MW=Estimated_NameplateCapacity_MW)]
  if(any(!unit$Facility_Unit.ID%in%static$Facility_Unit.ID))stop("Missing facility metadata")
  append_csv(merge(unit,static,by="Facility_Unit.ID",all.x=TRUE),"Yearly_Facility_Level_Results.csv")
  idx<-idx+1L;audit[[idx]]<-data.table(Simulation=sim,Pathway=path,Hours=nrow(h),Units=uniqueN(f$Facility_Unit.ID),Facility_rows=nrow(f),Peak_balance_error=max(abs(balance)),Legacy_HI_cap_reduction_mmBtu=sum(f$Legacy_HI_cap_reduction),Missing_HI_caps=sum(!is.finite(f$Max_Hourly_HI_Rate)),Observed_profile_hours=sum(f$Historical_profile_available),Ramp_supported_missing_hours=sum(f$Ramp_supported_missing_hour),Unsupported_missing_hours=sum(!f$Profile_or_ramp_eligible),Supply_surplus_MWh=sum(h$Calibrated_Curtailments_MWh),Unserved_MWh=sum(h$Calibrated_Shortage_MWh))
  rm(x,h,f,chk,sums,unit);gc()
 }
}
fwrite(rbindlist(audit),file.path(work,"Coverage_and_accounting.csv"))
writeLines(if(arc$hours>0)"PARTIAL HORIZON TEST — NOT FINAL ANNUAL RESULTS" else "Full intended hourly coverage passed. Valuation, storage-unit and scientific checks remain required.",file.path(work,"SUMMARY_COMPLETE.txt"))
stopifnot(file.rename(work,final));cat("Validated summaries:",final,"\n")
