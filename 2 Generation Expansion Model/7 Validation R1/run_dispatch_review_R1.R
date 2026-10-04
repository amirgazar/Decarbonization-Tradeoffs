# Purpose: Prepare indexed inputs, run bounded partitions and check final accounting; supports reproducibility of the operational method.
# use and archive the new operating-rate implementation.
# serial, bounded-memory runner and result-preserving speed benchmark.
# Usage: Rscript run_dispatch_review_R1.R ROOT DATA_ROOT INPUT_EXTRACT OUTPUT HOURS SIM_IDS [benchmark]
# HOURS=0 means full supplied horizon; INPUT_EXTRACT must contain selected original random columns.
suppressPackageStartupMessages({library(data.table);library(lubridate);library(zoo)})
a<-commandArgs(TRUE);stopifnot(length(a)>=6)
root<-normalizePath(a[1]);data_root<-normalizePath(a[2]);extract<-normalizePath(a[3]);out<-a[4]
hours<-as.integer(a[5]);sims<-as.integer(strsplit(a[6],',',fixed=TRUE)[[1]])
benchmark<-length(a)>=7&&a[7]=='benchmark'
start_year<-if(length(a)>=8)as.integer(a[8]) else 2025L
if(dir.exists(out))stop('Choose a new output directory; no existing results are overwritten')
dir.create(out,recursive=TRUE);dir.create(file.path(out,'partitions R1'))
# use allocated data.table CPUs; keep one R process per batch.
threads <- as.integer(Sys.getenv('SLURM_CPUS_PER_TASK',Sys.getenv('PHASED_THREADS','4')))
if(is.na(threads)||threads<1L)stop('Invalid allocated thread count')
setDTthreads(threads)
# Mirror R output to Slurm stdout and the active job/simulation log, including conditions.
log_connection <- NULL
open_log <- function(path){
 if(!is.null(log_connection)){sink();close(log_connection)}
 log_connection <<- file(path,'at');sink(log_connection,split=TRUE)
}
log_event <- function(...){cat(format(Sys.time(),'%Y-%m-%d %H:%M:%S %Z'),..., '\n');flush(log_connection);flush.console()}
open_log(file.path(out,'execution.log'))
globalCallingHandlers(warning=function(c){log_event('WARNING:',conditionMessage(c));invokeRestart('muffleWarning')},message=function(c){log_event('MESSAGE:',conditionMessage(c));invokeRestart('muffleMessage')},error=function(c){log_event('FAILED:',conditionMessage(c));cat('Calls:',paste(vapply(sys.calls(),function(x)paste(deparse(x),collapse=' '),character(1)),collapse=' -> '),'\n');flush(log_connection)})
log_event('START batch; simulations',paste(sims,collapse=','),'data.table threads',getDTthreads(),'R',as.character(getRversion()))
log_event('Loading shared inputs once for this R process')
code<-file.path(root,'2 Generation Expansion Model/5 Dispatch Curve/dispatch_curve_base_v2.R')
if(!file.exists(code))code<-file.path(root,'dispatch_curve_base_v2.R')
s<-readLines(code);e<-new.env(parent=globalenv());paths_read<-character()
e$p<-function(...){rel<-file.path(...);path<-file.path(data_root,rel)
 # explicit support for the established flat ARC dataset folder.
 if(Sys.getenv("PHASED_FLAT_DATA")=="1")path<-file.path(data_root,basename(rel))
 if((basename(path)=='Random_Sequence.csv' || (hours>0 && basename(path)=='Fossil_Fuel_Generation_Emissions.csv')) && file.exists(file.path(extract,basename(path))))path<-file.path(extract,basename(path))
 # Local missing inputs resolve to the supplied backup; ARC stays in its verified flat dataset.
 if(Sys.getenv("PHASED_FLAT_DATA")!="1") {
  relpath <- file.path(root,rel)
  fallback <- file.path(Sys.getenv("PHASED_DATA_ROOT",Sys.getenv("PHASED_R1_ROOT", unset=getwd())),rel)
  if(!file.exists(path) && file.exists(relpath)) path <- relpath
  if(!file.exists(path) && file.exists(fallback)) path <- fallback
  reference <- file.path(Sys.getenv("PHASED_REFERENCE_ROOT",Sys.getenv("PHASED_DATA_ROOT", unset=Sys.getenv("PHASED_R1_ROOT", unset=getwd()))),rel)
  if(!file.exists(path) && file.exists(reference))path <- reference
 }
 paths_read<<-unique(c(paths_read,path));path}
e$fread<-function(path,...){x<-data.table::fread(file=path,...)
 if(hours>0){
  name<-basename(path);days<-ceiling(hours/24)
  if(name=='Hourly_Installed_Capacity.csv')x<-x[Year==start_year & DayLabel<=days]
  if(name=='demand_data.csv'){x[,Date:=as.Date(Date)];setorder(x,Date,Hour);x<-head(x[year(Date)==start_year],hours)}
  if(name=='Fossil_Fuel_hr_maxmin.csv'){x[,Date:=as.Date(Date)];x<-x[year(Date)==start_year & yday(Date)<=days]}
  if(name %in%c('offwind_CF.csv','onwind_CF.csv','solar_CF.csv','Imports_CF.csv'))x<-x[DayLabel<=days]
 }
 x}
for(x in parse(code))if(is.call(x)&&identical(x[[1]],as.name('<-'))&&is.symbol(x[[2]])&&as.character(x[[2]])%in%c('cfg','dispatch_curve','dispatch_curve_adjustments','dispatch_curve_calibrations'))eval(x,e)
tload<-system.time(eval(parse(text=s[(grep('^## ----- 1\\) Load data',s)+1):(grep('^## ----- 2\\) Functions',s)-1)]),e))['elapsed']
log_event('Shared input load complete; seconds',tload)
r<-e$Random_sequence;e$Random_sequence<-vector('list',max(sims))
for(i in sims){name<-paste0('V',i);if(!name%in%names(r))stop('Missing original random column ',name);e$Random_sequence[[i]]<-r[[name]]}
# ARC pilot may select B1 without silently running every pathway.
pathways<-if(length(a)>=9L)strsplit(a[9],",",fixed=TRUE)[[1]] else sort(unique(e$Hourly_Installed_Capacity$Pathway))
if(!length(pathways)||anyDuplicated(pathways)||any(!pathways%in%e$Hourly_Installed_Capacity$Pathway))stop('Invalid requested pathways')
log_event('Explicit scope:',paste(pathways,collapse=','),'requested hours',hours)
regional<-list();facilities<-list();metrics<-list()
# timestamp each stage without changing dispatch equations.
run<-function(fast,sim,path){options(phased.fast_lookup=fast);log_event('START generation/storage',path);d<-e$dispatch_curve(sim,path);log_event('START fossil adjustment/emissions',path);f<-e$dispatch_curve_adjustments(d);log_event('START final balance/storage',path);h<-e$dispatch_curve_calibrations(d,f);log_event('END calculations',path);h[,`:=`(Simulation=sim,Pathway=path)];f[,`:=`(Simulation=sim,Pathway=path)];list(hourly=h,facility=f)}
logged_sim <- NA_integer_
for(sim in sims)for(path in pathways){
 # one durable log directory per simulation; inputs remain loaded across this loop.
 if(!identical(logged_sim,sim)){
  simdir<-file.path(out,paste0('sim_R1_',sim));dir.create(simdir)
  open_log(file.path(simdir,'execution.log'));logged_sim<-sim
  log_event('START simulation',sim,'threads',getDTthreads(),'shared inputs already loaded in batch; see ../execution.log')
 }
 key<-sprintf('sim_%04d_%s',sim,path);cat(key,'\n')
 reps<-if(benchmark)3L else 1L
 for(rep in seq_len(reps)){
  slow_time<-NA_real_;equal<-NA
  if(benchmark){gc();slow_time<-system.time(slow<-run(FALSE,sim,path))['elapsed']}
  gc();fast_time<-system.time(fast<-run(TRUE,sim,path))['elapsed']
  if(benchmark){equal<-isTRUE(all.equal(slow,fast,tolerance=0,check.attributes=TRUE));if(!equal)stop('Exact equivalence failed: ',key);rm(slow)}
  h<-fast$hourly;f<-fast$facility
  if(any(!is.finite(h$Balance_residual))||max(abs(h$Balance_residual))>.03)stop('Energy balance failed: ',key)
  metrics[[paste(key,rep)]]<-data.table(Simulation=sim,Pathway=path,Repeat=rep,Slow_seconds=slow_time,Fast_seconds=fast_time,Exact_equal=equal,Hourly_rows=nrow(h),Facility_rows=nrow(f),Peak_balance_error=max(abs(h$Balance_residual)))
  if(rep==1L){
   log_event('Saving partition',key,'calculation seconds',fast_time);dest<-file.path(out,'partitions R1',paste0(key,'.rds'));saveRDS(fast,paste0(dest,'.tmp'),compress='gzip');stopifnot(file.rename(paste0(dest,'.tmp'),dest))
   h[,Year:=year(Date)];f[,Year:=year(Date)]
   flux<-grep('_MWh$|_grid$|^CO2_tons$|^NOx_lbs$|^SO2_lbs$|^HI_mmBtu$',names(h),value=TRUE)
   z<-h[,lapply(.SD,sum),by=.(Simulation,Pathway,Year),.SDcols=flux]
   z<-merge(z,h[,.(Demand_MWh=sum(Demand),Hours_present=.N),by=.(Simulation,Pathway,Year)],by=c('Simulation','Pathway','Year'))
   for(n in grep('_MWh$',names(z),value=TRUE))set(z,j=sub('_MWh$','_TWh',n),value=z[[n]]/1e6)
   caps<-grep('_MW$',names(h),value=TRUE)
   cap<-h[,lapply(.SD,function(v){if(uniqueN(v)!=1L)stop('Within-year capacity change needs an explicit aggregate rule');v[1]}),by=.(Simulation,Pathway,Year),.SDcols=caps]
   z<-merge(z,cap,by=c('Simulation','Pathway','Year'));z[,Demand:=Demand_MWh]
   regional[[key]]<-z
   facilities[[key]]<-f[,.(total_generation_GWh=sum(Gen_MWh_adj)/1000,total_CO2_tons=sum(CO2_tons),total_NOx_lbs=sum(NOx_lbs),total_SO2_lbs=sum(SO2_lbs),total_HI_mmBtu=sum(HI_mmBtu),Hours_present=uniqueN(paste(Date,Hour))),by=.(Facility_Unit.ID,Simulation,Pathway,Year)]
  }
  log_event('PASS',key,'balance error',max(abs(h$Balance_residual)),'hourly rows',nrow(h),'facility rows',nrow(f));rm(fast,h,f);gc()
 }
 if(path==tail(pathways,1))log_event('COMPLETE simulation',sim)
}
log_event('END calculations for batch; writing annual summaries')
open_log(file.path(out,'execution.log'))
fwrite(rbindlist(metrics),file.path(out,'dispatch_benchmark.csv'))
fwrite(rbindlist(regional),file.path(out,'Yearly_Results.csv'))
unit<-rbindlist(facilities)
meta<-e$Fossil_Fuels_NPC[,.(Facility_Unit.ID,State,Fuel_type_1=Primary_Fuel_Type,Fuel_type_2=Secondary_Fuel_Type,latitude=Latitude,longitude=Longitude,Ramp_hr=Ramp,Fossil.NPC_MW=Estimated_NameplateCapacity_MW)]
unit<-merge(unit,meta,by='Facility_Unit.ID',all.x=TRUE)
fwrite(unit,file.path(out,'Yearly_Facility_Level_Results.csv'))
fwrite(rbindlist(regional)[,.(Simulation,Pathway,Year,Unmet_Demand_total_MWh=Calibrated_Shortage_MWh)],file.path(out,'Yearly_Results_Shortages.csv'))
fwrite(data.table(File=paths_read,Bytes=file.info(paths_read)$size,Modified=as.character(file.info(paths_read)$mtime)),file.path(out,'input_manifest.csv'))
writeLines(c(paste('Code MD5',tools::md5sum(code)),paste('Input load seconds',tload),paste('Requested hours',hours),paste('Local start year',start_year),paste('Simulation IDs',paste(sims,collapse=',')),paste('Pathways',paste(pathways,collapse=',')),if(hours>0)'PARTIAL HORIZON TEST: NOT ANNUAL OR FINAL COST RESULTS' else 'FULL PROVIDED HORIZON: verify scope before final reporting'),file.path(out,'RUN_SCOPE.txt'))
file.copy(code,file.path(out,'dispatch_code_used_R1.R'))
# retain the exact helper, model artifact checksum and selected-model metadata with the run.
if(exists('Operating_emission_models',e,inherits=FALSE)){
 file.copy(e$operating_model_helper,file.path(out,'Operating emission models_R1.R'))
 writeLines(c(paste('Artifact version',e$Operating_emission_models$version),paste('Artifact MD5',unname(tools::md5sum(e$operating_model_path))),paste('Helper MD5',e$Operating_emission_models$model_code_md5),paste('Calibration code MD5',e$Operating_emission_models$calibration_code_md5)),file.path(out,'Emissions_provenance_R1.v2.txt'))
}
# an unperformed equivalence test must not print TRUE.
print(rbindlist(metrics)[,.(Median_slow_seconds=median(Slow_seconds,na.rm=TRUE),Median_fast_seconds=median(Fast_seconds),Exact_equal=if(all(is.na(Exact_equal)))NA else all(Exact_equal,na.rm=TRUE))])

log_event('COMPLETE batch and annual outputs')
sink();close(log_connection);log_connection<-NULL
