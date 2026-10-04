# Purpose: Fit or verify unit operating-rate relationships and calculate emissions from final load/ramp rather than independently sampled masses.
# paired emissions replay using saved final unit generation, not a fresh dispatch simulation.
suppressPackageStartupMessages({library(data.table);library(lubridate);library(zoo)})
setDTthreads(4L)
a<-commandArgs(TRUE);stopifnot(length(a)>=5)
root<-normalizePath(a[1]);newcode<-normalizePath(a[2]);helper<-normalizePath(a[3]);artifact_path<-normalizePath(a[4]);out<-a[5]
if(dir.exists(out))stop('Choose a new replay output directory');dir.create(out,recursive=TRUE)
oldcode<-file.path(root,'2 Generation Expansion Model/5 Dispatch Curve/1 Test Results R1/Review baseline 20260909/dispatch_snapshot.R')
model_env<-function(path){e<-new.env(parent=globalenv());for(x in parse(path,keep.source=FALSE))if(is.call(x)&&identical(x[[1]],as.name('<-'))&&is.symbol(x[[2]])&&as.character(x[[2]])%in%c('cfg','dispatch_curve','dispatch_curve_adjustments','dispatch_curve_calibrations'))eval(x,e);e}
e1<-model_env(oldcode);e2<-model_env(newcode)
fleet_path<-file.path(root,'2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv')
fleet<-fread(fleet_path)[Retirement_year>=2025]
# Exercise the loader and emission calculation branches.
e2$Fossil_Fuels_NPC<-fleet
e2$p<-function(...){rel<-file.path(...);n<-basename(rel);if(n==basename(helper))return(helper);if(n==basename(artifact_path))return(artifact_path);file.path(root,rel)}
text<-readLines(newcode);lo<-grep('^## ----- 1\\) Load data',text);hi<-grep('^## ----- 2\\) Functions',text)
loads<-parse(text=text[(lo+1):(hi-1)])
branch<-Filter(function(x)is.call(x)&&identical(x[[1]],as.name('if'))&&grepl('Operating_emission_models <-',paste(deparse(x),collapse=' ')),as.list(loads))
stopifnot(length(branch)==1L);eval(branch[[1]],e2)
stopifnot(identical(e2$Operating_emission_models$model_code_md5,unname(tools::md5sum(helper))))
tab<-fread(file.path(dirname(fleet_path),'Operating_emission_rates_R1.csv'));setorder(tab,Unit,Load,Ramp);e1$Operating_rate_lookup<-split(tab,by='Unit',keep.by=TRUE)
extract_emissions<-function(e){
 b<-as.list(body(e$dispatch_curve_adjustments))[-1L]
 id<-which(vapply(b,function(x)is.call(x)&&identical(x[[1]],as.name('if'))&&grepl('phased.emissions_model',paste(deparse(x[[2]]),collapse=' '))&&grepl('rate_columns',paste(deparse(x),collapse=' ')),logical(1)))
 stopifnot(length(id)==1L);f<-function(results_updated)NULL;body(f)<-b[[id]];environment(f)<-e
 list(fun=f,physical_prefix=b[seq_len(id-1L)])
}
q1<-extract_emissions(e1);q2<-extract_emissions(e2)
stopifnot(identical(q1$physical_prefix,q2$physical_prefix),identical(body(e1$dispatch_curve),body(e2$dispatch_curve)),identical(body(e1$dispatch_curve_calibrations),body(e2$dispatch_curve_calibrations)),identical(e1$cfg,e2$cfg))
options(phased.emissions_model='operating')
partroot<-file.path(root,'2 Generation Expansion Model/5 Dispatch Curve/1 Test Results R1')
if(length(a)>=6L){runs<-strsplit(a[6],'|',fixed=TRUE)[[1]]}else{
 candidates<-list.dirs(partroot,recursive=FALSE,full.names=TRUE)
 runs<-vapply(c(2025,2050),function(y){v<-sort(candidates[grepl(paste0('R1_',y,'_test_'),basename(candidates))],decreasing=TRUE);v<-v[dir.exists(file.path(v,'Dispatch R1','partitions R1'))];if(!length(v))stop('No saved test for ',y);file.path(v[1],'Dispatch R1')},character(1))
}
changes<-list();checks<-list();times<-list();manifests<-list();coverage<-list();k<-0L
masscols<-c('CO2_tons','NOx_lbs','SO2_lbs','HI_mmBtu');ratecols<-c('CO2_ton_per_MWh','NOx_lb_per_MWh','SO2_lb_per_MWh','HI_mmBtu_per_MWh')
for(run in runs)for(path in list.files(file.path(run,'partitions R1'),pattern='[.]rds$',full.names=TRUE)){
 k<-k+1L;x<-readRDS(path);h<-as.data.table(x$hourly);f<-as.data.table(x$facility);yr<-unique(year(h$Date));pway<-unique(h$Pathway)
 stopifnot(length(yr)==1L,length(pway)==1L,all(f$Facility_Unit.ID%in%fleet$Facility_Unit.ID),!anyDuplicated(f,by=c('Facility_Unit.ID','Date','Hour')))
 coverage[[k]]<-data.table(Year=yr,Pathway=pway,Unit=as.character(fleet$Facility_Unit.ID),Expected_by_retirement=pway%in%c('A','D')|fleet$Retirement_year>=yr,Present_in_saved_partition=as.character(fleet$Facility_Unit.ID)%in%f$Facility_Unit.ID)
 counts<-f[,.(Hours_present=uniqueN(paste(Date,Hour))),by=.(Unit=Facility_Unit.ID)]
 coverage[[k]]<-merge(coverage[[k]],counts,by='Unit',all.x=TRUE);coverage[[k]][is.na(Hours_present),Hours_present:=0L]
 old<-q1$fun(copy(f));new<-q2$fun(copy(f))
 physical<-setdiff(names(f),c(masscols,ratecols));stopifnot(isTRUE(all.equal(old[,..physical],new[,..physical],tolerance=0)))
 old_error<-max(abs(as.matrix(old[,..masscols])-as.matrix(f[,..masscols])));stopifnot(old_error<1e-7)
 # Rebuild the final regional balance using the unmodified calibration function and new emission masses.
 d<-copy(h);d[,c('Old_Fossil_Fuels_adj_MWh',masscols):=NULL]
 hnew<-e2$dispatch_curve_calibrations(d,new)
 samecols<-setdiff(names(h),masscols)
 stopifnot(setequal(names(hnew),names(h)),isTRUE(all.equal(h[,..samecols],hnew[,..samecols],tolerance=0,check.attributes=FALSE)))
 z<-new[,lapply(.SD,sum),by=.(Date,Hour),.SDcols=masscols];z<-merge(hnew,z,by=c('Date','Hour'),suffixes=c('_regional','_unit'))
 agg_error<-max(vapply(masscols,function(n)max(abs(z[[paste0(n,'_regional')]]-z[[paste0(n,'_unit')]])),numeric(1)))
 stopifnot(agg_error<1e-7,all(is.finite(as.matrix(new[,..masscols]))),all(as.matrix(new[,..masscols])>=0),all(as.matrix(new[Gen_MWh_adj==0,..masscols])==0),max(abs(hnew$Balance_residual))<=.005001)
 for(n in masscols){v1<-sum(old[[n]]);v2<-sum(new[[n]]);changes[[paste(k,n)]]<-data.table(Year=yr,Pathway=pway,Component=n,R1=v1,R1_v2=v2,Change_percent=if(v1==0)NA_real_ else 100*(v2/v1-1))}
 # Repeated paired timing excludes copying and file I/O; alternate order to reduce order effects.
 for(rep in 1:5){methods<-if(rep%%2)c('R1','R1.v2') else c('R1.v2','R1');for(method in methods){input<-copy(f);fun<-if(method=='R1')q1$fun else q2$fun;t<-system.time(ans<-fun(input))['elapsed'];times[[paste(k,rep,method)]]<-data.table(Year=yr,Pathway=pway,Repeat=rep,Method=method,Seconds=unname(t))}}
 # Storage comparison is representative B1 only; other partitions are not duplicated.
 if(pway=='B1'){
  saveRDS(list(hourly=hnew,facility=new),file.path(out,paste0('Replay_',yr,'_B1_R1.rds')),compress='gzip',version=2)
 }
 checks[[k]]<-data.table(Year=yr,Pathway=pway,Hours=nrow(h),Facility_rows=nrow(f),Units=uniqueN(f$Facility_Unit.ID),R1_reproduction_error=old_error,Physical_values_identical=TRUE,Unit_regional_error=agg_error,Max_balance_MWh=max(abs(hnew$Balance_residual)),Pass=TRUE)
 manifests[[k]]<-data.table(File=path,MD5=unname(tools::md5sum(path)),Bytes=file.info(path)$size)
 cat('PASS saved-dispatch emissions replay:',yr,pway,nrow(h),'hours\n')
}
fwrite(data.table(File=c(oldcode,newcode,helper,artifact_path,fleet_path),MD5=unname(tools::md5sum(c(oldcode,newcode,helper,artifact_path,fleet_path)))),file.path(out,'Code_and_model_R1.v2.csv'));fwrite(rbindlist(coverage),file.path(out,'Saved_unit_coverage_R1.v2.csv'));fwrite(rbindlist(changes),file.path(out,'Emission_changes_R1.v2.csv'));fwrite(rbindlist(checks),file.path(out,'Replay_checks_R1.v2.csv'));fwrite(rbindlist(times),file.path(out,'Replay_timing_R1.v2.csv'));fwrite(rbindlist(manifests),file.path(out,'Source_partitions_R1.v2.csv'))
writeLines(c('Saved-dispatch emissions replay, not a new generation/dispatch run.','Actual emission branches from the R1 and R1.v2 dispatch files are applied to the same final unit generation.','Generation, fossil allocation, ramp/minimum adjustments and final balance code are unchanged.','Reported emissions are existing-fossil only; new gas, imports and other technologies are unchanged.','Repeated timing covers emission evaluation only, not full simulation runtime.'),file.path(out,'Replay_scope_R1.v2.txt'))
print(rbindlist(checks));print(rbindlist(times)[,.(Median_seconds=median(Seconds)),by=Method])
