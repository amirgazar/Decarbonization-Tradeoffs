# Purpose: Fit or verify unit operating-rate relationships and calculate emissions from final load/ramp rather than independently sampled masses.
# compare continuous curves with fixed and binned rates.
# offline historical load/ramp calibration, chronological validation and held-out evaluation.
suppressPackageStartupMessages({library(data.table);library(lubridate)})
setDTthreads(1L)
a<-commandArgs(TRUE);stopifnot(length(a)>=2L);root<-normalizePath(a[1]);out<-a[2]
self<-normalizePath(gsub('~+~',' ',sub('^--file=','',grep('^--file=',commandArgs(),value=TRUE)[1]),fixed=TRUE))
source(file.path(dirname(self),'Operating emission models_R1.R'))
if(dir.exists(out))stop('Choose a new calibration output directory');dir.create(out,recursive=TRUE)
fleet<-fread(file.path(root,'2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv'))[Retirement_year>=2025]
stopifnot(!anyDuplicated(fleet$Facility_Unit.ID));cols<-c('CO2_Mass_short_tons','NOx_Mass_lbs','SO2_Mass_lbs','Heat_Input_mmBtu');polls<-c('CO2','NOx','SO2','HI');quality_cols<-c('CO2_Mass_Indicator','NOx_Mass_Indicator','SO2_Mass_Indicator','Heat_Input_Indicator')
all<-list();coverage<-list();manifests<-list()
for(st in c('CT','ME','MA','NH','RI','VT')){
 path<-file.path(root,'4 External Data/U.S. EPA CAMPD/States Historical and Simulated Data',st,paste0('Hourly_Emissions_',st,'_Clean.rds'))
 manifests[[st]]<-data.table(State=st,File=path,Bytes=file.info(path)$size,MD5=unname(tools::md5sum(path)))
 cat('Reading',st,'\n');z<-as.data.table(readRDS(path));z<-z[Facility_Unit.ID%in%fleet$Facility_Unit.ID,c('Facility_Unit.ID','Date','Hour','Operating_Time','Gross_Load_MW',cols,quality_cols),with=FALSE]
 z[,Date:=as.Date(Date)];setorder(z,Facility_Unit.ID,Date,Hour)
 z[,duplicate:=duplicated(z[,.(Facility_Unit.ID,Date,Hour)])|duplicated(z[,.(Facility_Unit.ID,Date,Hour)],fromLast=TRUE)]
 coverage[[st]]<-z[,.(First_date=min(Date),Last_date=max(Date),Years=uniqueN(year(Date)),Rows=.N,Duplicate_rows=sum(duplicate),Full_positive_hours=sum(Operating_Time==1&Gross_Load_MW>0,na.rm=TRUE)),by=Facility_Unit.ID]
 z<-z[duplicate==FALSE];z[,Capacity:=fleet$Estimated_NameplateCapacity_MW[match(Facility_Unit.ID,fleet$Facility_Unit.ID)]]
 z[,tick:=as.numeric(Date)*24+Hour];z[,`:=`(previous_tick=shift(tick),previous_gen=shift(Gross_Load_MW),previous_op=shift(Operating_Time)),by=Facility_Unit.ID]
 z[,Ramp:=4L];z[is.finite(previous_gen)&previous_gen>0&previous_op==1&tick-previous_tick==1,Ramp:=fifelse((Gross_Load_MW-previous_gen)/Capacity>0.1,3L,fifelse((Gross_Load_MW-previous_gen)/Capacity< -0.1,1L,2L))]
 z<-z[Operating_Time==1&is.finite(Gross_Load_MW)&Gross_Load_MW>0&is.finite(Capacity)&Capacity>0]
 z[,Ramp_fraction:=fifelse(Ramp==4L,NA_real_,(Gross_Load_MW-previous_gen)/Capacity)]
 z[,Load_fraction:=Gross_Load_MW/Capacity]
 z[,`:=`(Load=pmin(5L,pmax(1L,as.integer(ceiling(4*Gross_Load_MW/Capacity)))),Year=year(Date))]
 all[[st]]<-z[,c('Facility_Unit.ID','Date','Year','Load','Ramp','Load_fraction','Ramp_fraction','Gross_Load_MW',cols,quality_cols),with=FALSE];rm(z);gc()
}
fwrite(rbindlist(manifests),file.path(out,'Historical_source_manifest_R1.v2.csv'));d<-rbindlist(all);d<-d[Year>=2013L & Year<=2023L];rm(all);gc();fwrite(rbindlist(coverage),file.path(out,'Historical_coverage_R1.v2.csv'))
# Selection uses 2021 only; 2022–2023 remain an independent test for the method comparison.
quality_audit<-list();selections<-list();metrics<-list();audits<-list();entries<-list();rates<-list();k<-0L
for(uid in fleet$Facility_Unit.ID){
 source<-d[Facility_Unit.ID==uid];entry<-list(capacity=fleet[Facility_Unit.ID==uid,Estimated_NameplateCapacity_MW],pollutants=list())
 for(pi in seq_along(polls)){
  poll<-polls[pi];z<-copy(source);z[,mass:=get(cols[pi])];z[,quality_ok:=get(quality_cols[pi])%in%c('Measured','Calculated')]
  quality_audit[[paste(uid,poll)]]<-z[,.(Unit=uid,Pollutant=poll,Full_positive_rows=.N,Quality_eligible_rows=sum(quality_ok),Finite_nonnegative_eligible=sum(quality_ok&is.finite(mass)&mass>=0))]
  z<-z[quality_ok==TRUE&is.finite(mass)&mass>=0];train<-z[Year<=2020];val<-z[Year==2021];test<-z[Year>=2022 & Year<=2023]
  selection<-list(r1_model='fixed',model='fixed',load_curve_fallback=FALSE)
  eligible<-nrow(train)>=200&&uniqueN(train$Date)>=20&&nrow(val)>=100
  if(eligible){selection<-r1v2_select(r1v2_fit(train),val);audit<-as.data.table(selection$audit);audit[,`:=`(Unit=uid,Pollutant=poll)];audits[[paste(uid,poll)]]<-audit}
  evaluation<-z[Year<=2021]
  if(nrow(evaluation)>=200&&uniqueN(evaluation$Date)>=20&&nrow(test)>=100){
   f<-r1v2_fit(evaluation)
   for(comparison in c('fixed','R1','R1.v2')){
    model<-switch(comparison,fixed='fixed',R1=selection$r1_model,R1.v2=selection$model)
    pred<-test$Gross_Load_MW*r1v2_rate(f,model,test$Load_fraction,test$Ramp_fraction,test$Ramp,selection$r1_model,selection$load_curve_fallback)
    k<-k+1L;metrics[[k]]<-data.table(Unit=uid,Pollutant=poll,Comparison=comparison,Model=model,Hours=nrow(test),Abs=sum(abs(pred-test$mass)),Sq=sum((pred-test$mass)^2),Observed=sum(test$mass),Predicted=sum(pred))
   }
  }
  valid<-nrow(z)>=200&&uniqueN(z$Date)>=20
  selections[[paste(uid,poll)]]<-data.table(Unit=uid,Pollutant=poll,Model=selection$model,Selected_model_2021=selection$model,Deployment_fallback='',R1_model=selection$r1_model,Load_curve_fallback=selection$load_curve_fallback,Selection_supported=eligible,Training_hours=nrow(train),Validation_hours=nrow(val),Test_hours=nrow(test),Calibration_hours=nrow(z),Historical_supported=valid)
  if(valid){
   f<-r1v2_fit(z)
   deployed<-selection$model;lf<-selection$load_curve_fallback&&!is.null(f$curve_load)
   if(startsWith(deployed,'curve')&&is.null(f[[deployed]])){
    deployed<-if(lf)'curve_load' else selection$r1_model
    selections[[paste(uid,poll)]][,Deployment_fallback:='Final refit does not meet curve support criteria']
   }
   selections[[paste(uid,poll)]][,`:=`(Model=deployed,Load_curve_fallback=lf)]
   entry$pollutants[[poll]]<-list(fit=f,model=deployed,r1_model=selection$r1_model,load_curve_fallback=lf,source='unit_history')
   rates[[paste(uid,poll)]]<-data.table(Unit=uid,Pollutant=poll,Unit_mean_rate=f$fixed[1])
  }
 }
 entries[[as.character(uid)]]<-entry
 cat('Calibrated unit',uid,'\n')
}
# Use the documented fixed-rate fallback hierarchy. Never transfer a donor's curve.
for(uid in fleet$Facility_Unit.ID)for(pi in seq_along(polls)){
 poll<-polls[pi];key<-as.character(uid);entry<-entries[[key]]
 if(!is.null(entry$pollutants[[poll]]))next
 row<-fleet[Facility_Unit.ID==uid];donor<-as.character(row$Similar_Facility_Unit_ID);donor_entry<-if(length(donor)==1L&&!is.na(donor)&&nzchar(donor))entries[[donor]] else NULL
 if(!is.null(donor_entry)&&!is.null(donor_entry$pollutants[[poll]])&&donor_entry$pollutants[[poll]]$source=='unit_history'){
  rate<-donor_entry$pollutants[[poll]]$fit$fixed[1];origin<-paste0('supplied_donor:',donor)
 }else{
  obs<-switch(poll,CO2='mean_CO2_tons_MW',NOx='mean_NOx_lbs_MW',SO2='mean_SO2_lbs_MW',HI='mean_HI_mmBtu_per_MW')
  rate<-row[[obs]][1];origin<-'supplied_static'
  if(!is.finite(rate)&&poll!='HI'){rate<-row[[paste0(obs,'_estimate')]][1];origin<-'supplied_estimate'}
 }
 if(!is.finite(rate)||rate<0){
  peers<-fleet[Primary_Fuel_Type==row$Primary_Fuel_Type&Unit_Type==row$Unit_Type,Facility_Unit.ID];z<-d[Facility_Unit.ID%in%peers];mass<-z[[cols[pi]]];keep<-is.finite(mass)&mass>=0&z[[quality_cols[pi]]]%in%c('Measured','Calculated')
  rate<-sum(mass[keep])/sum(z$Gross_Load_MW[keep]);origin<-'fuel_and_unit_type_pool'
 }
 if(!is.finite(rate)||rate<0)stop('Unresolved R1.v2 fallback: ',uid,' ',poll)
 f<-list(fixed=rep(rate,20),load=rep(rate,20),load_ramp=rep(rate,20),curve_load=NULL,curve_load_ramp=NULL)
 entry$pollutants[[poll]]<-list(fit=f,model='fixed',r1_model='fixed',load_curve_fallback=FALSE,source=origin);entries[[key]]<-entry
 rates[[paste(uid,poll)]]<-data.table(Unit=uid,Pollutant=poll,Unit_mean_rate=rate)
}
for(uid in names(entries)){
 entries[[uid]]$pollutants<-entries[[uid]]$pollutants[polls]
 for(poll in polls)selections[[paste(uid,poll)]][,Deployment_source:=entries[[uid]]$pollutants[[poll]]$source]
}
fleet_path<-file.path(root,'2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv')
artifact<-list(version='R1.v2',created_utc=format(Sys.time(),tz='UTC',usetz=TRUE),fleet_md5=unname(tools::md5sum(fleet_path)),configuration=r1v2_config,units=entries,
 historical_sources=rbindlist(manifests),calibration_code_md5=unname(tools::md5sum(self)),model_code_md5=unname(tools::md5sum(file.path(dirname(self),'Operating emission models_R1.R'))))
r1v2_validate_artifact(artifact,fleet,artifact$fleet_md5)
saveRDS(artifact,file.path(out,'Operating_emission_models_R1.rds'),compress='gzip',version=2)
fwrite(rbindlist(selections),file.path(out,'Model_selection_R1.v2.csv'));fwrite(rbindlist(audits),file.path(out,'Validation_candidates_R1.v2.csv'))
fwrite(rbindlist(quality_audit),file.path(out,'Quality_coverage_R1.v2.csv'));fwrite(rbindlist(rates),file.path(out,'Unit_mean_rates_R1.v2.csv'))
m<-rbindlist(metrics);fwrite(m,file.path(out,'Heldout_unit_metrics_R1.v2.csv'))
summary<-m[,.(Units=uniqueN(Unit),Hours=sum(Hours),MAE=sum(Abs)/sum(Hours),RMSE=sqrt(sum(Sq)/sum(Hours)),Bias_percent=100*(sum(Predicted)/sum(Observed)-1)),by=.(Pollutant,Comparison)]
fwrite(summary,file.path(out,'Heldout_summary_R1.v2.csv'));print(summary)
selection<-rbindlist(selections);print(selection[, .N,by=.(Pollutant,Model)])
writeLines(c('R1.v2: exact generation multiplied by a fixed, binned or validated continuous operating rate.',
 'Training 2013–2020; model selection 2021; independent evaluation 2022–2023; final refit 2013–2023.',
 'Baseline models and historical filters match R1. Continuous load candidate: quadratic rate in exact load/nameplate.',
 'Continuous load/ramp candidate adds exact consecutive-hour ramp and load*ramp interaction. It is an association, not a causal ramp penalty.',
 'Curves require >=2000 hours, >=100 dates, 5–95% load span >=0.4 and >=100 observations in at least three load quarters.',
 'Ramp curves also require >=500 hours and >=30 dates in each of down, steady and up categories. Categories govern evidence checks, not curve evaluation.',
 'Curves must improve validation mass MSE by >=1% over the retained simpler model and worsen absolute aggregate validation bias by no more than 1 percentage point.',
 'Negative predicted rates are floored at zero; no polynomial extrapolation outside observed load/ramp support.',
 'Outside support or with unknown ramp, use the validated load curve when selected, otherwise the retained R1 binned/fixed model.',
 'Zero generation yields zero mass; startup emissions, aging, controls changes and fuel-supply constraints remain outside this model.',
 'No historical records or source R1 files are overwritten.'),file.path(out,'METHOD_R1.v2.txt'))
cat('R1.v2 calibration complete:',out,'\n')
