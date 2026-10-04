# Purpose: Fit or verify unit operating-rate relationships and calculate emissions from final load/ramp rather than independently sampled masses.
# validated continuous rates at exact modeled load and ramp.
# Base-R numeric prediction; no model fitting or random emission draws during dispatch.
r1v2_config <- list(min_unit_hours=200L,min_unit_days=20L,min_cell_hours=100L,
 min_cell_days=20L,min_curve_hours=2000L,min_curve_days=100L,
 min_curve_load_span=0.4,min_curve_load_groups=3L,min_ramp_hours=500L,
 min_ramp_days=30L,min_relative_mse_improvement=0.01,max_bias_worsening_pp=1)
r1v2_bins <- function(load,ramp) (pmin(5L,pmax(1L,as.integer(ceiling(4*load))))-1L)*4L+ramp
r1v2_fit <- function(z, cfg=r1v2_config) {
 stopifnot(nrow(z)>0L,all(is.finite(z$mass)),all(z$mass>=0),all(z$Gross_Load_MW>0))
 mean_rate <- sum(z$mass)/sum(z$Gross_Load_MW)
 base <- rep(mean_rate,20L); load_rates <- base; joint_rates <- base
 for(l in 1:5){
  q <- z[z$Load==l,]
  if(nrow(q)>=cfg$min_cell_hours && length(unique(q$Date))>=cfg$min_cell_days)
   load_rates[(l-1L)*4L+1:4] <- sum(q$mass)/sum(q$Gross_Load_MW)
 }
 joint_rates <- load_rates
 for(l in 1:5)for(r in 1:3){
  q <- z[z$Load==l & z$Ramp==r,]
  if(nrow(q)>=cfg$min_cell_hours && length(unique(q$Date))>=cfg$min_cell_days)
   joint_rates[(l-1L)*4L+r] <- sum(q$mass)/sum(q$Gross_Load_MW)
 }
 f <- list(fixed=base,load=load_rates,load_ramp=joint_rates,curve_load=NULL,curve_load_ramp=NULL)
 eligible <- nrow(z)>=cfg$min_curve_hours && length(unique(z$Date))>=cfg$min_curve_days &&
  diff(quantile(z$Load_fraction,c(.05,.95),names=FALSE))>=cfg$min_curve_load_span &&
  sum(table(z$Load)>=cfg$min_cell_hours)>=cfg$min_curve_load_groups
 if(!eligible)return(f)
 fit_curve <- function(q,with_ramp=FALSE){
  x<-q$Load_fraction;X<-cbind(1,x,x*x)
  if(with_ramp){d<-q$Ramp_fraction;X<-cbind(X,d,x*d)}
  m<-lm.wfit(X,q$mass/q$Gross_Load_MW,(q$Gross_Load_MW/max(q$Gross_Load_MW))^2)
  if(m$rank<ncol(X)||any(!is.finite(m$coefficients)))return(NULL)
  list(coefficients=unname(m$coefficients),load_range=range(x),
       ramp_range=if(with_ramp)range(q$Ramp_fraction) else NULL,
       Hours=nrow(q),Days=length(unique(q$Date)))
 }
 f$curve_load<-fit_curve(z)
 q<-z[is.finite(z$Ramp_fraction),]
 adequate_ramps<-all(vapply(1:3,function(r){a<-q[q$Ramp==r,];nrow(a)>=cfg$min_ramp_hours&&length(unique(a$Date))>=cfg$min_ramp_days},logical(1)))
 if(adequate_ramps&&nrow(q)>=cfg$min_curve_hours&&length(unique(q$Date))>=cfg$min_curve_days)
  f$curve_load_ramp<-fit_curve(q,TRUE)
 f
}
r1v2_rate <- function(f,model,load,ramp_fraction,ramp_code,base_model='fixed',load_curve_fallback=FALSE) {
 if(model%in%c('fixed','load','load_ramp'))return(f[[model]][r1v2_bins(load,ramp_code)])
 rate<-f[[base_model]][r1v2_bins(load,ramp_code)]
 # Never extrapolate a polynomial outside the fitted operating range.
 apply_curve<-function(curve,with_ramp=FALSE){
  if(is.null(curve))return(invisible(NULL))
  ok<-is.finite(load)&load>=curve$load_range[1]&load<=curve$load_range[2]
  if(with_ramp)ok<-ok&is.finite(ramp_fraction)&ramp_fraction>=curve$ramp_range[1]&ramp_fraction<=curve$ramp_range[2]
  i<-which(ok);x<-load[i];b<-curve$coefficients;v<-b[1]+b[2]*x+b[3]*x*x
  if(with_ramp){d<-ramp_fraction[i];v<-v+b[4]*d+b[5]*x*d}
  if(any(!is.finite(v)))stop('Non-finite continuous emission rate')
  rate[i]<<-pmax(0,v)
 }
 if(model=='curve_load'||load_curve_fallback)apply_curve(f$curve_load)
 if(model=='curve_load_ramp')apply_curve(f$curve_load_ramp,TRUE)
 rate
}
r1v2_metrics <- function(obs,pred) {
 stopifnot(length(obs)>0L,length(obs)==length(pred),all(is.finite(pred)),all(pred>=0))
 c(MSE=mean((pred-obs)^2),Bias_pp=if(sum(obs)>0)100*(sum(pred)-sum(obs))/sum(obs) else if(sum(pred)==0)0 else Inf)
}
r1v2_select <- function(f,val,cfg=r1v2_config) {
 score<-function(model,base='fixed',lf=FALSE)r1v2_metrics(val$mass,val$Gross_Load_MW*r1v2_rate(f,model,val$Load_fraction,val$Ramp_fraction,val$Ramp,base,lf))
 models<-c('fixed','load','load_ramp');scores<-lapply(models,score);names(scores)<-models
 old<-models[which.min(vapply(scores,function(x)x['MSE'],numeric(1)))];best<-old;bestscore<-scores[[old]];load_ok<-FALSE
 audit<-data.frame(Model=models,MSE=vapply(scores,function(x)x['MSE'],numeric(1)),Bias_pp=vapply(scores,function(x)x['Bias_pp'],numeric(1)),Eligible=TRUE,Accepted=FALSE)
 for(candidate in c('curve_load','curve_load_ramp')){
  available<-!is.null(f[[candidate]])
  s<-if(available)score(candidate,old,load_ok) else c(MSE=NA_real_,Bias_pp=NA_real_)
  pass<-available&&is.finite(s['MSE'])&&is.finite(s['Bias_pp'])&&
    s['MSE']<(1-cfg$min_relative_mse_improvement)*bestscore['MSE']&&
    abs(s['Bias_pp'])<=abs(bestscore['Bias_pp'])+cfg$max_bias_worsening_pp
  audit<-rbind(audit,data.frame(Model=candidate,MSE=s['MSE'],Bias_pp=s['Bias_pp'],Eligible=available,Accepted=pass))
  if(pass){best<-candidate;bestscore<-s;if(candidate=='curve_load')load_ok<-TRUE}
 }
 list(r1_model=old,model=best,load_curve_fallback=load_ok,audit=audit)
}
r1v2_predict_unit <- function(gen,previous,consecutive,entry,mode='selected') {
 stopifnot(mode%in%c('selected','r1','fixed'),length(gen)==length(previous),length(gen)==length(consecutive),
           all(is.finite(gen)),all(gen>=0),is.finite(entry$capacity),entry$capacity>0)
 load<-gen/entry$capacity;delta<-(gen-previous)/entry$capacity
 delta[!consecutive|is.na(consecutive)|!is.finite(previous)|previous<=0]<-NA_real_
 ramp<-rep(4L,length(gen));known<-is.finite(delta)
 ramp[known]<-ifelse(delta[known]>0.1,3L,ifelse(delta[known]< -0.1,1L,2L))
 lapply(entry$pollutants,function(p){
  model<-switch(mode,selected=p$model,r1=p$r1_model,fixed='fixed')
  r1v2_rate(p$fit,model,load,delta,ramp,p$r1_model,p$load_curve_fallback)
 })
}
r1v2_validate_artifact <- function(a,fleet,fleet_hash) {
 if(!identical(a$version,'R1.v2')||!identical(a$fleet_md5,fleet_hash))stop('R1.v2 emission artifact version or fleet checksum mismatch')
 if(!setequal(names(a$units),as.character(fleet$Facility_Unit.ID)))stop('R1.v2 emission model unit coverage mismatch')
 for(uid in names(a$units)){
  e<-a$units[[uid]];cap<-fleet$Estimated_NameplateCapacity_MW[match(uid,fleet$Facility_Unit.ID)]
  if(!isTRUE(all.equal(e$capacity,cap))||cap<=0)stop('R1.v2 emission model capacity mismatch: ',uid)
  if(!identical(names(e$pollutants),c('CO2','NOx','SO2','HI')))stop('R1.v2 pollutant coverage mismatch: ',uid)
  for(p in e$pollutants){
   if(!p$model%in%c('fixed','load','load_ramp','curve_load','curve_load_ramp')||!p$r1_model%in%c('fixed','load','load_ramp'))stop('Unknown R1.v2 model')
   if(startsWith(p$model,'curve')&&is.null(p$fit[[p$model]]))stop('Selected R1.v2 curve absent')
   if(isTRUE(p$load_curve_fallback)&&is.null(p$fit$curve_load))stop('R1.v2 load curve fallback absent')
   for(k in c('fixed','load','load_ramp'))if(length(p$fit[[k]])!=20L||any(!is.finite(p$fit[[k]])|p$fit[[k]]<0))stop('Invalid R1.v2 baseline rates')
   for(k in c('curve_load','curve_load_ramp'))if(!is.null(p$fit[[k]])){
    q<-p$fit[[k]];nr<-if(k=='curve_load')3L else 5L
    if(length(q$coefficients)!=nr||any(!is.finite(q$coefficients))||length(q$load_range)!=2L||any(!is.finite(q$load_range))||diff(q$load_range)<=0)stop('Invalid R1.v2 curve')
    if(k=='curve_load_ramp'&&(length(q$ramp_range)!=2L||any(!is.finite(q$ramp_range))||diff(q$ramp_range)<=0))stop('Invalid R1.v2 ramp support')
   }
  }
 }
 invisible(TRUE)
}
