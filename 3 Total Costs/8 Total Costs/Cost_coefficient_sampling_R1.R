# Purpose: Allow an explicit annual-only subset without fossil VOM; keep full-run sampling unchanged by default.
# sample existing coefficient ranges because averaging their cases omits cost uncertainty from paired totals.
# Uniform interpolation follows the approved CAPEX/FOM convention. Each key shares one percentile
# across pathways and years; different coefficient keys are independent assumptions, not fitted correlations.
r1_sample_cost_ranges <- function(inputs, simulations, seed=20260906L, include_fossil_vom=TRUE) {
 ids <- sort(unique(as.character(simulations)))
 read <- function(name) as.data.table(copy(inputs[[name]]))
 tables <- list()
 add <- function(d, category, technology, case, amount, low, high, fixed=FALSE) {
  z <- data.table(Simulation=if(fixed) NA_character_ else as.character(d$Simulation),
   Pathway=as.character(d$Pathway),Technology=technology,Component=category,
   Case=as.character(case),Amount=as.numeric(amount))
  z <- z[Case %in% c(low,high)]
  if(!nrow(z)||any(!is.finite(z$Amount)))stop('Missing/nonfinite coefficient endpoints: ',category)
  z[,Case:=fifelse(Case==low,'Lower','Upper')]
  if(fixed)z <- merge(z[,Simulation:=NULL][,Join_R1:=1L],data.table(Simulation=ids,Join_R1=1L),by='Join_R1',allow.cartesian=TRUE)[,Join_R1:=NULL]
  tables[[length(tables)+1L]] <<- z
 }
 # allow an explicitly labeled annual-only VOM subset while the facility input downloads; full costing keeps the default.
 if(include_fossil_vom){f<-read('VOM_Fossil_by_Simulation');add(f,'VOM',paste0('Fossil:',f$Technology),f$scenario,f$NPV,'Advanced','Conservative')}
 n<-read('VOM_Non_Fossil');add(n,'VOM',paste0('Nonfossil:',n$Technology),n$ATB_Scenario,n$NPV,'Advanced','Conservative')
 n<-read('Fuel_Non_Fossil');add(n,'Fuel',paste0('Nonfossil:',n$Technology),n$ATB_Scenario,n$NPV,'Advanced','Conservative')
 n<-read('Imports');add(n,'Imports',paste0('Purchase:',n$Jurisdiction),n$Cost_Type,n$NPV,'Lower','Upper')
 n<-read('CAPEX_FOM_Imports')
 for(k in c('CAPEX','FOM'))add(n,k,rep('Import_infrastructure',nrow(n)),n$Cost_Type,n[[paste0('NPV_',k)]],'Lower','Upper',TRUE)
 n<-read('CAPEX_FOM_CAN_Hydro')
 for(k in c('CAPEX','FOM','VOM'))add(n,paste0('CAN_',k),rep('Canadian_hydro',nrow(n)),n$Cost_Type,n[[paste0('NPV_',k)]],'Lower','Upper')
 n<-read('CH4_CAN_Hydro');n<-melt(n,id.vars=c('Simulation','Pathway'),measure.vars=c('NPV_CH4_Lower','NPV_CH4_Upper'),variable.name='Case',value.name='Amount')
 add(n,'CAN_CH4',rep('Canadian_reservoir',nrow(n)),n$Case,n$Amount,'NPV_CH4_Lower','NPV_CH4_Upper')
 z<-rbindlist(tables,use.names=TRUE)
 if(anyNA(z)||!setequal(z$Simulation,ids))stop('Missing coefficient keys or simulation IDs')
 if(any(z[,.(Complete=setequal(Simulation,ids)),by=.(Pathway,Technology,Component)]$Complete==FALSE))stop('Incomplete simulation coverage for a coefficient key')
 # aggregate duplicate technology labels before interpolation because some modules split a technology into several rows.
 z<-z[,.(Amount=sum(Amount)),by=.(Simulation,Pathway,Technology,Component,Case)]
 bounds<-dcast(z,Simulation+Pathway+Technology+Component~Case,value.var='Amount')
 if(anyNA(bounds))stop('Every represented cost key needs both endpoint cases')
 draws<-unique(bounds[,.(Simulation,Technology,Component)])
 had_seed<-exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE)
 if(had_seed)old_seed<-get('.Random.seed',envir=.GlobalEnv)
 old_kind<-RNGkind()
 on.exit({do.call(RNGkind,as.list(old_kind));if(had_seed)assign('.Random.seed',old_seed,envir=.GlobalEnv) else if(exists('.Random.seed',envir=.GlobalEnv,inherits=FALSE))rm('.Random.seed',envir=.GlobalEnv)},add=TRUE)
 RNGkind('Mersenne-Twister','Inversion','Rejection')
 draws[,U:=vapply(paste(seed,Simulation,Technology,Component,sep='|'),function(key){
  set.seed(strtoi(substr(digest::digest(key,algo='sha256',serialize=FALSE),1,7),16L));runif(1)
 },numeric(1))]
 setorder(draws,Simulation,Technology,Component)
 sampled<-merge(bounds,draws,by=c('Simulation','Technology','Component'))
 sampled[,Amount:=(Lower+U*(Upper-Lower))/1e9]
 if(any(!is.finite(sampled$Amount)))stop('Nonfinite sampled costs')
 list(totals=sampled[,.(Amount=sum(Amount)),by=.(Simulation,Pathway,Component)],
      details=sampled,draws=draws,bounds=bounds)
}

# replace only the trial-value column so existing deterministic endpoint envelopes remain identifiable.
r1_replace_sample <- function(target, sampled, component) {
 x<-as.data.table(copy(target));type<-target$Simulation
 x[,Simulation:=as.character(Simulation)]
 a<-sampled[Component==component,.(Simulation,Pathway,Amount)]
 if(anyDuplicated(a,by=c('Simulation','Pathway')))stop('Duplicate sampled component keys')
 x<-merge(x,a,by=c('Simulation','Pathway'),all.x=TRUE,sort=FALSE)
 if(anyNA(x$Amount))stop('Missing sampled component: ',component)
 x[,(paste0(component,'_mean_bUSD')):=Amount];x[,Amount:=NULL]
 x[,Simulation:=type[match(Simulation,as.character(type))]]
 x[]
}
