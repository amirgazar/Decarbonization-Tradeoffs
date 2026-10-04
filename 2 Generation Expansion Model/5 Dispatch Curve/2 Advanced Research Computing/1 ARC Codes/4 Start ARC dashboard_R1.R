# Purpose:
# Purpose: Read-only monitoring of all your active ARC jobs, recent history and locally recorded jobs.
# Open in RStudio and Source. Install Shiny once if needed: install.packages('shiny')
DASHBOARD_ROOT_R1 <- Sys.getenv('PHASED_R1_ROOT',Sys.getenv("PHASED_R1_ROOT", unset=getwd()))
DASHBOARD_FOLDER_R1 <- file.path(DASHBOARD_ROOT_R1,'2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes')
DASHBOARD_RUN_R1 <- 'R1_test50_20260919_01'
DASHBOARD_HOST_R1 <- 'amirgazar@tinkercliffs2.arc.vt.edu'
if(!requireNamespace('shiny',quietly=TRUE)||!requireNamespace('ssh',quietly=TRUE))stop('In RStudio run install.packages(c("shiny", "ssh")), then Source this file again.')
dash_exec_R1 <- function(connection,command){r<-ssh::ssh_exec_internal(connection,command,error=FALSE);list(status=r$status,text=trimws(rawToChar(r$stdout)),error=trimws(rawToChar(r$stderr)))}
dash_connect_R1 <- function()ssh::ssh_connect(DASHBOARD_HOST_R1)
dash_disconnect_R1 <- function(connection)ssh::ssh_disconnect(connection)
dash_ledger_R1 <- function(run){p<-file.path(DASHBOARD_FOLDER_R1,'Logs R1',run,'jobs.csv');if(file.exists(p))read.csv(p,colClasses='character') else data.frame(Label=character(),JobID=character(),Status=character())}
# Archived ledgers without settings cannot be monitored; reject them before switching runs.
dash_runs_R1 <- function(){
 p<-list.dirs(file.path(DASHBOARD_FOLDER_R1,'Generated R1'),recursive=FALSE,full.names=TRUE)
 sort(basename(p[file.exists(file.path(p,'bundle R1','1 ARC Settings_R1.R'))]))
}
dash_config_R1 <- function(run){
 if(!run%in%dash_runs_R1())stop('Unknown run')
 p<-file.path(DASHBOARD_FOLDER_R1,'Generated R1',run,'bundle R1','1 ARC Settings_R1.R')
 if(!file.exists(p))stop('Generated settings not found for this run')
 # Read the generated data-only list; reject function calls other than data constructors.
 expressions<-parse(p);x<-expressions[[length(expressions)]]
 if(!is.call(x)||!identical(x[[1]],as.name('<-'))||!identical(x[[2]],as.name('arc')))stop('Unexpected settings format')
 allowed<-function(v){if(is.call(v)){if(!as.character(v[[1]])%in%c('list','c',':','structure','integer','character','numeric','logical'))return(FALSE);return(all(vapply(as.list(v)[-1],allowed,logical(1))))};is.atomic(v)||is.null(v)}
 if(!allowed(x[[3]]))stop('Settings contain executable expressions; not loaded by monitoring dashboard')
 eval(x[[3]],envir=baseenv())
}
dash_parse_R1 <- function(text,names){
 if(!nzchar(trimws(text)))return(setNames(as.data.frame(replicate(length(names),character(),simplify=FALSE)),names))
 x<-read.table(text=text,sep='|',header=FALSE,fill=TRUE,colClasses='character',quote='',comment.char='',na.strings=NULL)
 if(ncol(x)<length(names))stop('Unexpected scheduler response')
 x<-x[,seq_along(names),drop=FALSE];names(x)<-names;x
}
# Show actual ARC jobs together, including jobs outside local run ledgers.
dash_all_ledger_R1 <- function(){
 dirs<-list.dirs(file.path(DASHBOARD_FOLDER_R1,'Logs R1'),recursive=FALSE,full.names=TRUE)
 out<-data.frame(Run=character(),Label=character(),JobID=character(),stringsAsFactors=FALSE)
 for(p in dirs){
  f<-file.path(p,'jobs.csv');if(!file.exists(f))next
  x<-read.csv(f,colClasses='character')
  if(!all(c('Label','JobID')%in%names(x)))next
  x<-x[!is.na(x$JobID)&grepl('^[0-9]+(_[0-9]+)?$',x$JobID),,drop=FALSE]
  if(nrow(x))out<-rbind(out,data.frame(Run=basename(p),Label=x$Label,JobID=x$JobID))
 }
 out[!duplicated(out$JobID,fromLast=TRUE),,drop=FALSE]
}
dash_snapshot_R1 <- function(ledger,queue='',accounting=''){
 fields<-c('JobID','State','Elapsed','Detail','Name')
 q<-dash_parse_R1(queue,fields);a<-dash_parse_R1(accounting,fields)
 x<-rbind(q,a);x<-x[grepl('^[0-9]+(_[0-9]+)?$',x$JobID),,drop=FALSE];x<-x[!duplicated(x$JobID),,drop=FALSE]
 ids<-unique(c(x$JobID,ledger$JobID))
 z<-data.frame(JobID=ids,Name='',Run='',Label='',State='Status unavailable',Elapsed='',Detail='',stringsAsFactors=FALSE)
 for(i in seq_along(ids)){
  k<-match(ids[i],x$JobID);j<-match(ids[i],ledger$JobID)
  if(!is.na(k))z[i,c('State','Elapsed','Detail','Name')]<-x[k,c('State','Elapsed','Detail','Name')]
  if(!is.na(j)){z$Run[i]<-ledger$Run[j];z$Label[i]<-ledger$Label[j];z$Name[i]<-ledger$Label[j]}
 }
 known<-grepl('^[A-Z_]+([+ ].*)?$',z$State);z$State[known]<-sub('[+ ].*$','',z$State[known])
 priority<-ifelse(z$State%in%c('RUNNING','COMPLETING'),1,ifelse(z$State=='PENDING',2,3))
 z[order(priority,-suppressWarnings(as.numeric(sub('_.*','',z$JobID))),na.last=TRUE),,drop=FALSE]
}
dash_location_R1 <- function(cfg,label){
 if(grepl('^simulation_R1_[0-9]+$',label)){
  sim<-as.integer(sub('simulation_R1_','',label,fixed=TRUE))
  return(list(sim=sim,root=file.path(cfg$results_root,'Tasks R1',paste0('sim_R1_',sim),'ensemble_R1',paste0('sim_R1_',sim))))
 }
 if(grepl('^(pilot|ensemble)_R1_[0-9]+_[0-9]+$',label)){
  nums<-as.integer(tail(strsplit(label,'_',fixed=TRUE)[[1]],2))
  if(nums[1]!=nums[2])return(NULL)
  mode<-if(startsWith(label,'pilot_'))'pilot_R1' else 'ensemble_R1'
  return(list(sim=nums[1],root=file.path(cfg$results_root,mode,paste0('sim_R1_',nums[1]))))
 }
 NULL
}
dash_logs_R1 <- function(connection,run,label){
 l<-dash_ledger_R1(run);r<-match(label,l$Label);if(is.na(r)||!grepl('^[0-9]+$',l$JobID[r]))stop('Select a recorded ARC job')
 id<-l$JobID[r];cfg<-dash_config_R1(run);q<-function(x)shQuote(x,type='sh')
 command<-paste('find',q(cfg$code_root),'-maxdepth 1 -type f \\( -name',q(paste0('*_',id,'.out')),'-o -name',q(paste0('*_',id,'.err')),'\\)')
 found<-dash_exec_R1(connection,command);paths<-strsplit(found$text,'\n',fixed=TRUE)[[1]];paths<-paths[nzchar(paths)&startsWith(paths,paste0(cfg$code_root,'/'))]
 location<-dash_location_R1(cfg,label)
 if(!is.null(location))paths<-c(paths,file.path(location$root,'execution.log'),file.path(location$root,paste0('sim_R1_',location$sim),'execution.log'))
 if(!length(paths))return('No logs yet; pending jobs may not have created output files.')
 paste(vapply(head(unique(paths),4L),function(p){r<-dash_exec_R1(connection,paste('test -f',q(p),'&& tail -n 60',q(p)));paste(p,if(r$status==0L)r$text else 'Not available yet',sep='\n')},character(1)),collapse='\n\n')
}
dash_job_R1 <- function(run,label){
 l<-dash_ledger_R1(run);i<-which(l$Label==label)
 if(length(i)!=1L||is.na(l$JobID[i])||!grepl('^[0-9]+$',l$JobID[i]))stop('Select a single recorded ARC job')
 list(label=label,id=l$JobID[i])
}
dash_results_R1 <- function(connection,run,label){
 result<-list(note='Saved results are not available for this job type.',rows=list(),progress=NULL)
 cfg<-dash_config_R1(run);location<-dash_location_R1(cfg,label)
 if(is.null(location))return(result)
 sim<-as.character(location$sim);root<-location$root
 small<-function(path,limit){r<-dash_exec_R1(connection,paste('head -c',limit+1L,shQuote(path,type='sh')));if(r$status!=0L||nchar(r$text,type='bytes')>limit)return(NULL);r$text}
 log<-small(file.path(root,paste0('sim_R1_',sim),'execution.log'),256000L)
 if(!is.null(log)){
  keys<-regmatches(log,gregexpr(paste0('PASS sim_',sprintf('%04d',as.integer(sim)),'_[^[:space:]]+'),log,perl=TRUE))[[1]]
  paths<-sub(paste0('PASS sim_',sprintf('%04d',as.integer(sim)),'_'),'',keys,fixed=TRUE)
  result$progress<-list(done=length(intersect(unique(paths),cfg$pathways)),total=length(cfg$pathways))
 }
 csv<-small(file.path(root,'Yearly_Results.csv'),1000000L)
 if(is.null(csv)){result$note<-'Annual output not yet available (or exceeds the 1 MB preview limit).';return(result)}
 x<-tryCatch(read.csv(text=csv,check.names=FALSE),error=function(e)NULL)
 if(is.null(x)||!all(c('Simulation','Pathway','Year')%in%names(x))){result$note<-'Annual output format could not be read.';return(result)}
 x<-x[as.character(x$Simulation)==sim,,drop=FALSE]
 # Keep source units and per-year values; do not silently infer full-horizon totals or costs.
 cols<-intersect(c('Simulation','Pathway','Year','Hours_present','Demand_MWh','CO2_tons','NOx_lbs','SO2_lbs','Calibrated_Shortage_MWh','Final_Supply_Surplus_MWh','Calibrated_Curtailments_MWh'),names(x))
 result$rows<-lapply(seq_len(nrow(x)),function(i)as.list(x[i,cols,drop=FALSE]))
 result$note<-'Saved annual output preview, in source-column units. Emissions columns describe the existing fossil fleet; this is not a total-cost or full life-cycle result. Coverage/accounting still require validation.'
 result
}

dash_inspect_R1 <- function(connection,run,label){
 j<-dash_job_R1(run,label)
 live<-dash_exec_R1(connection,paste('scontrol show job',j$id))
 history<-dash_exec_R1(connection,paste('sacct --jobs',j$id,'--format=JobID,State,Elapsed,Timelimit,AllocCPUS,ReqMem,MaxRSS,TotalCPU,ExitCode --parsable2'))
 j$text<-paste('CURRENT SCHEDULER DETAILS',if(live$status==0L)live$text else 'No current scheduler record (the job may have finished).','ACCOUNTING AND RESOURCE USE',if(history$status==0L)history$text else history$error,sep='\n\n')
 j$logs<-dash_logs_R1(connection,run,label);j$results<-dash_results_R1(connection,run,label);j
}

# One read-only view replaces run selection; mixed job types cannot share a credible finish estimate.
dashboard_server_R1 <- function(input,output,session){
 connection<-NULL;jobs<-dash_snapshot_R1(dash_all_ledger_R1());updated<-'';updated_epoch<-NULL
 quota<-'';quota_updated<-'';quota_clock<-as.POSIXct(NA);jobdetail<-NULL
 notice<-'Connect to see all your active jobs and the last seven days of ARC history.';error<-FALSE
 push<-function(){session$sendCustomMessage('arc_dashboard',list(connected=!is.null(connection),jobs=lapply(seq_len(nrow(jobs)),function(i)as.list(jobs[i,,drop=FALSE])),notice=notice,error=error,updated=updated,updated_epoch=updated_epoch,quota=quota,quota_updated=quota_updated,jobdetail=jobdetail))}
 refresh_quota<-function(){
  r<-dash_exec_R1(connection,"bash -lc 'showusage'");quota_clock<<-Sys.time()
  if(r$status!=0L){quota<<-'Quota unavailable; try refreshing again.';return()}
  quota<<-gsub('\033\\[[0-9;]*[[:alpha:]]','',r$text);quota_updated<<-as.character(Sys.time())
 }
 refresh<-function(){
  ledger<-dash_all_ledger_R1()
  q<-dash_exec_R1(connection,'squeue --array --user="$USER" --noheader --format="%i|%T|%M|%R|%j"')
  a<-dash_exec_R1(connection,'sacct -X --user="$USER" --starttime=now-7days --format=JobIDRaw,State,Elapsed,ExitCode,JobName%100 --parsable2 --noheader')
  if(q$status!=0L||a$status!=0L)stop('ARC status refresh was incomplete. Previous status retained; try again.')
  history<-a$text
  if(nrow(ledger)){
   ids<-split(ledger$JobID,ceiling(seq_len(nrow(ledger))/100))
   for(group in ids){r<-dash_exec_R1(connection,paste('sacct -X --starttime=1970-01-01 --jobs',paste(group,collapse=','),'--format=JobIDRaw,State,Elapsed,ExitCode,JobName%100 --parsable2 --noheader'))
    if(r$status==0L&&nzchar(r$text))history<-paste(history,r$text,sep='\n')
   }
  }
  jobs<<-dash_snapshot_R1(ledger,q$text,history);updated<<-as.character(Sys.time());updated_epoch<<-as.numeric(Sys.time())
  notice<<-'All your active ARC jobs, seven days of history, and locally recorded jobs.'
  if(is.na(quota_clock)||as.numeric(difftime(Sys.time(),quota_clock,units='secs'))>=300)refresh_quota()
 }
 inspect<-function(id){
  if(length(id)!=1L||!id%in%jobs$JobID||!grepl('^[0-9]+(_[0-9]+)?$',id))stop('Choose a job from the table')
  j<-jobs[match(id,jobs$JobID),,drop=FALSE]
  live<-dash_exec_R1(connection,paste('scontrol show job',id))
  history<-dash_exec_R1(connection,paste('sacct --jobs',id,'--format=JobID,State,Elapsed,Timelimit,AllocCPUS,ReqMem,MaxRSS,TotalCPU,ExitCode --parsable2'))
  d<-list(id=id,text=paste('SCHEDULER',if(live$status==0L)live$text else 'No live scheduler record.','RESOURCE USE',if(history$status==0L)history$text else history$error,sep='\n\n'),logs='Local output paths are unavailable for this job.',results=list(note='No local result mapping.',rows=list()))
  if(nzchar(j$Run)&&j$Run%in%dash_runs_R1()){
   d$logs<-tryCatch(dash_logs_R1(connection,j$Run,j$Label),error=function(e)conditionMessage(e))
   d$results<-tryCatch(dash_results_R1(connection,j$Run,j$Label),error=function(e)list(note=conditionMessage(e),rows=list()))
  }
  jobdetail<<-d
 }
 shiny::observeEvent(input$dashboard_ready,{push()})
 shiny::observeEvent(input$dashboard_action,{
  a<-input$dashboard_action;error<<-FALSE
  tryCatch({
   if(length(a$op)!=1L||!a$op%in%c('connect','disconnect','refresh','usage','inspect'))stop('Unsupported dashboard action')
   if(a$op=='connect'){if(is.null(connection))connection<<-dash_connect_R1();refresh()}
   else if(a$op=='disconnect'){if(!is.null(connection))dash_disconnect_R1(connection);connection<<-NULL;notice<<-'Disconnected; jobs continue on ARC.'}
   else {if(is.null(connection))stop('Connect to ARC first')
    switch(a$op,refresh=refresh(),usage=refresh_quota(),inspect=inspect(a$job))
   }
  },error=function(e){error<<-TRUE;notice<<-conditionMessage(e)})
  push()
 },ignoreInit=TRUE)
 session$onSessionEnded(function(){if(!is.null(connection))try(dash_disconnect_R1(connection),silent=TRUE)})
}
start_dashboard_R1 <- function(){
 html<-file.path(DASHBOARD_FOLDER_R1,'ARC dashboard_R1.html');stopifnot(file.exists(html))
 ui<-shiny::bootstrapPage(shiny::tags$head(shiny::tags$title('ARC job dashboard'),shiny::tags$meta(name='viewport',content='width=device-width, initial-scale=1')),shiny::includeHTML(html))
 shiny::runApp(shiny::shinyApp(ui,dashboard_server_R1),host='127.0.0.1',port=getOption('phased.dashboard.port',NULL),launch.browser=getOption('phased.dashboard.browser',TRUE))
}
if(!isTRUE(getOption('phased.dashboard.no_run',FALSE)))start_dashboard_R1()
