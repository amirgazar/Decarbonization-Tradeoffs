# best-effort stage-aware notification; credentials stay on ARC.
# Optional arguments 4/5: elapsed shell seconds and generated settings path. Old three-argument jobs remain supported.
arc_notification_text <- function(job, exit_code, cfg=list(), elapsed=NA_real_, job_id='unknown') {
 success<-identical(as.character(exit_code),'0')
 stage<-if(job=='prepare-random')'Input preparation' else if(grepl('^summary-',job))'Summary and accounting checks' else 'Dispatch calculation'
 run<-if(!is.null(cfg$code_root))basename(cfg$code_root) else 'unspecified run'
 title<-if(!success)'PHASED | Job failed' else if(grepl('^summary-',job))'PHASED | Outputs ready for review' else if(job=='prepare-random')'PHASED | Inputs ready' else 'PHASED | Dispatch complete'
 ids<-if(grepl('pilot_R1',job))cfg$simulations[1] else cfg$simulations
 # For a batched dispatch job, report its actual simulation interval rather than the entire ensemble.
 if(grepl('^(pilot|ensemble)(_R1)?_[0-9]+_[0-9]+$',job)){
  parts<-tail(strsplit(job,'_',fixed=TRUE)[[1]],2);ids<-cfg$simulations[cfg$simulations>=as.integer(parts[1])&cfg$simulations<=as.integer(parts[2])]
 }
 scope<-if(length(ids))paste0('Simulation(s): ',if(length(ids)<=5)paste(ids,collapse=', ') else paste0(min(ids),'-',max(ids),' (',length(ids),' total)')) else NULL
 paths<-if(length(cfg$pathways))paste('Pathways:',paste(cfg$pathways,collapse=', ')) else NULL
 horizon<-if(!is.null(cfg$hours))if(cfg$hours>0)paste('Horizon:',cfg$hours,'hours (partial test)') else paste0('Horizon: ',cfg$first_year,'-',cfg$last_year,' (full)') else NULL
 duration<-if(length(elapsed)==1L&&is.finite(elapsed)&&elapsed>=0){s<-as.integer(elapsed);sprintf('Elapsed: %dh %02dm %02ds',s%/%3600,(s%%3600)%/%60,s%%60)} else 'Elapsed: unavailable'
 next_step<-if(!success)'Next: inspect the Slurm error log before retrying; dependent jobs may be blocked.' else if(job=='prepare-random')'Next: the dependent dispatch job can start when scheduled.' else if(grepl('^summary-',job))'Next: RStudio action 6 to download; scientific review is still required.' else 'Next: summary checks are pending; this is not final validation.'
 location<-if(!success&&!is.null(cfg$code_root))paste('Error log:',file.path(cfg$code_root,paste0(job,'_',job_id,'.err'))) else NULL
 message<-paste(c(paste('Run:',run),paste('Stage:',stage),paste('Status:',if(success)'Completed' else paste('FAILED; exit',exit_code)),scope,paths,horizon,duration,paste('Slurm job:',job_id),next_step,location),collapse='\n')
 list(title=title,message=substr(message,1L,1024L),priority=if(success)0L else 1L)
}
notify<-function(a=commandArgs(TRUE)){
 if(!length(a)%in%c(3L,5L)){cat('Pushover skipped: invalid notification arguments\n');return()}
 cfg<-list()
 if(length(a)==5L&&file.exists(a[5])){e<-new.env(parent=baseenv());sys.source(a[5],e);cfg<-e$arc}
 elapsed<-if(length(a)==5L)suppressWarnings(as.numeric(a[4])) else NA_real_
 payload<-arc_notification_text(a[2],a[3],cfg,elapsed,Sys.getenv('SLURM_JOB_ID','unknown'))
 if(!file.exists(a[1])){cat('Pushover skipped: environment file not found\n');return()}
 if(!suppressWarnings(readRenviron(a[1]))){cat('Pushover skipped: environment file could not be read\n');return()}
 token<-Sys.getenv('PUSHOVER_TOKEN');user<-Sys.getenv('PUSHOVER_USER')
 if(!nzchar(token)||!nzchar(user)){cat('Pushover skipped: PUSHOVER_TOKEN or PUSHOVER_USER missing\n');return()}
 if(!requireNamespace('httr',quietly=TRUE)){cat('Pushover skipped: httr unavailable\n');return()}
 response<-httr::POST('https://api.pushover.net/1/messages.json',body=c(list(token=token,user=user),payload),encode='form',httr::timeout(15))
 body<-httr::content(response,as='parsed')
 if(httr::status_code(response)==200L&&identical(as.integer(body$status),1L))cat('Pushover accepted:',if(a[3]=='0')'SUCCESS' else 'FAILED','\n') else cat('Pushover not accepted; HTTP status',httr::status_code(response),'\n')
}
# Pure formatting can be tested without reading credentials or making a network request.
if(!isTRUE(getOption('phased.notification_preview',FALSE)))invisible(tryCatch(notify(),error=function(e)cat('Pushover failed: request/configuration error (credentials suppressed)\n')))
