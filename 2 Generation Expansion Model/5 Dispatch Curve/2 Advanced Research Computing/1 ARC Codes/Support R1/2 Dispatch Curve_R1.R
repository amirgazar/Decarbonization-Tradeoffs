# Run the continuous-rate dispatch model in a separate output directory.
# Shared dispatch source, explicit scope and bounded processing.
# Used by each generated job. No second copy of dispatch equations.
a <- commandArgs(TRUE); stopifnot(length(a)==3L)
source(a[1]); mode<-a[2]; ids<-a[3]
stopifnot(mode %in% c("pilot_R1","ensemble_R1"))
Sys.setenv(PHASED_FLAT_DATA=if(arc$flat_data)"1" else "0")
runner<-file.path(arc$code_root,"run_dispatch_review_R1.R")
jobname<-paste0("sim_R1_",gsub(",","_",ids,fixed=TRUE))
out<-file.path(arc$results_root,mode,jobname)
# Pass the explicit pathway scope to the shared runner.
args<-c(runner,arc$code_root,arc$data_root,file.path(arc$results_root,"random R1"),out,arc$hours,ids,"fresh",arc$start_year,paste(arc$pathways,collapse=","))
status<-system2(file.path(R.home("bin"),"Rscript"),vapply(as.character(args),shQuote,character(1)))
if(status!=0L)stop("Dispatch failed, exit ",status,". Retain failed output for diagnosis; use a new run directory to retry.")
writeLines("Dispatch process completed; summary coverage checks still required.",file.path(out,"DISPATCH_COMPLETE.txt"))
