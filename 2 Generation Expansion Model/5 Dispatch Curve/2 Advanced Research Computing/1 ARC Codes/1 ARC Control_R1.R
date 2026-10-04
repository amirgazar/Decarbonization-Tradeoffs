# Purpose: ARC preparation, upload, batched submission and monitoring controls.
# OPEN THIS FILE IN RSTUDIO. Edit section 1, then click Source. Choose actions in the R console menu.
# No Mac Terminal, saved password, automatic notification or automatic ensemble submission.

# 1 SETTINGS - only this file needs editing -------------------------------------
REPOSITORY <- Sys.getenv("PHASED_R1_ROOT", unset=getwd())
AUTHORITATIVE_BACKUP <- Sys.getenv("PHASED_DATA_ROOT", unset=Sys.getenv("PHASED_R1_ROOT", unset=getwd()))
RUN_NAME <- Sys.getenv("PHASED_RUN_NAME", unset="ensemble_full")                 # Keep this name when checking/downloading this run; new name for rerun.
SSH_HOST <- "amirgazar@tinkercliffs2.arc.vt.edu"
ARC_PROJECT <- "/projects/epadecarb/2 Generation Expansion Model"
SIMULATIONS <- 1:1000
TEST_HOURS <- 0L                   # Full 2025-2050 horizon.
PATHWAYS <- c("A","B1","B2","B3","C1","C2","C3","D")
SIMULATIONS_PER_JOB <- 1L              # One simulation per job.
WALLTIME <- "0-12:00:00"               # Batch allowance, not a measured requirement; adjust using measured runtimes.
MEMORY <- "64G"                       # Diagnostic starting request, not a validated full-horizon requirement.
CPUS <- 4L
ENSEMBLE_JOBS_THIS_SUBMISSION <- 10L     # Submit a small next group, never all jobs implicitly.
# Recovery only if connection was lost during sbatch: copy the confirmed job ID from action 5.
RECOVER_JOB_LABEL <- ""
RECOVER_JOB_ID <- ""

# 2 INTERNAL FUNCTIONS - ordinarily no edits below ------------------------------
ARC_FOLDER <- file.path(REPOSITORY,"2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes")
arc_paths <- function(){
 stopifnot(length(TEST_HOURS)==1L,is.finite(TEST_HOURS),TEST_HOURS>=0L,TEST_HOURS==as.integer(TEST_HOURS),
           length(PATHWAYS)>0L,!anyDuplicated(PATHWAYS),all(PATHWAYS%in%c("A","B1","B2","B3","C1","C2","C3","D")))
 stopifnot(grepl("^[A-Za-z0-9_-]+$",RUN_NAME),length(SIMULATIONS)>0,all(SIMULATIONS>0),all(SIMULATIONS==as.integer(SIMULATIONS)),!anyDuplicated(SIMULATIONS),ENSEMBLE_JOBS_THIS_SUBMISSION>=1L,ENSEMBLE_JOBS_THIS_SUBMISSION<=10L,ENSEMBLE_JOBS_THIS_SUBMISSION==as.integer(ENSEMBLE_JOBS_THIS_SUBMISSION))
 list(work=file.path(ARC_FOLDER,"Generated R1",RUN_NAME),bundle=file.path(ARC_FOLDER,"Generated R1",RUN_NAME,"bundle R1"),
      logs=file.path(ARC_FOLDER,"Logs R1",RUN_NAME),downloads=file.path(ARC_FOLDER,"Downloads R1",RUN_NAME))
}
arc_settings <- function()list(
 code_root=file.path(ARC_PROJECT,"2 R Codes","PHASED_R1",RUN_NAME),
 data_root=file.path(ARC_PROJECT,"3 Datasets","PHASED_R1",RUN_NAME),
 results_root=file.path(ARC_PROJECT,"4 Results","PHASED_R1",RUN_NAME),
 library=file.path(ARC_PROJECT,"1 Environment/env_singularity"),
 notification_env="/projects/epadecarb/2 Generation Expansion Model/3 Datasets/API_KEYS.env",
 image="/projects/arcsingularity/ood-rstudio141717-basic_4.1.0.sif",module="apptainer/1.4.0",
 account="epadecarb",partition="normal_q",walltime=WALLTIME,memory=MEMORY,cpus=CPUS,
 batch_size=SIMULATIONS_PER_JOB,simulations=SIMULATIONS,pathways=PATHWAYS,
 first_year=2025L,last_year=2050L,flat_data=TRUE,hours=TEST_HOURS,start_year=2025L,
 evoll=file.path(ARC_PROJECT,"3 Datasets","PHASED_R1",RUN_NAME,"evoll_curve.csv"))
arc_q <- function(x)shQuote(x,type="sh")
arc_backend <- list(connect=function()ssh::ssh_connect(SSH_HOST),disconnect=function(s)ssh::ssh_disconnect(s),
 exec=function(s,c)ssh::ssh_exec_internal(s,c,error=FALSE),
 upload=function(s,f,to)ssh::scp_upload(s,f,to=to),download=function(s,f,to)ssh::scp_download(s,f,to=to))
arc_exec <- function(session,command,check=TRUE){
 r<-arc_backend$exec(session,command);out<-rawToChar(r$stdout);err<-rawToChar(r$stderr)
 if(check&&r$status!=0)stop("ARC command failed: ",err,"\n",out)
 list(status=r$status,text=trimws(out),error=err)
}
arc_ledger <- function(){p<-file.path(arc_paths()$logs,"jobs.csv");if(file.exists(p))read.csv(p,colClasses="character") else data.frame(Label=character(),JobID=character(),Status=character(),stringsAsFactors=FALSE)}
arc_save_ledger <- function(x){p<-arc_paths();dir.create(p$logs,recursive=TRUE,showWarnings=FALSE);save<-file.path(p$logs,"jobs.csv");write.csv(x,paste0(save,".tmp"),row.names=FALSE);stopifnot(file.rename(paste0(save,".tmp"),save))}
arc_require_bundle <- function(){p<-arc_paths();if(!file.exists(file.path(p$bundle,"1 ARC Settings_R1.R")))stop("Choose action 1 first")
 e<-new.env();sys.source(file.path(p$bundle,"1 ARC Settings_R1.R"),e);if(!identical(e$arc,arc_settings()))stop("Settings differ from this run's generated files. Restore settings or choose a new RUN_NAME; do not mix runs.")
 invisible(p)
}
arc_generate <- function(){
 p<-arc_paths();if(dir.exists(p$work))stop("This RUN_NAME already has generated files. Reuse them, or choose a new RUN_NAME.")
 dir.create(p$work,recursive=TRUE);dir.create(p$logs,recursive=TRUE,showWarnings=FALSE)
 source<-file.path(p$work,"source R1");dir.create(source)
 support<-file.path(ARC_FOLDER,"Support R1");stopifnot(dir.exists(support))
 for(f in list.files(support,full.names=TRUE))if(!dir.exists(f))stopifnot(file.copy(f,source))
 con<-file(file.path(source,"1 ARC Settings_R1.R"),"w");writeLines("# save the selected run settings so all generated jobs use the same configuration.\narc <-",con);dput(arc_settings(),con);close(con)
 log<-file.path(p$logs,"generation.log")
 status<-system2(file.path(R.home("bin"),"Rscript"),vapply(c(file.path(source,"4 RCode_and_BASH_Generator_R1.R"),REPOSITORY,p$bundle),shQuote,character(1)),stdout=log,stderr=log)
 if(status!=0)stop("Generation failed; inspect ",log)
 # Archive the code only, so SCP transfers one file instead of thousands of launchers.
 old<-setwd(p$bundle);on.exit(setwd(old));utils::tar(file.path(p$work,"code_bundle.tar.gz"),files=list.files(".",all.files=FALSE),compression="gzip",tar="internal")
 cat("Generated files and upload archive in",p$work,"\nNothing uploaded or submitted.\n")
}
arc_upload_code <- function(session){
 p<-arc_require_bundle();cfg<-arc_settings();dest<-cfg$code_root;archive<-file.path(p$work,"code_bundle.tar.gz")
 md5<-unname(tools::md5sum(archive));marker<-file.path(dest,"UPLOAD_COMPLETE.md5")
 if(arc_exec(session,paste("test -f",arc_q(marker)),FALSE)$status==0){
  if(arc_exec(session,paste("cat",arc_q(marker)))$text==md5){cat("Identical code bundle already uploaded.\n");return(invisible(NULL))}
  stop("Different remote code already exists; choose a new RUN_NAME")
 }
 arc_exec(session,paste("mkdir -p",arc_q(dirname(dest))))
 arc_exec(session,paste("mkdir",arc_q(dest))) # Refuse to overwrite an existing/partial upload.
 arc_backend$upload(session,archive,dest)
 remote<-file.path(dest,basename(archive));got<-strsplit(arc_exec(session,paste("md5sum",arc_q(remote)))$text,"[[:space:]]+")[[1]][1]
 if(got!=md5)stop("Uploaded archive checksum mismatch")
 arc_exec(session,paste("tar -xzf",arc_q(remote),"-C",arc_q(dest)))
 arc_exec(session,paste("printf '%s\\n'",arc_q(md5),">",arc_q(marker)))
 cat("Uploaded and verified code. No jobs submitted.\n")
}
arc_input_paths <- function(){
 # Read the exact model input path calls, without executing its model or notification tail.
 code<-readLines(file.path(REPOSITORY,"2 Generation Expansion Model/5 Dispatch Curve/dispatch_curve_base_v2.R"))
 lo<-grep("^## ----- 1\\) Load data",code);hi<-grep("^## ----- 2\\) Functions",code)
 calls<-parse(text=code[(lo+1):(hi-1)]);rels<-character()
 walk<-function(x){if(missing(x))return(invisible(NULL));if(is.call(x)){if(identical(x[[1]],as.name("p"))){args<-as.list(x)[-1];if(all(vapply(args,is.character,logical(1))))rels<<-c(rels,do.call(file.path,args))};for(y in as.list(x)[-1])walk(y)}}
 for(x in calls)walk(x)
 rels<-unique(c(rels,"4 External Data/ISO-NE Unmet Demand/evoll_curve.csv"))
 files<-vapply(rels,function(rel){
  authoritative<-basename(rel)%in%c("Fossil_Fuel_Generation_Emissions.csv","Fossil_Fuel_hr_maxmin.csv","Fossil_Fuel_Facilities_Data.csv")
  candidates<-file.path(if(authoritative)c(AUTHORITATIVE_BACKUP,REPOSITORY,Sys.getenv("PHASED_REFERENCE_ROOT",Sys.getenv("PHASED_DATA_ROOT", unset=Sys.getenv("PHASED_R1_ROOT", unset=getwd())))) else c(REPOSITORY,AUTHORITATIVE_BACKUP,Sys.getenv("PHASED_REFERENCE_ROOT",Sys.getenv("PHASED_DATA_ROOT", unset=Sys.getenv("PHASED_R1_ROOT", unset=getwd())))),rel)
  available<-candidates[file.exists(candidates)];if(!length(available))stop("Missing local input: ",rel);if(authoritative&&available[1]!=candidates[1])message("Backup source absent; using repository copy for ",basename(rel),". Full path and checksum will be recorded.");available[1]
 },character(1))
 if(anyDuplicated(basename(files)))stop("Ambiguous flat input names")
 data.frame(File=basename(files),Local=unname(files),stringsAsFactors=FALSE)
}
arc_upload_inputs <- function(session){
 arc_require_bundle();cfg<-arc_settings();tab<-arc_input_paths();dir.create(arc_paths()$logs,recursive=TRUE,showWarnings=FALSE)
 if(nrow(arc_ledger()))stop("Jobs already recorded for this run: inputs are frozen. Use a new RUN_NAME to change them.")
 arc_exec(session,paste("mkdir -p",arc_q(cfg$data_root)));tab$MD5<-NA_character_;tab$Action<-NA_character_
 for(i in seq_len(nrow(tab))){
  cat("Checking",tab$File[i],"(large-file checks can take time)\n");hash<-unname(tools::md5sum(tab$Local[i]));tab$MD5[i]<-hash
  dest<-file.path(cfg$data_root,tab$File[i]);base<-file.path(ARC_PROJECT,"3 Datasets",tab$File[i])
  remote_hash<-function(path){r<-arc_exec(session,paste("md5sum",arc_q(path)),FALSE);if(r$status==0)strsplit(r$text,"[[:space:]]+")[[1]][1] else ""}
  if(remote_hash(dest)==hash){tab$Action[i]<-"already verified";next}
  if(arc_exec(session,paste("test -e",arc_q(dest)),FALSE)$status==0)stop("Different file in this run's data folder: ",dest,". Use a new RUN_NAME.")
  base_hash<-remote_hash(base)
  if(base_hash!=hash){
   prior <- arc_exec(session,paste("find",arc_q(file.path(ARC_PROJECT,"3 Datasets","PHASED_R1")),"-mindepth 2 -maxdepth 2 -type f -name",arc_q(tab$File[i])),FALSE)
   candidates <- strsplit(prior$text,"\n",fixed=TRUE)[[1]]
   for(candidate in candidates[nzchar(candidates)])if(candidate!=dest&&remote_hash(candidate)==hash){base<-candidate;base_hash<-hash;break}
  }
  if(base_hash==hash){
   # Copy on the remote filesystem: independent from later edits to the shared originals.
   arc_exec(session,paste("cp",arc_q(base),arc_q(dest)));tab$Action[i]<-"matching ARC input copied remotely"
  }else{arc_backend$upload(session,tab$Local[i],cfg$data_root);tab$Action[i]<-"uploaded from local authoritative file"}
  if(remote_hash(dest)!=hash)stop("Input checksum mismatch: ",dest)
 }
 manifest<-file.path(arc_paths()$logs,"Inputs_verified.csv");write.csv(tab,manifest,row.names=FALSE);arc_backend$upload(session,manifest,cfg$data_root)
 cat("All",nrow(tab),"inputs verified; no jobs submitted.\n")
}
arc_submit <- function(session,label,script,dependencies=character()){
 ledger<-arc_ledger();if(label%in%ledger$Label){row<-ledger[ledger$Label==label,];if(row$Status=="submitted")return(row$JobID);stop("Uncertain prior submission for ",label,". Check status and use recovery action; do not resubmit blindly.")}
 cfg<-arc_settings();args<-c("sbatch --parsable",if(length(dependencies))paste0("--dependency=afterok:",paste(dependencies,collapse=":")),arc_q(script))
 ledger<-rbind(ledger,data.frame(Label=label,JobID="",Status="submission_pending"));arc_save_ledger(ledger)
 r<-arc_exec(session,paste("cd",arc_q(cfg$code_root),"&&",paste(args,collapse=" ")),FALSE)
 # A definite scheduler dependency rejection is retryable; transport failures remain uncertain.
 if(r$status!=0L && !nzchar(r$text) && grepl("sbatch: error: Batch job submission failed: Job dependency problem",r$error,fixed=TRUE)){
  arc_save_ledger(ledger[ledger$Label!=label,,drop=FALSE])
  stop("ARC rejected this dependency; no job was accepted. Choose action 7 to resume.")
 }
 if(r$status!=0)stop("Submission failed or uncertain: ",r$error,". Inspect ARC status before recovery.")
 id<-sub(";.*$","",r$text);if(!grepl("^[0-9]+$",id))stop("Unrecognized sbatch response; check ARC before retry")
 ledger$JobID[ledger$Label==label]<-id;ledger$Status[ledger$Label==label]<-"submitted";arc_save_ledger(ledger)
 cat(label,"submitted as",id,"\n");id
}
arc_prepare <- function(session){
 arc_require_bundle();cfg<-arc_settings()
 arc_exec(session,paste("test -f",arc_q(file.path(cfg$code_root,"UPLOAD_COMPLETE.md5")),"&& test -f",arc_q(file.path(cfg$data_root,"Inputs_verified.csv"))))
 command<-paste("module load",arc_q(cfg$module),"&& apptainer exec --bind /projects --env",arc_q(paste0("R_LIBS_USER=",cfg$library)),arc_q(cfg$image),"Rscript -e",arc_q('for(p in c("data.table","lubridate","zoo","jsonlite","httr")){library(p,character.only=TRUE);cat(p,as.character(packageVersion(p)),"\\n")}'))
 cat(arc_exec(session,paste("bash -lc",arc_q(command)))$text,"\n")
 # Read credential presence on ARC only; never print values or copy the environment file.
 credential_check<-paste0('p <- ',encodeString(cfg$notification_env,quote='"'),
  '; stopifnot(file.exists(p), readRenviron(p), nzchar(Sys.getenv("PUSHOVER_TOKEN")), nzchar(Sys.getenv("PUSHOVER_USER"))); cat("Pushover environment ready\\n")')
 check_command<-paste("module load",arc_q(cfg$module),"&& apptainer exec --bind /projects --env",arc_q(paste0("R_LIBS_USER=",cfg$library)),arc_q(cfg$image),"Rscript -e",arc_q(credential_check))
 cat(arc_exec(session,paste("bash -lc",arc_q(check_command)))$text,"\n")
 arc_submit(session,"prepare","1_Prepare_Random_R1.sh")
}
arc_status <- function(session){
 cfg<-arc_settings();cat(arc_exec(session,"squeue -u \"$USER\"",FALSE)$text,"\n")
 ledger<-arc_ledger();ids<-ledger$JobID[nzchar(ledger$JobID)]
 if(length(ids))cat(arc_exec(session,paste("sacct -j",paste(ids,collapse=","),"--format=JobID,JobName,State,Elapsed,MaxRSS,ExitCode -P"),FALSE)$text,"\n")
 if(any(ledger$Status!="submitted"))cat("Uncertain submission entries exist. Check recent ARC jobs and use action 9 with a confirmed job ID.\n")
}
arc_download <- function(session){
 cfg<-arc_settings();p<-arc_paths();dir.create(p$downloads,recursive=TRUE,showWarnings=FALSE)
 for(mode in "ensemble_R1"){
  remote<-file.path(cfg$results_root,mode,"Final R1")
  if(arc_exec(session,paste("test -f",arc_q(file.path(remote,"SUMMARY_COMPLETE.txt"))),FALSE)$status==0){
   dest<-file.path(p$downloads,mode);dir.create(dest,recursive=TRUE,showWarnings=FALSE);arc_backend$download(session,remote,dest)
  }
 }
 # Only small Slurm logs, never the massive hourly partitions by default.
 for(pattern in c("*.out","*.err")){
  command<-paste("find",arc_q(cfg$code_root),"-maxdepth 1 -type f -name",arc_q(pattern))
  paths<-strsplit(arc_exec(session,command)$text,"\n",fixed=TRUE)[[1]]
  for(path in paths[nzchar(paths)])arc_backend$download(session,path,p$downloads)
 }
 paths<-strsplit(arc_exec(session,paste("find",arc_q(cfg$results_root),"-type f -name execution.log"),FALSE)$text,"\n",fixed=TRUE)[[1]]
 for(path in paths[nzchar(paths)]){
  rel<-substring(path,nchar(cfg$results_root)+2L);dest<-file.path(p$downloads,dirname(rel));dir.create(dest,recursive=TRUE,showWarnings=FALSE)
  arc_backend$download(session,path,dest)
 }
 cat("Downloaded available completed summaries and logs to",p$downloads,"\n")
}
arc_execution_logs <- function(session){
 command<-paste("find",arc_q(arc_settings()$results_root),"-type f -name execution.log -printf '%T@ %p\\n' | sort -nr | head -n 3 | cut -d ' ' -f2-")
 paths<-strsplit(arc_exec(session,command,FALSE)$text,"\n",fixed=TRUE)[[1]]
 for(path in paths[nzchar(paths)])cat("\n",path,"\n",arc_exec(session,paste("tail -n 40",arc_q(path)),FALSE)$text,"\n")
 if(!any(nzchar(paths)))cat("No R execution logs yet; use action 5 for queue status and action 6 for Slurm startup/error logs.\n")
}
# Completed preparation jobs can age out of scheduler dependency records. Verify saved inputs instead.
arc_preparation_ready <- function(session){
 root<-file.path(arc_settings()$results_root,"random R1")
 code<-paste0("import json,hashlib,pathlib; p=pathlib.Path(",encodeString(root,quote='"'),
 "); m=json.loads((p/'manifest.json').read_text()); f=p/'Random_Sequence.csv'; ",
 "assert m['simulations']==[",paste(SIMULATIONS,collapse=","),"]; assert m['random_rows']>0; ",
 "assert hashlib.sha256(f.read_bytes()).hexdigest()==m['outputs']['Random_Sequence.csv']['sha256']")
 arc_exec(session,paste("python3 -c",arc_q(code)),FALSE)$status==0L
}
arc_ensemble <- function(session){
 if(TEST_HOURS>0L)stop("This is a partial-horizon test configuration. Use a new full-horizon RUN_NAME before ensemble submission.")
 arc_require_bundle()
 prep<-arc_ledger();prep<-prep[prep$Label=="prepare" & prep$Status=="submitted",,drop=FALSE]
 if(nrow(prep)!=1L||!grepl("^[0-9]+$",prep$JobID))stop("Choose action 4 to prepare random inputs first")
 dependencies<-prep$JobID
 jobs<-read.csv(file.path(arc_paths()$bundle,"Jobs.csv"));labels<-unique(jobs$Job[jobs$Mode=="ensemble_R1"]);ledger<-arc_ledger()
 if(any(ledger$Status!="submitted"))stop("Resolve uncertain submissions first")
 # Submit remaining simulations in groups of ten, with 15 seconds between successful groups.
 repeat{
  ledger<-arc_ledger()
  if(any(ledger$Status!="submitted"))stop("Resolve uncertain submissions before continuing")
  todo<-head(setdiff(labels,ledger$Label),ENSEMBLE_JOBS_THIS_SUBMISSION)
  if(!length(todo)){cat("All ensemble jobs are recorded. Choose action 8 to queue the final summary.\n");break}
  if(arc_preparation_ready(session))dependencies<-character()
  for(label in todo)arc_submit(session,label,file.path("jobs R1",paste0(label,"_R1.sh")),dependencies)
  remaining<-length(setdiff(labels,arc_ledger()$Label))
  cat("Group submitted;",remaining,"ensemble jobs remain.\n")
  if(remaining>0L){cat("Waiting 15 seconds before the next group.\n");Sys.sleep(15)}
 }
}
arc_summary <- function(session){
 if(TEST_HOURS>0L)stop("Full-horizon ensemble configuration required.")
 arc_require_bundle();jobs<-read.csv(file.path(arc_paths()$bundle,"Jobs.csv"));labels<-unique(jobs$Job[jobs$Mode=="ensemble_R1"]);ledger<-arc_ledger()
 rows<-match(labels,ledger$Label);if(anyNA(rows)||any(ledger$Status[rows]!="submitted"))stop("Not all intended ensemble jobs have been submitted")
 arc_submit(session,"summary_ensemble","5_BASH_Summary_ensemble_R1.sh",ledger$JobID[rows])
}
arc_recover <- function(session){
 stopifnot(nzchar(RECOVER_JOB_LABEL),grepl("^[0-9]+$",RECOVER_JOB_ID));ledger<-arc_ledger()
 if(!RECOVER_JOB_LABEL%in%ledger$Label)stop("No pending entry with that label")
 info<-arc_exec(session,paste("sacct -j",RECOVER_JOB_ID,"--format=JobID,JobName%100,WorkDir%500 -P"))$text
 cat(info,"\n");if(!grepl(arc_settings()$code_root,info,fixed=TRUE))stop("Job work directory does not match this run")
 expected<-if(RECOVER_JOB_LABEL=="prepare")"prepare-random" else if(RECOVER_JOB_LABEL=="summary_ensemble")"summary-ensemble" else RECOVER_JOB_LABEL
 if(!grepl(expected,info,fixed=TRUE))stop("Job name does not match pending label")
 ledger$JobID[ledger$Label==RECOVER_JOB_LABEL]<-RECOVER_JOB_ID;ledger$Status[ledger$Label==RECOVER_JOB_LABEL]<-"submitted";arc_save_ledger(ledger)
}
arc_calibrate_emissions <- function(){
 if(dir.exists(arc_paths()$work))stop("Choose a new RUN_NAME before recalibrating; generated runs must retain their input version")
 out <- file.path(ARC_FOLDER,"Logs R1",paste0("Emissions_calibration_R1.v2_",format(Sys.time(),"%Y%m%d_%H%M%S")))
 script <- file.path(ARC_FOLDER,"Support R1","0 Calibrate Operating Emissions_R1.R")
 status <- system2(file.path(R.home("bin"),"Rscript"),vapply(c(script,REPOSITORY,out),shQuote,character(1)))
 if(status!=0)stop("Calibration failed; inspect ",out)
 dest <- file.path(REPOSITORY,"2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Operating_emission_models_R1.rds")
 if(file.exists(dest))stopifnot(file.copy(dest,file.path(out,"Previous_Operating_emission_models_R1.rds")))
 stopifnot(file.copy(file.path(out,"Operating_emission_models_R1.rds"),dest,overwrite=TRUE))
 cat("Calibrated operating-rate lookup saved. Review held-out metrics in",out,"before a new production run.\n")
}
# Recover scheduler launch holds separately, using existing job IDs; report execution failures for output review.
arc_recover_launches <- function(session){
 ledger<-arc_ledger();ledger<-ledger[ledger$Status=="submitted" & grepl("^[0-9]+$",ledger$JobID),,drop=FALSE]
 if(!nrow(ledger)){cat("No recorded jobs to check.\n");return(invisible(NULL))}
 if(anyDuplicated(ledger$JobID))stop("Duplicate recorded job IDs; review ledger first")
 queue<-arc_exec(session,'squeue --user="$USER" --noheader --format="%i|%T|%r"')$text
 lines<-strsplit(queue,"\n",fixed=TRUE)[[1]];released<-character()
 for(line in lines[nzchar(lines)]){
  v<-strsplit(line,"|",fixed=TRUE)[[1]];if(length(v)!=3L||!v[1]%in%ledger$JobID)next
  if(v[2]!="PENDING"||!grepl("launch failed requeued held",v[3],fixed=TRUE))next
  id<-v[1]
  # Recheck with the same scheduler format: scontrol may encode the reason differently.
  live<-arc_exec(session,paste("squeue --jobs",id,"--noheader --format='%i|%T|%r'"))$text
  current<-strsplit(trimws(live),"|",fixed=TRUE)[[1]]
  if(length(current)!=3L||current[1]!=id||current[2]!="PENDING"||
     !grepl("launch failed requeued held",current[3],fixed=TRUE)){
   cat(id,"not currently launch-held; scheduler reports:",if(nzchar(live))live else "no queue record","\n");next
  }
  arc_exec(session,paste("scontrol release",id))
  released<-c(released,id);cat("Released launch hold on existing job",id,"; ARC will schedule it again.\n")
 }
 failures<-character()
 for(ids in split(ledger$JobID,ceiling(seq_len(nrow(ledger))/100))){
  history<-arc_exec(session,paste("sacct -X --starttime=2026-09-19 --jobs",paste(ids,collapse=","),"--format=JobIDRaw,State,ExitCode --parsable2 --noheader"))$text
  for(line in strsplit(history,"\n",fixed=TRUE)[[1]]){
   v<-strsplit(line,"|",fixed=TRUE)[[1]]
   if(length(v)>=3L && v[1]%in%ids && grepl("^(FAILED|TIMEOUT|OUT_OF_MEMORY|NODE_FAIL|CANCELLED|PREEMPTED|BOOT_FAIL|DEADLINE)",v[2]))failures<-c(failures,line)
  }
 }
 cat(length(released),"launch holds released.\n")
 if(length(failures)){cat("Execution failures requiring log/partial-output review before rerunning:\n",paste(unique(failures),collapse="\n"),"\n")}
 else cat("No execution failures found in available accounting records.\n")
 invisible(list(released=released,failures=unique(failures)))
}
# Aggregate the RDS partitions and merge the annual summaries.
arc_menu <- function(){
 if(!requireNamespace("ssh",quietly=TRUE))stop('Install once in the RStudio Console: install.packages("ssh")')
 session<-NULL;on.exit(if(!is.null(session))arc_backend$disconnect(session))
 actions<-list(arc_generate,arc_upload_code,arc_upload_inputs,arc_prepare,arc_status,arc_download,arc_ensemble,arc_summary,arc_recover,arc_calibrate_emissions,arc_execution_logs,arc_recover_launches)
 labels<-c("Generate local ARC files (no jobs submitted)","Upload code using current SSH session","Verify/upload required inputs","Extract saved random inputs for the selected simulations","Check ARC job status","Download available summaries and logs","Submit all remaining ensemble jobs (10 at a time; 15-second pauses)","Submit final ensemble summary","Recover an uncertain submission using settings","Recalibrate operating emission rates locally","Show latest R execution logs","Check recorded jobs and release launch-failure holds")
 repeat{
  choice<-utils::menu(labels,title=paste("PHASED ARC —",RUN_NAME,"— choose an action; 0 exits"));if(choice==0)break
  tryCatch({if(choice%in%c(1,10))actions[[choice]]() else{
   if(!requireNamespace("ssh",quietly=TRUE))stop('Install once in the RStudio Console: install.packages("ssh")')
   if(is.null(session))session<-arc_backend$connect()
   actions[[choice]](session)
  }},error=function(e)message(conditionMessage(e)))
 }
}
# Source opens the menu in RStudio. Automated local tests set phased.arc.no_menu=TRUE.
if(!isTRUE(getOption("phased.arc.no_menu",FALSE)))arc_menu()
