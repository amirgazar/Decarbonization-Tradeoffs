# Open this file in RStudio and click Source.
# Uses the same ssh::ssh_connect method as 1 ARC Control_R1.R.
# One authenticated connection uploads, submits, monitors and downloads.
# Source again with the same RUN_NAME to resume. Change RUN_NAME for a new run.

AP4_CONFIG <- list(
  local_dir = Sys.getenv("PHASED_AP4_ARC", unset=""),
  host = "amirgazar@tinkercliffs2.arc.vt.edu",
  remote_parent = "/projects/epadecarb/AP4_Validation",
  run_name = "AP4_native_20260920_01",
  account = "epadecarb",
  partition = "normal_q",
  cpus = 4L,
  memory = "8G",
  walltime = "01:00:00",
  matlab_module = "MATLAB",
  poll_seconds = 30,
  monitor_hours = 24
)

ap4_backend <- list(
  connect = function(host) ssh::ssh_connect(host),
  disconnect = function(s) ssh::ssh_disconnect(s),
  exec = function(s, command) ssh::ssh_exec_internal(s, command, error=FALSE),
  upload = function(s, files, to) ssh::scp_upload(s, files, to=to),
  download = function(s, files, to) ssh::scp_download(s, files, to=to)
)
ap4_q <- function(x) shQuote(x, type="sh")
ap4_exec <- function(session, command, check=TRUE) {
  z <- ap4_backend$exec(session, command)
  ans <- list(status=z$status, text=trimws(rawToChar(z$stdout)), error=trimws(rawToChar(z$stderr)))
  if (check && ans$status != 0L) stop("ARC command failed: ", ans$error, "\n", ans$text)
  ans
}
ap4_paths <- function(cfg) {
  stopifnot(grepl("^[A-Za-z0-9_-]+$", cfg$run_name), cfg$poll_seconds >= 5,
            cfg$cpus >= 1, cfg$monitor_hours > 0)
  remote <- file.path(cfg$remote_parent, cfg$run_name)
  work <- file.path(remote,"AP4_ARC")
  list(remote=remote, work=work,
       analysis=file.path(work,"AP4_Investigation_2026-09-20"),
       archive=file.path(cfg$local_dir,"uploads","AP4_ARC_Validation.zip"),
       logs=file.path(cfg$local_dir,"logs",cfg$run_name),
       outputs=file.path(cfg$local_dir,"outputs",cfg$run_name))
}
ap4_save <- function(state,p) {
  f <- file.path(p$logs,"controller_state.rds")
  saveRDS(state,paste0(f,".tmp"))
  if (!file.rename(paste0(f,".tmp"),f)) stop("Could not save controller state")
}
ap4_remote_exists <- function(s,path) ap4_exec(s,paste("test -e",ap4_q(path)),FALSE)$status==0L
ap4_hash <- function(file) unname(tools::md5sum(file))
ap4_prepare <- function(s,cfg,p,hash) {
  marker <- file.path(p$remote,"UPLOAD_COMPLETE.md5")
  if (ap4_remote_exists(s,marker)) {
    if (!identical(ap4_exec(s,paste("cat",ap4_q(marker)))$text,hash))
      stop("This remote run has different inputs. Choose a new run_name.")
    cat("Identical model upload already verified.\n")
    return(invisible(NULL))
  }
  if (ap4_remote_exists(s,file.path(p$remote,"submission.lock")))
    stop("A submission exists without a verified upload marker; inspect this run before proceeding.")
  ap4_exec(s,paste("mkdir -p",ap4_q(p$remote)))
  remote_archive <- file.path(p$remote,basename(p$archive))
  existing <- ap4_exec(s,paste("md5sum",ap4_q(remote_archive)),FALSE)
  remote_hash <- if (existing$status==0L) strsplit(existing$text,"[[:space:]]+")[[1]][1] else ""
  if (!identical(remote_hash,hash)) {
    cat("Uploading AP4 package (about 702 MB)...\n")
    ap4_backend$upload(s,p$archive,p$remote)
  }
  got <- strsplit(ap4_exec(s,paste("md5sum",ap4_q(remote_archive)))$text,"[[:space:]]+")[[1]][1]
  if (!identical(got,hash)) stop("Uploaded archive checksum mismatch")
  ap4_exec(s,paste("unzip -oq",ap4_q(remote_archive),"-d",ap4_q(p$remote)))
  ap4_exec(s,paste("printf '%s\\n'",ap4_q(hash),">",ap4_q(marker)))
  cat("Model uploaded, extracted and checksum verified.\n")
}
ap4_make_job <- function(cfg,p) {
  # A marker is written only after MATLAB exits successfully, including comparisons.
  script <- c("#!/bin/bash -l", "set -eo pipefail",
    paste("cd",ap4_q(p$work)),
    paste("trap",ap4_q(paste("printf '%s\\n' 'MATLAB_OR_JOB_FAILED' >",ap4_q(file.path(p$remote,"AP4_FAILED.txt")))),"ERR"),
    paste("module load",ap4_q(cfg$matlab_module)), "module list",
    "matlab -batch \"run('START_AP4_ARC.m')\"",
    paste("printf '%s\\n' 'NATIVE_COMPARISON_PASSED' >",ap4_q(file.path(p$remote,"AP4_SUCCESS.txt"))))
  f <- file.path(p$logs,"run_ap4_controller.slurm")
  writeLines(script,f)
  f
}
ap4_submit <- function(s,cfg,p) {
  jobfile <- file.path(p$remote,"job_id.txt")
  if (ap4_remote_exists(s,jobfile)) {
    id <- ap4_exec(s,paste("cat",ap4_q(jobfile)))$text
    if (grepl("^[0-9]+$",id)) {cat("Resuming ARC job",id,"\n");return(id)}
  }
  lock <- file.path(p$remote,"submission.lock")
  if (ap4_remote_exists(s,lock))
    stop("Prior submission is uncertain. Inspect job_id.txt, submission_error.log and ARC queue; no duplicate job submitted.")
  script <- ap4_make_job(cfg,p)
  ap4_backend$upload(s,script,p$remote)
  args <- c("sbatch --parsable",paste0("--account=",ap4_q(cfg$account)),
    paste0("--partition=",ap4_q(cfg$partition)),"--nodes=1 --ntasks=1",
    paste0("--cpus-per-task=",as.integer(cfg$cpus)),paste0("--mem=",ap4_q(cfg$memory)),
    paste0("--time=",ap4_q(cfg$walltime)),paste0("--job-name=",ap4_q(cfg$run_name)),
    paste0("--output=",ap4_q(file.path(p$remote,"slurm-%j.log"))),
    ap4_q(file.path(p$remote,basename(script))))
  # Lock and job ID live remotely so a lost network reply cannot trigger a second job.
  command <- paste("mkdir",ap4_q(lock),"&&",paste(args,collapse=" "),
     ">",ap4_q(file.path(p$remote,"submission_response.txt")),"2>",ap4_q(file.path(p$remote,"submission_error.log")),
     "&& cut -d ';' -f1",ap4_q(file.path(p$remote,"submission_response.txt")),
     ">",ap4_q(jobfile),"&& cat",ap4_q(jobfile))
  z <- ap4_exec(s,command,FALSE)
  if (z$status!=0L || !grepl("^[0-9]+$",z$text)) {
    detail <- ap4_exec(s,paste("cat",ap4_q(file.path(p$remote,"submission_error.log"))),FALSE)$text
    writeLines(c(detail,z$error),file.path(p$logs,"submission_error.log"))
    stop("Submission failed or its outcome is uncertain. No automatic resubmission. ",detail,
         " Inspect the saved submission log and ARC queue.")
  }
  cat("Submitted MATLAB job",z$text,"\n")
  z$text
}
ap4_parse_accounting <- function(text,id) {
  lines <- strsplit(text,"\n",fixed=TRUE)[[1]]
  for (line in lines) {
    fields <- strsplit(line,"|",fixed=TRUE)[[1]]
    if (length(fields)>=3L && fields[1]==id)
      return(list(state=sub("[ +].*$","",fields[2]),exit_code=fields[3]))
  }
  list(state="ACCOUNTING_PENDING",exit_code="")
}
ap4_job_status <- function(s,id) {
  q <- ap4_exec(s,paste("squeue -h -j",id,"-o %T"),FALSE)
  if (q$status==0L && nzchar(q$text)) return(list(state=strsplit(q$text,"\n")[[1]][1],exit_code=""))
  a <- ap4_exec(s,paste("sacct -n -P -j",id,"--format=JobIDRaw,State,ExitCode"),FALSE)
  if (a$status!=0L) return(list(state="ACCOUNTING_PENDING",exit_code=""))
  ap4_parse_accounting(a$text,id)
}
ap4_download <- function(s,p) {
  dir.create(p$outputs,recursive=TRUE,showWarnings=FALSE)
  # Transfer available partial results and diagnostic logs even if MATLAB failed.
  result_dir <- file.path(p$analysis,"native_validation")
  roots <- c(p$remote,p$analysis,result_dir)
  for (root in roots) {
    found <- ap4_exec(s,paste("find",ap4_q(root),"-maxdepth 1 -type f"),FALSE)
    if (found$status!=0L || !nzchar(found$text)) next
    files <- strsplit(found$text,"\n",fixed=TRUE)[[1]]
    if (identical(root,result_dir)) destination <- p$outputs else {
      files <- files[grepl("\\.(log|txt|md5)$",files)]
      destination <- p$logs
    }
    for (f in files) ap4_backend$download(s,f,destination)
  }
  cat("Downloaded available results to",p$outputs,"\nLogs:",p$logs,"\n")
}
ap4_monitor <- function(s,cfg,p,id) {
  deadline <- Sys.time()+cfg$monitor_hours*3600
  previous <- ""
  active <- c("PENDING","RUNNING","CONFIGURING","COMPLETING","SUSPENDED",
              "RESIZING","REQUEUED","REQUEUE_FED","REQUEUE_HOLD","SIGNALING","STAGE_OUT","ACCOUNTING_PENDING")
  repeat {
    st <- ap4_job_status(s,id)
    if (!identical(st$state,previous)) {
      cat(format(Sys.time(),"%H:%M:%S"),"Job",id,st$state,"\n")
      previous <- st$state
    }
    if (!(st$state %in% active)) {
      ap4_download(s,p)
      if (st$state!="COMPLETED" || st$exit_code!="0:0" || !ap4_remote_exists(s,file.path(p$remote,"AP4_SUCCESS.txt")))
        stop("ARC job ended as ",st$state," (",st$exit_code,"). Inspect downloaded logs. This run will not be resubmitted automatically.")
      if (!file.exists(file.path(p$outputs,"native_results.mat")))
        stop("Job succeeded but native_results.mat was not downloaded; source again to retry retrieval.")
      cat("Native MATLAB coefficient comparisons passed. Identifier ordering still needs review.\n")
      return(invisible(st))
    }
    if (Sys.time()>=deadline) {
      cat("Monitoring time limit reached. The ARC job continues. Source this file again to resume.\n")
      return(invisible(st))
    }
    Sys.sleep(cfg$poll_seconds)
  }
}
ap4_run <- function(cfg=AP4_CONFIG) {
  if (!requireNamespace("ssh",quietly=TRUE)) stop('Install the same package used by the earlier controller: install.packages("ssh")')
  p <- ap4_paths(cfg)
  if (!file.exists(p$archive)) stop("Upload package is missing: ",p$archive)
  dir.create(p$logs,recursive=TRUE,showWarnings=FALSE)
  dir.create(p$outputs,recursive=TRUE,showWarnings=FALSE)
  cat("AP4 run:",cfg$run_name,"\nConnecting through R; complete any authentication prompt.\n")
  s <- ap4_backend$connect(cfg$host)
  on.exit(ap4_backend$disconnect(s),add=TRUE)
  cat("Connected to",ap4_exec(s,"hostname")$text,"\n")
  ap4_exec(s,"command -v sbatch >/dev/null && command -v unzip >/dev/null && command -v md5sum >/dev/null")
  hash <- ap4_hash(p$archive)
  saved <- file.path(p$logs,"controller_state.rds")
  if (file.exists(saved)) {
    previous <- readRDS(saved)
    if (!identical(previous$config,cfg) || !identical(previous$archive_md5,hash))
      stop("This run's settings or archive changed. Restore them to resume, or choose a new run_name.")
  }
  ap4_save(list(config=cfg,archive_md5=hash,job_id=NULL),p)
  ap4_prepare(s,cfg,p,hash)
  id <- ap4_submit(s,cfg,p)
  ap4_save(list(config=cfg,archive_md5=hash,job_id=id),p)
  ap4_monitor(s,cfg,p,id)
}

# Tests can set options(ap4.arc.autorun=FALSE) before sourcing definitions.
if (isTRUE(getOption("ap4.arc.autorun",TRUE))) ap4_run()
