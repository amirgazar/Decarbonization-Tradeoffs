# Generate shared-source ARC jobs, correct summary filenames and attach Pushover notifications.
# Usage: Rscript "4 RCode_and_BASH_Generator_R1.R" REPOSITORY NEW_BUNDLE_DIRECTORY
# This generates files locally. It does not connect to ARC or submit a job.
a<-commandArgs(TRUE);stopifnot(length(a)==2L)
root<-normalizePath(a[1]);out<-a[2];if(dir.exists(out))stop("Use a NEW bundle directory")
self<-normalizePath(gsub("~+~"," ",sub("^--file=","",grep("^--file=",commandArgs(),value=TRUE)[1]),fixed=TRUE))
here<-dirname(self);source(file.path(here,"1 ARC Settings_R1.R"))
stopifnot(length(arc$simulations)>0L,all(arc$simulations>0),!anyDuplicated(arc$simulations),arc$batch_size>=1L)
if(any(grepl("[\r\n]",unlist(arc))))stop("Configuration contains newline")
if(arc$hours==0L&&!nzchar(arc$memory))stop("Full-horizon ARC jobs require an explicit MEMORY setting in 1 ARC Control_R1.R")
dir.create(out,recursive=TRUE);dir.create(file.path(out,"jobs R1"))
for(f in c("1 ARC Settings_R1.R","2 Dispatch Curve_R1.R","8 ARC Notification_R1.R","6_Summarization_Code_Final_R1.R"))stopifnot(file.copy(file.path(here,f),out))
rels<-c("2 Generation Expansion Model/5 Dispatch Curve/dispatch_curve_base_v2.R",
 "2 Generation Expansion Model/7 Validation R1/run_dispatch_review_R1.R",
 "2 Generation Expansion Model/7 Validation R1/prepare_dispatch_inputs_R1.py")
for(rel in rels){dest<-file.path(out,basename(rel));dir.create(dirname(dest),recursive=TRUE,showWarnings=FALSE);stopifnot(file.copy(file.path(root,rel),dest))}
settings<-file.path(arc$code_root,"1 ARC Settings_R1.R")
q<-function(x)shQuote(x,type="sh")
header<-function(name)c("#!/bin/bash","# use the configured ARC resources so generated jobs match the selected run.","set -euo pipefail")
# Slurm directives must precede the first shell command.
slurm<-function(name)c("#!/bin/bash","# use the configured ARC resources so generated jobs match the selected run.",
 paste0("#SBATCH --job-name=",name),paste0("#SBATCH --account=",arc$account),paste0("#SBATCH --partition=",arc$partition),
 "#SBATCH --nodes=1","#SBATCH --ntasks=1",paste0("#SBATCH --cpus-per-task=",arc$cpus),paste0("#SBATCH --time=",arc$walltime),
 if(nzchar(arc$memory))paste0("#SBATCH --mem=",arc$memory),"#SBATCH --output=%x_%j.out","#SBATCH --error=%x_%j.err",
 "set -euo pipefail",paste("module load",q(arc$module)),"# Use allocated data.table CPUs and prevent nested BLAS threading.","export OMP_NUM_THREADS=${SLURM_CPUS_PER_TASK:-1} OPENBLAS_NUM_THREADS=1 MKL_NUM_THREADS=1")
container<-paste("apptainer exec --bind /projects --env",q(paste0("R_LIBS_USER=",arc$library)),q(arc$image),"Rscript")
notification_env<-if(is.null(arc$notification_env))"/projects/epadecarb/2 Generation Expansion Model/3 Datasets/API_KEYS.env" else arc$notification_env
notification_trap<-function(name){
 command<-paste("rc=$?; trap - EXIT; set +e;",container,q(file.path(arc$code_root,"8 ARC Notification_R1.R")),q(notification_env),q(name),'"$rc" "$SECONDS"',q(settings),'; exit "$rc"')
 paste("trap",q(command),"EXIT")
}
rows<-list();k<-0L
for(mode in "ensemble_R1"){
 ids<-arc$simulations
 groups<-split(ids,ceiling(seq_along(ids)/arc$batch_size))
 for(g in groups){
  name<-paste0(mode,"_",min(g),"_",max(g));rfile<-paste0(name,"_R1.R");shfile<-paste0(name,"_R1.sh")
  # Small explicit launchers replace regex-edited copies of the full model.
  launch<-c("# call the shared dispatch source so batch jobs use identical model code.",
   paste0("args <- c(",paste(vapply(c(file.path(arc$code_root,"2 Dispatch Curve_R1.R"),settings,mode,paste(g,collapse=",")),function(x)encodeString(x,quote='"'),character(1)),collapse=","),")"),
   "status <- system2(file.path(R.home('bin'),'Rscript'),vapply(args,shQuote,character(1)))", "if(status!=0L)quit(status=status)")
  writeLines(launch,file.path(out,"jobs R1",rfile))
  writeLines(c(slurm(name),notification_trap(name),paste("/usr/bin/time -v",container,q(file.path(arc$code_root,"jobs R1",rfile)))),file.path(out,"jobs R1",shfile))
  for(sim in g){k<-k+1L;rows[[k]]<-data.frame(Mode=mode,Simulation=sim,Job=name,Directory=paste0("sim_R1_",paste(g,collapse="_")))}
 }
}
write.csv(do.call(rbind,rows),file.path(out,"Jobs.csv"),row.names=FALSE)
prep<-paste("python3",q(file.path(arc$code_root,basename(rels[3]))),"--data-root",q(arc$data_root),"--output",q(file.path(arc$results_root,"random R1")),"--random-only --simulations",q(paste(arc$simulations,collapse=",")),if(arc$flat_data)"--flat-data" else "")
writeLines(c(slurm("prepare-random"),notification_trap("prepare-random"),prep),file.path(out,"1_Prepare_Random_R1.sh"))
for(mode in "ensemble_R1")writeLines(c(slurm(paste0("summary-",mode)),notification_trap(paste0("summary-",mode)),paste("/usr/bin/time -v",container,q(file.path(arc$code_root,"6_Summarization_Code_Final_R1.R")),q(settings),q(mode))),file.path(out,paste0("5_BASH_Summary_",mode,".sh")))
tracked<-list.files(out,recursive=TRUE,full.names=TRUE)
write.csv(data.frame(File=substring(tracked,nchar(out)+2L),MD5=unname(tools::md5sum(tracked))),file.path(out,"Bundle_checksums.csv"),row.names=FALSE)
cat("Generated bundle:",out,"\nNo files uploaded and no jobs submitted.\n")
