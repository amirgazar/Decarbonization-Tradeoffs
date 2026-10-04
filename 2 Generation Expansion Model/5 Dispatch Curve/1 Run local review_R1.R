# RStudio entry for calibration, exact-generation replay and fresh local dispatch.
# 1. SETTINGS: open this file in RStudio, choose ACTION, then click Source.
REPOSITORY <- Sys.getenv("PHASED_R1_ROOT", unset=getwd())
AUTHORITATIVE_BACKUP <- Sys.getenv("PHASED_DATA_ROOT", unset=Sys.getenv("PHASED_R1_ROOT", unset=getwd()))
ACTION <- 'test' # 'replay', 'calibrate', or 'test'. Replay uses saved 2025/2050 unit outputs.
HOURS <- 2000L
START_YEAR <- 2050L
PATHWAYS <- c('A','B1','B2','B3','C1','C2','C3','D')
SAVED_RUNS <- NULL # Optional vector of existing 'Dispatch R1' directories for replay.
RANDOM_FILE <- Sys.getenv('PHASED_RANDOM_FILE', unset=file.path(AUTHORITATIVE_BACKUP,'2 Generation Expansion Model/4 Randomization/1 Randomized Data/Random_Sequence.csv')) # Only needed for ACTION='test'; leave blank to reuse the saved simulation-1 column.

# 2. RUN: fit once; each simulation only evaluates the saved coefficients.
run_local_R1v2 <- function(){
 stopifnot(ACTION%in%c('replay','calibrate','test'),HOURS>0,HOURS<=8760)
 dispatch<-file.path(REPOSITORY,'2 Generation Expansion Model/5 Dispatch Curve')
 support<-file.path(dispatch,'2 Advanced Research Computing/1 ARC Codes/Support R1')
 validation<-file.path(REPOSITORY,'2 Generation Expansion Model/7 Validation R1')
 artifact<-file.path(REPOSITORY,'2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Operating_emission_models_R1.rds')
 stamp<-paste0('R1_',ACTION,'_',format(Sys.time(),'%Y%m%d_%H%M%S'))
 out<-file.path(dispatch,'1 Test Results R1',stamp)
 dir.create(out,recursive=TRUE,showWarnings=FALSE)
 launch<-function(args){status<-system2(file.path(R.home('bin'),'Rscript'),vapply(as.character(args),shQuote,character(1)));if(status!=0L)stop('Failed; retain output in ',out)}
 if(ACTION=='calibrate'){
  cal<-file.path(out,'Calibration R1.v2')
  launch(c(file.path(support,'0 Calibrate Operating Emissions_R1.R'),REPOSITORY,cal))
  if(file.exists(artifact))stopifnot(file.copy(artifact,file.path(out,'Previous_Operating_emission_models_R1.rds')))
  stopifnot(file.copy(file.path(cal,basename(artifact)),artifact,overwrite=TRUE))
  cat('Calibration complete. Review Heldout_summary_R1.v2.csv and Model_selection_R1.v2.csv in ',cal,'\n',sep='')
  return(invisible(out))
 }
 if(!file.exists(artifact))stop('Run ACTION="calibrate" first')
 launch(c(file.path(validation,'Test operating emission models_R1.R'),file.path(support,'Operating emission models_R1.R'),file.path(out,'Unit_checks_R1.v2.txt'),artifact))
 if(ACTION=='replay'){
  args<-c(file.path(validation,'Compare saved dispatch_R1.R'),REPOSITORY,file.path(dispatch,'dispatch_curve_base_v2.R'),file.path(support,'Operating emission models_R1.R'),artifact,file.path(out,'Replay R1.v2'))
  if(!is.null(SAVED_RUNS))args<-c(args,paste(SAVED_RUNS,collapse='|'))
  launch(args)
  cat('Replay complete. This applies the new emissions calculation to saved final unit generation; it does not rerun availability or dispatch.\n')
 }else{
  # Full generation-percentile CSV is needed only for a fresh dispatch, not for calibration or replay.
  fossil_rel<-'2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/2 Fossil Fuels Generation and Emissions/Fossil_Fuel_Generation_Emissions.csv'
  if(!any(file.exists(file.path(c(AUTHORITATIVE_BACKUP,REPOSITORY,Sys.getenv('PHASED_REFERENCE_ROOT')),fossil_rel))))stop('Restore Fossil_Fuel_Generation_Emissions.csv under REPOSITORY or AUTHORITATIVE_BACKUP for a fresh local dispatch. ACTION="replay" works without it.')
  suppressPackageStartupMessages(library(data.table));extract<-file.path(out,'Inputs R1.v2');dir.create(extract)
  random<-RANDOM_FILE
  if(!nzchar(random)){
   candidates<-list.files(file.path(dispatch,'1 Test Results R1'),pattern='^Random_Sequence[.]csv$',recursive=TRUE,full.names=TRUE)
   if(!length(candidates))stop('Set RANDOM_FILE to the saved random-sequence CSV')
   random<-sort(candidates)[1]
  }
  z<-fread(random,select=1L);setnames(z,'V1');stopifnot(nrow(z)>0,all(z$V1%in%1:99));fwrite(z,file.path(extract,'Random_Sequence.csv'))
  fwrite(data.table(File=normalizePath(random),MD5=unname(tools::md5sum(random))),file.path(extract,'Random_source_R1.v2.csv'))
  data_root<-if(dir.exists(AUTHORITATIVE_BACKUP))AUTHORITATIVE_BACKUP else REPOSITORY
  launch(c(file.path(validation,'run_dispatch_review_R1.R'),REPOSITORY,data_root,extract,file.path(out,'Dispatch R1.v2'),HOURS,1,'fresh',START_YEAR,paste(PATHWAYS,collapse=',')))
  source(file.path(REPOSITORY,'2 Generation Expansion Model/7 Validation R1/Validate_local_R1.R'),local=TRUE)
  validate_local_R1(file.path(out,'Dispatch R1.v2'))
 }
 cat('Output folder: ',out,'\n',sep='');invisible(out)
}
if(!isTRUE(getOption('phased.r1v2.no_autorun',FALSE)))run_local_R1v2()
