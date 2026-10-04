# Purpose: Include the cost-sampling helper in run provenance; preserve final 1000-run AP4 defaults.
# run components with shared inputs and coefficient draws so pathway differences remain paired.
# 1. In RStudio edit ROOT_R1 / FINAL_R1 if needed, then Source this file. No Terminal or ARC rerun is needed.
ROOT_R1 <- Sys.getenv("PHASED_R1_ROOT", unset=getwd())
if (!file.exists(file.path(ROOT_R1, "README.md"))) stop("Set PHASED_R1_ROOT to the repository root")
COST_REVIEW_HOME_R1 <- Sys.getenv('PHASED_COST_REVIEW_HOME_R1',unset=file.path(ROOT_R1,'7 Reproduction Information Document/Cost production audit R1'))
FINAL_R1 <- Sys.getenv('PHASED_FINAL_R1',unset=file.path(ROOT_R1,'2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/Downloads R1/R1_ensemble101_1000_20260920_01/Final'))
EXPECTED_SIMULATIONS_R1 <- 1:1000
AIR_MODEL_R1 <- Sys.getenv('PHASED_AIR_MODEL_R1',unset='AP4_hybrid')
source(file.path(COST_REVIEW_HOME_R1,'Final_cost_support_R1.R'))
# load the shared table exporter so cost runs and figure preparation use the same definitions.
source(file.path(ROOT_R1,'6 Figures/Publication revision R1/Publication_tables_R1.R'))
RATES_R1 <- c(0.015,0.02,0.025)
suppressPackageStartupMessages({library(data.table);library(dplyr);library(tidyr);library(lubridate);library(readxl)})
setDTthreads(4)
review_run_costs <- function(
  review_root=Sys.getenv("PHASED_R1_ROOT", unset=getwd()),
  review_backup=Sys.getenv("PHASED_DATA_ROOT", unset=Sys.getenv("PHASED_R1_ROOT", unset=getwd())),
  review_run='R1_final1000_AP4', review_final_override=NULL, review_rates=c(0.015,0.02,0.025), review_resume=NULL) {
if(is.null(review_final_override)) stop('R1: provide an explicit completed Final folder')
# validate before creating output or running a cost module.
review_checked <- r1_preflight(review_final_override,EXPECTED_SIMULATIONS_R1)
review_original <- '__PROJECT_ROOT__'
review_home <- file.path(review_root,'3 Total Costs/0 Pilot Cost Sanity Checks R1')
review_cpi <- file.path(review_home,'Inputs R1/CPIAUCSL_2000_to_2024-01-01.rds')
review_final <- file.path(review_root,'2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes/Downloads R1',review_run,'pilot_R1/Final R1')
if (!is.null(review_final_override)) review_final <- normalizePath(review_final_override, mustWork=TRUE)
review_out <- if(is.null(review_resume)) file.path(COST_REVIEW_HOME_R1,'Results R1',paste0(review_run,'_',format(Sys.time(),'%Y%m%d_%H%M%S'))) else normalizePath(review_resume,mustWork=TRUE)
# resume only steps whose saved source and output hashes still match the recorded checkpoint.
review_done <- if(is.null(review_resume)) data.table() else fread(file.path(review_out,'Resume_steps_R1.csv'))
if(nrow(review_done)) for(k in seq_len(nrow(review_done))) {
 stopifnot(file.exists(review_done$File[k]),unname(tools::md5sum(review_done$File[k]))==review_done$MD5[k])
}
dir.create(review_out,recursive=TRUE,showWarnings=FALSE)
# Print warnings when they occur so the saved log includes them.
review_old_options<-options(warn=1, phased.r1.cost_runner=TRUE, phased.r1.sample_cost_ranges=TRUE, phased.r1.air_model=AIR_MODEL_R1);on.exit(options(review_old_options),add=TRUE)
review_log <- file(file.path(review_out,'execution.log'),open=if(is.null(review_resume))'wt' else 'at')
sink(review_log,split=TRUE);sink(review_log,type='message');on.exit({sink(type='message');sink();close(review_log)},add=TRUE)
cat('R1 final-ensemble cost calculation: ',review_run,'\nResults: ',review_out,'\n')
# 1. Validate full pilot scope and attach geography only; no archived energy is used.
# reuse the checked tables because the complete facility file has 17.54 million rows.
review_y <- review_checked$yearly
review_f <- review_checked$facility
review_c <- review_checked$coverage
fwrite(review_checked$heat_check,file.path(review_out,'Heat_input_aggregation_R1.csv'))
rm(review_checked)
review_paths <- c('A','B1','B2','B3','C1','C2','C3','D')
stopifnot(setequal(review_y$Pathway,review_paths),setequal(unique(review_y$Simulation),EXPECTED_SIMULATIONS_R1),
 nrow(review_y)==208L*length(EXPECTED_SIMULATIONS_R1),!anyDuplicated(review_y,by=c('Simulation','Pathway','Year')),
 all(review_y$Year %in% 2025:2050),all(review_c$Hours==227904),all(review_c$Peak_balance_error<=0.005001))
review_county <- fread(file.path(COST_REVIEW_HOME_R1,'Inputs R1/Unit_county_lookup_R1.csv'))
stopifnot(!anyDuplicated(review_county$Facility_Unit.ID),all(review_f$Facility_Unit.ID %in% review_county$Facility_Unit.ID))
review_f <- merge(review_f,review_county[,.(Facility_Unit.ID,GEOID,County)],by='Facility_Unit.ID',all.x=TRUE)
stopifnot(!anyNA(review_f))
if(is.null(review_resume))fwrite(review_f,file.path(review_out,'Yearly_Facility_Level_Results_County_added_in.csv'))
fwrite(review_c,file.path(review_out,'Pilot_coverage.csv'))
# release the full facility copy before component execution to bound memory use.
rm(review_f,review_y,review_c);gc()
review_timing <- list();review_manifest<-list()
for(review_rate in review_rates) {
review_folder <- file.path(review_out,paste0('discount_R1_',review_rate));dir.create(review_folder,showWarnings=FALSE)
review_stage <- file.path(review_folder,'Sources R1');dir.create(review_stage,showWarnings=FALSE)
review_components <- file.path(review_folder,'Components R1');dir.create(review_components,showWarnings=FALSE)
review_totals <- file.path(review_folder,'Totals R1');dir.create(review_totals,showWarnings=FALSE)
review_replace <- function(x,a,b) gsub(a,b,x,fixed=TRUE)
review_files <- list.files(file.path(review_root,'3 Total Costs'),pattern='\\.R$',recursive=TRUE,full.names=TRUE)
review_files <- review_files[!grepl('/0 Pilot Cost Sanity Checks R1/',review_files,fixed=TRUE)]
for(review_i in 1:13){
 review_file <- review_files[grepl(paste0('^',review_i,'_'),basename(review_files))]; if(any(grepl('_R1[.]R$',review_file))) review_file <- review_file[grepl('_R1[.]R$',review_file)]; stopifnot(length(review_file)==1)
 if(nrow(review_done) && any(review_done$Rate==review_rate & review_done$Step==review_i & review_done$File==review_file)) {
  cat('Reusing completed step',review_i,'at',review_rate,'after hash checks.\n');next
 }
 # Corrected modules 12 and 13 now live in the standard cost folders.
 review_lines <- readLines(review_file,warn=FALSE)
 review_lines <- review_lines[!grepl('API KEY|fredr_set_key',review_lines)]
 if(review_i==1)review_lines<-review_lines[seq_len(grep('write.csv\\(combined_npvs_all',review_lines)[1])]
 if(review_i==2)review_lines<-review_lines[seq_len(grep('write.csv\\(combined_npvs,',review_lines)[1])]
 if(review_i==13)review_lines<-review_lines[seq_len(grep('write.csv\\(All_Costs_final,',review_lines)[1])]
 review_text <- paste(review_lines,collapse='\n')
 # Paths are explicit and isolated. The archived county file supplies geography only.
 review_text <- review_replace(review_text,paste0(review_original,'/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results/Yearly_Facility_Level_Results_County_added_in.csv'),file.path(review_out,'Yearly_Facility_Level_Results_County_added_in.csv'))
 review_text <- review_replace(review_text,paste0(review_original,'/2 Generation Expansion Model/5 Dispatch Curve/4 Final Results/1 Comprehensive Days Summary Results'),review_final)
 review_text <- review_replace(review_text,paste0(review_original,'/3 Total Costs/9 Total Costs Results'),review_components)
 review_text <- review_replace(review_text,paste0(review_original,'/4 External Data/NREL ATB/ATBe_2024.csv'),file.path(COST_REVIEW_HOME_R1,'Inputs R1/ATB_2024_numeric_R1.csv'))
 review_text <- review_replace(review_text,paste0(review_original,"/"),paste0(review_root,"/"))
 review_text <- gsub('(?m)^cpi_data <- fredr\\([^\n]*',paste0('cpi_data <- readRDS(',dQuote(review_cpi,q=FALSE),')'),review_text,perl=TRUE)
 review_text <- gsub('(?m)^discount_rate <- [0-9.]+',paste0('discount_rate <- ',review_rate),review_text,perl=TRUE)
 endpoints <- switch(as.character(review_rate),'0.02'=c(212,2025,60267,308,4231,92996),'0.025'=c(130,1590,39972,205,3547,65635),'0.015'=c(360,2737,95210,482,5260,136799))
 stopifnot(length(endpoints)==6)
 for(j in 1:2) review_text <- gsub(paste0('(?m)^costs_',c(2025,2050)[j],' <- list[^\n]*'),sprintf('costs_%d <- list(CO2=%s*conversion_rate_2020_24,CH4=%s*conversion_rate_2020_24,N2O=%s*conversion_rate_2020_24)',c(2025,2050)[j],endpoints[3*j-2],endpoints[3*j-1],endpoints[3*j]),review_text,perl=TRUE)
 if(review_i==5)review_text<-review_replace(review_text,'detectCores()', '2L')
 review_input_paths <- regmatches(review_text,gregexpr('"/[^"\\n]+"',review_text))[[1]]
 review_input_paths <- gsub('^"|"$','',review_input_paths)
 review_input_paths <- review_input_paths[file.exists(review_input_paths) & !dir.exists(review_input_paths) & !startsWith(review_input_paths,review_out)]
 review_manifest[[length(review_manifest)+1L]]<-data.table(Rate=review_rate,Source=review_input_paths,MD5=unname(tools::md5sum(review_input_paths)))
 review_target <- file.path(review_stage,if(grepl('_R1[.]R$',basename(review_file))) basename(review_file) else sub('[.]R$','_R1.R',basename(review_file)))
 cat('\nSTEP',review_i,basename(review_file),'\n');flush.console()
 review_start<-proc.time()[3]
 Sys.setenv(PHASED_COST_INPUT_DIR=review_components,PHASED_COST_OUTPUT_DIR=review_totals)
 review_env<-new.env(parent=globalenv());review_env$review_eval_env<-review_env
 # pass the project root so the AP4 helper can locate validated coefficients.
 review_env$review_root<-review_root
 review_text<-review_replace(review_text,'assign(name, dt, envir = .GlobalEnv)','assign(name, dt, envir = review_eval_env)')
 writeLines(c('# bind run inputs, valuation settings and outputs here because component files retain original paths.',review_text),review_target)
 # use a fresh R process for each component because retained package/global objects exhausted memory on the third full-facility pass.
 review_child <- tempfile(fileext='.R')
 review_child_lines <- c(
  sprintf('options(phased.r1.cost_runner=TRUE,phased.r1.sample_cost_ranges=TRUE,phased.r1.air_model=%s)',dQuote(AIR_MODEL_R1,q=FALSE)),
  'data.table::setDTthreads(4L)',
  paste0('review_root <- ',dQuote(review_root,q=FALSE)),
  'review_eval_env <- globalenv()',
  paste0('source(',dQuote(file.path(COST_REVIEW_HOME_R1,'Final_cost_support_R1.R'),q=FALSE),')'),
  paste0('source(',dQuote(review_target,q=FALSE),')'))
 if(review_i!=13) review_child_lines <- c(review_child_lines,
  'audit_names <- intersect(c("mean_ratios","mean_ratios_USA","Fossil_Fuels_NPC_new","selected_cols","Yearly_Results","BFL_costs_pathway_B3","Hydropower"),ls(globalenv()))',
  paste0('saveRDS(mget(audit_names,envir=globalenv()),',dQuote(file.path(review_stage,paste0(review_i,'_audit.rds')),q=FALSE),')'))
 writeLines(review_child_lines,review_child)
 review_status <- system2(file.path(R.home('bin'),'Rscript'),shQuote(review_child))
 unlink(review_child)
 if(review_status!=0)stop('Component ',review_i,' failed at rate ',review_rate,'; completed steps remain in ',review_out)
 review_elapsed<-unname(proc.time()[3]-review_start)
 review_timing[[length(review_timing)+1L]]<-data.table(Rate=review_rate,Step=review_i,Seconds=review_elapsed)
 review_manifest[[length(review_manifest)+1L]]<-data.table(Rate=review_rate,Source=review_file,MD5=unname(tools::md5sum(review_file)))
 cat('Elapsed seconds:',review_elapsed,'\n')
 # release each isolated component environment before reading the next full facility table.
 rm(review_env);gc()
}

review_x <- fread(file.path(review_totals,'All_Costs_per_Simulation.csv'))
stopifnot(nrow(review_x)==9L*length(EXPECTED_SIMULATIONS_R1),uniqueN(review_x$Pathway)==9L,!anyNA(review_x))
review_numeric<-names(review_x)[vapply(review_x,is.numeric,logical(1))]
stopifnot(all(vapply(review_x[,..review_numeric],function(x)all(is.finite(x)),logical(1))))
# Raw particulate assumptions and Canada investment timing remain review items.
cat('PASS: all nine cost presentations are complete and finite. Scientific review remains required.\n')
}
fwrite(rbindlist(review_timing),file.path(review_out,'Execution_times.csv'))
fwrite(rbindlist(review_manifest),file.path(review_out,'Cost_source_manifest.csv'))
# include helper, native rates and selected valuation mode in provenance.
fwrite(data.table(Air_model=AIR_MODEL_R1,Cost_sampling="bounded_coefficients_v1",ATB_price_basis="2022_to_January2024_CPI",Expected_simulations=length(EXPECTED_SIMULATIONS_R1),Rates=paste(review_rates,collapse=";"),Final=review_final),file.path(review_out,'Run_settings_R1.csv'))
review_inputs<-c(file.path(review_root,"3 Total Costs/8 Total Costs/Cost_coefficient_sampling_R1.R"),file.path(ROOT_R1,'6 Figures/Publication revision R1/Publication_tables_R1.R'),
file.path(COST_REVIEW_HOME_R1,'Final_cost_support_R1.R'),
file.path(review_root,'3 Total Costs/5 Air Pollutant Emissions Costs/AP4_ARC/outputs/AP4_native_20260920_01/validated_tables',c('egu_point_USD2020_per_metric_tonne.csv','non_egu_point_USD2020_per_metric_tonne.csv')),
list.files(review_final,full.names=TRUE),review_cpi,file.path(COST_REVIEW_HOME_R1,'Inputs R1/Unit_county_lookup_R1.csv'),file.path(COST_REVIEW_HOME_R1,'Inputs R1/ATB_2024_numeric_R1.csv'))
fwrite(data.table(File=review_inputs,MD5=unname(tools::md5sum(review_inputs))),file.path(review_out,'Pilot_input_manifest.csv'))
review_analysis_env<-new.env(parent=globalenv());sys.source(file.path(review_home,'2 Evaluate costs_R1.R'),envir=review_analysis_env)
review_analysis_env$review_validate_pilot(review_out,review_final,review_rates)
source(file.path(COST_REVIEW_HOME_R1,"2 Evaluate full costs_R1.R"),local=TRUE)
review_full_cost_report_R1(review_out)
r1_export_tables(review_out)
writeLines(c(paste('R1 COST CALCULATIONS AND ANNUAL CHECKS COMPLETED;',AIR_MODEL_R1,'; SCIENTIFIC AND HOURLY VALIDATION ARE SEPARATE.'),
paste(length(EXPECTED_SIMULATIONS_R1),'paired dispatch simulations. Review corrected Canada accounting and metadata geography; scientific parameter decisions remain pending.')),file.path(review_out,'COMPLETE.txt'))
cat('Completed. Open ',file.path(review_out,'Evaluation.md'),'\n',sep='')
invisible(review_out)
}

# 2. Preserve the already audited numeric ATB source; required cost cells must be finite.
if(!isTRUE(getOption('phased.r1.load_only',FALSE))) {
 # check required files here; the complete data checks run once inside review_run_costs.
 stopifnot(all(file.exists(file.path(FINAL_R1,c('Yearly_Results.csv','Yearly_Facility_Level_Results.csv','Coverage_and_accounting.csv','Yearly_Results_Shortages.csv','SUMMARY_COMPLETE.txt')))))
 dir.create(file.path(COST_REVIEW_HOME_R1,'Inputs R1'),recursive=TRUE,showWarnings=FALSE)
 source_atb_R1 <- file.path(ROOT_R1,'4 External Data/NREL ATB/ATBe_2024.csv')
 # use the author-supplied project ATB file because the previous runner read a different checkout; record its hash below.
 stopifnot(file.exists(source_atb_R1))
 atb_R1 <- fread(source_atb_R1);atb_R1[,value:=suppressWarnings(as.numeric(value))]
 # the 2024 ATB is in 2022 dollars; convert monetary coefficients to the same retained January-2024 CPI basis used elsewhere.
 cpi_atb_R1 <- as.data.table(readRDS(file.path(ROOT_R1,'3 Total Costs/0 Pilot Cost Sanity Checks R1/Inputs R1/CPIAUCSL_2000_to_2024-01-01.rds')))
 atb_price_factor_R1 <- mean(cpi_atb_R1[year(date)==2024,value])/mean(cpi_atb_R1[year(date)==2022,value])
 stopifnot(is.finite(atb_price_factor_R1),atb_price_factor_R1>1,atb_price_factor_R1<1.2)
 atb_R1[core_metric_parameter %in% c('CAPEX','Fixed O&M','Variable O&M','Fuel'),value:=value*atb_price_factor_R1]
 stopifnot(!anyNA(atb_R1[core_metric_parameter %in% c('CAPEX','Fixed O&M','Variable O&M','Fuel'),value]))
 fwrite(atb_R1,file.path(COST_REVIEW_HOME_R1,'Inputs R1/ATB_2024_numeric_R1.csv'))
 fwrite(data.table(File=source_atb_R1,MD5=unname(tools::md5sum(source_atb_R1)),Transformation='Numeric conversion and 2022-to-January-2024 CPI conversion for monetary coefficients',Source_dollar_year=2022,Factor=atb_price_factor_R1,Source_documentation='https://atb.nrel.gov/electricity/2024/index'),file.path(COST_REVIEW_HOME_R1,'Inputs R1/ATB_provenance_R1.csv'))
 # 3. Execute all 13 production cost modules at each rate with the matching GHG schedule.
 resume_R1 <- Sys.getenv('PHASED_COST_RESUME_R1',unset='')
 result_R1 <- review_run_costs(review_root=ROOT_R1,review_final_override=FINAL_R1,review_rates=RATES_R1,review_resume=if(nzchar(resume_R1))resume_R1 else NULL)
 writeLines(result_R1,file.path(COST_REVIEW_HOME_R1,'Last_output_R1.txt'))
}
