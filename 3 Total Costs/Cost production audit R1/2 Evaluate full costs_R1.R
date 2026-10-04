# Purpose: Describe the expanded coefficient sampling and remaining fixed assumptions accurately.
# distinguish completed dispatch from outstanding hourly/scientific checks.
# complete paired accounting and uncertainty checks; no IQR exclusions.
review_full_cost_report_R1 <- function(output_R1) {
 suppressPackageStartupMessages(library(data.table))
 components_R1 <- c('CAPEX','FOM','VOM','Fuel','Imports','GHG','Air_emissions','Unmet_demand','CAN_Imports_adj','CAN_CAPEX','CAN_FOM','CAN_VOM','CAN_CH4')
 rows_R1<-list();pairs_R1<-list();sd_R1<-list()
 for(folder_R1 in list.dirs(output_R1,recursive=FALSE,full.names=TRUE)) {
  if(!startsWith(basename(folder_R1),'discount_R1_'))next
  rate_R1<-as.numeric(sub('discount_R1_','',basename(folder_R1),fixed=TRUE))
  d<-fread(file.path(folder_R1,'Totals R1/All_Costs_per_Simulation.csv'))
  cols<-paste0(components_R1,'_mean_bUSD');stopifnot(all(cols %in% names(d)),!anyDuplicated(d,by=c('Simulation','Pathway')),!anyNA(d))
  err<-max(abs(rowSums(d[,..cols])-d$Total_Costs_mean_bUSD));stopifnot(err<1e-8)
  ids<-unique(d[Pathway=='B1',Simulation]);stopifnot(all(d[,.(OK=setequal(Simulation,ids)),by=Pathway]$OK))
  summary<-d[,.(N=.N,Mean=mean(Total_Costs_mean_bUSD),SD=sd(Total_Costs_mean_bUSD),P05=quantile(Total_Costs_mean_bUSD,.05),P95=quantile(Total_Costs_mean_bUSD,.95)),by=Pathway];summary[,Rate:=rate_R1];rows_R1[[length(rows_R1)+1L]]<-summary
  b<-d[Pathway=='B1',.(Simulation,B1=Total_Costs_mean_bUSD)]
  a<-merge(d,b,by='Simulation');a[,Difference:=Total_Costs_mean_bUSD-B1]
  ps<-a[,.(N=.N,Mean=mean(Difference),SD=sd(Difference),P05=quantile(Difference,.05),P95=quantile(Difference,.95),Lower_than_B1=sum(Difference<0),Independent_SD=sqrt(var(Total_Costs_mean_bUSD)+var(B1)),Correlation_with_B1=cor(Total_Costs_mean_bUSD,B1)),by=Pathway]
  ps[Pathway=='B1',`:=`(Independent_SD=NA_real_,Correlation_with_B1=1)];ps[,Rate:=rate_R1];pairs_R1[[length(pairs_R1)+1L]]<-ps
  long<-melt(d,id.vars=c('Simulation','Pathway'),measure.vars=cols,variable.name='Component',value.name='Cost');long[,Component:=sub('_mean_bUSD','',Component,fixed=TRUE)]
  ss<-long[,.(N=.N,Mean=mean(Cost),SD=sd(Cost)),by=.(Pathway,Component)];ss[,Rate:=rate_R1];sd_R1[[length(sd_R1)+1L]]<-ss
  can<-fread(file.path(folder_R1,'Components R1/CAPEX_FOM_CAN_Hydro.csv'));ch4<-fread(file.path(folder_R1,'Components R1/CH4_CAN_Hydro.csv'))
  stopifnot(nrow(can)==2*length(ids),setequal(can$Simulation,ids),nrow(ch4)==length(ids),setequal(ch4$Simulation,ids))
  cat('Verified',rate_R1,':',nrow(d),'cost presentations, maximum additive error',err,'billion USD\n')
 }
 fwrite(rbindlist(rows_R1),file.path(output_R1,'Total_cost_summary_R1.csv'));fwrite(rbindlist(pairs_R1),file.path(output_R1,'Paired_B1_summary_R1.csv'));fwrite(rbindlist(sd_R1),file.path(output_R1,'Component_uncertainty_R1.csv'))
 writeLines(c('# Full production-module cost evaluation R1','',
 sprintf('All 13 cost modules executed at rates %s using %d complete downloaded dispatch simulations and the matching GHG valuation schedules.',paste(sort(unique(rbindlist(rows_R1)$Rate)),collapse=', '),length(ids)),
 'Revised code retains Simulation through Canadian cost aggregation and joins. The geography lookup follows the supplied facility metadata for all represented units.',
 'Canadian capacity/allocation and coefficient endpoints remain unchanged; documented coefficient ranges are now sampled with common draws.',
 'CH4-C to CH4 conversion by 16/12 was already present in module 12 and is retained, not claimed as a new correction.',
 'The tables report empirical simulation ranges. They do not quantify demand, fossil-fuel-price, AP4 coefficient, weather-dependence, pollution-control or other omitted uncertainty.',
 'The dispatch ensemble was supplied by the author. Annual cost checks do not certify hourly validation or resolve omitted uncertainty. No ARC jobs are submitted.'),file.path(output_R1,'Evaluation.md'))
}
