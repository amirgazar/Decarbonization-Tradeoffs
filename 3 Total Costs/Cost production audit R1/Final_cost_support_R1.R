# Purpose: Keep input and AP4 validation here; move publication table definitions beside the figure code.
# check annual inputs before costing because incomplete downloads can otherwise produce partial results.
r1_preflight <- function(final, expected) {
 suppressPackageStartupMessages(library(data.table))
 required <- c("Yearly_Results.csv","Yearly_Facility_Level_Results.csv","Yearly_Results_Shortages.csv","Coverage_and_accounting.csv","SUMMARY_COMPLETE.txt")
 absent <- required[!file.exists(file.path(final,required))]
 if(length(absent)) stop("R1 download incomplete: ",paste(absent,collapse=", "))
 paths <- c("A","B1","B2","B3","C1","C2","C3","D")
 y <- fread(file.path(final,required[1])); f <- fread(file.path(final,required[2]))
 s <- fread(file.path(final,required[3])); c <- fread(file.path(final,required[4]))
 check_grid <- function(x,keys,grid,label) {
  if(!all(keys %in% names(x))) stop(label,": missing keys")
  if(anyNA(x[,..keys]) || anyDuplicated(x,by=keys) ||
     nrow(x)!=nrow(grid) || nrow(fsetdiff(x[,..keys],grid)) ||
     nrow(fsetdiff(grid,x[,..keys]))) stop(label,": incomplete or duplicate simulation/pathway/year keys")
 }
 grid <- CJ(Simulation=as.integer(expected),Pathway=paths,Year=2025:2050)
 check_grid(y,c("Simulation","Pathway","Year"),grid,"Yearly results")
 check_grid(c,c("Simulation","Pathway"),CJ(Simulation=as.integer(expected),Pathway=paths),"Coverage")
 if(!nrow(s) || !all(c("Simulation","Pathway") %in% names(s)) ||
    any(!s$Simulation %in% expected) || any(!s$Pathway %in% paths)) stop("Invalid shortages scope")
 if(!all(c("Hours","Peak_balance_error") %in% names(c)) || anyNA(c) ||
    any(c$Hours!=227904) || any(!is.finite(c$Peak_balance_error)) ||
    any(c$Peak_balance_error<0 | c$Peak_balance_error>0.005001)) stop("Coverage/accounting check failed")
 key <- c("Simulation","Pathway","Year","Facility_Unit.ID")
 if(!all(key %in% names(f)) || anyNA(f[,..key]) || anyDuplicated(f,by=key)) stop("Invalid facility keys")
 fg <- unique(f[,.(Simulation,Pathway,Year)])
 check_grid(fg,c("Simulation","Pathway","Year"),grid,"Facility annual coverage")
 numeric_ok <- function(x) all(vapply(x[,names(x)[vapply(x,is.numeric,logical(1))],with=FALSE],function(v)all(is.finite(v)),logical(1)))
 if(!numeric_ok(y)||!numeric_ok(f)||!numeric_ok(s)) stop("Nonfinite annual inputs")
 mass <- c("total_generation_GWh","total_CO2_tons","total_NOx_lbs","total_SO2_lbs","total_HI_mmBtu")
 regional <- c("Old_Fossil_Fuels_adj_MWh","CO2_tons","NOx_lbs","SO2_lbs","HI_mmBtu")
 if(!all(mass %in% names(f)) || !all(regional %in% names(y))) stop("Missing activity/emission columns")
 if(any(as.matrix(f[,..mass])<0))stop("Negative activity/emissions")
 a <- f[,lapply(.SD,sum),by=.(Simulation,Pathway,Year),.SDcols=mass]
 a[,total_generation_GWh:=total_generation_GWh*1000]
 setnames(a,mass,paste0("facility_",regional))
 q <- merge(y,a,by=c("Simulation","Pathway","Year"),all=TRUE)
 for(k in setdiff(regional,"HI_mmBtu")) if(max(abs(q[[k]]-q[[paste0("facility_",k)]]))>1e-4)stop("Annual reconciliation failed: ",k)
 # the backup facility aggregator caps hourly heat input before summation, while regional heat is uncapped.
 # Keep both downloaded series and report the reduction because heat-based PM/VOC costs use the facility convention.
 provenance <- paste(readLines(file.path(final,"SUMMARY_COMPLETE.txt"),warn=FALSE),collapse=" ")
 heat_delta <- q$HI_mmBtu-q$facility_HI_mmBtu
 backup_cap <- grepl("Backup aggregation",provenance,fixed=TRUE) && grepl("Active backup formulas retained",provenance,fixed=TRUE)
 if(any(abs(heat_delta)>1e-4) && (!backup_cap || any(heat_delta < -1e-4)))stop("Unexplained facility/regional heat-input discrepancy")
 heat_check <- q[,.(Simulation,Pathway,Year,Regional_uncapped_HI_mmBtu=HI_mmBtu,Facility_capped_HI_mmBtu=facility_HI_mmBtu)]
 heat_check[,Reduction_mmBtu:=Regional_uncapped_HI_mmBtu-Facility_capped_HI_mmBtu]
 if(any(abs(heat_delta)>1e-4))cat("Recorded backup heat cap: maximum reduction",max(heat_delta),"MMBtu; maximum fraction",max(heat_delta/pmax(1,q$HI_mmBtu)),". Hourly cap reconstruction remains unverified.\n")
 balance_cols <- c("Clean_MWh","Old_Fossil_Fuels_adj_MWh","New_Fossil_Fuel_MWh","Calibrated_Total_import_net_MWh","Calibrated_Battery_discharge_grid","Calibrated_Shortage_MWh","Demand","Calibrated_Battery_charge_grid","Calibrated_Curtailments_MWh","Hours_present")
 if(!all(balance_cols %in% names(y)))stop("Missing annual energy balance columns")
 residual <- with(y,Clean_MWh+Old_Fossil_Fuels_adj_MWh+New_Fossil_Fuel_MWh+Calibrated_Total_import_net_MWh+Calibrated_Battery_discharge_grid+Calibrated_Shortage_MWh-Demand-Calibrated_Battery_charge_grid-Calibrated_Curtailments_MWh)
 if(any(y$Hours_present!=ifelse(y$Year%%4==0,8784,8760)) || any(abs(residual)>y$Hours_present*.005001))stop("Annual hours/balance check failed")
 cat("R1 preflight passed:",length(expected),"simulations;",nrow(y),"annual pathway rows.\n")
 cat("Annual checks do not certify hourly constraints. Summary provenance:\n",paste(readLines(file.path(final,"SUMMARY_COMPLETE.txt"),warn=FALSE),collapse="\n"),"\n")
 invisible(list(yearly=y,facility=f,coverage=c,heat_check=heat_check))
}

r1_apply_ap4 <- function(activity, root, output, cpi, mode="AP4_hybrid") {
 if(!mode %in% c("AP4_hybrid","AP4_common4","AP3_pipeline"))stop("Unknown R1 air valuation mode")
 native <- file.path(root,"3 Total Costs/5 Air Pollutant Emissions Costs/AP4_ARC/outputs/AP4_native_20260920_01/validated_tables")
 pol <- c("NOx","SO2","PM2.5","VOC")
 e <- fread(file.path(native,"egu_point_USD2020_per_metric_tonne.csv"),colClasses=c(orispl="character",fips="character"))
 c <- fread(file.path(native,"non_egu_point_USD2020_per_metric_tonne.csv"),colClasses=c(fips="character"))
 price <- cpi$value[as.Date(cpi$date)==as.Date("2024-01-01")]/cpi$value[as.Date(cpi$date)==as.Date("2020-07-01")]
 if(length(price)!=1 || !is.finite(price) || price<=0)stop("Missing AP4 CPI anchor")
 meta <- fread(file.path(root,"2 Generation Expansion Model/2 Generation/2 Fossil Generation/1 Existing Fossil Fuels/1 Fossil Fuels Facilities Data/Fossil_Fuel_Facilities_Data.csv"),select=c("Facility_Unit.ID","Facility_ID"))
 if(anyDuplicated(meta$Facility_Unit.ID))stop("Duplicate source metadata")
 map <- unique(activity[,.(Facility_Unit.ID,FIPS=sprintf("%05d",as.integer(GEOID)))])
 map[,Facility_ID:=as.character(meta$Facility_ID[match(Facility_Unit.ID,meta$Facility_Unit.ID)])]
 if(anyDuplicated(map$Facility_Unit.ID)||anyNA(map))stop("Ambiguous facility geography")
 count <- e[!is.na(orispl)&nzchar(orispl),.N,by=orispl]
 map[,AP4_matches:=count$N[match(Facility_ID,count$orispl)]]
 map[is.na(AP4_matches),AP4_matches:=0L]
 if(any(map$AP4_matches>1))stop("Ambiguous AP4 ORIS match; resolve explicitly")
 if(anyDuplicated(c$fips)||!all(map$FIPS %in% c$fips))stop("Missing/duplicate AP4 county proxy")
 map[,Mapping:=fifelse(AP4_matches==1,"EGU_exact_plant","county_nonEGU_proxy")]
 map[,Native_FIPS:=e$fips[match(Facility_ID,e$orispl)]]
 map[,County_mismatch:=!is.na(Native_FIPS)&FIPS!=Native_FIPS]
 for(p in pol) {
  v <- c[[p]][match(map$FIPS,c$fips)]
  exact <- map$AP4_matches==1
  v[exact] <- e[[p]][match(map$Facility_ID[exact],e$orispl)]
  if(any(!is.finite(v)|v<0))stop("Invalid AP4 rates: ",p)
  map[,(p):=v*price]
 }
 map[,`:=`(Rate_units="2024 USD per metric tonne",Valuation_mode=mode)]
 fwrite(map,file.path(output,"AP4_source_mapping_R1.csv"),na="")
 old <- unique(activity[,c("Facility_Unit.ID",pol,"PM10","CO"),with=FALSE])
 if(anyDuplicated(old$Facility_Unit.ID))stop("AP3 rates vary within a unit")
 fwrite(old,file.path(output,"AP3_reference_coefficients_R1.csv"))
 if(mode!="AP3_pipeline") {
  idx <- match(activity$Facility_Unit.ID,map$Facility_Unit.ID)
  for(p in pol)set(activity,j=p,value=map[[p]][idx])
  if(mode=="AP4_common4")activity[,`:=`(PM10=0,CO=0)]
 }
 # Native VSL; CPI convention matches the independently checked paired AP4 comparison.
 attr(activity,"AP4_VSL_R1") <- 9972805.091358194*price
 fwrite(data.table(Mode=mode,AP4_CPI_factor=price,AP4_VSL_USD2024=9972805.091358194*price,
  PM10_CO=if(mode=="AP4_common4")"Excluded in common-four sensitivity" else "AP3 PM10 because unavailable in AP4 tables; separate published CO cost range",
  NH3="AP4 coefficient available; NH3 mass is not modeled"),file.path(output,"AP4_valuation_settings_R1.csv"))
 activity
}

