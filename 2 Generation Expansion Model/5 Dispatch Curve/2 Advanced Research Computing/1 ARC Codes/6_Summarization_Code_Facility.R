# ───────────────────────────────────────────────────────────────────────────────
# 0) Setup ----------------------------------------------------------------------
# ───────────────────────────────────────────────────────────────────────────────

# Specify the existing library path
lib_path <- "/projects/epadecarb/2 Generation Expansion Model/1 Environment/env"

# Helper to load from that library
load_packages <- function(pkg, lib) {
  if (!require(pkg, character.only=TRUE, lib.loc=lib)) {
    stop(sprintf("Package '%s' not found in %s", pkg, lib))
  }
}

# Load required packages
pkgs <- c("httr","htmltools","jsonlite","data.table","lubridate","zoo")
invisible(lapply(pkgs, load_packages, lib_path))

# ───────────────────────────────────────────────────────────────────────────────
# 1) Static data ----------------------------------------------------------------
# ───────────────────────────────────────────────────────────────────────────────

# Fossil fuel facility metadata (for HI cap & static info)
fossil_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/Fossil_Fuel_Facilities_Data.csv"
Fossil_Fuels_NPC <- fread(fossil_path)[, .(
  Facility_Unit.ID,
  State,
  Fuel_type_1    = Primary_Fuel_Type,
  Fuel_type_2    = Secondary_Fuel_Type,
  latitude       = Latitude,
  longitude      = Longitude,
  Ramp_hr        = Ramp,
  Fossil.NPC_MW  = Estimated_NameplateCapacity_MW,
  Max_Hourly_HI_Rate
)]

# ───────────────────────────────────────────────────────────────────────────────
# 2) Output directories ---------------------------------------------------------
# ───────────────────────────────────────────────────────────────────────────────

summary_results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized"
final_results_path   <- file.path(summary_results_path, "Final")
if (!dir.exists(final_results_path)) {
  dir.create(final_results_path, recursive = TRUE)
}

# ───────────────────────────────────────────────────────────────────────────────
# 3) Skip already-processed simulation||pathway combos --------------------------
# ───────────────────────────────────────────────────────────────────────────────

get_existing_simulation_pathways <- function(pattern, path=summary_results_path) {
  files <- list.files(path, pattern=pattern, full.names=TRUE)
  keys <- character()
  for (f in files) {
    dt   <- fread(f, select=c("Simulation","Pathway"))
    keys <- unique(c(keys, paste(dt$Simulation, dt$Pathway, sep="||")))
  }
  keys
}

existing_sim_facility <- 
  get_existing_simulation_pathways("Yearly_Facility_Level_Results_Chunk_.*\\.csv")

# ───────────────────────────────────────────────────────────────────────────────
# 4) Gather & filter raw facility files ----------------------------------------
# ───────────────────────────────────────────────────────────────────────────────

data_path           <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/1 Stepwise"
all_csvs            <- list.files(data_path, "\\.csv$", full.names=TRUE)
facility_files_all  <- grep("Facility_Level_Results", all_csvs, value=TRUE)

filter_files_by_sim_pathway <- function(files, existing_keys, min_size=0) {
  keep <- character()
  for (f in files) {
    # optional size filter (set min_size to 0 to disable)
    if (min_size>0 && file.info(f)$size < min_size) next
    
    meta <- tryCatch(fread(f, nrows=1, select=c("Simulation","Pathway")),
                     error = function(e) NULL)
    if (is.null(meta) || nrow(meta)==0) next
    
    key <- paste(meta$Simulation[1], meta$Pathway[1], sep="||")
    if (!(key %in% existing_keys)) keep <- c(keep, f)
  }
  keep
}

# Filter out already-processed sims/pathways; keep anything ≥ 0 bytes
facility_files <- filter_files_by_sim_pathway(
  facility_files_all, existing_sim_facility, min_size = 0
)

# ───────────────────────────────────────────────────────────────────────────────
# 5) Split into manageable chunks ----------------------------------------------
# ───────────────────────────────────────────────────────────────────────────────

chunk_size       <- 200
facility_chunks  <- split(
  facility_files,
  ceiling(seq_along(facility_files)/chunk_size)
)

# ───────────────────────────────────────────────────────────────────────────────
# 6) Core processing function --------------------------------------------------
# ───────────────────────────────────────────────────────────────────────────────

process_chunk <- function(files, chunk_idx) {
  # fuel→code map as named character vector
  fuel_map <- c(
    "Pipeline Natural Gas" = "NG",
    "Other Oil"            = "DFO",
    "Diesel Oil"           = "DFO",
    "Residual Oil"         = "RFO",
    "Coal"                 = "BIT",
    "Wood"                 = "WC",
    "Tire Derived Fuel"    = "TDF"
  )
  
  yearly_list  <- vector("list", length(files))
  #monthly_list <- vector("list", length(files))
  
  for (i in seq_along(files)) {
    f <- files[i]
    message(sprintf("  • Loading %s", basename(f)))
    
    dt <- fread(
      f,
      select = c(
        "Facility_Unit.ID","Date","Gen_MWh_adj",
        "CO2_tons","NOx_lbs","SO2_lbs","HI_mmBtu",
        "Simulation","Pathway"
      ),
      fill = TRUE,
      blank.lines.skip = TRUE
    )
    
    # extract year/month
    # dt[, `:=`(
    #   Year  = year(Date),
    #   Month = month(Date)
    # )]
     dt[, `:=`(
       Year  = year(Date)
     )]
    
    # cap HI at the facility max
    dt <- merge(
      dt,
      Fossil_Fuels_NPC[, .(Facility_Unit.ID, Max_Hourly_HI_Rate)],
      by = "Facility_Unit.ID",
      all.x = TRUE
    )
    dt[HI_mmBtu > Max_Hourly_HI_Rate, HI_mmBtu := Max_Hourly_HI_Rate]
    
    # YEARLY summary
    yearly_list[[i]] <- dt[, .(
      total_generation_GWh = sum(Gen_MWh_adj, na.rm=TRUE)/1e3,
      total_CO2_tons       = sum(CO2_tons,   na.rm=TRUE),
      total_NOx_lbs        = sum(NOx_lbs,    na.rm=TRUE),
      total_SO2_lbs        = sum(SO2_lbs,    na.rm=TRUE),
      total_HI_mmBtu       = sum(HI_mmBtu,   na.rm=TRUE)
    ), by=.(Year, Pathway, Simulation, Facility_Unit.ID)]
    
    # MONTHLY summary (need Fuel_type_1 for ENERGY_SOURCE)
    # dt <- merge(
    #   dt,
    #   Fossil_Fuels_NPC[, .(Facility_Unit.ID, Fuel_type_1)],
    #   by = "Facility_Unit.ID",
    #   all.x = TRUE
    # )
    # monthly_list[[i]] <- dt[, .(
    #   total_generation_GWh = sum(Gen_MWh_adj, na.rm=TRUE)/1e3,
    #   total_CO2_tons       = sum(CO2_tons,   na.rm=TRUE),
    #   total_NOx_lbs        = sum(NOx_lbs,    na.rm=TRUE),
    #   total_SO2_lbs        = sum(SO2_lbs,    na.rm=TRUE),
    #   total_HI_mmBtu       = sum(HI_mmBtu,   na.rm=TRUE)
    # ), by=.(Year, Month, Pathway, Simulation, Facility_Unit.ID, Fuel_type_1)]
    
    rm(dt)
    gc()
  }
  
  # bind chunk
  yearly_chunk  <- rbindlist(yearly_list)
  #monthly_chunk <- rbindlist(monthly_list)
  
  # merge static facility info into YEARLY
  static_info <- unique(Fossil_Fuels_NPC[, .(
    Facility_Unit.ID, State, Fuel_type_1, Fuel_type_2,
    latitude, longitude, Ramp_hr, Fossil.NPC_MW
  )])
  yearly_chunk <- merge(
    yearly_chunk, static_info,
    by = "Facility_Unit.ID", all.x = TRUE
  )
  
  # map ENERGY_SOURCE on MONTHLY
  #monthly_chunk[, ENERGY_SOURCE := fuel_map[Fuel_type_1]]
  
  # write chunk files
  write.csv(
    yearly_chunk,
    file.path(
      summary_results_path,
      sprintf("Yearly_Facility_Level_Results_Chunk_%02d.csv", chunk_idx)
    ),
    row.names = FALSE
  )
  # write.csv(
  #   monthly_chunk,
  #   file.path(
  #     summary_results_path,
  #     sprintf("Monthly_Facility_Level_Results_Chunk_%02d.csv", chunk_idx)
  #   ),
  #   row.names = FALSE
  # )
  
  rm(yearly_list, yearly_chunk, static_info)
  gc()
}

# ───────────────────────────────────────────────────────────────────────────────
# 7) Run all chunks -------------------------------------------------------------
# ───────────────────────────────────────────────────────────────────────────────

for (j in seq_along(facility_chunks)) {
  message(sprintf("Processing chunk %d of %d …",
                  j, length(facility_chunks)))
  process_chunk(facility_chunks[[j]], j)
}

# ───────────────────────────────────────────────────────────────────────────────
# 8) Combine chunked summaries into final outputs -------------------------------
# ───────────────────────────────────────────────────────────────────────────────

# Yearly
yearly_files <- list.files(
  summary_results_path,
  pattern = "Yearly_Facility_Level_Results_Chunk_.*\\.csv$",
  full.names = TRUE
)
Yearly_Facility_Level_Results <- rbindlist(lapply(yearly_files, fread))
fwrite(
  Yearly_Facility_Level_Results,
  file.path(final_results_path, "Yearly_Facility_Level_Results.csv")
)

# # Monthly
# monthly_files <- list.files(
#   summary_results_path,
#   pattern = "Monthly_Facility_Level_Results_Chunk_.*\\.csv$",
#   full.names = TRUE
# )
# Monthly_Facility_Level_Results <- rbindlist(lapply(monthly_files, fread))
# fwrite(
#   Monthly_Facility_Level_Results,
#   file.path(final_results_path, "Monthly_Facility_Level_Results.csv")
# )

# ───────────────────────────────────────────────────────────────────────────────
# 9) Notify via Pushover --------------------------------------------------------
# ───────────────────────────────────────────────────────────────────────────────

pushover_user  <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
POST("https://api.pushover.net/1/messages.json",
     body = list(
       token   = pushover_token,
       user    = pushover_user,
       message = "The summary code has finished running."
     ),
     encode = "form")
