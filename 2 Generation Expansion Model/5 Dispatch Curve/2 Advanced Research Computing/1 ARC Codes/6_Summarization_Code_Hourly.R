# Specify the existing library path
lib_path <- "/projects/epadecarb/2 Generation Expansion Model/1 Environment/env"

# Function to load packages from that library
load_packages <- function(package, lib) {
  if (!require(package, character.only = TRUE, lib.loc = lib)) {
    stop(paste("Package", package, "is not available in the library path:", lib))
  }
}

# Load required packages
packages <- c("httr", "htmltools", "jsonlite", "data.table", "lubridate", "zoo")
invisible(lapply(packages, load_packages, lib = lib_path))

# Load EVOLL cost curve
evoll_path <- "/projects/epadecarb/2 Generation Expansion Model/3 Datasets/evoll_curve.csv"
Cost_Curve <- fread(evoll_path)

# Directory for summary outputs
summary_results_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/2 Stepwise Summarized/"

# Helper: get already-processed Simulation||Pathway keys
get_existing_simulation_pathways <- function(pattern, path = summary_results_path) {
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  keys <- character()
  if (length(files) > 0) {
    for (file in files) {
      dt <- fread(file, select = c("Simulation", "Pathway"))
      file_keys <- paste(dt$Simulation, dt$Pathway, sep = "||")
      keys <- unique(c(keys, file_keys))
    }
  }
  return(keys)
}

existing_sim_hourly <- get_existing_simulation_pathways("Yearly_Results_Chunk_.*\\.csv")

# Load all hourly-result CSV filenames
data_path <- "/projects/epadecarb/2 Generation Expansion Model/4 Results/1 Stepwise"
all_files <- list.files(path = data_path, pattern = "\\.csv$", full.names = TRUE)
hourly_files_all <- grep("Hourly_Results", all_files, value = TRUE)

# Filter out already-processed simulation/pathway combos
filter_files_by_sim_pathway <- function(file_list, existing_keys) {
  filtered <- character(0)
  for (f in file_list) {
    dt <- fread(f, nrows = 1, select = c("Simulation", "Pathway"))
    key <- paste(dt$Simulation[1], dt$Pathway[1], sep = "||")
    if (!(key %in% existing_keys)) {
      filtered <- c(filtered, f)
    }
  }
  return(filtered)
}

hourly_files <- filter_files_by_sim_pathway(hourly_files_all, existing_sim_hourly)

# Split into chunks of ~200 files each
hourly_chunks <- split(hourly_files, ceiling(seq_along(hourly_files) / 200))

# Function to process each chunk, with a file‐size guard
process_hourly_files <- function(chunk_files, chunk_index) {
  results_path <- summary_results_path
  Hourly_Results <- data.table()
  
  # 10 MB threshold (in bytes)
  size_threshold <- 10 * 1024^2
  
  for (f in chunk_files) {
    info <- file.info(f)
    if (is.na(info$size) || info$size <= size_threshold) {
      warning(sprintf("Skipping '%s' (size = %s bytes ≤ 10 MB)", f, info$size))
      next
    }
    
    temp_data <- fread(f)
    Hourly_Results <- rbind(Hourly_Results, temp_data)
    rm(temp_data)
    gc()
  }
  
  # Summarize to yearly totals
  numeric_columns <- names(Hourly_Results)[sapply(Hourly_Results, is.numeric)]
  excluded_columns <- c("Hour", "DayLabel", "Year", "Simulation")
  excluded_patterns <- "_CF$"
  unit_columns <- numeric_columns[!numeric_columns %in% excluded_columns &
                                    !grepl(excluded_patterns, numeric_columns)]
  
  Yearly_Results <- Hourly_Results[,
                                   lapply(.SD, sum, na.rm = TRUE),
                                   by = .(Simulation, Year, Pathway),
                                   .SDcols = unit_columns
  ]
  
  # Convert MWh → TWh
  mw_columns <- grep("_MWh$", names(Yearly_Results), value = TRUE)
  Yearly_Results[, (mw_columns) := lapply(.SD, function(x) x / 1e6), .SDcols = mw_columns]
  setnames(Yearly_Results,
           old = mw_columns,
           new = sub("_MWh$", "_TWh", mw_columns))
  
  # Write out chunked yearly summary
  outfile <- file.path(results_path, paste0("Yearly_Results_Chunk_", chunk_index, ".csv"))
  write.csv(Yearly_Results, outfile, row.names = FALSE)
  
  # Compute and summarize shortages
  Hourly_Results[, Shortage_ratio := Calibrated_Shortage_MWh / Demand]
  Hourly_Results[, Cost_USD_per_MWh := approx(
    Cost_Curve$Percentage,
    Cost_Curve$Cost_per_MWh,
    Shortage_ratio
  )$y]
  Hourly_Results[is.na(Cost_USD_per_MWh), Cost_USD_per_MWh := 0]
  Hourly_Results[, Unmet_Demand_USD := Demand * Shortage_ratio * Cost_USD_per_MWh]
  
  Yearly_Results_Shortages <- Hourly_Results[, .(
    Unmet_Demand_total_MWh   = sum(Calibrated_Shortage_MWh, na.rm = TRUE),
    Unmet_Demand_USD_total   = sum(Unmet_Demand_USD,       na.rm = TRUE)
  ), by = .(Simulation, Year, Pathway)]
  
  outfile_short <- file.path(results_path, paste0("Yearly_Results_Shortages_Chunk_", chunk_index, ".csv"))
  write.csv(Yearly_Results_Shortages, outfile_short, row.names = FALSE)
  
  # Clean up
  rm(Hourly_Results, Yearly_Results, Yearly_Results_Shortages)
  gc()
}

# Run processing for each chunk
for (chunk_index in seq_along(hourly_chunks)) {
  process_hourly_files(hourly_chunks[[chunk_index]], chunk_index)
  gc()
}

# --- Combine all chunk outputs into final files ---
results_path_final <- summary_results_path

yearly_files   <- list.files(results_path_final,
                             pattern = "Yearly_Results_Chunk_.*\\.csv$",
                             full.names = TRUE)
shortages_files <- list.files(results_path_final,
                              pattern = "Yearly_Results_Shortages_Chunk_.*\\.csv$",
                              full.names = TRUE)

Yearly_Results           <- rbindlist(lapply(yearly_files,   fread))
Yearly_Results_Shortages <- rbindlist(lapply(shortages_files, fread))

final_results_path <- file.path(results_path_final, "Final")
dir.create(final_results_path, showWarnings = FALSE, recursive = TRUE)

write.csv(Yearly_Results,           file.path(final_results_path, "Yearly_Results.csv"),           row.names = FALSE)
write.csv(Yearly_Results_Shortages, file.path(final_results_path, "Yearly_Results_Shortages.csv"), row.names = FALSE)

# --- Notify via Pushover when done ---
pushover_user  <- "u52e6wsanmq1r129cccczuitn4a15y"
pushover_token <- "ag3xix77dm6ow6akuuk53bxokgxwni"
response <- POST(
  "https://api.pushover.net/1/messages.json",
  body = list(
    token   = pushover_token,
    user    = pushover_user,
    message = "The Summary_code executed."
  ),
  encode = "form"
)
