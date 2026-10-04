
library(arrow)

# 1. Define your input and output directories
input_dir  <- Sys.getenv("PHASED_SIMULATED_DATA_FACILITY_LEVEL", unset="")
output_dir <- file.path(dirname(input_dir), "Simulated Data parquet")

# 2. Create the top-level output folder if necessary
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# 3. Find all CSVs under input_dir recursively
csv_paths <- list.files(
  path       = input_dir,
  pattern    = "\\.csv$",
  full.names = TRUE,
  recursive  = TRUE
)

# 4. Loop: for each CSV...
for (csv_path in csv_paths) {
  info <- file.info(csv_path)

  # skip files under 10 KB
  if (is.na(info$size) || info$size < 10 * 1024) {
    message("⚠️ Skipping (size < 10 KB): ", csv_path)
    next
  }

  # compute relative path under input_dir
  rel_path <- substr(csv_path, nchar(input_dir) + 2, nchar(csv_path))
  rel_dir  <- dirname(rel_path)

  # sanitize filename (remove any non-alphanumeric or underscore)
  raw_name  <- tools::file_path_sans_ext(basename(csv_path))
  safe_name <- gsub("[^A-Za-z0-9_]", "", raw_name)
  if (safe_name == "") safe_name <- "file"

  # prepare output subfolder and parquet path
  out_subdir   <- file.path(output_dir, rel_dir)
  if (!dir.exists(out_subdir)) {
    dir.create(out_subdir, recursive = TRUE)
  }
  parquet_path <- file.path(out_subdir, paste0(safe_name, ".parquet"))

  # skip if already converted
  if (file.exists(parquet_path)) {
    message("⏭ Already exists: ", parquet_path)
    next
  }

  # read and write
  tbl <- tryCatch(
    arrow::read_csv_arrow(csv_path),
    error = function(e) {
      message("❌ Failed to read CSV: ", rel_path, " (", e$message, ")")
      return(NULL)
    }
  )
  if (is.null(tbl)) next

  tryCatch({
    arrow::write_parquet(tbl, parquet_path)
  }, error = function(e) {
    message("❌ Failed to write Parquet for: ", rel_path, " (", e$message, ")")
    return(NULL)
  })

  # at this point, conversion succeeded-delete the CSV
  unlink(csv_path)

  message("✓ Converted & deleted: ", rel_path, " → ", file.path(rel_dir, paste0(safe_name, ".parquet")))
}

message("All done. Parquet files live in '", output_dir, "', originals removed.")


# helper: strip to [A–Z, a–z, 0–9, _]
sanitize <- function(x) {
  y <- gsub("[^A-Za-z0-9_]", "", x)
  if (y == "") stop("Filename became empty after sanitizing: ‘", x, "’")
  y
}

parquet_root <- Sys.getenv("PHASED_SIMULATED_DATA_PARQUET", unset="")

# grab every .parquet under parquet_root
all_parquets <- list.files(
  path       = parquet_root,
  pattern    = "\\.parquet$",
  full.names = TRUE,
  recursive  = TRUE
)

for (old_path in all_parquets) {
  # state is the first directory under parquet_root
  rel_path <- substring(old_path, nchar(parquet_root) + 2)
  parts    <- strsplit(rel_path, .Platform$file.sep)[[1]]
  state    <- parts[1]

  fname <- basename(old_path)
  # only touch those *not* already prefixed
  if (grepl(paste0("^", state, "_"), fname)) next

  base     <- tools::file_path_sans_ext(fname)
  safe     <- sanitize(base)
  new_name <- paste0(state, "_", safe, ".parquet")
  new_path <- file.path(dirname(old_path), new_name)

  # if a file with the target name already exists, delete it first
  if (file.exists(new_path)) {
    message("Overwriting existing: ", new_name)
    file.remove(new_path)
  }

  # perform the rename
  if (file.rename(old_path, new_path)) {
    message("✓ Renamed: ", fname, " → ", new_name)
  } else {
    warning("✗ Failed to rename: ", fname)
  }
}

message("All unprefixed .parquet files have been forcibly renamed with STATE_ prefixes.")

