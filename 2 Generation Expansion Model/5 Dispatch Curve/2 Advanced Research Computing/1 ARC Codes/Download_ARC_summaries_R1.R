# Purpose: download ZIP summaries through the same RStudio SSH connection as
# ARC Control. Save verified chunks, reconnect after failure, then assemble.
# Source on your Mac. ARC files are read only. No Terminal window; optional password read from a private local file.

SSH_HOST <- "amirgazar@datatransfer.arc.vt.edu"
REMOTE_FOLDER <- paste0(
  "/projects/epadecarb/2 Generation Expansion Model/4 Results/PHASED_R1/",
  "R1_ensemble101_1000_20260920_01/Backup summary 1000 R1/Final"
)
DOWNLOAD_FOLDER <- path.expand("~/Downloads/PHASED_1000_summaries")
# Yearly summary already downloaded; resume only the facility ZIP.
FILES <- "Yearly_Facility_Level_Results.zip"
CHUNK_BYTES <- 4L * 1024L * 1024L
PARALLEL_DOWNLOADS <- 10L
PASSWORD_ENV_FILE <- path.expand("~/.config/phased/arc-download.env")
MAX_ATTEMPTS <- 20L
RETRY_SECONDS <- 15L

arc_q <- function(x) shQuote(x, type = "sh")
# Read a literal password without evaluating shell code or printing credentials.
# Each session uses it once; Duo/verification prompts remain interactive.
arc_password_callback <- function(env_file) {
  password <- ""
  if (file.exists(env_file)) {
    lines <- readLines(env_file, warn = FALSE)
    values <- lines[startsWith(lines, "ARC_PASSWORD=")]
    if (length(values) > 1L) stop("Keep only one ARC_PASSWORD line in the password file.")
    if (length(values)) password <- substring(values, nchar("ARC_PASSWORD=") + 1L)
  }
  used <- FALSE
  function(prompt) {
    is_password <- grepl("password", prompt, ignore.case = TRUE) &&
      !grepl("duo|passcode|verification|one.time|otp|push|token|factor", prompt, ignore.case = TRUE)
    if (nzchar(password) && !used && is_password) {
      used <<- TRUE
      return(password)
    }
    askpass::askpass(prompt)
  }
}

arc_backend <- list(
  connect = function() ssh::ssh_connect(SSH_HOST, passwd = arc_password_callback(PASSWORD_ENV_FILE)),
  disconnect = function(s) ssh::ssh_disconnect(s),
  exec = function(s, command) ssh::ssh_exec_internal(s, command, error = FALSE)
)
file_md5 <- function(path) unname(tools::md5sum(path))

arc_connection <- function() {
  state <- new.env(parent = emptyenv())
  state$session <- NULL
  close <- function() {
    if (!is.null(state$session)) try(arc_backend$disconnect(state$session), silent = TRUE)
    state$session <- NULL
  }
  request <- function(command) {
    for (attempt in seq_len(MAX_ATTEMPTS)) {
      result <- tryCatch({
        if (is.null(state$session)) {
          cat("\nConnecting to ARC; authenticate in RStudio when prompted.\n")
          state$session <- arc_backend$connect()
        }
        arc_backend$exec(state$session, command)
      }, error = function(e) e)
      if (!inherits(result, "error")) {
        if (result$status != 0L) stop("ARC command failed: ", rawToChar(result$stderr))
        return(result$stdout)
      }
      close()
      cat("\nConnection interrupted: ", conditionMessage(result),
          "\nCompleted chunks are saved.\n", sep = "")
      if (attempt < MAX_ATTEMPTS) {
        cat("Reconnecting in ", RETRY_SECONDS, " seconds (attempt ", attempt + 1L,
            "/", MAX_ATTEMPTS, ").\n", sep = "")
        Sys.sleep(RETRY_SECONDS)
      }
    }
    stop("Unable to reconnect. Restore VPN and Source this file again to resume.")
  }
  list(request = request, close = close)
}

remote_info <- function(connection, path) {
  # GNU stat and md5sum are available on ARC; only stdout metadata is returned.
  command <- paste("stat -c %s --", arc_q(path), "&& md5sum --", arc_q(path))
  lines <- strsplit(rawToChar(connection$request(command)), "\n", fixed = TRUE)[[1]]
  size <- suppressWarnings(as.numeric(lines[1]))
  hash <- substr(lines[2], 1L, 32L)
  if (!is.finite(size) || size <= 0 || !grepl("^[0-9a-f]{32}$", hash))
    stop("Invalid ARC file size/checksum: ", path)
  list(size = size, md5 = hash)
}

chunk_valid <- function(path, size) {
  receipt <- paste0(path, ".md5")
  if (!file.exists(path) || !file.exists(receipt) || file.info(path)$size != size) return(FALSE)
  identical(file_md5(path), readLines(receipt, warn = FALSE, n = 1L))
}

assemble_zip <- function(parts, output) {
  temporary <- paste0(output, ".assembling")
  out <- file(temporary, "wb")
  on.exit(close(out), add = TRUE)
  for (part in parts) {
    input <- file(part, "rb")
    tryCatch({
      repeat {
        bytes <- readBin(input, "raw", n = CHUNK_BYTES)
        if (!length(bytes)) break
        writeBin(bytes, out)
      }
    }, finally = close(input))
  }
  invisible(temporary)
}

# Independent R workers own independent SSH connections (no shared sockets).
worker_connect <- function(host, env_file, callback_factory) {
  options(rlib_interactive = TRUE) # Allow native Mac password dialogs in workers.
  if (exists(".arc_session", envir = .GlobalEnv, inherits = FALSE))
    try(ssh::ssh_disconnect(get(".arc_session", .GlobalEnv)), silent = TRUE)
  assign(".arc_session", ssh::ssh_connect(host, passwd = callback_factory(env_file)), envir = .GlobalEnv)
  TRUE
}

worker_chunk <- function(task) {
  tryCatch({
    command <- paste0("dd if=", shQuote(task$remote, type = "sh"),
                      " bs=", task$block, " skip=", task$index - 1L,
                      " count=1 status=none")
    result <- ssh::ssh_exec_internal(get(".arc_session", .GlobalEnv), command, error = FALSE)
    if (result$status != 0L) stop(rawToChar(result$stderr))
    if (length(result$stdout) != task$size) stop("Unexpected chunk size")
    temporary <- paste0(task$path, ".partial")
    writeBin(result$stdout, temporary)
    hash <- unname(tools::md5sum(temporary))
    if (!file.rename(temporary, task$path)) stop("Cannot save local chunk")
    writeLines(hash, paste0(task$path, ".md5"))
    list(ok = TRUE, error = "")
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
}

arc_download_zip <- function(connection, name, pool) {
  remote <- paste0(REMOTE_FOLDER, "/", name)
  target <- file.path(DOWNLOAD_FOLDER, name)
  cat("\nChecking ARC file: ", name, "\n", sep = "")
  info <- remote_info(connection, remote)
  if (file.exists(target) && identical(file_md5(target), info$md5)) {
    cat("Already downloaded and verified: ", name, "\n", sep = "")
    return(invisible(target))
  }
  # Content-specific folders prevent chunks from different ZIP versions mixing.
  chunk_dir <- file.path(DOWNLOAD_FOLDER, ".resume", paste0(name, "-", info$md5, "-", CHUNK_BYTES))
  dir.create(chunk_dir, recursive = TRUE, showWarnings = FALSE)
  n <- ceiling(info$size / CHUNK_BYTES)
  parts <- file.path(chunk_dir, sprintf("part-%06d", seq_len(n)))
  sizes <- pmin(CHUNK_BYTES, info$size - (seq_len(n) - 1) * CHUNK_BYTES)
  valid <- vapply(seq_len(n), function(i) chunk_valid(parts[i], sizes[i]), logical(1))
  done <- sum(sizes[valid])
  cat(sprintf("%.1f MB total; %.1f MB already saved.\n", info$size / 1024^2, done / 1024^2))
  progress <- txtProgressBar(min = 0, max = info$size, initial = done, style = 3)
  on.exit(close(progress), add = TRUE)
  remaining <- which(!valid)
  while (length(remaining)) {
    batch <- head(remaining, length(pool))
    tasks <- lapply(batch, function(i) list(remote = remote, block = CHUNK_BYTES,
                   index = i, size = sizes[i], path = parts[i]))
    pending <- seq_along(batch)
    for (attempt in seq_len(MAX_ATTEMPTS)) {
      # Exactly one task per selected worker; completed tasks are never resubmitted.
      results <- parallel::clusterApply(pool[pending], tasks[pending], worker_chunk)
      failed <- integer()
      for (j in seq_along(pending)) {
        slot <- pending[j]
        if (isTRUE(results[[j]]$ok)) {
          done <- done + sizes[batch[slot]]
          setTxtProgressBar(progress, done)
        } else {
          failed <- c(failed, slot)
          cat("\nChunk ", batch[slot], " interrupted: ", results[[j]]$error, "\n", sep = "")
        }
      }
      if (!length(failed)) break
      pending <- failed
      if (attempt == MAX_ATTEMPTS)
        stop("Retries exhausted. Completed chunks retained. Source again to resume.")
      cat("\nRetrying failed chunks in ", RETRY_SECONDS, " seconds.\n", sep = "")
      Sys.sleep(RETRY_SECONDS)
      # Reconnect sequentially so authentication dialogs do not overlap.
      for (slot in pending) {
        tryCatch(parallel::clusterCall(pool[slot], worker_connect, SSH_HOST, PASSWORD_ENV_FILE, arc_password_callback),
                 error = function(e) message("Reconnect failed: ", conditionMessage(e)))
      }
    }
    remaining <- remaining[-seq_along(batch)]
  }
  cat("\nAssembling ZIP and verifying against ARC...\n")
  temp <- assemble_zip(parts, target)
  if (!identical(file_md5(temp), info$md5))
    stop("ZIP checksum mismatch. Saved chunks retained; do not use the assembled file.")
  latest <- remote_info(connection, remote)
  if (!identical(info, latest)) stop("ARC source changed during transfer. Source again for the new version.")
  if (!file.rename(temp, target)) stop("Cannot save completed ZIP: ", target)
  # Remove only this file's temporary chunks after complete checksum validation.
  unlink(chunk_dir, recursive = TRUE)
  cat("Verified: ", target, "\n", sep = "")
  invisible(target)
}

run_downloads <- function() {
  if (!requireNamespace("ssh", quietly = TRUE))
    stop('Install the connection package first: install.packages("ssh")')
  dir.create(DOWNLOAD_FOLDER, recursive = TRUE, showWarnings = FALSE)
  # A lock prevents two R sessions from writing the same chunks simultaneously.
  lock <- file.path(DOWNLOAD_FOLDER, ".download-lock")
  if (!dir.create(lock, showWarnings = FALSE))
    stop("Another download may be running. If none is running, remove the stale folder: ", lock)
  on.exit(unlink(lock, recursive = TRUE), add = TRUE)
  if (Sys.info()[["sysname"]] != "Darwin") stop("Run this downloader on your Mac.")
  pool <- parallel::makePSOCKcluster(PARALLEL_DOWNLOADS, outfile = "")
  on.exit(parallel::stopCluster(pool), add = TRUE)
  for (i in seq_along(pool)) {
    cat("Connecting download channel ", i, "/", length(pool), "...\n", sep = "")
    parallel::clusterCall(pool[i], worker_connect, SSH_HOST, PASSWORD_ENV_FILE, arc_password_callback)
  }
  connection <- arc_connection()
  on.exit(connection$close(), add = TRUE)
  cat("Keep VPN connected. ", PARALLEL_DOWNLOADS, " parallel channels; progress updates after each group of 4 MB chunks.\n",
      "Stop safely with Esc; Source again to resume. Reconnection may require sign-in.\n",
      "Destination: ", DOWNLOAD_FOLDER, "\n", sep = "")
  for (i in seq_along(FILES)) {
    cat(sprintf("\nFILE %d OF %d\n", i, length(FILES)))
    arc_download_zip(connection, FILES[i], pool)
  }
  cat("\nDONE: selected ZIP files downloaded and verified. Original ARC files unchanged.\n")
}

run_downloads()
