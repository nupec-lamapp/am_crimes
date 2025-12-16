############################################################
# paths.R
# Resolve paths for read/write across local + shinyapps.io.
############################################################

crimes_am_is_writable_dir <- function(path) {
  if (!dir.exists(path)) return(FALSE)
  isTRUE(file.access(path, 2) == 0)
}

crimes_am_ensure_dir <- function(path) {
  tryCatch(
    dir.create(path, recursive = TRUE, showWarnings = FALSE),
    error = function(e) FALSE
  )
  dir.exists(path)
}

crimes_am_init_paths <- function() {
  project_root <- "."

  requested_workdir <- Sys.getenv("CRIMES_AM_WORKDIR", "")
  workdir <- if (nzchar(requested_workdir)) requested_workdir else project_root

  logs_candidate <- file.path(workdir, "logs")
  ok_logs <- crimes_am_ensure_dir(logs_candidate) && crimes_am_is_writable_dir(logs_candidate)

  if (!ok_logs) {
    workdir <- file.path(tempdir(), "crimes_am")
    crimes_am_ensure_dir(workdir)
  }

  data_dir    <- file.path(workdir, "data")
  raw_dir     <- file.path(data_dir, "raw")
  proc_dir    <- file.path(data_dir, "processed")
  logs_dir    <- file.path(workdir, "logs")
  outputs_dir <- file.path(workdir, "outputs")

  crimes_am_ensure_dir(raw_dir)
  crimes_am_ensure_dir(proc_dir)
  crimes_am_ensure_dir(logs_dir)
  crimes_am_ensure_dir(outputs_dir)

  list(
    CRIMES_AM_PROJECT_ROOT = project_root,
    CRIMES_AM_WORKDIR      = workdir,
    DIR_RAW                = raw_dir,
    DIR_PROCESSED          = proc_dir,
    DIR_LOGS               = logs_dir,
    DIR_OUTPUTS            = outputs_dir
  )
}

crimes_am_set_paths <- function(envir = .GlobalEnv) {
  p <- crimes_am_init_paths()
  for (nm in names(p)) assign(nm, p[[nm]], envir = envir)
  invisible(p)
}

