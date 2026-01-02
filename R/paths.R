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
  home_workdir <- file.path(path.expand("~"), "crimes_am_data")
  candidates <- list(
    env = requested_workdir,
    project = project_root,
    home = home_workdir
  )

  workdir <- NULL
  workdir_source <- NULL
  for (nm in names(candidates)) {
    candidate <- candidates[[nm]]
    if (!nzchar(candidate)) next
    data_candidate <- file.path(candidate, "data")
    ok_data <- crimes_am_ensure_dir(data_candidate) && crimes_am_is_writable_dir(data_candidate)
    if (ok_data) {
      workdir <- candidate
      workdir_source <- nm
      break
    }
  }

  if (is.null(workdir)) {
    workdir <- file.path(tempdir(), "crimes_am")
    crimes_am_ensure_dir(workdir)
    workdir_source <- "temp"
  }
  storage_mode <- if (identical(workdir_source, "temp")) "temp" else "persistent"

  data_dir    <- file.path(workdir, "data")
  raw_dir     <- file.path(data_dir, "raw")
  proc_dir    <- file.path(data_dir, "processed")
  logs_dir_candidate <- file.path(workdir, "logs")
  outputs_dir <- file.path(workdir, "outputs")

  crimes_am_ensure_dir(raw_dir)
  crimes_am_ensure_dir(proc_dir)
  crimes_am_ensure_dir(outputs_dir)

  ok_logs <- crimes_am_ensure_dir(logs_dir_candidate) && crimes_am_is_writable_dir(logs_dir_candidate)
  logs_dir <- if (ok_logs) logs_dir_candidate else file.path(tempdir(), "crimes_am_logs")
  crimes_am_ensure_dir(logs_dir)
  logs_mode <- if (ok_logs) "persistent" else "temp"

  list(
    CRIMES_AM_PROJECT_ROOT = project_root,
    CRIMES_AM_WORKDIR      = workdir,
    CRIMES_AM_WORKDIR_SOURCE = workdir_source,
    CRIMES_AM_STORAGE_MODE = storage_mode,
    CRIMES_AM_LOGS_MODE    = logs_mode,
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

