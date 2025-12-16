############################################################
# utils.R
# Funções auxiliares compartilhadas pelo pipeline crimes_am
# NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(glue)
  library(lubridate)
})

DIR_LOGS <- "logs"
if (!dir.exists(DIR_LOGS)) dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)

LOG_PIPELINE_FILE <- file.path(DIR_LOGS, "pipeline.log")

log_msg <- function(level = "INFO", msg, log_file = LOG_PIPELINE_FILE) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  line <- sprintf("[%s] [%s] %s", timestamp, level, msg)
  message(line)
  tryCatch(cat(line, file = log_file, append = TRUE, sep = "\n"), error = function(e) invisible(NULL))
}

safe_sys <- function(expr, context = "generic") {
  tryCatch(
    expr,
    error = function(e) {
      log_msg("ERROR", glue("Erro em {context}: {e$message}"))
      stop(e)
    }
  )
}

normalize_encoding <- function(x) {
  if (is.null(x)) return(NA_character_)
  x <- as.character(x)
  Encoding(x) <- "UTF-8"
  x
}

parse_date_safe <- function(x, orders = c("Y-m-d", "d/m/Y")) {
  out <- suppressWarnings(lubridate::parse_date_time(x, orders = orders, tz = "America/Manaus"))
  as.Date(out)
}
