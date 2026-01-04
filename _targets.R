library(targets)

tar_option_set(
  packages = c(
    "dplyr",
    "readr",
    "purrr",
    "stringr",
    "tibble",
    "lubridate",
    "xml2",
    "rvest",
    "httr"
  )
)

source("scripts/01_scraping.R")
source("scripts/02_parse.R")
source("scripts/03_cleaning.R")

get_pipeline_params <- function() {
  data_inicio <- Sys.getenv("CRIMES_AM_DATA_INICIO", unset = "")
  data_fim <- Sys.getenv("CRIMES_AM_DATA_FIM", unset = "")

  if (!nzchar(data_inicio)) {
    data_inicio <- Sys.Date() - 6
  } else {
    data_inicio <- as.Date(data_inicio)
  }

  if (!nzchar(data_fim)) {
    data_fim <- Sys.Date()
  } else {
    data_fim <- as.Date(data_fim)
  }

  list(
    data_inicio = data_inicio,
    data_fim = data_fim
  )
}

list(
  tar_target(params, get_pipeline_params()),
  tar_target(raw_data, rodar_scraping(params$data_inicio, params$data_fim)),
  tar_target(parsed_data, parse_raw_files()),
  tar_target(clean_data, clean_and_enrich_data(parsed_data))
)
