#!/usr/bin/env Rscript

source_if_exists <- function(path) {
  if (!file.exists(path)) stop(sprintf("Arquivo obrigatório não encontrado: %s", path))
  sys.source(path, envir = .GlobalEnv)
}

if (file.exists("R/paths.R")) source("R/paths.R")
if (exists("crimes_am_set_paths")) crimes_am_set_paths()

log_dir <- if (exists("DIR_LOGS")) DIR_LOGS else "logs"
if (!dir.exists(log_dir)) dir.create(log_dir, showWarnings = FALSE)
log_file <- file.path(log_dir, "pipeline.log")

log_pipeline <- function(level, msg) {
  linha <- sprintf("[%s] [%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, msg)
  message(linha)
  tryCatch(cat(linha, file = log_file, append = TRUE, sep = "\n"), error = function(e) invisible(NULL))
}

run_step <- function(nome, fun) {
  log_pipeline("INFO", sprintf("Iniciando etapa: %s", nome))
  res <- fun()
  log_pipeline("INFO", sprintf("Etapa '%s' concluída.", nome))
  res
}

run_pipeline_cli <- function(args = commandArgs(trailingOnly = TRUE)) {
  if (length(args) == 0) {
    data_fim <- Sys.Date()
    data_inicio <- data_fim - 6
  } else if (length(args) == 1) {
    data_inicio <- as.Date(args[1])
    data_fim <- data_inicio
  } else {
    data_inicio <- as.Date(args[1])
    data_fim <- as.Date(args[2])
  }

  if (is.na(data_inicio) || is.na(data_fim)) stop("Datas inválidas fornecidas ao pipeline.")
  if (data_inicio > data_fim) stop("Data inicial não pode ser maior que a data final.")

  log_pipeline("INFO", sprintf("Executando pipeline de %s a %s",
                               format(data_inicio, "%d/%m/%Y"),
                               format(data_fim, "%d/%m/%Y")))

  source_if_exists("scripts/01_scraping.R")
  source_if_exists("scripts/02_parse.R")
  source_if_exists("scripts/03_cleaning.R")

  if (file.exists("scripts/04_analysis.R")) {
    analise_path <- "scripts/04_analysis.R"
  } else {
    analise_path <- NULL
  }

  df_raw <- run_step("Scraping", function() {
    rodar_scraping(data_inicio, data_fim)
  })

  df_parsed <- run_step("Parse (02)", function() {
    parse_raw_files()
  })

  df_clean <- run_step("Cleaning (03)", function() {
    clean_and_enrich_data(df_parsed)
  })

  if (!is.null(analise_path)) {
    run_step("Analises e exportacao (04)", function() {
      sys.source(analise_path, envir = new.env(parent = globalenv()))
    })
  }

  log_pipeline("INFO", sprintf("Pipeline finalizado com %s registros brutos, %s parsed e %s limpos.",
                               nrow(df_raw), nrow(df_parsed), nrow(df_clean)))
}

run_pipeline_cli()
