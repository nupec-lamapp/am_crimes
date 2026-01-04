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

get_env_value <- function(nome) {
  val <- Sys.getenv(nome, unset = "")
  if (identical(val, "")) return(NA_character_)
  val
}

write_run_metadata <- function(meta, path) {
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    jsonlite::write_json(meta, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  } else {
    linhas <- c("# pipeline metadata", capture.output(str(meta)))
    writeLines(linhas, con = path)
  }
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

  run_id <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_pipeline("INFO", sprintf("Run ID: %s", run_id))

  log_pipeline("INFO", sprintf("Executando pipeline de %s a %s",
                               format(data_inicio, "%d/%m/%Y"),
                               format(data_fim, "%d/%m/%Y")))

  source_if_exists("scripts/01_scraping.R")
  source_if_exists("scripts/02_parse.R")
  source_if_exists("scripts/03_cleaning.R")

  meta <- list(
    run_id = run_id,
    started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    args = args,
    data_inicio = as.character(data_inicio),
    data_fim = as.character(data_fim),
    workdir = getwd(),
    env = list(
      CRIMES_AM_MAX_PAGINAS = get_env_value("CRIMES_AM_MAX_PAGINAS"),
      CRIMES_AM_INCLUIR_SEM_DATA = get_env_value("CRIMES_AM_INCLUIR_SEM_DATA"),
      CRIMES_AM_DEDUP_POR = get_env_value("CRIMES_AM_DEDUP_POR")
    )
  )
  if (exists("listar_coletores")) {
    meta$coletores <- listar_coletores()
  }

  meta_path <- file.path(log_dir, paste0("pipeline_run_", run_id, ".json"))
  tryCatch({
    write_run_metadata(meta, meta_path)
    log_pipeline("INFO", sprintf("Metadata de execucao salva em: %s", meta_path))
  }, error = function(e) {
    log_pipeline("WARN", sprintf("Falha ao salvar metadata: %s", e$message))
  })

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
