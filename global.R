############################################################
# global.R
# Configuracoes globais e funcoes auxiliares do app Shiny
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
  library(ggplot2)
  library(DT)
  library(readr)
  library(lubridate)
  library(stringr)
  library(fs)
  library(plotly)
  library(reactable)
})

try(Sys.setlocale("LC_CTYPE", "pt_BR.UTF-8"), silent = TRUE)
options(stringsAsFactors = FALSE)

############################################################
# Caminhos
############################################################

if (file.exists("R/paths.R")) source("R/paths.R")
if (exists("crimes_am_set_paths")) crimes_am_set_paths()

DIR_PIPELINE  <- "scripts"
DIR_PROCESSED <- if (exists("DIR_PROCESSED")) DIR_PROCESSED else file.path("data", "processed")
DIR_OUTPUTS   <- if (exists("DIR_OUTPUTS")) DIR_OUTPUTS else "outputs"

############################################################
# Controle de acesso ao botao "Nova Coleta"
############################################################

pipeline_keys_env <- Sys.getenv("CRIMES_AM_PIPELINE_KEYS", "")
ADMIN_PIPELINE_KEYS <- if (nzchar(pipeline_keys_env)) {
  trimws(unlist(strsplit(pipeline_keys_env, ",")))
} else {
  character()
}
PIPELINE_AUTH_ENABLED <- length(ADMIN_PIPELINE_KEYS) > 0

############################################################
# Helpers de caminho e carregamento
############################################################

localizar_arquivo <- function(pasta, nome) {
  pastas <- unique(na.omit(as.character(pasta)))
  pastas <- c(pastas, file.path("data", "processed"), "outputs")
  for (p in unique(pastas)) {
    caminho <- file.path(p, nome)
    if (file.exists(caminho)) return(caminho)
  }
  if (file.exists(nome)) return(nome)
  NULL
}

# Heuristicas de genero/faixa etaria especificas da camada visual
extrair_genero_app <- function(texto) {
  if (is.na(texto)) return("indefinido")
  texto <- tolower(texto)
  if (str_detect(texto, "\\b(mulher|esposa|namorada|jovem|menina|senhora|mae|filha|adolescente)\\b")) return("feminino")
  if (str_detect(texto, "\\b(homem|marido|namorado|rapaz|menino|senhor|pai|filho|suspeito)\\b")) return("masculino")
  "indefinido"
}

classificar_faixa_app <- function(texto) {
  if (is.na(texto)) return("idade nao informada")
  match <- str_extract(texto, "\\b\\d{1,2}(?=\\s?anos)\\b")
  if (is.na(match)) return("idade nao informada")
  idade <- as.integer(match)
  if (idade <= 11) return("0-11 (crianca)")
  if (idade <= 17) return("12-17 (adolescente)")
  if (idade <= 29) return("18-29 (jovem)")
  if (idade <= 59) return("30-59 (adulto)")
  "60+ (idoso)"
}

carregar_principal <- function() {
  arquivo <- localizar_arquivo(DIR_PROCESSED, "crimes_classificados.csv")
  if (is.null(arquivo)) return(NULL)

  tryCatch({
    df <- read_csv(arquivo, show_col_types = FALSE) %>%
      mutate(data_publicacao = as.Date(data_publicacao))

    if (!"tipo_principal" %in% names(df)) df$tipo_principal <- "Outros"
    if (!"gravidade" %in% names(df)) df$gravidade <- "indefinida"
    if (!"crime_violento" %in% names(df)) df$crime_violento <- FALSE
    if (!"categoria" %in% names(df)) df$categoria <- "Geral"

    if (!"genero_vitima" %in% names(df)) df$genero_vitima <- vapply(df$titulo, extrair_genero_app, character(1))
    if (!"faixa_etaria" %in% names(df))  df$faixa_etaria  <- vapply(df$titulo, classificar_faixa_app, character(1))

    df$data_pub <- df$data_publicacao
    df
  }, error = function(e) {
    warning("Falha ao carregar crimes_classificados.csv: ", e$message)
    NULL
  })
}

carregar_estaticos <- function() {
  lista <- list()
  f_anom <- localizar_arquivo(DIR_OUTPUTS, "04_anomalias_classificacao.csv")
  if (!is.null(f_anom)) lista$anomalias <- read_csv(f_anom, show_col_types = FALSE)

  f_ind <- localizar_arquivo(DIR_OUTPUTS, "04_indice_letal_mensal.csv")
  if (!is.null(f_ind)) lista$mensal <- read_csv(f_ind, show_col_types = FALSE)

  f_res <- localizar_arquivo(DIR_OUTPUTS, "04_resumo_geral.csv")
  if (!is.null(f_res)) lista$resumo <- read_csv(f_res, show_col_types = FALSE)

  lista
}

carregar_apresentacao <- function() {
  arquivo <- "APRESENTACAO.md"
  if (!file.exists(arquivo)) {
    return(HTML("<p>Arquivo de apresentacao nao encontrado.</p>"))
  }

  tryCatch({
    linhas <- readLines(arquivo, encoding = "UTF-8", warn = FALSE)
    html_parts <- character()
    in_list <- FALSE

    for (linha in linhas) {
      if (grepl("^---$", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        html_parts <- c(html_parts, "<hr style='margin:20px 0;' />")
        next
      }

      if (grepl("^### ", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        titulo <- gsub("^### ", "", linha)
        html_parts <- c(html_parts, sprintf(
          "<h3 style='margin-top:20px; color:#2c3e50;'>%s</h3>", titulo))
        next
      }
      if (grepl("^## ", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        titulo <- gsub("^## ", "", linha)
        html_parts <- c(html_parts, sprintf(
          "<h2 style='margin-top:25px; color:#2c3e50; border-bottom:2px solid #667eea; padding-bottom:5px;'>%s</h2>", titulo))
        next
      }
      if (grepl("^# ", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        titulo <- gsub("^# ", "", linha)
        html_parts <- c(html_parts, sprintf(
          "<h1 style='margin-top:30px; color:#667eea;'>%s</h1>", titulo))
        next
      }

      if (grepl("^- ", linha)) {
        if (!in_list) {
          html_parts <- c(html_parts, "<ul style='margin-left:20px;'>")
          in_list <- TRUE
        }
        item <- gsub("^- ", "", linha)
        item <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", item)
        html_parts <- c(html_parts, sprintf(
          "<li style='margin:5px 0;'>%s</li>", item))
        next
      }

      if (trimws(linha) == "") {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        html_parts <- c(html_parts, "<br />")
        next
      }

      if (grepl("^!\\[", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        match <- regmatches(linha, regexpr("!\\[([^\\]]+)\\]\\(([^)]+)\\)", linha))
        if (length(match) > 0) {
          alt <- gsub("!\\[([^\\]]+)\\]\\(.*\\)", "\\1", match)
          src <- gsub("!\\[.*\\]\\(([^)]+)\\)", "\\1", match)
          html_parts <- c(html_parts, sprintf(
            "<div style='text-align:center; margin:15px 0;'><img src='%s' alt='%s' style='max-width:250px; height:auto; margin:10px;' /></div>",
            src, alt))
        }
        next
      }

      if (in_list) {
        html_parts <- c(html_parts, "</ul>")
        in_list <- FALSE
      }

      linha <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", linha)
      linha <- gsub("\\[([^\\]]+)\\]\\(([^)]+)\\)", "<a href='\\2' target='_blank'>\\1</a>", linha)

      html_parts <- c(html_parts, sprintf(
        "<p style='text-align:justify; line-height:1.6; margin:10px 0;'>%s</p>", linha))
    }

    if (in_list) {
      html_parts <- c(html_parts, "</ul>")
    }

    HTML(paste(html_parts, collapse = "\n"))
  }, error = function(e) {
    HTML(sprintf("<p>Erro ao carregar apresentacao: %s</p>", e$message))
  })
}
