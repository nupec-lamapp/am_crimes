############################################################
# 02_parse.R
# Leitura e padronizacao inicial dos dados brutos
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(tibble)
  library(stringr)
})

options(stringsAsFactors = FALSE)

if (file.exists("R/paths.R")) source("R/paths.R")
if (exists("crimes_am_set_paths")) crimes_am_set_paths()
if (!exists("DIR_RAW", inherits = TRUE)) DIR_RAW <- file.path("data", "raw")

normalizar_para_dup <- function(titulo) {
  if (is.null(titulo)) return("")
  titulo <- tolower(titulo)
  titulo <- suppressWarnings(iconv(titulo, from = "", to = "ASCII//TRANSLIT"))
  titulo <- stringr::str_replace_all(titulo, "[[:punct:]]", " ")
  stringr::str_squish(titulo)
}

remover_duplicados_janela <- function(df, dias = 7, dedup_por = c("portal", "titulo_norm")) {
  if (!"data_publicacao" %in% names(df)) return(df)
  df <- df %>%
    mutate(titulo_norm = normalizar_para_dup(titulo))

  dedup_cols <- dedup_por
  if (is.null(dedup_cols) || length(dedup_cols) == 0) dedup_cols <- "titulo_norm"
  dedup_cols <- unique(dedup_cols)
  missing_cols <- setdiff(dedup_cols, names(df))
  if (length(missing_cols) > 0) {
    warning("Colunas em dedup_por ausentes: ", paste(missing_cols, collapse = ", "),
            ". Usando apenas colunas disponiveis.")
    dedup_cols <- setdiff(dedup_cols, missing_cols)
  }
  if (length(dedup_cols) == 0) dedup_cols <- "titulo_norm"

  df %>%
    arrange(across(all_of(dedup_cols)), data_publicacao, url) %>%
    group_by(across(all_of(dedup_cols))) %>%
    mutate(
      diff_prev = as.numeric(difftime(data_publicacao, lag(data_publicacao), units = "days")),
      duplicado_janela = !is.na(diff_prev) & diff_prev <= dias
    ) %>%
    ungroup() %>%
    {
      qtd <- sum(.$duplicado_janela, na.rm = TRUE)
      if (qtd > 0) {
        message("Removendo ", qtd, " registros duplicados (janela +/-", dias, " dias).")
      }
      filter(., !duplicado_janela | is.na(duplicado_janela)) %>%
        select(-titulo_norm, -diff_prev, -duplicado_janela)
    }
}

############################################################
# parse_raw_files()
# Le todos os arquivos CSV em data/raw/, garante colunas
# minimas e retorna um data.frame padronizado.
############################################################

parse_raw_files <- function(dir_raw = DIR_RAW, dedup_dias = 7, dedup_por = c("portal", "titulo_norm")) {
  if (!dir.exists(dir_raw)) {
    stop("Diretorio data/raw/ nao encontrado (", dir_raw, "). Rode 01_scraping.R primeiro.")
  }

  arquivos_raw <- list.files(dir_raw, pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos_raw) == 0) {
    stop("Nenhum arquivo encontrado em data/raw. Rode primeiro o 01_scraping.R.")
  }

  message("Lendo arquivos de data/raw/ ...")

  lista_dfs <- map(arquivos_raw, function(arq) {
    message(" - ", arq)
    df <- readr::read_csv(arq, show_col_types = FALSE)
    df$arquivo_origem <- basename(arq)
    df
  })

  df_raw <- bind_rows(lista_dfs) %>%
    distinct(portal, data_publicacao, titulo, url, .keep_all = TRUE) %>%
    mutate(
      data_publicacao = as.Date(data_publicacao)
    )

  cols_essenciais <- c("portal", "data_publicacao", "titulo", "url")
  faltando <- setdiff(cols_essenciais, names(df_raw))
  if (length(faltando) > 0) {
    stop("Colunas essenciais ausentes em df_raw: ", paste(faltando, collapse = ", "))
  }

  message("Total de registros brutos apos juncao/deduplicacao: ", nrow(df_raw))

  dedup_env <- Sys.getenv("CRIMES_AM_DEDUP_POR", unset = "")
  if (nzchar(dedup_env)) {
    dedup_por <- trimws(unlist(strsplit(dedup_env, ",")))
    dedup_por <- dedup_por[dedup_por != ""]
  }

  remover_duplicados_janela(df_raw, dias = dedup_dias, dedup_por = dedup_por)
}

