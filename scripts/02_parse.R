############################################################
# 02_parse.R
# Leitura e padronização inicial dos dados brutos
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(purrr)
  library(tibble)
})

options(stringsAsFactors = FALSE)

DIR_RAW  <- file.path("data", "raw")

############################################################
# parse_raw_files()
# Lê todos os arquivos CSV em data/raw/, garante colunas
# mínimas e retorna um data.frame padronizado.
############################################################

parse_raw_files <- function(dir_raw = DIR_RAW) {
  if (!dir.exists(dir_raw)) {
    stop("Diretório data/raw/ não encontrado (", dir_raw, "). Rode 01_scraping.R primeiro.")
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

  message("Total de registros brutos após junção/deduplicação: ", nrow(df_raw))

  df_raw
}

