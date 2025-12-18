############################################################
# 01_exportar_para_validacao.R
# Gera CSV com casos prioritÃ¡rios para validaÃ§Ã£o manual
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

options(stringsAsFactors = FALSE)

DIR_PROCESSED <- file.path("data", "processed")
DIR_EXPORTS   <- file.path("data", "exports")

if (!dir.exists(DIR_EXPORTS)) {
  dir.create(DIR_EXPORTS, recursive = TRUE, showWarnings = FALSE)
}

arq_class <- file.path(DIR_PROCESSED, "crimes_classificados.csv")
arq_nlp   <- file.path(DIR_PROCESSED, "crimes_classificados_nlp.csv")

if (!file.exists(arq_class)) {
  stop("Arquivo crimes_classificados.csv nÃ£o encontrado em data/processed/. Rode a pipeline primeiro.")
}

message("Lendo crimes_classificados.csv ...")
df <- readr::read_csv(arq_class, show_col_types = FALSE)

if (file.exists(arq_nlp)) {
  message("Lendo crimes_classificados_nlp.csv para sugestÃµes do modelo...")
  df_nlp <- readr::read_csv(arq_nlp, show_col_types = FALSE)

  df <- df %>%
    left_join(
      df_nlp %>%
        select(any_of(c("url", "titulo", "tipo_ml", "prob_tipo_ml"))),
      by = c("url", "titulo")
    )
}

if (!"tipo_ml" %in% names(df))      df$tipo_ml      <- NA_character_
if (!"prob_tipo_ml" %in% names(df)) df$prob_tipo_ml <- NA_real_

df_validar <- df %>%
  mutate(
    nao_classificado = categoria == "Outros" |
      tipo_principal == "Não classificado com precisão" |
      tipo_principal == "Não classificado com precisão"
  ) %>%
  filter(
    nao_classificado |
      (!is.na(tipo_ml) & tipo_ml != tipo_principal & prob_tipo_ml >= 0.8)
  ) %>%
  select(
    data_publicacao, portal, titulo, url,
    categoria_atual       = categoria,
    tipo_principal_atual  = tipo_principal,
    gravidade_atual       = gravidade,
    tipo_sugerido_ml      = tipo_ml,
    prob_tipo_ml
  ) %>%
  mutate(
    tipo_corrigido       = tipo_principal_atual,
    categoria_corrigida  = categoria_atual,
    gravidade_corrigida  = gravidade_atual,
    validado             = NA_character_,
    observacoes          = NA_character_
  )

if (nrow(df_validar) == 0) {
  message("Nenhum caso candidato Ã  validaÃ§Ã£o encontrado nas condiÃ§Ãµes atuais.")
} else {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  arq_out <- file.path(DIR_EXPORTS, paste0("para_validacao_", ts, ".csv"))
  readr::write_csv(df_validar, arq_out)
  message("Arquivo para validaÃ§Ã£o salvo em: ", arq_out)
}
