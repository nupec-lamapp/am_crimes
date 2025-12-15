############################################################
# 02_aplicar_validacao.R
# Aplica correções de tipologia provenientes da validação manual
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

options(stringsAsFactors = FALSE)

DIR_PROCESSED <- file.path("data", "processed")
DIR_EXPORTS   <- file.path("data", "exports")

arq_class <- file.path(DIR_PROCESSED, "crimes_classificados.csv")

if (!file.exists(arq_class)) {
  stop("Arquivo crimes_classificados.csv não encontrado em data/processed/. Rode a pipeline primeiro.")
}

arquivos_val <- if (dir.exists(DIR_EXPORTS)) {
  list.files(DIR_EXPORTS, pattern = "^para_validacao_.*\\.csv$", full.names = TRUE)
} else {
  character(0)
}

if (length(arquivos_val) == 0) {
  stop("Nenhum arquivo de validação encontrado em data/exports/. Rode 01_exportar_para_validacao.R e preencha o CSV antes.")
}

info <- file.info(arquivos_val)
arq_val <- arquivos_val[which.max(info$mtime)]

message("Lendo base principal ...")
df <- readr::read_csv(arq_class, show_col_types = FALSE)

message("Lendo arquivo de validação: ", arq_val)
dv <- readr::read_csv(arq_val, show_col_types = FALSE)

dv_aplicar <- dv %>%
  filter(
    !is.na(validado),
    tolower(trimws(validado)) %in% c("sim", "ok", "1", "true", "y", "yes")
  )

if (nrow(dv_aplicar) == 0) {
  message("Nenhuma linha marcada como validada (campo 'validado'). Nada a aplicar. Saindo sem alterar a base.")
} else {
  df2 <- df %>%
    left_join(
      dv_aplicar %>%
        select(
          url, titulo,
          tipo_corrigido,
          categoria_corrigida,
          gravidade_corrigida
        ),
      by = c("url", "titulo")
    ) %>%
    mutate(
      categoria      = dplyr::coalesce(categoria_corrigida, categoria),
      tipo_principal = dplyr::coalesce(tipo_corrigido, tipo_principal),
      gravidade      = dplyr::coalesce(gravidade_corrigida, gravidade)
    ) %>%
    select(-tipo_corrigido, -categoria_corrigida, -gravidade_corrigida)

  backup_path <- file.path(
    DIR_PROCESSED,
    paste0("crimes_classificados_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
  )
  readr::write_csv(df, backup_path)
  message("Backup salvo em: ", backup_path)

  readr::write_csv(df2, arq_class)
  message("Atualizado crimes_classificados.csv com correções da validação (arquivo aplicado: ", basename(arq_val), ").")
}
