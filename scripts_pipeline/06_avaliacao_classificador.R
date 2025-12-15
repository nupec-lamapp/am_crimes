############################################################
# 06_avaliacao_classificador.R  [EXTRA / OPCIONAL]
# Avaliação da classificação de tipos de crime
# com base na validação manual.
#
# Atenção:
# - Este script NÃO faz parte do pipeline oficial (run_pipeline.R).
# - NÃO é utilizado diretamente pelo app Shiny.
# - Mantido apenas como material de apoio para auditoria detalhada.
#
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
  library(ggplot2)
  library(forcats)
})

options(stringsAsFactors = FALSE)

DIR_PROCESSED <- file.path("data", "processed")
DIR_OUTPUTS   <- "outputs"
DIR_EVAL      <- file.path(DIR_OUTPUTS, "eval")

if (!dir.exists(DIR_OUTPUTS)) dir.create(DIR_OUTPUTS, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(DIR_EVAL))    dir.create(DIR_EVAL,    recursive = TRUE, showWarnings = FALSE)

############################################################
# 1. Carregar bases
############################################################

arq_class <- file.path(DIR_PROCESSED, "crimes_classificados.csv")
arq_val   <- file.path(DIR_PROCESSED, "validacao_manual_tipos.csv")

if (!file.exists(arq_class)) {
  stop("Arquivo não encontrado: ", arq_class,
       "\nAntes rode o 02_processamento.R.")
}
if (!file.exists(arq_val)) {
  stop("Arquivo não encontrado: ", arq_val,
       "\nO template de validação manual não foi gerado ou preenchido.")
}

dados_class <- readr::read_csv(arq_class, show_col_types = FALSE)
dados_val   <- readr::read_csv(arq_val,   show_col_types = FALSE)

message("Registros em crimes_classificados: ", nrow(dados_class))
message("Registros em validacao_manual_tipos: ", nrow(dados_val))

############################################################
# 2. Preparar base de avaliação
############################################################

cols_necessarias_val <- c("titulo", "tipo_principal", "tipo_detalhado",
                          "gravidade", "validado", "tipo_corrigido", "observacoes")
faltando_val <- setdiff(cols_necessarias_val, names(dados_val))
if (length(faltando_val) > 0) {
  warning("Colunas esperadas ausentes em validacao_manual_tipos: ",
          paste(faltando_val, collapse = ", "),
          "\nO script seguirá, mas alguns campos podem ficar vazios.")
}

base_eval <- dados_val %>%
  select(any_of(cols_necessarias_val)) %>%
  left_join(
    dados_class %>%
      select(titulo, portal, data_publicacao, url, categoria,
             tipo_principal_alg = tipo_principal,
             gravidade_alg      = gravidade),
    by = "titulo"
  )

base_eval <- base_eval %>%
  mutate(
    tipo_corrigido = ifelse(is.na(tipo_corrigido) | tipo_corrigido == "",
                            tipo_principal_alg,
                            tipo_corrigido)
  ) %>%
  filter(!is.na(tipo_corrigido),
         !is.na(tipo_principal_alg))

nivels <- union(unique(base_eval$tipo_corrigido),
                unique(base_eval$tipo_principal_alg))

base_eval <- base_eval %>%
  mutate(
    tipo_true = factor(tipo_corrigido,     levels = nivels),
    tipo_pred = factor(tipo_principal_alg, levels = nivels)
  )

if (nrow(base_eval) == 0) {
  stop("Nenhuma linha disponível para avaliação.\n",
       "Preencha 'validacao_manual_tipos.csv' com pelo menos alguns registros.")
}

message("Registros utilizados na avaliação: ", nrow(base_eval))

############################################################
# 3. Métricas globais e matriz de confusão
############################################################

acuracia_geral <- mean(base_eval$tipo_pred == base_eval$tipo_true)

matriz_confusao <- as.data.frame(table(
  Verdadeiro = base_eval$tipo_true,
  Previsto   = base_eval$tipo_pred
))

readr::write_csv(
  matriz_confusao,
  file.path(DIR_EVAL, "matriz_confusao_tipo_principal.csv")
)

############################################################
# 4. Métricas por classe
############################################################

tipos <- nivels
metrics_list <- list()

for (tp in tipos) {
  TP <- sum(base_eval$tipo_true == tp & base_eval$tipo_pred == tp)
  FP <- sum(base_eval$tipo_true != tp & base_eval$tipo_pred == tp)
  FN <- sum(base_eval$tipo_true == tp & base_eval$tipo_pred != tp)
  TN <- sum(base_eval$tipo_true != tp & base_eval$tipo_pred != tp)
  
  precision <- if ((TP + FP) == 0) NA_real_ else TP / (TP + FP)
  recall    <- if ((TP + FN) == 0) NA_real_ else TP / (TP + FN)
  f1        <- if (is.na(precision) | is.na(recall) | (precision + recall) == 0) {
    NA_real_
  } else {
    2 * precision * recall / (precision + recall)
  }
  
  suporte <- sum(base_eval$tipo_true == tp)
  
  metrics_list[[tp]] <- tibble(
    tipo_principal = tp,
    suporte        = suporte,
    TP = TP, FP = FP, FN = FN, TN = TN,
    precision = round(precision, 3),
    recall    = round(recall, 3),
    f1        = round(f1, 3)
  )
}

metrics_por_classe <- bind_rows(metrics_list) %>%
  arrange(desc(suporte))

readr::write_csv(
  metrics_por_classe,
  file.path(DIR_EVAL, "metricas_por_tipo_principal.csv")
)

valid_classes <- metrics_por_classe %>%
  dplyr::filter(!is.na(f1))

f1_macro <- mean(valid_classes$f1, na.rm = TRUE)

f1_ponderado <- with(valid_classes, {
  sum(f1 * suporte, na.rm = TRUE) / sum(suporte, na.rm = TRUE)
})

acuracia_bal <- mean(valid_classes$recall, na.rm = TRUE)

metricas_globais <- tibble::tibble(
  acuracia_geral = round(acuracia_geral, 3),
  f1_macro       = round(f1_macro, 3),
  f1_ponderado   = round(f1_ponderado, 3),
  acuracia_bal   = round(acuracia_bal, 3)
)

readr::write_csv(
  metricas_globais,
  file.path(DIR_EVAL, "metricas_globais_tipo_principal.csv")
)

############################################################
# 5. Lista de erros para análise qualitativa
############################################################

erros <- base_eval %>%
  filter(tipo_true != tipo_pred) %>%
  arrange(data_publicacao) %>%
  select(
    data_publicacao,
    portal,
    titulo,
    tipo_previsto   = tipo_pred,
    tipo_verdadeiro = tipo_true,
    gravidade_alg,
    validado,
    tipo_corrigido,
    observacoes,
    url
  )

readr::write_csv(
  erros,
  file.path(DIR_EVAL, "erros_classificacao_detalhados.csv")
)

############################################################
# 6. Gráficos de avaliação
############################################################

df_plot_metrics <- metrics_por_classe %>%
  mutate(
    tipo_principal = fct_reorder(tipo_principal, suporte)
  )

p_suporte <- ggplot(df_plot_metrics,
                    aes(x = tipo_principal, y = suporte)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Suporte (nº de casos anotados) por tipo de crime",
    x     = "Tipo de crime",
    y     = "Número de casos na base de avaliação"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(DIR_EVAL, "01_suporte_por_tipo.png"),
  plot     = p_suporte,
  width    = 8,
  height   = 6,
  dpi      = 300
)

p_f1 <- ggplot(df_plot_metrics,
               aes(x = tipo_principal, y = f1)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "F1 por tipo de crime (base anotada)",
    x     = "Tipo de crime",
    y     = "F1"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(DIR_EVAL, "02_f1_por_tipo.png"),
  plot     = p_f1,
  width    = 8,
  height   = 6,
  dpi      = 300
)

############################################################
# 7. Resumo no console
############################################################

cat("\n=== AVALIAÇÃO DO CLASSIFICADOR DE TIPOS DE CRIME ===\n\n")

cat("Número de registros avaliados:", nrow(base_eval), "\n")
cat("Acurácia geral: ", round(acuracia_geral, 3), "\n\n")

cat("Métricas por tipo (top 10 por suporte):\n")
print(metrics_por_classe %>% slice_max(suporte, n = 10))

cat("\nMatriz de confusão salva em:\n - ",
    file.path(DIR_EVAL, "matriz_confusao_tipo_principal.csv"), "\n")
cat("Métricas por tipo salvas em:\n - ",
    file.path(DIR_EVAL, "metricas_por_tipo_principal.csv"), "\n")
cat("Lista de erros em:\n - ",
    file.path(DIR_EVAL, "erros_classificacao_detalhados.csv"), "\n")
cat("Gráficos em:\n - ",
    DIR_EVAL, "/01_suporte_por_tipo.png\n - ",
    DIR_EVAL, "/02_f1_por_tipo.png\n", sep = "")

cat("\n=== FIM DA AVALIAÇÃO (04) ===\n")
