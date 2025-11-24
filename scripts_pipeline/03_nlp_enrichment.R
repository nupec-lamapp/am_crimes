############################################################
# 03_nlp_enrichment.R
# Classificação automática de tipologia (NLP simples)
# Usando tidymodels + textrecipes (TF-IDF)
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tidymodels)
  library(textrecipes)
  library(forcats)
  library(tidyr)
})

options(stringsAsFactors = FALSE)

DIR_PROCESSED <- file.path("data", "processed")
DIR_MODELS    <- "models"

if (!dir.exists(DIR_MODELS)) dir.create(DIR_MODELS, showWarnings = FALSE)

modelo_tipo_path <- file.path(DIR_MODELS, "modelo_tipo_principal.rds")

treinar_modelo_tipo <- function(df) {
  df_treino <- df %>%
    filter(
      !is.na(tipo_principal),
      !is.na(titulo),
      nchar(titulo) >= 10,
      !tipo_principal %in% c(
        "Não classificado com precisão",
        "N�o classificado com precis�o"
      )
    )

  if (nrow(df_treino) < 50) {
    stop("Poucos exemplos rotulados para treinar o modelo de NLP.")
  }

  df_treino <- df_treino %>%
    mutate(
      tipo_principal = forcats::fct_lump_min(tipo_principal, min = 10)
    )

  set.seed(123)
  split <- initial_split(df_treino, strata = tipo_principal)
  train_data <- training(split)

  recipe_tipo <- recipe(tipo_principal ~ titulo, data = train_data) %>%
    step_tokenize(titulo) %>%
    step_stopwords(titulo, language = "pt") %>%
    step_tokenfilter(titulo, max_tokens = 5000) %>%
    step_tfidf(titulo)

  modelo_spec <- multinom_reg(penalty = tune(), mixture = 1) %>%
    set_engine("glmnet")

  grid <- grid_regular(penalty(range = c(-4, 0)), levels = 5)

  wf <- workflow() %>%
    add_model(modelo_spec) %>%
    add_recipe(recipe_tipo)

  set.seed(123)
  folds <- vfold_cv(train_data, v = 5, strata = tipo_principal)

  resultados <- tune_grid(
    wf,
    resamples = folds,
    grid = grid,
    metrics = metric_set(accuracy)
  )

  melhores <- select_best(resultados, "accuracy")

  modelo_final <- finalize_workflow(wf, melhores) %>%
    fit(train_data)

  metrica_final <- collect_metrics(resultados) %>%
    dplyr::filter(.metric == "accuracy") %>%
    arrange(desc(mean)) %>%
    slice(1)

  message(
    sprintf(
      "Modelo NLP treinado para tipo_principal. Acurácia média (cv): %.3f",
      metrica_final$mean
    )
  )

  saveRDS(modelo_final, modelo_tipo_path)
  modelo_final
}

carregar_ou_treinar_modelo_tipo <- function(df) {
  if (file.exists(modelo_tipo_path)) {
    tryCatch(
      {
        message("Carregando modelo NLP salvo em disco...")
        readRDS(modelo_tipo_path)
      },
      error = function(e) {
        message("Falha ao carregar modelo salvo. Re-treinando. Erro: ", e$message)
        treinar_modelo_tipo(df)
      }
    )
  } else {
    treinar_modelo_tipo(df)
  }
}

enriquecer_com_nlp <- function(
  arq_in  = file.path(DIR_PROCESSED, "crimes_classificados.csv"),
  arq_out = file.path(DIR_PROCESSED, "crimes_classificados_nlp.csv")
) {
  if (!file.exists(arq_in)) {
    stop(sprintf("Arquivo de entrada não encontrado: %s", arq_in))
  }

  message("Lendo base consolidada: ", arq_in)
  df <- readr::read_csv(arq_in, show_col_types = FALSE)

  if (!"titulo" %in% names(df)) {
    stop("Coluna 'titulo' não encontrada em crimes_classificados.csv")
  }
  if (!"tipo_principal" %in% names(df)) {
    stop("Coluna 'tipo_principal' não encontrada em crimes_classificados.csv")
  }

  modelo <- carregar_ou_treinar_modelo_tipo(df)

  message("Gerando predições de tipologia com modelo NLP...")
  preds_classe <- predict(modelo, new_data = df, type = "class")
  preds_prob   <- predict(modelo, new_data = df, type = "prob")

  max_prob <- preds_prob %>%
    mutate(.row_id = dplyr::row_number()) %>%
    tidyr::pivot_longer(-.row_id, names_to = "classe", values_to = "prob") %>%
    group_by(.row_id) %>%
    slice_max(prob, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(.row_id)

  df_saida <- df %>%
    mutate(
      tipo_ml        = preds_classe$.pred_class,
      prob_tipo_ml   = max_prob$prob,
      tipo_ml_classe = max_prob$classe
    )

  readr::write_csv(df_saida, arq_out)
  message("Base enriquecida com NLP salva em: ", arq_out)

  invisible(df_saida)
}

if (!interactive()) {
  enriquecer_com_nlp()
}

