############################################################
# 03_cleaning.R
# Limpeza, classificação e enriquecimento dos dados
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
  library(stringdist)
})

options(stringsAsFactors = FALSE)

if (file.exists("R/paths.R")) source("R/paths.R")
if (exists("crimes_am_set_paths")) crimes_am_set_paths()
if (!exists("DIR_PROCESSED", inherits = TRUE)) DIR_PROCESSED <- file.path("data", "processed")
if (!dir.exists(DIR_PROCESSED)) dir.create(DIR_PROCESSED, recursive = TRUE, showWarnings = FALSE)

# Utilitários compartilhados de classificação/enriquecimento
source("R/classification_utils.R")
source("scripts/utils.R")

construir_componentes_dup <- function(edges_a, edges_b) {
  if (length(edges_a) == 0) {
    return(tibble(id_dup = integer(), base_id = integer()))
  }

  arestas <- tibble(id_a = as.integer(edges_a), id_b = as.integer(edges_b)) %>%
    distinct()
  ids <- sort(unique(c(arestas$id_a, arestas$id_b)))
  adj <- setNames(vector("list", length(ids)), as.character(ids))

  for (i in seq_len(nrow(arestas))) {
    a <- as.character(arestas$id_a[i])
    b <- as.character(arestas$id_b[i])
    adj[[a]] <- unique(c(adj[[a]], arestas$id_b[i]))
    adj[[b]] <- unique(c(adj[[b]], arestas$id_a[i]))
  }

  visitado <- setNames(rep(FALSE, length(ids)), as.character(ids))
  componentes <- list()

  for (id in ids) {
    chave <- as.character(id)
    if (visitado[[chave]]) next
    pilha <- id
    membros <- integer()

    while (length(pilha) > 0) {
      atual <- pilha[[length(pilha)]]
      pilha <- pilha[-length(pilha)]
      chave_atual <- as.character(atual)
      if (visitado[[chave_atual]]) next
      visitado[[chave_atual]] <- TRUE
      membros <- c(membros, atual)
      viz <- adj[[chave_atual]]
      if (length(viz) > 0) {
        pilha <- c(pilha, viz)
      }
    }
    componentes[[length(componentes) + 1]] <- membros
  }

  if (length(componentes) == 0) {
    return(tibble(id_dup = integer(), base_id = integer()))
  }

  bind_rows(lapply(componentes, function(membros) {
    tibble(
      id_dup = membros,
      base_id = min(membros)
    )
  }))
}

marcar_duplicados <- function(df, janela_dias = 3, limiar_sim = 0.85) {
  if (!"data_publicacao" %in% names(df) || !"titulo" %in% names(df) || nrow(df) < 2) {
    return(df %>% mutate(flag_duplicado = FALSE, dup_id = NA_character_))
  }

  df_aux <- df %>%
    mutate(
      data_publicacao = as.Date(data_publicacao),
      titulo_norm_comparacao = normalizar_titulo(titulo),
      .id_dup = dplyr::row_number()
    ) %>%
    select(.id_dup, portal, data_publicacao, titulo_norm_comparacao)

  offsets <- seq(-janela_dias, janela_dias)
  candidatos <- tidyr::crossing(df_aux, offset = offsets) %>%
    mutate(match_date = data_publicacao + offset) %>%
    select(
      id_a = .id_dup,
      portal_a = portal,
      titulo_a = titulo_norm_comparacao,
      match_date
    )

  alvo <- df_aux %>%
    transmute(
      id_b = .id_dup,
      portal_b = portal,
      titulo_b = titulo_norm_comparacao,
      match_date = data_publicacao
    )

  pares <- candidatos %>%
    inner_join(alvo, by = "match_date") %>%
    filter(id_a < id_b, portal_a != portal_b)

  if (nrow(pares) == 0) {
    return(df %>% mutate(flag_duplicado = FALSE, dup_id = NA_character_))
  }

  pares <- pares %>%
    mutate(sim = stringdist::stringsim(titulo_a, titulo_b, method = "jw")) %>%
    filter(sim >= limiar_sim)

  if (nrow(pares) == 0) {
    return(df %>% mutate(flag_duplicado = FALSE, dup_id = NA_character_))
  }

  mapa_componentes <- construir_componentes_dup(pares$id_a, pares$id_b)

  df %>%
    mutate(
      titulo_norm_comparacao = normalizar_titulo(titulo),
      .id_dup = dplyr::row_number()
    ) %>%
    left_join(mapa_componentes, by = c(".id_dup" = "id_dup")) %>%
    mutate(
      flag_duplicado = !is.na(base_id),
      dup_id = if_else(flag_duplicado, sprintf("DUP-%05d", base_id), NA_character_)
    ) %>%
    select(-.id_dup, -titulo_norm_comparacao, -base_id)
}

############################################################
# clean_and_enrich_data()
# Recebe df já parseado e retorna df final limpo + grava
# artefatos em data/processed/.
############################################################

clean_and_enrich_data <- function(df_parsed,
                                  dir_processed = DIR_PROCESSED) {

  if (is.null(df_parsed) || !is.data.frame(df_parsed) || nrow(df_parsed) == 0) {
    stop("Objeto df_parsed vazio ou inválido em clean_and_enrich_data().")
  }

  cols_essenciais <- c("portal", "data_publicacao", "titulo", "url")
  faltando <- setdiff(cols_essenciais, names(df_parsed))
  if (length(faltando) > 0) {
    stop("Colunas essenciais ausentes em df_parsed: ", paste(faltando, collapse = ", "))
  }

  df_class <- aplicar_classificacao_completa(df_parsed)

  df_final <- df_class %>%
    mutate(
      crime_violento = eh_crime_violento_v2(titulo),
      genero_vitima  = vapply(titulo, extrair_genero, character(1)),
      idade_vitima   = vapply(titulo, extrair_idade, integer(1)),
      faixa_etaria   = vapply(idade_vitima, classificar_faixa_etaria, character(1))
    )

  df_final <- marcar_duplicados(df_final, janela_dias = 3, limiar_sim = 0.85)

  arq_out <- file.path(dir_processed, "crimes_classificados.csv")
  readr::write_csv(df_final, arq_out)
  message("Base consolidada salva em: ", arq_out)

  dic_observado <- df_final %>%
    count(categoria, tipo_principal, gravidade, sort = TRUE)

  arq_dic_obs <- file.path(dir_processed, "dicionario_tipos_observado.csv")
  readr::write_csv(dic_observado, arq_dic_obs)
  message("Dicionário observado de tipos salvo em: ", arq_dic_obs)

  dicionario_crimes <- tibble(
    categoria = c(
      "Crime Letal Violento",
      "Crime Letal Violento",
      "Crime Letal Violento",
      "Crime Letal Violento",
      "Violência Sexual",
      "Violência Sexual",
      "Violência Sexual",
      "Violência Doméstica",
      "Violência Doméstica",
      "Roubo Violento",
      "Roubo Violento",
      "Roubo Violento",
      "Lesão Corporal Grave",
      "Lesão Corporal Grave",
      "Lesão Corporal Grave",
      "Privação de Liberdade",
      "Crime Organizado",
      "Violência Lethal Contextual"
    ),
    tipo_principal = c(
      "Homicídio",
      "Tentativa de Homicídio",
      "Feminicídio",
      "Chacina",
      "Estupro",
      "Tentativa de Estupro",
      "Abuso Sexual",
      "Agressão Doméstica",
      "Feminicídio Doméstico",
      "Latrocínio",
      "Roubo à Mão Armada",
      "Roubo/Assalto",
      "Esfaqueamento",
      "Espancamento",
      "Agressão com Objeto",
      "Sequestro/Cárcere Privado",
      "Tráfico de Drogas/Facções",
      "Acerto de Contas"
    ),
    gravidade = c(
      "extrema",
      "muito alta",
      "extrema",
      "extrema",
      "extrema",
      "muito alta",
      "muito alta",
      "alta",
      "extrema",
      "extrema",
      "muito alta",
      "alta",
      "alta",
      "alta",
      "alta",
      "muito alta",
      "alta",
      "extrema"
    ),
    descricao = c(
      "Morte intencional consumada",
      "Tentativa de provocar morte",
      "Morte de mulher por parceiro íntimo",
      "Múltiplas mortes em mesmo evento",
      "Penetração sexual sem consentimento",
      "Tentativa de penetração sexual sem consentimento",
      "Contato sexual sem consentimento",
      "Agressão física entre parceiros",
      "Morte de mulher no contexto doméstico",
      "Morte durante roubo",
      "Roubo usando arma de fogo",
      "Roubo com confronto",
      "Lesão grave com faca/instrumento cortante",
      "Lesão grave com agressão física",
      "Lesão grave com objeto contundente",
      "Privação forçada de liberdade",
      "Distribuição/venda de drogas em contexto violento",
      "Execução por disputa ou vingança"
    )
  )

  arq_dic <- file.path(dir_processed, "dicionario_tipos_crime.csv")
  readr::write_csv(dicionario_crimes, arq_dic)
  message("Dicionário de tipos de crime salvo em: ", arq_dic)

  validacao_template <- df_final %>%
    select(titulo, tipo_principal, tipo_detalhado, gravidade) %>%
    mutate(
      validado       = NA_character_,
      tipo_corrigido = tipo_principal,
      observacoes    = NA_character_
    )

  arq_val <- file.path(dir_processed, "validacao_manual_tipos.csv")
  readr::write_csv(validacao_template, arq_val)
  message("Template de validação manual criado em: ", arq_val)

  df_final
}
