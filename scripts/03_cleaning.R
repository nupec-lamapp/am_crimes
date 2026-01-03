############################################################
# 03_cleaning.R
# Limpeza, classificação e enriquecimento dos dados
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
})

options(stringsAsFactors = FALSE)

if (file.exists("R/paths.R")) source("R/paths.R")
if (exists("crimes_am_set_paths")) crimes_am_set_paths()
if (!exists("DIR_PROCESSED", inherits = TRUE)) DIR_PROCESSED <- file.path("data", "processed")
if (!dir.exists(DIR_PROCESSED)) dir.create(DIR_PROCESSED, recursive = TRUE, showWarnings = FALSE)

# Utilitários compartilhados de classificação/enriquecimento
source("R/classification_utils.R")

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

  tipo_vulneravel <- classificar_crime_completo("Estupro de crianca")$tipo_principal
  if (is.null(tipo_vulneravel) || is.na(tipo_vulneravel) || !nzchar(tipo_vulneravel)) {
    tipo_vulneravel <- "Estupro de Vulneravel"
  }
  categoria_sexual <- dicionario_crimes$categoria[match("Estupro", dicionario_crimes$tipo_principal)]
  if (length(categoria_sexual) == 0 || is.na(categoria_sexual)) {
    categoria_sexual <- "Violencia Sexual"
  }
  if (!any(dicionario_crimes$tipo_principal == tipo_vulneravel)) {
    dicionario_crimes <- bind_rows(
      dicionario_crimes,
      tibble(
        categoria = categoria_sexual,
        tipo_principal = tipo_vulneravel,
        gravidade = "extrema",
        descricao = "Estupro de crianca/adolescente"
      )
    )
  }

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
  if (!file.exists(arq_val)) {
  readr::write_csv(validacao_template, arq_val)
  message("Template de validação manual criado em: ", arq_val)

  } else {
    message("Template de validacao manual ja existe em: ", arq_val)
  }

  df_final
}
