############################################################
# 02_processamento.R
# Consolidação, classificação e enriquecimento dos dados
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(stringi)
  library(lubridate)
  library(purrr)
  library(tibble)
})

options(stringsAsFactors = FALSE)

DIR_RAW       <- file.path("data", "raw")
DIR_PROCESSED <- file.path("data", "processed")

if (!dir.exists(DIR_PROCESSED)) dir.create(DIR_PROCESSED, recursive = TRUE, showWarnings = FALSE)

# Utilitários compartilhados
source("R/classification_utils.R")

############################################################
# Processar dados raw -> base consolidada
############################################################

processar_dados_raw <- function() {
  
  arquivos_raw <- list.files(DIR_RAW, pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos_raw) == 0) {
    stop("Nenhum arquivo encontrado em data/raw. Rode primeiro o 01_scraping.R.")
  }
  
  message("Lendo arquivos de data/raw/ ...")
  
  lista_dfs <- lapply(arquivos_raw, function(arq) {
    message(" - ", arq)
    readr::read_csv(arq, show_col_types = FALSE)
  })
  
  df_raw <- bind_rows(lista_dfs) %>%
    distinct(portal, data_publicacao, titulo, url, .keep_all = TRUE) %>%
    mutate(
      data_publicacao = as.Date(data_publicacao)
    )
  
  message("Total de registros brutos após junção/deduplicação: ", nrow(df_raw))
  
  # aplicar classificação completa
  df_class <- aplicar_classificacao_completa(df_raw)
  
  # enriquecer com variáveis auxiliares
  df_final <- df_class %>%
    mutate(
      crime_violento = eh_crime_violento_v2(titulo),
      genero_vitima       = sapply(titulo, extrair_genero),
      idade_vitima        = sapply(titulo, extrair_idade),
      faixa_etaria        = sapply(idade_vitima, classificar_faixa_etaria)
    )
  
  # salvar base consolidada
  arq_out <- file.path(DIR_PROCESSED, "crimes_classificados.csv")
  readr::write_csv(df_final, arq_out)
  message("Base consolidada salva em: ", arq_out)
  
  # dicionário de crimes
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
  
  arq_dic <- file.path(DIR_PROCESSED, "dicionario_tipos_crime.csv")
  readr::write_csv(dicionario_crimes, arq_dic)
  message("Dicionário de tipos de crime salvo em: ", arq_dic)
  
  # template de validação manual
  validacao_template <- df_final %>%
    select(titulo, tipo_principal, tipo_detalhado, gravidade) %>%
    mutate(
      validado      = NA_character_,
      tipo_corrigido = tipo_principal,
      observacoes   = NA_character_
    )
  
  arq_val <- file.path(DIR_PROCESSED, "validacao_manual_tipos.csv")
  readr::write_csv(validacao_template, arq_val)
  message("Template de validação manual criado em: ", arq_val)
  
  df_final
}

############################################################
# Execução quando rodar via Rscript
############################################################

# if (!interactive()) {
#   processar_dados_raw()
# }
