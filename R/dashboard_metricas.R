############################################################
# dashboard_metricas.R
# Dashboard de métricas e KPIs para controle de coleta
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
})

#' Calcular KPIs principais de cobertura
#' 
#' @param dados_processados Data frame com dados processados
#' @param data_inicio Data de início do período
#' @param data_fim Data de fim do período
#' @return Lista com KPIs
#' @export
calcular_kpis_cobertura <- function(
  dados_processados,
  data_inicio = NULL,
  data_fim = NULL
) {
  if (is.null(dados_processados) || nrow(dados_processados) == 0) {
    return(list(
      cobertura_media_geral = 0,
      cobertura_media_por_portal = tibble(),
      portal_melhor_cobertura = NA_character_,
      portal_pior_cobertura = NA_character_,
      total_dias_periodo = 0,
      total_dias_coletados = 0,
      total_registros = 0,
      portais_ativos = 0
    ))
  }
  
  # Determinar coluna de data
  col_data <- if ("data_publicacao" %in% names(dados_processados)) {
    "data_publicacao"
  } else if ("data_pub" %in% names(dados_processados)) {
    "data_pub"
  } else {
    return(list(
      cobertura_media_geral = 0,
      cobertura_media_por_portal = tibble(),
      portal_melhor_cobertura = NA_character_,
      portal_pior_cobertura = NA_character_,
      total_dias_periodo = 0,
      total_dias_coletados = 0,
      total_registros = 0,
      portais_ativos = 0
    ))
  }
  
  # Converter data
  dados <- dados_processados %>%
    mutate(data = as.Date(.data[[col_data]]))
  
  # Determinar intervalo
  if (is.null(data_inicio)) {
    data_inicio <- min(dados$data, na.rm = TRUE)
  } else {
    data_inicio <- as.Date(data_inicio)
  }
  
  if (is.null(data_fim)) {
    data_fim <- max(dados$data, na.rm = TRUE)
  } else {
    data_fim <- as.Date(data_fim)
  }
  
  # Filtrar dados no intervalo
  dados_filtrados <- dados %>%
    filter(.data$data >= data_inicio, .data$data <= data_fim)
  
  if (nrow(dados_filtrados) == 0) {
    total_dias <- as.integer(difftime(data_fim, data_inicio, units = "days")) + 1
    return(list(
      cobertura_media_geral = 0,
      cobertura_media_por_portal = tibble(),
      portal_melhor_cobertura = NA_character_,
      portal_pior_cobertura = NA_character_,
      total_dias_periodo = total_dias,
      total_dias_coletados = 0,
      total_registros = 0,
      portais_ativos = 0
    ))
  }
  
  # Calcular métricas gerais
  total_dias <- as.integer(difftime(data_fim, data_inicio, units = "days")) + 1
  dias_coletados_geral <- n_distinct(dados_filtrados$data)
  cobertura_media_geral <- round(100 * dias_coletados_geral / total_dias, 1)
  total_registros <- nrow(dados_filtrados)
  
  # Calcular por portal
  cobertura_por_portal <- dados_filtrados %>%
    group_by(.data$portal) %>%
    summarise(
      dias_coletados = n_distinct(.data$data),
      registros = n(),
      cobertura_pct = round(100 * .data$dias_coletados / total_dias, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(.data$cobertura_pct))
  
  # Identificar melhor e pior portal
  if (nrow(cobertura_por_portal) > 0) {
    portal_melhor <- cobertura_por_portal$portal[1]
    portal_pior <- cobertura_por_portal$portal[nrow(cobertura_por_portal)]
  } else {
    portal_melhor <- NA_character_
    portal_pior <- NA_character_
  }
  
  list(
    cobertura_media_geral = cobertura_media_geral,
    cobertura_media_por_portal = cobertura_por_portal,
    portal_melhor_cobertura = portal_melhor,
    portal_pior_cobertura = portal_pior,
    total_dias_periodo = total_dias,
    total_dias_coletados = dias_coletados_geral,
    total_registros = total_registros,
    portais_ativos = nrow(cobertura_por_portal)
  )
}

#' Calcular métricas de qualidade dos dados
#' 
#' @param dados_processados Data frame com dados processados
#' @return Lista com métricas de qualidade
#' @export
calcular_metricas_qualidade <- function(dados_processados) {
  if (is.null(dados_processados) || nrow(dados_processados) == 0) {
    return(list(
      pct_com_titulo = 0,
      pct_com_texto = 0,
      pct_com_data = 0,
      pct_com_local = 0,
      registros_duplicados = 0
    ))
  }
  
  total <- nrow(dados_processados)
  
  # Verificar campos obrigatórios
  tem_titulo <- if ("titulo" %in% names(dados_processados)) {
    sum(!is.na(dados_processados$titulo) & nchar(trimws(dados_processados$titulo)) > 0)
  } else if ("title" %in% names(dados_processados)) {
    sum(!is.na(dados_processados$title) & nchar(trimws(dados_processados$title)) > 0)
  } else {
    0
  }
  
  tem_texto <- if ("texto" %in% names(dados_processados)) {
    sum(!is.na(dados_processados$texto) & nchar(trimws(dados_processados$texto)) > 0)
  } else if ("text" %in% names(dados_processados)) {
    sum(!is.na(dados_processados$text) & nchar(trimws(dados_processados$text)) > 0)
  } else {
    0
  }
  
  tem_data <- if ("data_publicacao" %in% names(dados_processados)) {
    sum(!is.na(dados_processados$data_publicacao))
  } else if ("data_pub" %in% names(dados_processados)) {
    sum(!is.na(dados_processados$data_pub))
  } else {
    0
  }
  
  tem_local <- if ("local" %in% names(dados_processados)) {
    sum(!is.na(dados_processados$local) & nchar(trimws(dados_processados$local)) > 0)
  } else {
    0
  }
  
  # Detectar duplicados (por título e data)
  col_titulo <- if ("titulo" %in% names(dados_processados)) "titulo" else "title"
  col_data <- if ("data_publicacao" %in% names(dados_processados)) "data_publicacao" else "data_pub"
  
  duplicados <- dados_processados %>%
    group_by(.data[[col_titulo]], .data[[col_data]]) %>%
    summarise(n = n(), .groups = "drop") %>%
    filter(.data$n > 1) %>%
    nrow()
  
  list(
    pct_com_titulo = round(100 * tem_titulo / total, 1),
    pct_com_texto = round(100 * tem_texto / total, 1),
    pct_com_data = round(100 * tem_data / total, 1),
    pct_com_local = round(100 * tem_local / total, 1),
    registros_duplicados = duplicados
  )
}

#' Comparar portais
#' 
#' @param dados_processados Data frame com dados processados
#' @param data_inicio Data de início
#' @param data_fim Data de fim
#' @return Data frame com comparação
#' @export
comparar_portais <- function(
  dados_processados,
  data_inicio = NULL,
  data_fim = NULL
) {
  if (is.null(dados_processados) || nrow(dados_processados) == 0) {
    return(tibble())
  }
  
  # Determinar coluna de data
  col_data <- if ("data_publicacao" %in% names(dados_processados)) {
    "data_publicacao"
  } else if ("data_pub" %in% names(dados_processados)) {
    "data_pub"
  } else {
    return(tibble())
  }
  
  dados <- dados_processados %>%
    mutate(data = as.Date(.data[[col_data]]))
  
  if (is.null(data_inicio)) {
    data_inicio <- min(dados$data, na.rm = TRUE)
  } else {
    data_inicio <- as.Date(data_inicio)
  }
  
  if (is.null(data_fim)) {
    data_fim <- max(dados$data, na.rm = TRUE)
  } else {
    data_fim <- as.Date(data_fim)
  }
  
  dados_filtrados <- dados %>%
    filter(.data$data >= data_inicio, .data$data <= data_fim)
  
  if (nrow(dados_filtrados) == 0) {
    return(tibble())
  }
  
  total_dias <- as.integer(difftime(data_fim, data_inicio, units = "days")) + 1
  
  comparacao <- dados_filtrados %>%
    group_by(.data$portal) %>%
    summarise(
      registros = n(),
      dias_coletados = n_distinct(.data$data),
      cobertura_pct = round(100 * .data$dias_coletados / total_dias, 1),
      media_registros_dia = round(.data$registros / .data$dias_coletados, 1),
      primeiro_registro = min(.data$data, na.rm = TRUE),
      ultimo_registro = max(.data$data, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(.data$cobertura_pct))
  
  comparacao
}



