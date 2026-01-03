############################################################
# analise_cobertura_temporal.R
# Análise de cobertura temporal com gráficos
# Evolução da cobertura e comparação entre portais
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(purrr)
  library(tidyr)
})

#' Preparar dados para análise temporal de cobertura
#' 
#' Cria série temporal de cobertura por portal
#' 
#' @param dados_processados Data frame com dados processados
#' @param data_inicio Data de início do período
#' @param data_fim Data de fim do período
#' @param agrupamento Nível de agrupamento: "mes", "semana", "dia" (padrão: "mes")
#' @return Data frame com série temporal
#' @export
preparar_serie_temporal_cobertura <- function(
  dados_processados,
  data_inicio = NULL,
  data_fim = NULL,
  agrupamento = "mes"
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
    return(tibble())
  }
  
  # Criar sequência de períodos
  if (agrupamento == "mes") {
    periodos <- seq(floor_date(data_inicio, "month"), 
                   floor_date(data_fim, "month"), 
                   by = "month")
    dados_filtrados <- dados_filtrados %>%
      mutate(periodo = floor_date(.data$data, "month"))
  } else if (agrupamento == "semana") {
    periodos <- seq(floor_date(data_inicio, "week"), 
                   floor_date(data_fim, "week"), 
                   by = "week")
    dados_filtrados <- dados_filtrados %>%
      mutate(periodo = floor_date(.data$data, "week"))
  } else {  # dia
    periodos <- seq(data_inicio, data_fim, by = "day")
    dados_filtrados <- dados_filtrados %>%
      mutate(periodo = .data$data)
  }
  
  # Calcular cobertura por portal e período
  cobertura_por_portal <- dados_filtrados %>%
    group_by(.data$portal, .data$periodo) %>%
    summarise(
      dias_com_dados = n_distinct(.data$data),
      registros = n(),
      .groups = "drop"
    )
  
  # Criar grade completa (todos os portais x todos os períodos)
  portais <- unique(dados_filtrados$portal)
  grade_completa <- tidyr::expand_grid(
    portal = portais,
    periodo = periodos
  )
  
  # Calcular dias totais em cada período
  if (agrupamento == "mes") {
    grade_completa <- grade_completa %>%
      mutate(
        dias_totais = as.integer(days_in_month(.data$periodo))
      )
  } else if (agrupamento == "semana") {
    grade_completa <- grade_completa %>%
      mutate(
        dias_totais = 7  # Semana tem 7 dias
      )
  } else {
    grade_completa <- grade_completa %>%
      mutate(
        dias_totais = 1  # Dia tem 1 dia
      )
  }
  
  # Juntar com dados reais
  serie_temporal <- grade_completa %>%
    left_join(cobertura_por_portal, by = c("portal", "periodo")) %>%
    mutate(
      dias_com_dados = ifelse(is.na(.data$dias_com_dados), 0, .data$dias_com_dados),
      registros = ifelse(is.na(.data$registros), 0, .data$registros),
      cobertura_pct = round(100 * .data$dias_com_dados / .data$dias_totais, 1),
      periodo_label = format(.data$periodo, ifelse(agrupamento == "mes", "%Y-%m", "%Y-%m-%d"))
    ) %>%
    arrange(.data$portal, .data$periodo)
  
  serie_temporal
}

#' Calcular tendência de cobertura
#' 
#' @param serie_temporal Data frame retornado por preparar_serie_temporal_cobertura()
#' @return Data frame com tendência por portal
#' @export
calcular_tendencia_cobertura <- function(serie_temporal) {
  if (nrow(serie_temporal) == 0) {
    return(tibble())
  }
  
  serie_temporal %>%
    group_by(.data$portal) %>%
    summarise(
      periodo_inicio = min(.data$periodo, na.rm = TRUE),
      periodo_fim = max(.data$periodo, na.rm = TRUE),
      cobertura_media = round(mean(.data$cobertura_pct, na.rm = TRUE), 1),
      cobertura_inicial = .data$cobertura_pct[which.min(.data$periodo)],
      cobertura_final = .data$cobertura_pct[which.max(.data$periodo)],
      tendencia = .data$cobertura_final - .data$cobertura_inicial,
      .groups = "drop"
    ) %>%
    mutate(
      direcao_tendencia = case_when(
        .data$tendencia > 5 ~ "Melhorando",
        .data$tendencia < -5 ~ "Piorando",
        TRUE ~ "Estável"
      )
    )
}


