############################################################
# visualizacao_lacunas.R
# Visualização de calendário de lacunas na coleta
# Mostra dias coletados, faltando e períodos sem dados
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(purrr)
})

#' Preparar dados para visualização de calendário
#' 
#' Cria uma grade de dias com status de coleta para visualização
#' 
#' @param dados_processados Data frame com dados processados
#' @param portal Portal específico (NULL para todos)
#' @param data_inicio Data de início do período
#' @param data_fim Data de fim do período
#' @return Data frame com grade de dias e status
#' @export
preparar_calendario_lacunas <- function(
  dados_processados,
  portal = NULL,
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
  
  # Filtrar por portal se especificado
  dados_filtrados <- dados_processados
  if (!is.null(portal)) {
    dados_filtrados <- dados_filtrados %>%
      filter(.data$portal == portal)
  }
  
  if (nrow(dados_filtrados) == 0) {
    return(tibble())
  }
  
  # Converter data
  dados_filtrados <- dados_filtrados %>%
    mutate(data = as.Date(.data[[col_data]]))
  
  # Determinar intervalo
  if (is.null(data_inicio)) {
    data_inicio <- min(dados_filtrados$data, na.rm = TRUE)
  } else {
    data_inicio <- as.Date(data_inicio)
  }
  
  if (is.null(data_fim)) {
    data_fim <- max(dados_filtrados$data, na.rm = TRUE)
  } else {
    data_fim <- as.Date(data_fim)
  }
  
  # Criar sequência de dias
  dias_seq <- seq(data_inicio, data_fim, by = "day")
  
  # Obter dias com dados
  dias_com_dados <- unique(dados_filtrados$data[dados_filtrados$data >= data_inicio & 
                                                 dados_filtrados$data <= data_fim])
  
  meses_pt <- c(
    "Janeiro", "Fevereiro", "Março", "Abril",
    "Maio", "Junho", "Julho", "Agosto",
    "Setembro", "Outubro", "Novembro", "Dezembro"
  )
  
  # Criar grade de calendário
  calendario <- tibble(
    data = dias_seq,
    ano = year(dias_seq),
    mes = month(dias_seq),
    dia = day(dias_seq),
    dia_semana = wday(dias_seq, week_start = 1),  # 1 = segunda-feira
    semana_ano = isoweek(dias_seq),
    tem_dados = dias_seq %in% dias_com_dados,
    status = ifelse(dias_seq %in% dias_com_dados, "coletado", "faltando")
  ) %>%
    mutate(
    mes_nome = meses_pt[mes],
    mes_ano = sprintf("%s %d", mes_nome, ano),
      status_display = ifelse(tem_dados, "✓ Coletado", "✗ Faltando")
    )
  
  calendario
}

#' Resumir lacunas por mês
#' 
#' @param calendario Data frame retornado por preparar_calendario_lacunas()
#' @return Data frame com resumo mensal
#' @export
resumir_lacunas_mensal <- function(calendario) {
  if (nrow(calendario) == 0) {
    return(tibble())
  }
  
  calendario %>%
    group_by(.data$mes_ano, .data$ano, .data$mes) %>%
    summarise(
      dias_total = n(),
      dias_coletados = sum(.data$tem_dados),
      dias_faltando = .data$dias_total - .data$dias_coletados,
      cobertura_pct = round(100 * .data$dias_coletados / .data$dias_total, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(.data$ano), desc(.data$mes)) %>%
    mutate(
      status_mes = case_when(
        cobertura_pct == 100 ~ "Completo",
        cobertura_pct >= 80 ~ "Quase completo",
        cobertura_pct >= 50 ~ "Parcial",
        cobertura_pct > 0 ~ "Incompleto",
        TRUE ~ "Sem dados"
      )
    )
}

#' Identificar períodos críticos de lacunas
#' 
#' Identifica sequências de dias faltando que podem ser problemáticas
#' 
#' @param calendario Data frame retornado por preparar_calendario_lacunas()
#' @param min_dias_sequencia Mínimo de dias consecutivos faltando para considerar crítico (padrão: 7)
#' @return Data frame com períodos críticos
#' @export
identificar_periodos_criticos <- function(calendario, min_dias_sequencia = 7) {
  if (nrow(calendario) == 0) {
    return(tibble())
  }
  
  calendario_ordenado <- calendario %>%
    arrange(.data$data) %>%
    mutate(
      mudou_status = .data$tem_dados != lag(.data$tem_dados, default = .data$tem_dados[1]),
      grupo = cumsum(.data$mudou_status)
    )
  
  periodos_criticos <- calendario_ordenado %>%
    filter(!.data$tem_dados) %>%
    group_by(.data$grupo) %>%
    summarise(
      data_inicio = min(.data$data),
      data_fim = max(.data$data),
      dias_sequencia = n(),
      .groups = "drop"
    ) %>%
    filter(.data$dias_sequencia >= min_dias_sequencia) %>%
    mutate(
      periodo = sprintf("%s a %s", 
                       format(.data$data_inicio, "%d/%m/%Y"),
                       format(.data$data_fim, "%d/%m/%Y")),
      criticidade = case_when(
        .data$dias_sequencia >= 30 ~ "Crítico",
        .data$dias_sequencia >= 14 ~ "Alto",
        TRUE ~ "Médio"
      )
    ) %>%
    arrange(desc(.data$dias_sequencia))
  
  periodos_criticos
}

#' Calcular estatísticas de cobertura
#' 
#' @param calendario Data frame retornado por preparar_calendario_lacunas()
#' @return Lista com estatísticas
#' @export
calcular_estatisticas_cobertura <- function(calendario) {
  if (nrow(calendario) == 0) {
    return(list(
      total_dias = 0,
      dias_coletados = 0,
      dias_faltando = 0,
      cobertura_pct = 0,
      maior_lacuna = 0
    ))
  }
  
  total_dias <- nrow(calendario)
  dias_coletados <- sum(calendario$tem_dados)
  dias_faltando <- total_dias - dias_coletados
  cobertura_pct <- round(100 * dias_coletados / total_dias, 1)
  
  # Calcular maior lacuna
  periodos_criticos <- identificar_periodos_criticos(calendario, min_dias_sequencia = 1)
  maior_lacuna <- if (nrow(periodos_criticos) > 0) {
    max(periodos_criticos$dias_sequencia)
  } else {
    0
  }
  
  list(
    total_dias = total_dias,
    dias_coletados = dias_coletados,
    dias_faltando = dias_faltando,
    cobertura_pct = cobertura_pct,
    maior_lacuna = maior_lacuna
  )
}

