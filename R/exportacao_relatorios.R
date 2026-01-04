############################################################
# exportacao_relatorios.R
# Funções para exportação de relatórios e alertas
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(readr)
})

#' Gerar relatório de cobertura em CSV
#' 
#' @param dados_processados Data frame com dados processados
#' @param inventario Data frame com inventário de arquivos
#' @param sugestoes Data frame com sugestões de coleta
#' @param arquivo_saida Caminho do arquivo de saída
#' @param data_inicio Data de início
#' @param data_fim Data de fim
#' @return Caminho do arquivo gerado
#' @export
gerar_relatorio_cobertura_csv <- function(
  dados_processados,
  inventario = NULL,
  sugestoes = NULL,
  arquivo_saida = NULL,
  data_inicio = NULL,
  data_fim = NULL
) {
  if (is.null(arquivo_saida)) {
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    arquivo_saida <- file.path("outputs", sprintf("relatorio_cobertura_%s.csv", timestamp))
  }
  
  # Criar diretório se não existir
  dir_output <- dirname(arquivo_saida)
  if (!dir.exists(dir_output)) {
    dir.create(dir_output, recursive = TRUE)
  }
  
  # Determinar coluna de data
  col_data <- if ("data_publicacao" %in% names(dados_processados)) {
    "data_publicacao"
  } else if ("data_pub" %in% names(dados_processados)) {
    "data_pub"
  } else {
    return(NULL)
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
  
  # Resumo por portal
  resumo_portal <- dados_filtrados %>%
    group_by(.data$portal) %>%
    summarise(
      registros = n(),
      dias_coletados = n_distinct(.data$data),
      primeiro_registro = min(.data$data, na.rm = TRUE),
      ultimo_registro = max(.data$data, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Adicionar informações do inventário se disponível
  if (!is.null(inventario) && nrow(inventario) > 0) {
    inventario_resumo <- inventario %>%
      group_by(.data$portal) %>%
      summarise(
        n_arquivos = n(),
        tamanho_total_mb = sum(.data$tamanho_kb, na.rm = TRUE) / 1024,
        arquivos_processados = sum(.data$processado, na.rm = TRUE),
        .groups = "drop"
      )
    
    resumo_portal <- resumo_portal %>%
      left_join(inventario_resumo, by = "portal") %>%
      mutate(
        tamanho_total_mb = ifelse(is.na(.data$tamanho_total_mb), 0, .data$tamanho_total_mb),
        n_arquivos = ifelse(is.na(.data$n_arquivos), 0, .data$n_arquivos),
        arquivos_processados = ifelse(is.na(.data$arquivos_processados), 0, .data$arquivos_processados)
      )
  }
  
  # Adicionar sugestões se disponíveis
  if (!is.null(sugestoes) && nrow(sugestoes) > 0) {
    sugestoes_resumo <- sugestoes %>%
      group_by(.data$portal) %>%
      summarise(
        n_sugestoes = n(),
        total_dias_faltando = sum(.data$`Dias Faltando`, na.rm = TRUE),
        .groups = "drop"
      )
    
    resumo_portal <- resumo_portal %>%
      left_join(sugestoes_resumo, by = "portal") %>%
      mutate(
        n_sugestoes = ifelse(is.na(.data$n_sugestoes), 0, .data$n_sugestoes),
        total_dias_faltando = ifelse(is.na(.data$total_dias_faltando), 0, .data$total_dias_faltando)
      )
  }
  
  # Adicionar metadados
  resumo_portal <- resumo_portal %>%
    mutate(
      periodo_inicio = data_inicio,
      periodo_fim = data_fim,
      data_geracao = Sys.time()
    )
  
  # Salvar CSV
  write_csv(resumo_portal, arquivo_saida)
  
  arquivo_saida
}

#' Verificar alertas de lacunas críticas
#' 
#' @param dados_processados Data frame com dados processados
#' @param portal Portal específico (NULL para todos)
#' @param data_inicio Data de início
#' @param data_fim Data de fim
#' @param limite_dias_critico Número mínimo de dias consecutivos para alerta
#' @return Lista com alertas
#' @export
verificar_alertas_lacunas <- function(
  dados_processados,
  portal = NULL,
  data_inicio = NULL,
  data_fim = NULL,
  limite_dias_critico = 7
) {
  if (is.null(dados_processados) || nrow(dados_processados) == 0) {
    return(list(
      alertas = tibble(),
      total_alertas = 0,
      criticidade_alta = 0
    ))
  }
  
  # Determinar coluna de data
  col_data <- if ("data_publicacao" %in% names(dados_processados)) {
    "data_publicacao"
  } else if ("data_pub" %in% names(dados_processados)) {
    "data_pub"
  } else {
    return(list(
      alertas = tibble(),
      total_alertas = 0,
      criticidade_alta = 0
    ))
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
  
  # Filtrar por portal se especificado
  if (!is.null(portal)) {
    dados <- dados %>% filter(.data$portal == portal)
  }
  
  # Criar sequência de dias
  sequencia_dias <- seq(data_inicio, data_fim, by = "day")
  dias_com_dados <- unique(dados$data)
  dias_faltando <- setdiff(sequencia_dias, dias_com_dados)
  
  if (length(dias_faltando) == 0) {
    return(list(
      alertas = tibble(),
      total_alertas = 0,
      criticidade_alta = 0
    ))
  }
  
  # Identificar períodos consecutivos de lacunas
  dias_ordenados <- sort(dias_faltando)
  grupos <- c()
  grupo_atual <- 1
  
  for (i in seq_along(dias_ordenados)) {
    if (i == 1) {
      grupos[i] <- grupo_atual
    } else {
      if (as.integer(difftime(dias_ordenados[i], dias_ordenados[i-1], units = "days")) == 1) {
        grupos[i] <- grupo_atual
      } else {
        grupo_atual <- grupo_atual + 1
        grupos[i] <- grupo_atual
      }
    }
  }
  
  # Agrupar lacunas
  lacunas_periodos <- tibble(
    data = dias_ordenados,
    grupo = grupos
  ) %>%
    group_by(.data$grupo) %>%
    summarise(
      data_inicio = min(.data$data),
      data_fim = max(.data$data),
      dias_consecutivos = n(),
      .groups = "drop"
    ) %>%
    filter(.data$dias_consecutivos >= limite_dias_critico) %>%
    mutate(
      criticidade = case_when(
        .data$dias_consecutivos >= 30 ~ "Crítica",
        .data$dias_consecutivos >= 14 ~ "Alta",
        TRUE ~ "Média"
      ),
      portal = ifelse(is.null(portal), "Todos", portal)
    ) %>%
    arrange(desc(.data$dias_consecutivos))
  
  list(
    alertas = lacunas_periodos,
    total_alertas = nrow(lacunas_periodos),
    criticidade_alta = sum(lacunas_periodos$criticidade %in% c("Crítica", "Alta"))
  )
}

#' Gerar histórico de coletas
#' 
#' @param inventario Data frame com inventário de arquivos
#' @return Data frame com histórico
#' @export
gerar_historico_coletas <- function(inventario) {
  if (is.null(inventario) || nrow(inventario) == 0) {
    return(tibble())
  }
  
  historico <- inventario %>%
    mutate(
      mes_coleta = floor_date(.data$data_criacao, "month"),
      ano_coleta = year(.data$data_criacao)
    ) %>%
    group_by(.data$portal, .data$mes_coleta, .data$ano_coleta) %>%
    summarise(
      n_coletas = n(),
      total_registros_estimado = sum(.data$dias_coletados, na.rm = TRUE),
      tamanho_total_mb = sum(.data$tamanho_kb, na.rm = TRUE) / 1024,
      .groups = "drop"
    ) %>%
    arrange(desc(.data$ano_coleta), desc(.data$mes_coleta), .data$portal)
  
  historico
}



