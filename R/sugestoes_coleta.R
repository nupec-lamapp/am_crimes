############################################################
# sugestoes_coleta.R
# Sistema de sugestões inteligentes de coleta
# Prioriza lacunas e sugere intervalos para coleta
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(tibble)
  library(purrr)
})

#' Gerar sugestões de coleta baseadas em lacunas
#' 
#' Analisa lacunas e gera sugestões priorizadas de intervalos para coleta
#' 
#' @param dados_processados Data frame com dados processados
#' @param portal Portal específico (NULL para todos)
#' @param data_inicio Data de início do período de análise
#' @param data_fim Data de fim do período de análise
#' @param max_sugestoes Número máximo de sugestões (padrão: 10)
#' @return Data frame com sugestões priorizadas
#' @export
gerar_sugestoes_coleta <- function(
  dados_processados,
  portal = NULL,
  data_inicio = NULL,
  data_fim = NULL,
  max_sugestoes = 10
) {
  if (is.null(dados_processados) || nrow(dados_processados) == 0) {
    return(tibble())
  }
  
  # Usar funções de visualização de lacunas
  if (!exists("preparar_calendario_lacunas", inherits = TRUE)) {
    if (file.exists("R/visualizacao_lacunas.R")) {
      source("R/visualizacao_lacunas.R", local = TRUE)
    } else {
      return(tibble())
    }
  }
  
  calendario <- preparar_calendario_lacunas(
    dados_processados = dados_processados,
    portal = portal,
    data_inicio = data_inicio,
    data_fim = data_fim
  )
  
  if (is.null(calendario) || nrow(calendario) == 0) {
    return(tibble())
  }
  
  # Identificar períodos críticos
  periodos_criticos <- identificar_periodos_criticos(calendario, min_dias_sequencia = 1)
  
  if (nrow(periodos_criticos) == 0) {
    return(tibble(
      prioridade = character(),
      periodo = character(),
      data_inicio = as.Date(character()),
      data_fim = as.Date(character()),
      dias_faltando = integer(),
      motivo = character(),
      estimativa_tempo = character()
    ))
  }
  
  # Agrupar lacunas próximas em sugestões
  sugestoes <- periodos_criticos %>%
    arrange(desc(.data$dias_sequencia), .data$data_inicio) %>%
    mutate(
      # Calcular prioridade baseada em criticidade e recência
      prioridade_score = case_when(
        .data$criticidade == "Crítico" ~ 100,
        .data$criticidade == "Alto" ~ 70,
        .data$criticidade == "Médio" ~ 40,
        TRUE ~ 20
      ),
      # Adicionar bônus para lacunas recentes (mais importantes)
      dias_atraso = as.integer(difftime(Sys.Date(), .data$data_fim, units = "days")),
      bonus_recencia = pmax(0, 30 - .data$dias_atraso),  # Bônus decresce com tempo
      prioridade_final = .data$prioridade_score + .data$bonus_recencia,
      # Classificar prioridade
      prioridade = case_when(
        .data$prioridade_final >= 100 ~ "Crítica",
        .data$prioridade_final >= 70 ~ "Alta",
        .data$prioridade_final >= 50 ~ "Média",
        TRUE ~ "Baixa"
      ),
      # Estimar tempo de coleta (aproximado: 1 minuto por dia)
      estimativa_minutos = ceiling(.data$dias_sequencia * 1.5),
      estimativa_tempo = ifelse(
        .data$estimativa_minutos < 60,
        sprintf("%d minutos", .data$estimativa_minutos),
        sprintf("%.1f horas", .data$estimativa_minutos / 60)
      ),
      # Formatar período
      periodo = sprintf("%s a %s",
                       format(.data$data_inicio, "%d/%m/%Y"),
                       format(.data$data_fim, "%d/%m/%Y")),
      motivo = sprintf("Lacuna de %d dias (%s)", 
                      .data$dias_sequencia, 
                      .data$criticidade)
    ) %>%
    select(
      prioridade,
      periodo,
      data_inicio,
      data_fim,
      dias_sequencia,
      motivo,
      estimativa_tempo,
      prioridade_final
    ) %>%
    rename(
      "Dias Faltando" = dias_sequencia
    ) %>%
    arrange(desc(.data$prioridade_final)) %>%
    head(max_sugestoes) %>%
    select(-prioridade_final)
  
  sugestoes
}

#' Agrupar sugestões próximas
#' 
#' Agrupa lacunas próximas (dentro de X dias) em uma única sugestão
#' 
#' @param sugestoes Data frame de sugestões
#' @param max_distancia_dias Distância máxima para agrupar (padrão: 7)
#' @return Data frame com sugestões agrupadas
#' @export
agrupar_sugestoes_proximas <- function(sugestoes, max_distancia_dias = 7) {
  if (nrow(sugestoes) == 0) {
    return(sugestoes)
  }
  
  sugestoes_ordenadas <- sugestoes %>%
    arrange(.data$data_inicio)
  
  grupos <- list()
  grupo_atual <- 1
  ultima_data_fim <- NULL
  
  for (i in 1:nrow(sugestoes_ordenadas)) {
    sugestao <- sugestoes_ordenadas[i, ]
    
    if (is.null(ultima_data_fim) || 
        as.integer(difftime(sugestao$data_inicio, ultima_data_fim, units = "days")) > max_distancia_dias) {
      # Nova sugestão agrupada
      grupos[[grupo_atual]] <- list(
        data_inicio = sugestao$data_inicio,
        data_fim = sugestao$data_fim,
        dias_total = sugestao$`Dias Faltando`,
        prioridade = sugestao$prioridade,
        sugestoes_originais = 1
      )
      ultima_data_fim <- sugestao$data_fim
      grupo_atual <- grupo_atual + 1
    } else {
      # Estender sugestão atual
      grupos[[grupo_atual - 1]]$data_fim <- max(grupos[[grupo_atual - 1]]$data_fim, sugestao$data_fim)
      grupos[[grupo_atual - 1]]$dias_total <- grupos[[grupo_atual - 1]]$dias_total + sugestao$`Dias Faltando`
      grupos[[grupo_atual - 1]]$sugestoes_originais <- grupos[[grupo_atual - 1]]$sugestoes_originais + 1
      ultima_data_fim <- grupos[[grupo_atual - 1]]$data_fim
    }
  }
  
  # Converter grupos em data frame
  sugestoes_agrupadas <- map_dfr(grupos, function(g) {
    dias_periodo <- as.integer(difftime(g$data_fim, g$data_inicio, units = "days")) + 1
    estimativa_minutos <- ceiling(dias_periodo * 1.5)
    
    tibble(
      prioridade = g$prioridade,
      periodo = sprintf("%s a %s",
                       format(g$data_inicio, "%d/%m/%Y"),
                       format(g$data_fim, "%d/%m/%Y")),
      data_inicio = g$data_inicio,
      data_fim = g$data_fim,
      "Dias Faltando" = g$dias_total,
      motivo = ifelse(g$sugestoes_originais > 1,
                     sprintf("Agrupamento de %d lacunas (%d dias no período)", 
                            g$sugestoes_originais, dias_periodo),
                     sprintf("Lacuna de %d dias", g$dias_total)),
      estimativa_tempo = ifelse(
        estimativa_minutos < 60,
        sprintf("%d minutos", estimativa_minutos),
        sprintf("%.1f horas", estimativa_minutos / 60)
      )
    )
  })
  
  sugestoes_agrupadas %>%
    arrange(desc(.data$prioridade), desc(.data$`Dias Faltando`))
}

#' Calcular resumo de sugestões
#' 
#' @param sugestoes Data frame de sugestões
#' @return Lista com resumo estatístico
#' @export
calcular_resumo_sugestoes <- function(sugestoes) {
  if (nrow(sugestoes) == 0) {
    return(list(
      total_sugestoes = 0,
      total_dias = 0,
      por_prioridade = tibble()
    ))
  }
  
  resumo_prioridade <- sugestoes %>%
    group_by(.data$prioridade) %>%
    summarise(
      quantidade = n(),
      dias_total = sum(.data$`Dias Faltando`, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(factor(.data$prioridade, levels = c("Crítica", "Alta", "Média", "Baixa")))
  
  list(
    total_sugestoes = nrow(sugestoes),
    total_dias = sum(sugestoes$`Dias Faltando`, na.rm = TRUE),
    por_prioridade = resumo_prioridade
  )
}

#' Sugerir estratégia de coleta
#' 
#' Gera uma estratégia recomendada baseada nas lacunas
#' 
#' @param sugestoes Data frame de sugestões
#' @return Lista com estratégia recomendada
#' @export
sugerir_estrategia_coleta <- function(sugestoes) {
  if (nrow(sugestoes) == 0) {
    return(list(
      estrategia = "Nenhuma lacuna detectada",
      recomendacao = "Todos os períodos estão cobertos.",
      proximos_passos = character()
    ))
  }
  
  criticas <- sugestoes %>% filter(.data$prioridade == "Crítica")
  altas <- sugestoes %>% filter(.data$prioridade == "Alta")
  
  if (nrow(criticas) > 0) {
    estrategia <- "Urgente"
    recomendacao <- sprintf(
      "Existem %d lacuna(s) crítica(s) com %d dias faltando. Priorize coletar esses períodos imediatamente.",
      nrow(criticas),
      sum(criticas$`Dias Faltando`, na.rm = TRUE)
    )
    proximos_passos <- c(
      "1. Coletar períodos críticos primeiro",
      "2. Verificar disponibilidade nos portais",
      "3. Executar coletas em lotes se necessário"
    )
  } else if (nrow(altas) > 0) {
    estrategia <- "Importante"
    recomendacao <- sprintf(
      "Existem %d lacuna(s) de alta prioridade. Recomenda-se coletar nos próximos dias.",
      nrow(altas)
    )
    proximos_passos <- c(
      "1. Planejar coletas para períodos de alta prioridade",
      "2. Agrupar coletas próximas para eficiência",
      "3. Monitorar lacunas críticas que possam surgir"
    )
  } else {
    estrategia <- "Manutenção"
    recomendacao <- "Lacunas são de baixa/média prioridade. Pode ser feito como manutenção regular."
    proximos_passos <- c(
      "1. Coletar em ordem de prioridade",
      "2. Agrupar múltiplas lacunas quando possível",
      "3. Manter coleta regular para evitar novas lacunas"
    )
  }
  
  list(
    estrategia = estrategia,
    recomendacao = recomendacao,
    proximos_passos = proximos_passos
  )
}

