############################################################
# inventario_coleta.R
# Funções utilitárias para inventário de arquivos coletados
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tibble)
  library(purrr)
})

#' Inventariar arquivos raw coletados
#' 
#' Lista todos os arquivos CSV em data/raw/ e extrai informações
#' sobre portal, datas, tamanho e status de processamento.
#' 
#' @param dir_raw Diretório onde estão os arquivos raw (padrão: data/raw)
#' @param dados_processados Data frame com dados processados para verificar status
#' @return Data frame com inventário detalhado
#' @export
inventariar_arquivos_raw <- function(
  dir_raw = file.path("data", "raw"),
  dados_processados = NULL
) {
  if (!dir.exists(dir_raw)) {
    return(tibble(
      arquivo = character(),
      portal = character(),
      data_inicio = as.Date(character()),
      data_fim = as.Date(character()),
      tamanho_kb = numeric(),
      data_criacao = as.POSIXct(character()),
      dias_coletados = integer(),
      status = character(),
      processado = logical()
    ))
  }
  
  arquivos <- list.files(dir_raw, pattern = "\\.csv$", full.names = TRUE)
  if (length(arquivos) == 0) {
    return(tibble(
      arquivo = character(),
      portal = character(),
      data_inicio = as.Date(character()),
      data_fim = as.Date(character()),
      tamanho_kb = numeric(),
      data_criacao = as.POSIXct(character()),
      dias_coletados = integer(),
      status = character(),
      processado = logical()
    ))
  }
  
  # Extrair informações dos nomes dos arquivos
  # Padrão: noticias_raw_{portal}_{data_inicio}_{data_fim}.csv
  info_arquivos <- map_dfr(arquivos, function(arq) {
    nome_base <- basename(arq)
    
    # Tentar extrair informações do padrão esperado
    match <- str_match(nome_base, "^noticias_raw_(.+?)_(\\d{8})_(\\d{8})\\.csv$")
    
    if (is.na(match[1, 1])) {
      # Arquivo não segue o padrão esperado
      return(tibble(
        arquivo = nome_base,
        portal = "desconhecido",
        data_inicio = as.Date(NA),
        data_fim = as.Date(NA),
        tamanho_kb = NA_real_,
        data_criacao = as.POSIXct(NA),
        dias_coletados = NA_integer_,
        status = "formato_invalido",
        processado = NA
      ))
    }
    
    portal <- match[1, 2]
    data_inicio_str <- match[1, 3]
    data_fim_str <- match[1, 4]
    
    data_inicio <- tryCatch(
      as.Date(data_inicio_str, format = "%Y%m%d"),
      error = function(e) as.Date(NA)
    )
    data_fim <- tryCatch(
      as.Date(data_fim_str, format = "%Y%m%d"),
      error = function(e) as.Date(NA)
    )
    
    # Obter informações do arquivo
    file_info <- file.info(arq)
    tamanho_kb <- if (!is.na(file_info$size)) round(file_info$size / 1024, 2) else NA_real_
    data_criacao <- file_info$mtime
    
    dias_coletados <- if (!is.na(data_inicio) && !is.na(data_fim)) {
      as.integer(difftime(data_fim, data_inicio, units = "days")) + 1L
    } else {
      NA_integer_
    }
    
    # Verificar se foi processado (comparar com dados processados)
    processado <- FALSE
    if (!is.null(dados_processados) && is.data.frame(dados_processados) && nrow(dados_processados) > 0) {
        if (!is.na(data_inicio) && !is.na(data_fim)) {
          # Verificar se há dados processados no intervalo deste arquivo
          portal_curr <- portal
          data_inicio_curr <- data_inicio
          data_fim_curr <- data_fim
          
          if ("data_publicacao" %in% names(dados_processados)) {
            dados_no_intervalo <- dados_processados %>%
              filter(
                .data$portal == portal_curr,
                .data$data_publicacao >= data_inicio_curr,
                .data$data_publicacao <= data_fim_curr
              )
            processado <- nrow(dados_no_intervalo) > 0
          } else if ("data_pub" %in% names(dados_processados)) {
            dados_no_intervalo <- dados_processados %>%
              filter(
                .data$portal == portal_curr,
                .data$data_pub >= data_inicio_curr,
                .data$data_pub <= data_fim_curr
              )
            processado <- nrow(dados_no_intervalo) > 0
          }
        }
    }
    
    status <- if (processado) "processado" else "nao_processado"
    
    tibble(
      arquivo = nome_base,
      caminho_completo = arq,
      portal = portal,
      data_inicio = data_inicio,
      data_fim = data_fim,
      tamanho_kb = tamanho_kb,
      data_criacao = data_criacao,
      dias_coletados = dias_coletados,
      status = status,
      processado = processado
    )
  })
  
  # Ordenar por data de criação (mais recente primeiro)
  info_arquivos %>%
    arrange(desc(data_criacao))
}

#' Detectar sobreposições entre arquivos coletados
#' 
#' Identifica arquivos que coletam o mesmo período (sobreposição)
#' 
#' @param inventario Data frame retornado por inventariar_arquivos_raw()
#' @return Data frame com informações de sobreposição
#' @export
detectar_sobreposicoes <- function(inventario) {
  if (nrow(inventario) == 0) {
    return(tibble())
  }
  
  # Filtrar apenas arquivos válidos
  inventario_valido <- inventario %>%
    filter(
      !is.na(.data$data_inicio),
      !is.na(.data$data_fim),
      .data$portal != "desconhecido"
    )
  
  if (nrow(inventario_valido) == 0) {
    return(tibble())
  }
  
  # Verificar sobreposições
  sobreposicoes <- map_dfr(seq_len(nrow(inventario_valido)), function(i) {
    arquivo1 <- inventario_valido[i, ]
    portal_curr <- arquivo1$portal
    data_fim_curr <- arquivo1$data_fim
    data_inicio_curr <- arquivo1$data_inicio
    
    sobrepostos <- inventario_valido %>%
      filter(
        row_number() != i,
        .data$portal == portal_curr,
        # Verificar sobreposição de intervalos
        (.data$data_inicio <= data_fim_curr & .data$data_fim >= data_inicio_curr)
      ) %>%
      select(.data$arquivo, .data$data_inicio, .data$data_fim)
    
    if (nrow(sobrepostos) > 0) {
      tibble(
        arquivo = arquivo1$arquivo,
        portal = arquivo1$portal,
        data_inicio = arquivo1$data_inicio,
        data_fim = arquivo1$data_fim,
        n_sobrepostos = nrow(sobrepostos),
        arquivos_sobrepostos = paste(sobrepostos$arquivo, collapse = "; ")
      )
    } else {
      NULL
    }
  })
  
  sobreposicoes
}

#' Resumir inventário por portal
#' 
#' @param inventario Data frame retornado por inventariar_arquivos_raw()
#' @return Data frame com resumo por portal
#' @export
resumir_inventario_por_portal <- function(inventario) {
  if (nrow(inventario) == 0) {
    return(tibble())
  }
  
  inventario %>%
    filter(.data$portal != "desconhecido", !is.na(.data$data_inicio), !is.na(.data$data_fim)) %>%
    group_by(.data$portal) %>%
    summarise(
      n_arquivos = n(),
      periodo_inicio = min(.data$data_inicio, na.rm = TRUE),
      periodo_fim = max(.data$data_fim, na.rm = TRUE),
      tamanho_total_mb = round(sum(.data$tamanho_kb, na.rm = TRUE) / 1024, 2),
      dias_coletados_total = sum(.data$dias_coletados, na.rm = TRUE),
      n_processados = sum(.data$processado, na.rm = TRUE),
      n_nao_processados = sum(!.data$processado, na.rm = TRUE),
      ultima_coleta = max(.data$data_criacao, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      pct_processado = ifelse(.data$n_arquivos > 0, round(100 * .data$n_processados / .data$n_arquivos, 1), 0),
      periodo = sprintf("%s a %s", 
                       format(.data$periodo_inicio, "%d/%m/%Y"),
                       format(.data$periodo_fim, "%d/%m/%Y"))
    ) %>%
    arrange(desc(.data$ultima_coleta))
}

