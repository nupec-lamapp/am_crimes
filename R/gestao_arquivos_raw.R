############################################################
# gestao_arquivos_raw.R
# Gestão inteligente de arquivos raw coletados
# Melhores práticas: retenção, consolidação e limpeza segura
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(stringr)
  library(tibble)
  library(purrr)
  library(fs)
})

#' Analisar arquivos raw para gestão
#' 
#' Identifica arquivos que podem ser consolidados ou removidos com segurança
#' seguindo melhores práticas de retenção de dados.
#' 
#' @param inventario Data frame do inventário
#' @param dados_processados Data frame com dados processados (para verificar se foram consolidados)
#' @param dias_retencao Dias para manter arquivos raw (padrão: 180 dias = 6 meses)
#' @param manter_sempre_ultimos_meses Manter sempre os últimos N meses (padrão: 3)
#' @return Lista com análise detalhada
#' @export
analisar_arquivos_para_gestao <- function(
  inventario,
  dados_processados = NULL,
  dias_retencao = 180,
  manter_sempre_ultimos_meses = 3
) {
  if (is.null(inventario) || nrow(inventario) == 0) {
    return(list(
      manter = tibble(),
      consolidar = tibble(),
      remover_seguro = tibble(),
      resumo = tibble()
    ))
  }
  
  hoje <- Sys.Date()
  data_limite_retencao <- hoje - dias_retencao
  data_limite_manter_sempre <- hoje - (manter_sempre_ultimos_meses * 30)
  
  inventario_valido <- inventario %>%
    filter(
      .data$portal != "desconhecido",
      !is.na(.data$data_inicio),
      !is.na(.data$data_fim),
      !is.na(.data$data_criacao)
    ) %>%
    mutate(
      idade_dias = as.integer(difftime(hoje, .data$data_criacao, units = "days")),
      periodo_medio = .data$data_inicio + as.integer(difftime(.data$data_fim, .data$data_inicio, units = "days")) / 2,
      idade_periodo_dias = as.integer(difftime(hoje, .data$periodo_medio, units = "days")),
      foi_processado = .data$processado
    )
  
  # 1. ARQUIVOS PARA MANTER (sempre)
  # - Arquivos recentes (últimos 3 meses)
  # - Arquivos não processados ainda
  # - Arquivos com menos de X dias
  manter <- inventario_valido %>%
    filter(
      .data$idade_dias <= (manter_sempre_ultimos_meses * 30) |
      !.data$foi_processado |
      .data$data_fim >= data_limite_manter_sempre
    ) %>%
    mutate(categoria = "manter", motivo = case_when(
      .data$idade_dias <= (manter_sempre_ultimos_meses * 30) ~ "Arquivo recente (últimos 3 meses)",
      !.data$foi_processado ~ "Ainda não processado",
      .data$data_fim >= data_limite_manter_sempre ~ "Período recente",
      TRUE ~ "Manter"
    ))
  
  # 2. ARQUIVOS PARA CONSOLIDAR (backup antes de remover)
  # - Arquivos antigos mas processados
  # - Arquivos com mais de X dias mas dados consolidados
  consolidar <- inventario_valido %>%
    filter(
      !.data$arquivo %in% manter$arquivo,
      .data$foi_processado,
      .data$idade_dias > dias_retencao,
      .data$data_fim < data_limite_manter_sempre
    ) %>%
    mutate(
      categoria = "consolidar",
      motivo = "Arquivo antigo e processado - pode ser consolidado em backup"
    )
  
  # 3. ARQUIVOS PARA REMOVER (apenas duplicatas confirmadas)
  # - Arquivos sobrepostos onde há versão mais recente
  # - NÃO implementar remoção automática - apenas identificar
  
  remover_seguro <- tibble(
    arquivo = character(),
    portal = character(),
    motivo = character()
  )
  
  # Resumo
  resumo <- tibble(
    categoria = c("Manter", "Consolidar (backup)", "Total"),
    quantidade = c(
      nrow(manter),
      nrow(consolidar),
      nrow(inventario_valido)
    ),
    tamanho_mb = c(
      round(sum(manter$tamanho_kb, na.rm = TRUE) / 1024, 2),
      round(sum(consolidar$tamanho_kb, na.rm = TRUE) / 1024, 2),
      round(sum(inventario_valido$tamanho_kb, na.rm = TRUE) / 1024, 2)
    )
  )
  
  list(
    manter = manter,
    consolidar = consolidar,
    remover_seguro = remover_seguro,
    resumo = resumo,
    parametros = list(
      dias_retencao = dias_retencao,
      manter_sempre_ultimos_meses = manter_sempre_ultimos_meses,
      data_limite_retencao = data_limite_retencao,
      data_limite_manter_sempre = data_limite_manter_sempre
    )
  )
}

#' Criar backup de arquivos antes de consolidação
#' 
#' @param arquivos Lista de arquivos para fazer backup
#' @param dir_backup Diretório de backup (padrão: data/raw/backup)
#' @return Data frame com resultado do backup
#' @export
criar_backup_arquivos <- function(arquivos, dir_backup = file.path("data", "raw", "backup")) {
  if (length(arquivos) == 0) {
    return(tibble(arquivo = character(), status = character(), caminho_backup = character()))
  }
  
  # Criar diretório de backup se não existir
  if (!dir.exists(dir_backup)) {
    dir.create(dir_backup, recursive = TRUE, showWarnings = FALSE)
  }
  
  dir_raw <- if (exists("DIR_RAW", inherits = TRUE)) {
    get("DIR_RAW", inherits = TRUE)
  } else {
    file.path("data", "raw")
  }
  
  resultados <- map_dfr(arquivos, function(arq) {
    caminho_origem <- file.path(dir_raw, arq)
    if (!file.exists(caminho_origem)) {
      return(tibble(
        arquivo = arq,
        status = "erro",
        caminho_backup = NA_character_,
        mensagem = "Arquivo não encontrado"
      ))
    }
    
    # Criar subdiretório por data (organização)
    data_backup <- format(Sys.Date(), "%Y%m")
    dir_backup_data <- file.path(dir_backup, data_backup)
    if (!dir.exists(dir_backup_data)) {
      dir.create(dir_backup_data, recursive = TRUE, showWarnings = FALSE)
    }
    
    caminho_backup <- file.path(dir_backup_data, arq)
    
    resultado <- tryCatch({
      file.copy(caminho_origem, caminho_backup, overwrite = FALSE)
      if (file.exists(caminho_backup)) {
        tibble(
          arquivo = arq,
          status = "sucesso",
          caminho_backup = caminho_backup,
          mensagem = "Backup criado com sucesso"
        )
      } else {
        tibble(
          arquivo = arq,
          status = "erro",
          caminho_backup = NA_character_,
          mensagem = "Falha ao criar backup"
        )
      }
    }, error = function(e) {
      tibble(
        arquivo = arq,
        status = "erro",
        caminho_backup = NA_character_,
        mensagem = paste("Erro:", e$message)
      )
    })
    
    resultado
  })
  
  resultados
}

#' Verificar integridade dos dados processados
#' 
#' Verifica se os dados nos arquivos raw estão realmente consolidados
#' nos dados processados antes de permitir remoção.
#' 
#' @param arquivo Nome do arquivo raw
#' @param dados_processados Data frame com dados processados
#' @param dir_raw Diretório dos arquivos raw
#' @return TRUE se dados estão consolidados, FALSE caso contrário
#' @export
verificar_dados_consolidados <- function(arquivo, dados_processados, dir_raw = file.path("data", "raw")) {
  if (is.null(dados_processados) || nrow(dados_processados) == 0) {
    return(FALSE)
  }
  
  # Extrair informações do arquivo
  match <- str_match(arquivo, "^noticias_raw_(.+?)_(\\d{8})_(\\d{8})\\.csv$")
  if (is.na(match[1, 1])) {
    return(FALSE)
  }
  
  portal <- match[1, 2]
  data_inicio <- tryCatch(as.Date(match[1, 3], format = "%Y%m%d"), error = function(e) NULL)
  data_fim <- tryCatch(as.Date(match[1, 4], format = "%Y%m%d"), error = function(e) NULL)
  
  if (is.null(data_inicio) || is.null(data_fim)) {
    return(FALSE)
  }
  
  # Verificar se há dados processados no intervalo
  col_data <- if ("data_publicacao" %in% names(dados_processados)) {
    "data_publicacao"
  } else if ("data_pub" %in% names(dados_processados)) {
    "data_pub"
  } else {
    return(FALSE)
  }
  
  dados_no_intervalo <- dados_processados %>%
    filter(
      .data$portal == portal,
      .data[[col_data]] >= data_inicio,
      .data[[col_data]] <= data_fim
    )
  
  # Considerar consolidado se houver pelo menos alguns registros
  # (pode haver menos devido a filtros, mas deve haver algo)
  nrow(dados_no_intervalo) > 0
}

