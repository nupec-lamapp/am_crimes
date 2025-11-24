############################################################
# 01_scraping.R
# Coleta de notícias policiais/violentas - NUPEC / LAMAPP
# Estrutura preparada para múltiplos portais
############################################################

suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(xml2)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(stringr)
  library(stringi)
  library(tibble)
  library(purrr)
})

options(stringsAsFactors = FALSE)

UA_NUPEC  <- "NUPEC-LAMAPP-MonitorCrimes/1.0 (uso academico, UFAM)"
DIR_RAW   <- file.path("data", "raw")
DIR_LOGS  <- "logs"
LOG_TEXTO <- file.path(DIR_LOGS, "scraping.log")

if (!dir.exists(DIR_RAW)) dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(DIR_LOGS)) dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

log_event <- function(level = "INFO", ...) {
  msg <- paste(...)
  linha <- sprintf("[%s][%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, msg)
  message(linha)
  cat(linha, file = LOG_TEXTO, append = TRUE, sep = "\n")
}

fazer_get_seguro <- function(url) {
  tryCatch({
    Sys.sleep(runif(1, 0.8, 2.5))

    resp <- httr::GET(
      url,
      httr::timeout(30),
      httr::user_agent(UA_NUPEC)
    )

    status <- httr::status_code(resp)

    if (status %in% c(429, 503)) {
      log_event("WARN", "[HTTP]", status, "em", url, "- backoff aplicado.")
      Sys.sleep(runif(1, 15, 30))
      resp <- httr::GET(url, httr::timeout(30), httr::user_agent(UA_NUPEC))
    }

    if (httr::http_error(resp)) {
      log_event("ERROR", "[HTTP]", status, "em", url)
      return(NULL)
    }

    resp
  }, error = function(e) {
    log_event("ERROR", "[GET]", url, "->", e$message)
    NULL
  })
}

coletores <- new.env(parent = emptyenv())

registrar_coletor <- function(nome, func) {
  assign(nome, func, envir = coletores)
}

listar_coletores <- function() ls(envir = coletores)

obter_coletor <- function(nome) {
  if (!exists(nome, envir = coletores)) return(NULL)
  get(nome, envir = coletores)
}

normalizar <- function(x) {
  if (is.null(x)) return("")
  x |> stringr::str_squish()
}

get_domain <- function(url) {
  m <- regmatches(url, regexpr("^https?://[^/]+", url))
  if (length(m) == 0) return("")
  m
}

extrair_data_publicacao <- function(doc) {
  meta_nodes <- rvest::html_elements(doc, "meta[property='article:published_time']")
  if (length(meta_nodes) > 0) {
    dt_attr <- rvest::html_attr(meta_nodes, "content")[1]
    if (!is.na(dt_attr) && dt_attr != "") {
      d <- suppressWarnings(lubridate::ymd_hms(dt_attr, quiet = TRUE))
      if (!is.na(d)) return(as.Date(d))
    }
  }

  time_nodes <- rvest::html_elements(doc, "time")
  if (length(time_nodes) > 0) {
    dt_attr <- rvest::html_attr(time_nodes, "datetime")[1]
    if (!is.na(dt_attr) && dt_attr != "") {
      d <- suppressWarnings(lubridate::ymd_hms(dt_attr, quiet = TRUE))
      if (!is.na(d)) return(as.Date(d))
    }
  }

  txt <- rvest::html_text(doc)
  padrao <- stringr::str_extract(txt, "\\b(\\d{2}/\\d{2}/\\d{4})\\b")
  if (!is.na(padrao)) {
    d <- suppressWarnings(lubridate::dmy(padrao))
    if (!is.na(d)) return(d)
  }

  NA_Date_
}

eh_crime_violento <- function(titulo) {
  txt <- tolower(titulo)
  grepl(paste(
    "homicid", "assassin", "morto", "morte", "latrocin",
    "feminicid", "estupro", "estuprad", "abuso sexual",
    "sequestro", "carcere privado", "espancad", "esfaquead",
    "tiroteio", "balead", "roubo", "assalto", "arma de fogo",
    "chacina", "massacre", "corpo encontrado", "execuca", "executad",
    sep = "|"
  ), txt)
}

############################################################
# Coletor A Crítica
############################################################

coletar_acritica <- function(data_inicio, data_fim, config = list()) {
  base_url <- "https://www.acritica.com/policia"
  max_paginas <- config$max_paginas %||% 120

  noticias_acumuladas <- list()
  pagina_atual <- 1
  continuar_buscando <- TRUE
  dominio <- get_domain(base_url)

  log_event("INFO", sprintf("--- Iniciando busca em '%s' ---", base_url))

  while (continuar_buscando && pagina_atual <= max_paginas) {
    url_listagem <- paste0(base_url, "?page=", pagina_atual)
    log_event("DEBUG", sprintf(">> Processando página %d...", pagina_atual))

    resp <- fazer_get_seguro(url_listagem)
    if (is.null(resp)) {
      log_event("ERROR", "[ACRITICA] Falha ao acessar listagem. Parando.")
      break
    }

    pagina_html <- httr::content(resp, as = "text", encoding = "UTF-8")
    doc <- xml2::read_html(pagina_html)

    links_nodes <- rvest::html_elements(doc, "h3 a")
    if (length(links_nodes) == 0) {
      log_event("INFO", sprintf("Nenhum link encontrado na página %d. Fim da paginação.", pagina_atual))
      break
    }

    df_links <- tibble(
      texto = rvest::html_text(links_nodes, trim = TRUE),
      href  = rvest::html_attr(links_nodes, "href")
    ) %>%
      filter(!is.na(href), href != "") %>%
      distinct(href, .keep_all = TRUE)

    log_event("INFO", sprintf("Encontrados %d links na listagem.", nrow(df_links)))

    n_antigos_na_pagina <- 0
    n_processados_pagina <- 0

    for (i in seq_len(nrow(df_links))) {
      link <- df_links$href[i]
      tit  <- df_links$texto[i]

      if (nchar(tit) < 15) next

      if (!grepl("^https?://", link)) {
        if (startsWith(link, "/")) {
          link <- paste0(dominio, link)
        } else {
          next
        }
      }

      resp_art <- fazer_get_seguro(link)
      if (is.null(resp_art)) next

      pag_art <- tryCatch(httr::content(resp_art, as = "text", encoding = "UTF-8"), error = function(e) "")
      if (pag_art == "") next

      doc_art  <- xml2::read_html(pag_art)
      data_pub <- extrair_data_publicacao(doc_art)

      n_processados_pagina <- n_processados_pagina + 1

      if (is.na(data_pub)) next
      if (data_pub > data_fim) next

      if (data_pub < data_inicio) {
        n_antigos_na_pagina <- n_antigos_na_pagina + 1
        next
      }

      noticias_acumuladas[[length(noticias_acumuladas) + 1]] <- tibble(
        portal          = "acritica",
        data_publicacao = format(data_pub, "%Y-%m-%d"),
        titulo          = normalizar(tit),
        url             = link
      )

      log_event("DEBUG", sprintf("[GUARDADO] %s - %s...", data_pub, substring(tit, 1, 60)))
    }

    if (n_processados_pagina > 0 && n_antigos_na_pagina == n_processados_pagina) {
      log_event("INFO", sprintf("Todos os itens da página %d são anteriores a %s. Parando.", pagina_atual, data_inicio))
      continuar_buscando <- FALSE
    }

    pagina_atual <- pagina_atual + 1
  }

  if (length(noticias_acumuladas) == 0) {
    log_event("WARN", "[ACRITICA] Nenhuma notícia encontrada no intervalo.")
    return(tibble())
  }

  bind_rows(noticias_acumuladas) %>%
    mutate(crime_violento = eh_crime_violento(titulo))
}

registrar_coletor("acritica", coletar_acritica)

############################################################
# Função principal
############################################################

rodar_scraping <- function(data_inicio = Sys.Date(),
                           data_fim    = Sys.Date(),
                           portais     = listar_coletores()) {

  data_inicio <- as.Date(data_inicio)
  data_fim    <- as.Date(data_fim)

  if (length(portais) == 0) stop("Nenhum coletor registrado.")

  resultados <- purrr::map(portais, function(portal) {
    coletor <- obter_coletor(portal)
    if (is.null(coletor)) {
      log_event("WARN", sprintf("Coletor '%s' não encontrado, ignorando.", portal))
      return(tibble())
    }

    log_event("INFO", sprintf("Iniciando scraping para %s (%s a %s)", portal,
                              format(data_inicio, "%d/%m/%Y"),
                              format(data_fim, "%d/%m/%Y")))

    df <- coletor(data_inicio, data_fim)
    if (nrow(df) == 0) return(tibble())

    nome_arquivo <- sprintf("noticias_raw_%s_%s_%s.csv",
                            portal,
                            format(data_inicio, "%Y%m%d"),
                            format(data_fim,    "%Y%m%d"))
    caminho <- file.path(DIR_RAW, nome_arquivo)
    readr::write_csv(df, caminho)
    log_event("INFO", sprintf("[%s] Arquivo salvo em: %s", portal, caminho))

    log_entry <- tibble(
      timestamp   = Sys.time(),
      data_inicio = data_inicio,
      data_fim    = data_fim,
      portal      = portal,
      n_noticias  = nrow(df),
      n_violentos = sum(df$crime_violento, na.rm = TRUE)
    )

    log_path <- file.path(DIR_LOGS, "scraping_log.csv")
    write_csv(log_entry, log_path, append = file.exists(log_path))

    df
  })

  resultados %>% bind_rows()
}
