############################################################
# 01_scraping.R
# Coleta de not¡cias policiais/violentas - NUPEC / LAMAPP
# Estrutura preparada para m£ltiplos portais
############################################################

suppressPackageStartupMessages({
  library(httr)
  library(rvest)
  library(xml2)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(stringr)
  library(tibble)
  library(purrr)
})

options(stringsAsFactors = FALSE)

if (file.exists("R/paths.R")) source("R/paths.R")
if (exists("crimes_am_set_paths")) crimes_am_set_paths()

UA_NUPEC  <- "NUPEC-LAMAPP-MonitorCrimes/1.1 (+https://github.com/nupec-lamapp/crimes_am)"
GET_CACHE <- new.env(parent = emptyenv())
if (!exists("DIR_RAW", inherits = TRUE))  DIR_RAW  <- file.path("data", "raw")
if (!exists("DIR_LOGS", inherits = TRUE)) DIR_LOGS <- "logs"
LOG_TEXTO <- file.path(DIR_LOGS, "scraping.log")

if (!dir.exists(DIR_RAW)) dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(DIR_LOGS)) dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

get_max_paginas <- function(config, default_max) {
  cfg_val <- config$max_paginas
  env_val <- suppressWarnings(as.integer(Sys.getenv("CRIMES_AM_MAX_PAGINAS", "")))
  max_paginas <- cfg_val %||% env_val %||% default_max
  if (is.na(max_paginas) || max_paginas <= 0) max_paginas <- default_max
  max_paginas
}

get_bool_env <- function(name, default = FALSE) {
  val <- tolower(Sys.getenv(name, ""))
  if (val %in% c("1", "true", "t", "yes", "y")) return(TRUE)
  if (val %in% c("0", "false", "f", "no", "n")) return(FALSE)
  default
}

get_incluir_sem_data <- function(config, default = TRUE) {
  if (!is.null(config$incluir_sem_data)) return(isTRUE(config$incluir_sem_data))
  get_bool_env("CRIMES_AM_INCLUIR_SEM_DATA", default)
}

parse_retry_after <- function(value) {
  if (is.null(value) || is.na(value)) return(NA_real_)
  value <- trimws(value)
  if (grepl("^[0-9]+$", value)) return(as.numeric(value))
  parsed <- suppressWarnings(lubridate::parse_date_time(
    value,
    orders = c("a, d b Y H:M:S z", "a, d b Y H:M:S", "Y-m-d H:M:S z", "Y-m-d H:M:S")
  ))
  if (is.na(parsed)) return(NA_real_)
  wait <- as.numeric(difftime(parsed, Sys.time(), units = "secs"))
  if (wait < 0) wait <- 0
  wait
}

respeita_robots <- function(url, ua = UA_NUPEC) {
  if (!get_bool_env("CRIMES_AM_RESPEITAR_ROBOTS", TRUE)) return(TRUE)
  if (!requireNamespace("robotstxt", quietly = TRUE)) {
    log_event("WARN", "[ROBOTS]", "Pacote robotstxt nao instalado; seguindo sem checagem.")
    return(TRUE)
  }
  ok <- tryCatch(
    robotstxt::paths_allowed(url, user_agent = ua),
    error = function(e) {
      log_event("WARN", "[ROBOTS]", "Falha ao consultar robots.txt:", e$message)
      TRUE
    }
  )
  if (!isTRUE(ok)) {
    log_event("WARN", "[ROBOTS]", "Acesso bloqueado por robots.txt:", url)
  }
  isTRUE(ok)
}

log_event <- function(level = "INFO", ...) {
  msg <- paste(...)
  linha <- sprintf("[%s][%s] %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), level, msg)
  message(linha)
  tryCatch(cat(linha, file = LOG_TEXTO, append = TRUE, sep = "\n"), error = function(e) invisible(NULL))
}

log_http <- function(url, status, etapa = "LISTAGEM") {
  log_event(
    "INFO",
    "[HTTP]",
    sprintf("etapa=%s status=%s url=%s", etapa, status, url)
  )
}

fazer_get_seguro_v2 <- function(
  url,
  max_retries  = 3,
  backoff_base = 2,
  timeout_seg  = 30,
  ua           = UA_NUPEC,
  usar_cache   = TRUE
) {
  cache_key <- paste0(ua, "::", url)
  if (usar_cache && exists(cache_key, envir = GET_CACHE, inherits = FALSE)) {
    return(get(cache_key, envir = GET_CACHE))
  }

  if (!respeita_robots(url, ua)) {
    return(NULL)
  }

  tentativa <- 1
  repeat {
    Sys.sleep(runif(1, 0.8, 2.5))

    resp <- tryCatch(
      httr::GET(
        url,
        httr::user_agent(ua),
        httr::timeout(timeout_seg),
        httr::add_headers(
          `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
          `Accept-Language` = "pt-BR,pt;q=0.9,en;q=0.8",
          `Cache-Control` = "no-cache",
          `Pragma` = "no-cache"
        )
      ),
      error = function(e) {
        log_event("ERROR", "[GET_V2]", url, "->", e$message)
        NULL
      }
    )

    if (is.null(resp)) {
      if (tentativa >= max_retries) return(NULL)
    } else {
      status <- httr::status_code(resp)
      if (!httr::http_error(resp)) {
        if (usar_cache) assign(cache_key, resp, envir = GET_CACHE)
        return(resp)
      }

      if (status %in% c(429, 500:599) && tentativa < max_retries) {
        retry_after <- parse_retry_after(httr::headers(resp)[["retry-after"]])
        wait <- if (!is.na(retry_after)) retry_after else backoff_base ^ tentativa + runif(1, 0, 1)
        log_event(
          "WARN",
          "[GET_V2]",
          "status", status,
          "retry em", round(wait, 1), "s para", url
        )
        Sys.sleep(wait)
      } else {
        log_event("ERROR", "[GET_V2]", "status", status, "abortando para", url)
        return(NULL)
      }
    }

    tentativa <- tentativa + 1
  }
}

fazer_get_seguro <- function(url) {
  fazer_get_seguro_v2(url, max_retries = 2, backoff_base = 2, timeout_seg = 30)
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

coletar_datas_json <- function(obj) {
  out <- character()
  if (is.list(obj)) {
    if (!is.null(obj$datePublished)) out <- c(out, obj$datePublished)
    if (!is.null(obj$dateCreated)) out <- c(out, obj$dateCreated)
    if (!is.null(obj$dateModified)) out <- c(out, obj$dateModified)
    for (item in obj) {
      out <- c(out, coletar_datas_json(item))
    }
  }
  out
}

extrair_data_jsonld <- function(doc) {
  if (!requireNamespace("jsonlite", quietly = TRUE)) return(NA_Date_)
  nodes <- rvest::html_elements(doc, "script[type='application/ld+json']")
  if (length(nodes) == 0) return(NA_Date_)
  datas <- character()
  for (node in nodes) {
    txt <- rvest::html_text(node, trim = TRUE)
    if (!nzchar(txt)) next
    json <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
    if (is.null(json)) next
    datas <- c(datas, coletar_datas_json(json))
  }
  datas <- datas[!is.na(datas) & nzchar(datas)]
  if (length(datas) == 0) return(NA_Date_)
  parsed <- suppressWarnings(lubridate::parse_date_time(
    datas,
    orders = c("Y-m-d H:M:S", "Y-m-d\\TH:M:S", "Y-m-d H:M", "Y-m-d\\TH:M", "Y-m-d"),
    tz = "UTC"
  ))
  parsed <- parsed[!is.na(parsed)]
  if (length(parsed) == 0) return(NA_Date_)
  as.Date(parsed[1])
}

extrair_data_publicacao <- function(doc) {
  d <- extrair_data_jsonld(doc)
  if (!is.na(d)) return(d)

  meta_nodes <- rvest::html_elements(
    doc,
    "meta[property='article:published_time'], meta[name='pubdate'], meta[name='publish-date']"
  )
  if (length(meta_nodes) > 0) {
    dt_attr <- rvest::html_attr(meta_nodes, "content")[1]
    if (!is.na(dt_attr) && dt_attr != "") {
      d <- suppressWarnings(lubridate::parse_date_time(
        dt_attr,
        orders = c("Y-m-d H:M:S", "Y-m-d\\TH:M:S", "Y-m-d H:M", "Y-m-d\\TH:M", "Y-m-d"),
        tz = "UTC"
      ))
      if (!is.na(d)) return(as.Date(d))
    }
  }

  time_nodes <- rvest::html_elements(doc, "time")
  if (length(time_nodes) > 0) {
    dt_attr <- rvest::html_attr(time_nodes, "datetime")[1]
    if (!is.na(dt_attr) && dt_attr != "") {
      d <- suppressWarnings(lubridate::parse_date_time(
        dt_attr,
        orders = c("Y-m-d H:M:S", "Y-m-d\\TH:M:S", "Y-m-d H:M", "Y-m-d\\TH:M", "Y-m-d"),
        tz = "UTC"
      ))
      if (!is.na(d)) return(as.Date(d))
    }
    txt_time <- rvest::html_text(time_nodes[1], trim = TRUE)
    if (!is.na(txt_time) && txt_time != "") {
      d <- suppressWarnings(lubridate::parse_date_time(txt_time, orders = c("d/m/Y", "Y-m-d")))
      if (!is.na(d)) return(as.Date(d))
    }
  }

  cand_nodes <- rvest::html_elements(
    doc,
    "time, .date, .post-date, .entry-date, .updated, [class*='data'], [class*='date'], [id*='data'], [id*='date']"
  )
  if (length(cand_nodes) > 0) {
    txt <- paste(rvest::html_text(cand_nodes, trim = TRUE), collapse = " ")
    padrao <- stringr::str_extract(txt, "\\b(\\d{1,2}/\\d{1,2}/\\d{4})\\b")
    if (!is.na(padrao)) {
      d <- suppressWarnings(lubridate::dmy(padrao))
      if (!is.na(d)) return(d)
    }
  }

  txt <- rvest::html_text(doc)
  padrao <- stringr::str_match(
    txt,
    "(?i)(publicado em|publicada em|publicado|publicada|atualizado em|atualizada em|atualizado|atualizada).{0,40}?(\\d{1,2}/\\d{1,2}/\\d{4})"
  )
  if (!is.na(padrao[3])) {
    d <- suppressWarnings(lubridate::dmy(padrao[3]))
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
# Coletor A Cr¡tica
############################################################

coletar_acritica <- function(data_inicio, data_fim, config = list()) {
  base_url <- "https://www.acritica.com/policia"
  max_paginas <- config$max_paginas %||% 20

  noticias_acumuladas <- list()
  pagina_atual <- 1
  continuar_buscando <- TRUE
  dominio <- get_domain(base_url)

  log_event("INFO", sprintf("--- Iniciando busca em '%s' ---", base_url))

  while (continuar_buscando && pagina_atual <= max_paginas) {
    url_listagem <- paste0(base_url, "?page=", pagina_atual)
    log_event("DEBUG", sprintf(">> Processando p gina %d...", pagina_atual))

    resp <- fazer_get_seguro(url_listagem)
    if (is.null(resp)) {
      log_event("ERROR", "[ACRITICA] Falha ao acessar listagem. Parando.")
      break
    }

    pagina_html <- httr::content(resp, as = "text", encoding = "UTF-8")
    doc <- xml2::read_html(pagina_html)

    links_nodes <- rvest::html_elements(doc, "h3 a")
    if (length(links_nodes) == 0) {
      log_event("INFO", sprintf("Nenhum link encontrado na pagina %d. Fim da paginacao.", pagina_atual))
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
      log_event("INFO", sprintf("Todos os itens da p gina %d sÆo anteriores a %s. Parando.", pagina_atual, data_inicio))
      continuar_buscando <- FALSE
    }

    pagina_atual <- pagina_atual + 1
  }

  if (length(noticias_acumuladas) == 0) {
    log_event("WARN", "[ACRITICA] Nenhuma not¡cia encontrada no intervalo.")
    return(tibble())
  }

  bind_rows(noticias_acumuladas) %>%
    mutate(crime_violento = eh_crime_violento(titulo))
}

registrar_coletor("acritica", coletar_acritica)

# Versao robusta (parada antecipada) do coletor A Critica
coletar_acritica_novo <- function(data_inicio, data_fim, config = list()) {
  base_url <- "https://www.acritica.com/policia"
  max_paginas <- config$max_paginas %||% 20

  noticias_acumuladas <- list()
  dominio <- get_domain(base_url)

  log_event("INFO", sprintf("--- Iniciando busca em '%s' ---", base_url))

  for (pagina_atual in seq_len(max_paginas)) {
    url_listagem <- paste0(base_url, "?page=", pagina_atual)
    log_event("DEBUG", sprintf(">> Processando pagina %d...", pagina_atual))

    resp <- fazer_get_seguro(url_listagem)
    if (is.null(resp)) {
      log_event("ERROR", "[ACRITICA] Falha ao acessar listagem. Parando.")
      break
    }

    pagina_html <- httr::content(resp, as = "text", encoding = "UTF-8")
    doc <- xml2::read_html(pagina_html)

    links_nodes <- rvest::html_elements(doc, "h3 a")
    if (length(links_nodes) == 0) {
      log_event("INFO", sprintf("Nenhum link encontrado na pagina %d. Fim da paginacao.", pagina_atual))
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
    links_novos <- 0

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
      links_novos <- links_novos + 1
    }

    if (n_processados_pagina > 0 && n_antigos_na_pagina == n_processados_pagina) {
      log_event("INFO", sprintf("Todos os itens da pagina %d sao anteriores a %s. Parando.", pagina_atual, data_inicio))
      break
    }

    if (links_novos == 0) {
      log_event("INFO", sprintf("Nenhuma noticia nova na pagina %d (fora do intervalo). Parando.", pagina_atual))
      break
    }
  }

  if (length(noticias_acumuladas) == 0) {
    log_event("WARN", "[ACRITICA] Nenhuma noticia encontrada no intervalo.")
    return(tibble())
  }

  bind_rows(noticias_acumuladas) %>%
    mutate(crime_violento = eh_crime_violento(titulo))
}

# Registrar novo coletor como padrao
registrar_coletor("acritica", coletar_acritica_novo)

# Coletor A Critica - override com parada antecipada limpa
coletar_acritica_final <- function(data_inicio, data_fim, config = list()) {
  base_url <- "https://www.acritica.com/policia"
  max_paginas <- get_max_paginas(config, 20)
  incluir_sem_data <- get_incluir_sem_data(config, TRUE)

  noticias_acumuladas <- list()
  dominio <- get_domain(base_url)

  log_event("INFO", sprintf("--- Iniciando busca (final) em '%s' ---", base_url))

  for (pagina_atual in seq_len(max_paginas)) {
    url_listagem <- paste0(base_url, "?page=", pagina_atual)
    log_event("DEBUG", sprintf(">> Processando pagina %d...", pagina_atual))

    resp <- fazer_get_seguro(url_listagem)
    if (is.null(resp)) {
      log_event("ERROR", "[ACRITICA] Falha ao acessar listagem. Parando.")
      break
    }

    pagina_html <- httr::content(resp, as = "text", encoding = "UTF-8")
    doc <- xml2::read_html(pagina_html)

    links_nodes <- rvest::html_elements(doc, "h3 a")
    if (length(links_nodes) == 0) {
      log_event("INFO", sprintf("Nenhum link encontrado na pagina %d. Fim da paginacao.", pagina_atual))
      break
    }

    df_links <- tibble(
      texto = rvest::html_text(links_nodes, trim = TRUE),
      href  = rvest::html_attr(links_nodes, "href")
    ) %>%
      filter(!is.na(href), href != "") %>%
      distinct(href, .keep_all = TRUE)

    log_event("INFO", sprintf("Encontrados %d links na listagem.", nrow(df_links)))

    n_processados_pagina <- 0
    n_com_data <- 0
    n_no_intervalo <- 0
    n_depois_fim <- 0
    n_antes_inicio <- 0
    n_sem_data <- 0

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

      if (is.na(data_pub)) {
        n_sem_data <- n_sem_data + 1
        if (incluir_sem_data) {
          noticias_acumuladas[[length(noticias_acumuladas) + 1]] <- tibble(
            portal                  = "acritica",
            data_publicacao         = NA_character_,
            data_publicacao_faltante = TRUE,
            titulo                  = normalizar(tit),
            url                     = link
          )
        }
        next
      }

      n_com_data <- n_com_data + 1

      if (data_pub > data_fim) {
        n_depois_fim <- n_depois_fim + 1
        next
      }

      if (data_pub < data_inicio) {
        n_antes_inicio <- n_antes_inicio + 1
        next
      }

      noticias_acumuladas[[length(noticias_acumuladas) + 1]] <- tibble(
        portal          = "acritica",
        data_publicacao = format(data_pub, "%Y-%m-%d"),
        data_publicacao_faltante = FALSE,
        titulo          = normalizar(tit),
        url             = link
      )

      log_event("DEBUG", sprintf("[GUARDADO] %s - %s...", data_pub, substring(tit, 1, 60)))
      n_no_intervalo <- n_no_intervalo + 1
    }

    if (n_processados_pagina > 0) {
      log_event(
        "INFO",
        sprintf(
          "[ACRITICA] pagina=%d no_intervalo=%d depois_fim=%d antes_inicio=%d sem_data=%d",
          pagina_atual, n_no_intervalo, n_depois_fim, n_antes_inicio, n_sem_data
        )
      )
    }

    if (n_com_data > 0 && n_antes_inicio == n_com_data) {
      log_event("INFO", sprintf("[ACRITICA] Pagina %d esta toda anterior a %s. Parando.", pagina_atual, data_inicio))
      break
    }

    if (n_com_data > 0 && n_no_intervalo == 0 && n_depois_fim > 0 && n_antes_inicio == 0) {
      next
    }

    if (n_com_data > 0 && n_no_intervalo == 0 && n_depois_fim == 0 && n_antes_inicio > 0) {
      log_event("INFO", sprintf("[ACRITICA] Ja passou do intervalo na pagina %d. Parando.", pagina_atual))
      break
    }
  }

  if (length(noticias_acumuladas) == 0) {
    log_event("WARN", "[ACRITICA] Nenhuma noticia encontrada no intervalo.")
    return(tibble())
  }

  bind_rows(noticias_acumuladas) %>%
    mutate(crime_violento = eh_crime_violento(titulo))
}

# Override definitivo
registrar_coletor("acritica", coletar_acritica_final)

############################################################
# Coletor Em Tempo
############################################################

coletar_emtempo <- function(data_inicio, data_fim, config = list()) {
  base_url    <- "https://emtempo.com.br/category/policia/"
  max_paginas <- get_max_paginas(config, 20)
  incluir_sem_data <- get_incluir_sem_data(config, TRUE)

  noticias <- list()
  pagina_atual <- 1
  dominio <- get_domain(base_url)

  log_event("INFO", sprintf("--- Iniciando busca em '%s' ---", base_url))

  while (pagina_atual <= max_paginas) {
    url_listagem <- if (pagina_atual == 1) {
      base_url
    } else {
      sprintf("%spage/%d/", base_url, pagina_atual)
    }
    log_event("DEBUG", sprintf(">> Processando pagina %d...", pagina_atual))

    resp <- fazer_get_seguro_v2(url_listagem)
    if (is.null(resp)) {
      log_event("ERROR", "[EMTEMPO] Falha ao acessar listagem. Parando.")
      break
    }

    pagina_html <- tryCatch(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    doc <- tryCatch(xml2::read_html(pagina_html), error = function(e) NULL)
    if (is.null(doc)) {
      log_event("ERROR", "[EMTEMPO] Falha ao ler HTML da listagem.")
      break
    }

    links_nodes <- rvest::html_elements(doc, "h2 a[href*='/policia/'], h5 a[href*='/policia/']")
    if (length(links_nodes) == 0) {
      log_event("INFO", sprintf("Nenhum link encontrado na pagina %d. Fim da paginacao.", pagina_atual))
      break
    }

    df_links <- tibble(
      titulo = rvest::html_text(links_nodes, trim = TRUE),
      href   = rvest::html_attr(links_nodes, "href")
    ) %>%
      filter(!is.na(href), href != "") %>%
      distinct(href, .keep_all = TRUE)

    log_event("INFO", sprintf("Encontrados %d links na listagem.", nrow(df_links)))

    n_antigos <- 0
    n_depois_fim <- 0
    n_sem_data <- 0
    n_processados <- 0
    links_novos <- 0
    n_com_data <- 0

    for (i in seq_len(nrow(df_links))) {
      link <- df_links$href[i]
      tit  <- df_links$titulo[i]

      if (nchar(tit) < 10) next

      if (!grepl("^https?://", link)) {
        if (startsWith(link, "/")) {
          link <- paste0(dominio, link)
        } else {
          next
        }
      }

      resp_art <- fazer_get_seguro_v2(link)
      if (is.null(resp_art)) next

      pag_art <- tryCatch(httr::content(resp_art, as = "text", encoding = "UTF-8"), error = function(e) "")
      if (pag_art == "") next

      doc_art  <- xml2::read_html(pag_art)
      data_pub <- extrair_data_publicacao(doc_art)

      n_processados <- n_processados + 1

      if (is.na(data_pub)) {
        n_sem_data <- n_sem_data + 1
        if (incluir_sem_data) {
          noticias[[length(noticias) + 1]] <- tibble(
            portal                  = "emtempo",
            data_publicacao         = NA_character_,
            data_publicacao_faltante = TRUE,
            titulo                  = normalizar(tit),
            url                     = link
          )
        }
        next
      }
      n_com_data <- n_com_data + 1
      if (data_pub > data_fim) {
        n_depois_fim <- n_depois_fim + 1
        next
      }

      if (data_pub < data_inicio) {
        n_antigos <- n_antigos + 1
        next
      }

      noticias[[length(noticias) + 1]] <- tibble(
        portal          = "emtempo",
        data_publicacao = format(data_pub, "%Y-%m-%d"),
        data_publicacao_faltante = FALSE,
        titulo          = normalizar(tit),
        url             = link
      )

      log_event("DEBUG", sprintf("[GUARDADO] %s - %s...", data_pub, substring(tit, 1, 60)))
      links_novos <- links_novos + 1
    }

    if (n_com_data > 0 && n_antigos == n_com_data) {
      log_event("INFO", sprintf("Itens da pagina %d sao anteriores a %s. Parando.", pagina_atual, data_inicio))
      break
    }

    if (n_processados > 0) {
      log_event(
        "INFO",
        sprintf(
          "[EMTEMPO] pagina=%d no_intervalo=%d depois_fim=%d antes_inicio=%d sem_data=%d",
          pagina_atual, links_novos, n_depois_fim, n_antigos, n_sem_data
        )
      )
    }

    if (links_novos == 0 && n_com_data > 0 && n_depois_fim > 0 && n_antigos == 0) {
      pagina_atual <- pagina_atual + 1
      next
    }

    if (links_novos == 0 && n_com_data > 0 && n_depois_fim == 0 && n_antigos > 0) {
      log_event("INFO", sprintf("[EMTEMPO] Ja passou do intervalo na pagina %d. Parando.", pagina_atual))
      break
    }

    pagina_atual <- pagina_atual + 1
  }

  if (length(noticias) == 0) {
    log_event("WARN", "[EMTEMPO] Nenhuma noticia encontrada no intervalo.")
    return(tibble())
  }

  bind_rows(noticias) %>%
    mutate(crime_violento = eh_crime_violento(titulo))
}

registrar_coletor("emtempo", coletar_emtempo)

############################################################
# Coletor G1 Amazonas
############################################################

parse_pubdate_g1 <- function(txt) {
  if (is.null(txt) || length(txt) == 0) return(NA_Date_)
  parsed <- suppressWarnings(suppressMessages(
    lubridate::parse_date_time(
      txt,
      orders = c("a, d b Y H:M:S z", "d b Y H:M:S z", "Y-m-d H:M:S"),
      tz = "UTC"
    )
  ))
  as.Date(parsed)
}

coletar_g1_amazonas <- function(data_inicio, data_fim, config = list()) {
  feed_url <- config$feed_url %||% "https://g1.globo.com/rss/g1/am/amazonas/"
  incluir_sem_data <- get_incluir_sem_data(config, TRUE)

  log_event("INFO", sprintf("--- Iniciando busca em '%s' ---", feed_url))

  resp <- fazer_get_seguro_v2(feed_url)
  if (is.null(resp)) {
    log_event("WARN", "[G1_AM] Falha ao baixar feed RSS.")
    return(tibble())
  }

  feed_xml <- tryCatch(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    error = function(e) ""
  )
  doc <- tryCatch(xml2::read_xml(feed_xml), error = function(e) NULL)
  if (is.null(doc)) {
    log_event("ERROR", "[G1_AM] Falha ao parsear XML do feed.")
    return(tibble())
  }

  itens <- xml2::xml_find_all(doc, ".//item")
  if (length(itens) == 0) {
    log_event("WARN", "[G1_AM] Feed sem itens.")
    return(tibble())
  }

  noticias <- purrr::map_dfr(seq_along(itens), function(i) {
    item <- itens[[i]]
    titulo <- xml2::xml_text(xml2::xml_find_first(item, "title"))
    link   <- xml2::xml_text(xml2::xml_find_first(item, "link"))
    pub    <- xml2::xml_text(xml2::xml_find_first(item, "pubDate"))
    data_pub <- parse_pubdate_g1(pub)

    tibble(
      portal          = "g1_amazonas",
      data_publicacao = data_pub,
      titulo          = normalizar(titulo),
      url             = link
    )
  }) %>%
    mutate(
      data_publicacao = as.Date(data_publicacao),
      url = dplyr::if_else(
        !is.na(url) & startsWith(url, "//"),
        paste0("https:", url),
        url
      )
    ) %>%
    filter(grepl("g1\\.globo\\.com/am/amazonas", url)) %>%
    distinct(url, .keep_all = TRUE)

  if (incluir_sem_data) {
    noticias <- noticias %>%
      filter(is.na(data_publicacao) |
               (data_publicacao >= data_inicio & data_publicacao <= data_fim))
  } else {
    noticias <- noticias %>%
      filter(!is.na(data_publicacao),
             data_publicacao >= data_inicio,
             data_publicacao <= data_fim)
  }

  if (nrow(noticias) == 0) {
    log_event("WARN", "[G1_AM] Feed nao trouxe noticias no intervalo.")
    return(tibble())
  }

  log_event("INFO", sprintf("[G1_AM] %d noticias no intervalo.", nrow(noticias)))

  noticias %>%
    mutate(
      data_publicacao_faltante = is.na(data_publicacao),
      data_publicacao = dplyr::if_else(
        is.na(data_publicacao),
        NA_character_,
        format(data_publicacao, "%Y-%m-%d")
      ),
      crime_violento  = eh_crime_violento(titulo)
    )
}

registrar_coletor("g1_amazonas", coletar_g1_amazonas)

############################################################
# Helpers e coletor D24AM
############################################################

parse_pubdate_d24 <- function(txt) {
  if (is.null(txt) || length(txt) == 0) return(NA_Date_)
  parsed <- suppressWarnings(suppressMessages(
    lubridate::parse_date_time(
      txt,
      orders = c("a, d b Y H:M:S z", "d b Y H:M:S z", "Y-m-d H:M:S"),
      tz = "America/Manaus"
    )
  ))
  as.Date(parsed)
}

converter_data_ptbr <- function(txt) {
  if (is.null(txt) || length(txt) == 0) return(NA_Date_)
  txt_ascii <- suppressWarnings(iconv(txt, from = "", to = "ASCII//TRANSLIT"))
  txt_limpo <- tolower(stringr::str_trim(ifelse(is.na(txt_ascii), txt, txt_ascii)))
  meses <- c(
    janeiro = 1, fevereiro = 2, marco = 3,
    abril = 4, maio = 5, junho = 6, julho = 7,
    agosto = 8, setembro = 9, outubro = 10,
    novembro = 11, dezembro = 12
  )
  padrao_extenso <- "(\\d{1,2})\\s+de\\s+(janeiro|fevereiro|marco|abril|maio|junho|julho|agosto|setembro|outubro|novembro|dezembro)\\s+de\\s+(\\d{4})"
  m <- stringr::str_match(txt_limpo, padrao_extenso)
  if (!all(is.na(m))) {
    dia <- as.integer(m[2])
    nome_mes <- m[3]
    mes <- meses[[nome_mes]]
    ano <- as.integer(m[4])
    if (!is.na(dia) && !is.na(mes) && !is.na(ano)) {
      return(as.Date(sprintf("%04d-%02d-%02d", ano, mes, dia)))
    }
  }
  num <- stringr::str_match(txt_limpo, "(\\d{1,2})/(\\d{1,2})/(\\d{4})")
  if (!all(is.na(num))) {
    dia <- as.integer(num[2])
    mes <- as.integer(num[3])
    ano <- as.integer(num[4])
    if (!is.na(dia) && !is.na(mes) && !is.na(ano)) {
      return(as.Date(sprintf("%04d-%02d-%02d", ano, mes, dia)))
    }
  }
  NA_Date_
}

extrair_data_publicacao_d24 <- function(doc) {
  d <- extrair_data_publicacao(doc)
  if (!is.na(d)) return(d)
  node <- rvest::html_element(doc, ".td-post-date time, time.entry-date")
  if (!inherits(node, "xml_node")) node <- rvest::html_element(doc, ".single__meta time, .single__meta span")
  if (!inherits(node, "xml_node")) return(NA_Date_)
  texto <- rvest::html_text(node, trim = TRUE)
  converter_data_ptbr(texto)
}

coletar_d24am_feed <- function(data_inicio, data_fim, feed_urls, incluir_sem_data = TRUE) {
  feed_doc <- NULL
  feed_usado <- NULL

  for (fu in feed_urls) {
    resp <- fazer_get_seguro_v2(fu)
    if (is.null(resp)) next

    pagina_xml <- tryCatch(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    doc <- tryCatch(xml2::read_xml(pagina_xml), error = function(e) NULL)
    if (!is.null(doc)) {
      feed_doc <- doc
      feed_usado <- fu
      break
    }
  }

  if (is.null(feed_doc)) {
    log_event("WARN", "[D24AM] Feed RSS indisponivel.")
    return(tibble())
  }

  itens <- xml2::xml_find_all(feed_doc, ".//item")
  if (length(itens) == 0) {
    log_event("WARN", "[D24AM] Feed sem itens: ", feed_usado)
    return(tibble())
  }

  noticias <- purrr::map_dfr(seq_along(itens), function(i) {
    item <- itens[[i]]
    titulo <- xml2::xml_text(xml2::xml_find_first(item, "title"))
    link   <- xml2::xml_text(xml2::xml_find_first(item, "link"))
    pub    <- xml2::xml_text(xml2::xml_find_first(item, "pubDate"))
    data_pub <- parse_pubdate_d24(pub)
    tibble(
      portal          = "d24am",
      data_publicacao = as.Date(data_pub),
      titulo          = normalizar(titulo),
      url             = link
    )
  })

  noticias <- noticias %>%
    distinct(url, .keep_all = TRUE)

  if (incluir_sem_data) {
    noticias <- noticias %>%
      filter(is.na(data_publicacao) |
               (data_publicacao >= data_inicio & data_publicacao <= data_fim))
  } else {
    noticias <- noticias %>%
      filter(!is.na(data_publicacao),
             data_publicacao >= data_inicio,
             data_publicacao <= data_fim)
  }

  if (nrow(noticias) == 0) {
    log_event("WARN", "[D24AM] Feed nao trouxe noticias no intervalo (", feed_usado, ").")
    return(tibble())
  }

  log_event("INFO", sprintf("[D24AM][FEED] %d noticias obtidas (%s).", nrow(noticias), feed_usado))
  noticias %>%
    mutate(
      data_publicacao_faltante = is.na(data_publicacao),
      data_publicacao = dplyr::if_else(
        is.na(data_publicacao),
        NA_character_,
        format(data_publicacao, "%Y-%m-%d")
      ),
      crime_violento = eh_crime_violento(titulo)
    )
}

coletar_d24am_listagem <- function(data_inicio, data_fim, config = list()) {
  base_url    <- "https://d24am.com/policia/"
  max_paginas <- get_max_paginas(config, 20)
  incluir_sem_data <- get_incluir_sem_data(config, TRUE)

  noticias <- list()
  pagina_atual <- 1
  dominio <- get_domain(base_url)

  log_event("INFO", sprintf("--- Iniciando busca em '%s' ---", base_url))

  while (pagina_atual <= max_paginas) {
    url_listagem <- if (pagina_atual == 1) {
      base_url
    } else {
      sprintf("%spage/%d/", base_url, pagina_atual)
    }

    log_event("DEBUG", sprintf("[D24AM] Processando pagina %d ...", pagina_atual))
    resp <- fazer_get_seguro_v2(url_listagem)
    if (is.null(resp)) {
      log_event("ERROR", "[D24AM] Falha ao acessar listagem. Parando.")
      break
    }

    pagina_html <- tryCatch(
      httr::content(resp, as = "text", encoding = "UTF-8"),
      error = function(e) ""
    )
    doc <- tryCatch(xml2::read_html(pagina_html), error = function(e) NULL)
    if (is.null(doc)) {
      log_event("ERROR", "[D24AM] Falha ao ler HTML da listagem.")
      break
    }

    links_nodes <- rvest::html_elements(
      doc,
      "article h2 a, article h3 a, article .entry-title a, .td-module-thumb > a, .td-block-span6 a"
    )
    if (length(links_nodes) == 0) {
      links_nodes <- rvest::html_elements(doc, "a[href*='d24am.com']")
    }
    if (length(links_nodes) == 0) {
      log_event("INFO", sprintf("[D24AM] Nenhum link encontrado na pagina %d. Encerrando.", pagina_atual))
      break
    }

    df_links <- tibble(
      titulo = rvest::html_text(links_nodes, trim = TRUE),
      href   = rvest::html_attr(links_nodes, "href")
    ) %>%
      filter(!is.na(href), href != "", nchar(titulo) > 5) %>%
      mutate(
        href = dplyr::if_else(startsWith(href, "/"), paste0(dominio, href), href)
      ) %>%
      filter(grepl("d24am\\.com", href), grepl("/policia", href)) %>%
      distinct(href, .keep_all = TRUE)

    n_processados <- 0
    n_antigos <- 0
    n_depois_fim <- 0
    n_sem_data <- 0
    links_novos <- 0
    n_com_data <- 0

    for (i in seq_len(nrow(df_links))) {
      link <- df_links$href[i]
      tit  <- df_links$titulo[i]

      if (!grepl("^https?://", link)) next
      resp_art <- fazer_get_seguro_v2(link)
      if (is.null(resp_art)) next

      pag_art <- tryCatch(httr::content(resp_art, as = "text", encoding = "UTF-8"), error = function(e) "")
      if (pag_art == "") next

      doc_art <- tryCatch(xml2::read_html(pag_art), error = function(e) NULL)
      if (is.null(doc_art)) next

      data_pub <- extrair_data_publicacao_d24(doc_art)
      n_processados <- n_processados + 1

      if (is.na(data_pub)) {
        n_sem_data <- n_sem_data + 1
        if (incluir_sem_data) {
          noticias[[length(noticias) + 1]] <- tibble(
            portal                  = "d24am",
            data_publicacao         = NA_character_,
            data_publicacao_faltante = TRUE,
            titulo                  = normalizar(tit),
            url                     = link
          )
        }
        next
      }
      n_com_data <- n_com_data + 1
      if (data_pub > data_fim) {
        n_depois_fim <- n_depois_fim + 1
        next
      }
      if (data_pub < data_inicio) {
        n_antigos <- n_antigos + 1
        next
      }

      noticias[[length(noticias) + 1]] <- tibble(
        portal          = "d24am",
        data_publicacao = format(data_pub, "%Y-%m-%d"),
        data_publicacao_faltante = FALSE,
        titulo          = normalizar(tit),
        url             = link
      )
      log_event("DEBUG", sprintf("[D24AM][GUARDADO] %s - %s...", data_pub, substring(tit, 1, 60)))
      links_novos <- links_novos + 1
    }

    if (n_com_data > 0 && n_antigos == n_com_data) {
      log_event("INFO", sprintf("[D24AM] Artigos da pagina %d sao anteriores a %s. Parando.", pagina_atual, data_inicio))
      break
    }

    if (n_processados > 0) {
      log_event(
        "INFO",
        sprintf(
          "[D24AM] pagina=%d no_intervalo=%d depois_fim=%d antes_inicio=%d sem_data=%d",
          pagina_atual, links_novos, n_depois_fim, n_antigos, n_sem_data
        )
      )
    }

    if (links_novos == 0 && n_com_data > 0 && n_depois_fim > 0 && n_antigos == 0) {
      pagina_atual <- pagina_atual + 1
      next
    }

    if (links_novos == 0 && n_com_data > 0 && n_depois_fim == 0 && n_antigos > 0) {
      log_event("INFO", sprintf("[D24AM] Ja passou do intervalo na pagina %d. Parando.", pagina_atual))
      break
    }

    pagina_atual <- pagina_atual + 1
  }

  if (length(noticias) == 0) {
    log_event("WARN", "[D24AM] Nenhuma noticia encontrada no intervalo.")
    return(tibble())
  }

  bind_rows(noticias) %>%
    mutate(crime_violento = eh_crime_violento(titulo))
}

coletar_d24am <- function(data_inicio, data_fim, config = list()) {
  feed_urls <- config$feed_urls %||% c(
    "https://d24am.com/policia/feed/",
    "https://d24am.com/category/policia/feed/"
  )
  incluir_sem_data <- get_incluir_sem_data(config, TRUE)
  noticias <- coletar_d24am_feed(data_inicio, data_fim, feed_urls, incluir_sem_data)
  if (nrow(noticias) > 0) return(noticias)

  log_event("WARN", "[D24AM] Tentando fallback via listagem HTML.")
  coletar_d24am_listagem(data_inicio, data_fim, config)
}

registrar_coletor("d24am", coletar_d24am)



############################################################
# Fun‡Æo principal
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
      log_event("WARN", sprintf("Coletor '%s' nÆo encontrado, ignorando.", portal))
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
    ok_salvo <- tryCatch({
      readr::write_csv(df, caminho)
      TRUE
    }, error = function(e) {
      log_event("ERROR", sprintf("[%s] Falha ao salvar CSV em %s: %s", portal, caminho, e$message))
      FALSE
    })
    if (ok_salvo) log_event("INFO", sprintf("[%s] Arquivo salvo em: %s", portal, caminho))

    log_entry <- tibble(
      timestamp   = Sys.time(),
      data_inicio = data_inicio,
      data_fim    = data_fim,
      portal      = portal,
      n_noticias  = nrow(df),
      n_violentos = sum(df$crime_violento, na.rm = TRUE)
    )

    log_path <- file.path(DIR_LOGS, "scraping_log.csv")
    tryCatch(
      write_csv(log_entry, log_path, append = file.exists(log_path)),
      error = function(e) log_event("WARN", sprintf("Falha ao registrar scraping_log.csv: %s", e$message))
    )

    df
  })

  resultados %>% bind_rows()
}
