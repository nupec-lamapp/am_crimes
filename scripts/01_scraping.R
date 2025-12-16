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

UA_NUPEC  <- "NUPEC-LAMAPP-MonitorCrimes/1.0 (uso academico, UFAM)"
if (!exists("DIR_RAW", inherits = TRUE))  DIR_RAW  <- file.path("data", "raw")
if (!exists("DIR_LOGS", inherits = TRUE)) DIR_LOGS <- "logs"
LOG_TEXTO <- file.path(DIR_LOGS, "scraping.log")

if (!dir.exists(DIR_RAW)) dir.create(DIR_RAW, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(DIR_LOGS)) dir.create(DIR_LOGS, recursive = TRUE, showWarnings = FALSE)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

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
  ua           = UA_NUPEC
) {
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
        return(resp)
      }

      if (status %in% c(429, 500:599) && tentativa < max_retries) {
        wait <- backoff_base ^ tentativa + runif(1, 0, 1)
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
  tryCatch({
    Sys.sleep(runif(1, 0.8, 2.5))

    resp <- httr::GET(
      url,
      httr::timeout(30),
      httr::user_agent(UA_NUPEC),
      httr::add_headers(
        `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        `Accept-Language` = "pt-BR,pt;q=0.9,en;q=0.8",
        `Cache-Control` = "no-cache",
        `Pragma` = "no-cache"
      )
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
# Coletor A Cr¡tica
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
      log_event("INFO", sprintf("Nenhum link encontrado na p gina %d. Fim da pagina‡Æo.", pagina_atual))
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

############################################################
# Coletor Em Tempo
############################################################

coletar_emtempo <- function(data_inicio, data_fim, config = list()) {
  base_url    <- "https://emtempo.com.br/category/policia/"
  max_paginas <- config$max_paginas %||% 60

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
    n_processados <- 0

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

      if (is.na(data_pub)) next
      if (data_pub > data_fim) next

      if (data_pub < data_inicio) {
        n_antigos <- n_antigos + 1
        next
      }

      noticias[[length(noticias) + 1]] <- tibble(
        portal          = "emtempo",
        data_publicacao = format(data_pub, "%Y-%m-%d"),
        titulo          = normalizar(tit),
        url             = link
      )

      log_event("DEBUG", sprintf("[GUARDADO] %s - %s...", data_pub, substring(tit, 1, 60)))
    }

    if (n_processados > 0 && n_antigos == n_processados) {
      log_event("INFO", sprintf("Itens da pagina %d sao anteriores a %s. Parando.", pagina_atual, data_inicio))
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
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      txt,
      orders = c("a, d b Y H:M:S z", "d b Y H:M:S z", "Y-m-d H:M:S"),
      tz = "UTC"
    )
  )
  as.Date(parsed)
}

coletar_g1_amazonas <- function(data_inicio, data_fim, config = list()) {
  feed_url <- config$feed_url %||% "https://g1.globo.com/rss/g1/am/amazonas/"

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
    filter(
      !is.na(data_publicacao),
      grepl("g1\\.globo\\.com/am/amazonas", url)
    ) %>%
    distinct(url, .keep_all = TRUE) %>%
    filter(data_publicacao >= data_inicio, data_publicacao <= data_fim)

  if (nrow(noticias) == 0) {
    log_event("WARN", "[G1_AM] Feed nao trouxe noticias no intervalo.")
    return(tibble())
  }

  log_event("INFO", sprintf("[G1_AM] %d noticias no intervalo.", nrow(noticias)))

  noticias %>%
    mutate(
      data_publicacao = format(data_publicacao, "%Y-%m-%d"),
      crime_violento  = eh_crime_violento(titulo)
    )
}

registrar_coletor("g1_amazonas", coletar_g1_amazonas)

############################################################
# Helpers e coletor D24AM
############################################################

parse_pubdate_d24 <- function(txt) {
  if (is.null(txt) || length(txt) == 0) return(NA_Date_)
  parsed <- suppressWarnings(
    lubridate::parse_date_time(
      txt,
      orders = c("a, d b Y H:M:S z", "d b Y H:M:S z", "Y-m-d H:M:S"),
      tz = "America/Manaus"
    )
  )
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

coletar_d24am_feed <- function(data_inicio, data_fim, feed_urls) {
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
  }) %>%
    filter(!is.na(data_publicacao))

  noticias <- noticias %>%
    distinct(url, .keep_all = TRUE) %>%
    filter(data_publicacao >= data_inicio, data_publicacao <= data_fim)

  if (nrow(noticias) == 0) {
    log_event("WARN", "[D24AM] Feed nao trouxe noticias no intervalo (", feed_usado, ").")
    return(tibble())
  }

  log_event("INFO", sprintf("[D24AM][FEED] %d noticias obtidas (%s).", nrow(noticias), feed_usado))
  noticias %>% mutate(crime_violento = eh_crime_violento(titulo))
}

coletar_d24am_listagem <- function(data_inicio, data_fim, config = list()) {
  base_url    <- "https://d24am.com/policia/"
  max_paginas <- config$max_paginas %||% 40

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
      filter(grepl("d24am\\.com", href)) %>%
      distinct(href, .keep_all = TRUE)

    n_processados <- 0
    n_antigos <- 0

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

      if (is.na(data_pub)) next
      if (data_pub > data_fim) next
      if (data_pub < data_inicio) {
        n_antigos <- n_antigos + 1
        next
      }

      noticias[[length(noticias) + 1]] <- tibble(
        portal          = "d24am",
        data_publicacao = format(data_pub, "%Y-%m-%d"),
        titulo          = normalizar(tit),
        url             = link
      )
      log_event("DEBUG", sprintf("[D24AM][GUARDADO] %s - %s...", data_pub, substring(tit, 1, 60)))
    }

    if (n_processados > 0 && n_antigos == n_processados) {
      log_event("INFO", sprintf("[D24AM] Artigos da pagina %d sao anteriores a %s. Parando.", pagina_atual, data_inicio))
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
  noticias <- coletar_d24am_feed(data_inicio, data_fim, feed_urls)
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
