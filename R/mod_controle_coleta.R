############################################################
# mod_controle_coleta.R
# Painel interativo para acionar raspagem e pipeline
############################################################

MES_PT <- c(
  "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
  "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
)
MONTH_SELECT_CHOICES <- c(
  "Todos" = "todos",
  setNames(sprintf("%02d", seq_along(MES_PT)), sprintf("%02d - %s", seq_along(MES_PT), MES_PT))
)
ANO_MINIMO <- 2000
ARTIFACT_FILES <- c(
  "data/processed/crimes_classificados.csv",
  "outputs/04_resumo_geral.csv",
  "outputs/04_indice_letal_mensal.csv",
  "outputs/04_anomalias_classificacao.csv"
)

mod_controle_coleta_ui <- function(id) {
  ns <- NS(id)

  fluidRow(
    column(
      width = 4,
      wellPanel(
        h4("Coleta de portais"),
        actionButton(ns("btn_refresh_portais"), "Atualizar lista de portais", class = "btn btn-secondary btn-sm"),
        textOutput(ns("portais_status")),
        selectInput(ns("portais"), "Portais:", choices = "Todos", multiple = TRUE),
        dateRangeInput(
          ns("range"),
          "Intervalo:",
          start = Sys.Date() - 7,
          end   = Sys.Date(),
          format = "dd/mm/yyyy",
          language = "pt-BR"
        ),
        div(
          class = "btn-group btn-group-sm",
          actionButton(ns("preset_3"), "3 dias", class = "btn btn-outline-secondary"),
          actionButton(ns("preset_7"), "7 dias", class = "btn btn-outline-secondary"),
          actionButton(ns("preset_30"), "30 dias", class = "btn btn-outline-secondary")
        ),
        checkboxInput(ns("rodar_pipeline"), "Rodar parse + cleaning + analises", value = TRUE),
        actionButton(ns("btn_executar"), "Executar coleta", class = "btn btn-primary"),
        actionButton(ns("btn_cancelar"), "Interromper (após etapa atual)", class = "btn btn-danger btn-sm")
      )
    ),
    column(
      width = 8,
      div(
        class = "card-panel",
        h4("Cobertura por portal (dados carregados)"),
        div(
          class = "status-summary",
          textOutput(ns("controle_info")),
          textOutput(ns("controle_periodo")),
          uiOutput(ns("status_widget")),
          htmlOutput(ns("artifact_list"))
        ),
        tableOutput(ns("tab_resumo_portal"))
      ),
      div(
        class = "card-panel",
        h4("Lacunas por portal"),
        selectInput(ns("portal_lacunas"), "Portal:", choices = "Selecione", width = "50%"),
        fluidRow(
          column(
            width = 6,
            selectInput(
              ns("ano_lacunas"),
              "Ano:",
              choices = c("Todos" = "todos"),
              width = "100%"
            )
          ),
          column(
            width = 6,
            selectInput(ns("mes_lacunas"), "Mês:", choices = MONTH_SELECT_CHOICES, width = "100%")
          )
        ),
        tags$small("Selecione um portal para ver, mês a mês (desde janeiro/2025), os dias que ainda faltam coletar. Use Ano/Mês para filtrar (ou deixe em \"Todos\")."),
        htmlOutput(ns("lacunas_info"))
      ),
      div(
        class = "card-panel",
        h4("Log da execução"),
        verbatimTextOutput(ns("log_exec"))
      )
    )
  )
}

mod_controle_coleta_server <- function(id, dados_enr, dados_est) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    portais_status <- reactiveVal("")
    status_summary <- reactiveVal("Pronto para iniciar coleta.")
    coleta_timestamp <- reactiveVal(NULL)
    status_details <- reactiveVal(list(state = "idle", message = "Pronto para iniciar coleta.", time = NULL, artifacts = NULL))

    set_status_details <- function(state, message, artifacts = NULL) {
      status_summary(message)
      status_details(list(state = state, message = message, time = Sys.time(), artifacts = artifacts))
    }
    artifact_info <- function(paths) {
      info <- file.info(paths)
      info <- info[!is.na(info$mtime), , drop = FALSE]
      if (nrow(info) == 0) return(NULL)
      tibble::tibble(
        path = rownames(info),
        modified = info$mtime
      )[order(info$mtime, decreasing = TRUE), , drop = FALSE]
    }
    set_status_details("idle", "Pronto para iniciar coleta.", NULL)

    output$portais_status <- renderText(portais_status())
    output$controle_info <- renderText(status_summary())
    output$controle_periodo <- renderText({
      rng <- input$range
      if (is.null(rng) || any(is.na(rng))) {
        return("")
      }
      start <- as.Date(rng[1])
      end <- as.Date(rng[2])
      sprintf("Período selecionado: %s a %s", format(start, "%d/%m/%Y"), format(end, "%d/%m/%Y"))
    })
    output$status_widget <- renderUI({
      detail <- status_details()
      state <- if (!is.null(detail$state) && nzchar(detail$state)) detail$state else "idle"
      badge_text <- switch(
        state,
        success = "OK",
        error = "ERRO",
        warning = "ATENÇÃO",
        running = "EM ANDAMENTO",
        idle = "PRONTO",
        "INFO"
      )
      badge_class <- switch(
        state,
        success = "text-success",
        error = "text-danger",
        warning = "text-warning",
        running = "text-warning",
        idle = "text-muted",
        "text-muted"
      )
      badge <- tags$span(class = badge_class, style = "font-weight:bold; margin-right:8px;", badge_text)
      message <- tags$span(detail$message)
      last_time <- detail$time
      time_note <- if (!is.null(last_time)) {
        tags$span(sprintf("(%s)", format(last_time, "%d/%m/%Y %H:%M:%S")), class = "text-muted", style = "margin-left:6px;")
      } else NULL
      tags$div(class = "status-widget", badge, message, time_note)
    })
    output$artifact_list <- renderUI({
      detail <- status_details()
      arts <- detail$artifacts
      if (is.null(arts) || nrow(arts) == 0) {
        return(NULL)
      }
      tags$div(
        class = "artifact-list",
        tags$small("Arquivos atualizados:"),
        tags$ul(
          lapply(seq_len(nrow(arts)), function(i) {
            tags$li(sprintf("%s (%s)", basename(arts$path[i]), format(arts$modified[i], "%d/%m/%Y %H:%M:%S")))
          })
        )
      )
    })

    listar_portais_seguro <- function() {
      portais_status("")
      out <- tryCatch({
        source("scripts/01_scraping.R", local = TRUE)
        list(portais = listar_coletores(), err = NULL)
      }, error = function(e) {
        list(portais = character(), err = conditionMessage(e))
      })
      if (!is.null(out$err) && nzchar(out$err)) {
        portais_status(paste0("Falha ao carregar coletores: ", out$err))
      }
      out$portais
    }

    atualizar_portais <- function() {
      portais <- listar_portais_seguro()
      if (length(portais) == 0) {
        portais <- "Todos"
        if (!nzchar(isolate(portais_status()))) {
          portais_status("Nenhum coletor disponivel. Verifique dependencias/pacotes do scraper.")
        }
      }
      updateSelectInput(
        session, "portais",
        choices = c("Todos", portais),
        selected = "Todos"
      )
      updateSelectInput(
        session, "portal_lacunas",
        choices = c("Selecione", portais),
        selected = if (length(portais) > 0) portais[[1]] else "Selecione"
      )
    }

    atualizar_portais()

    observeEvent(input$btn_refresh_portais, {
      atualizar_portais()
    })

    observeEvent(input$portais, {
      sel <- input$portais
      if (is.null(sel)) return()
      if (length(sel) == 0) {
        updateSelectInput(session, "portais", selected = "Todos")
        return()
      }
      if (length(sel) > 1 && "Todos" %in% sel) {
        updateSelectInput(session, "portais", selected = setdiff(sel, "Todos"))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$preset_3, {
      updateDateRangeInput(session, "range", start = Sys.Date() - 3, end = Sys.Date())
    })
    observeEvent(input$preset_7, {
      updateDateRangeInput(session, "range", start = Sys.Date() - 7, end = Sys.Date())
    })
    observeEvent(input$preset_30, {
      updateDateRangeInput(session, "range", start = Sys.Date() - 30, end = Sys.Date())
    })

    output$tab_resumo_portal <- renderTable({
      df <- dados_enr()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df %>%
        dplyr::filter(!is.na(data_pub)) %>%
        dplyr::group_by(portal) %>%
        dplyr::summarise(
          inicio = min(data_pub, na.rm = TRUE),
          fim    = max(data_pub, na.rm = TRUE),
          registros = dplyr::n(),
          dias_unicos = dplyr::n_distinct(data_pub),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          dias_periodo = as.integer(fim - inicio) + 1L,
          dias_faltando = pmax(dias_periodo - dias_unicos, 0L),
          cobertura_pct = ifelse(dias_periodo > 0, round(100 * dias_unicos / dias_periodo, 1), NA_real_),
          periodo = sprintf("%s - %s", format(inicio, "%d/%m/%Y"), format(fim, "%d/%m/%Y")),
          ultima_coleta = format(fim, "%d/%m/%Y")
        ) %>%
        dplyr::select(portal, periodo, dias_unicos, dias_faltando, cobertura_pct, registros, ultima_coleta) %>%
        dplyr::arrange(dplyr::desc(registros)) %>%
        dplyr::rename(
          "Portal" = portal,
          "Período" = periodo,
          "Dias únicos" = dias_unicos,
          "Dias faltando" = dias_faltando,
          "Cobertura (%)" = cobertura_pct,
          "Registros" = registros,
          "Última coleta" = ultima_coleta
        )
    }, rownames = FALSE)

    lacunas_cache <- reactive({
      df <- dados_enr()
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      inicio <- as.Date("2025-01-01")
      fim <- Sys.Date()
      if (inicio > fim) return(NULL)

      months_seq <- seq(lubridate::floor_date(inicio, "month"), lubridate::floor_date(fim, "month"), by = "month")
      if (length(months_seq) == 0) {
        return(NULL)
      }

      anos <- lubridate::year(months_seq)
      meses_num <- lubridate::month(months_seq)
      month_info <- tibble::tibble(
        mes = months_seq,
        ano = anos,
        mes_num = meses_num,
        mes_label = sprintf("%s %d", MES_PT[meses_num], anos),
        inicio = months_seq,
        fim = pmin(lubridate::ceiling_date(months_seq, "month") - 1, fim)
      )

      df_proc <- df %>%
        dplyr::mutate(data_pub = as.Date(data_publicacao)) %>%
        dplyr::filter(!is.na(data_pub), data_pub >= inicio, data_pub <= fim)

      if (nrow(df_proc) == 0) {
        return(NULL)
      }

      portal_ranges <- df_proc %>%
        dplyr::group_by(portal) %>%
        dplyr::summarise(
          inicio_portal = min(data_pub, na.rm = TRUE),
          fim_portal = max(data_pub, na.rm = TRUE),
          .groups = "drop"
        )

      portal_list <- sort(unique(df_proc$portal))
      portal_missing <- lapply(portal_list, function(portal) {
        presentes <- unique(df_proc$data_pub[df_proc$portal == portal])
        portal_range <- portal_ranges[portal_ranges$portal == portal, , drop = FALSE]
        portal_inicio <- portal_range$inicio_portal[1]
        portal_fim <- portal_range$fim_portal[1]

        calc <- mapply(function(primeiro, ultimo) {
          if (is.na(portal_inicio) || is.na(portal_fim)) {
            return(list(faltantes = as.Date(character()), fora_intervalo = TRUE))
          }
          calc_inicio <- max(primeiro, portal_inicio)
          calc_fim <- min(ultimo, portal_fim)
          if (is.na(calc_inicio) || is.na(calc_fim) || calc_inicio > calc_fim) {
            list(faltantes = as.Date(character()), fora_intervalo = TRUE)
          } else {
            dias_mes <- seq(calc_inicio, calc_fim, by = "day")
            list(faltantes = sort(setdiff(dias_mes, presentes)), fora_intervalo = FALSE)
          }
        }, month_info$inicio, month_info$fim, SIMPLIFY = FALSE)

        faltantes <- lapply(calc, `[[`, "faltantes")
        fora_intervalo <- vapply(calc, `[[`, logical(1), "fora_intervalo")
        month_info %>%
          dplyr::mutate(faltantes = faltantes, fora_intervalo = fora_intervalo)
      })
      names(portal_missing) <- portal_list

      portal_years <- df_proc %>%
        dplyr::group_by(portal) %>%
        dplyr::summarise(years = list(sort(unique(lubridate::year(data_pub)))), .groups = "drop")

      list(
        mes_info = month_info,
        year_span = sort(unique(month_info$ano)),
        year_domain = seq(ANO_MINIMO, lubridate::year(fim)),
        portal = portal_missing,
        portal_ranges = portal_ranges,
        portal_years = setNames(portal_years$years, portal_years$portal),
        inicio = min(month_info$inicio),
        fim = max(month_info$fim)
      )
    })

    observeEvent(lacunas_cache(), {
      cache <- lacunas_cache()
      if (is.null(cache)) return()
      year_options <- as.character(cache$year_domain)
      ano_choices <- c("Todos" = "todos", setNames(year_options, year_options))
      ano_atual <- isolate(input$ano_lacunas)
      mes_atual <- isolate(input$mes_lacunas)
      updateSelectInput(
        session, "ano_lacunas",
        choices = ano_choices,
        selected = if (!is.null(ano_atual) && ano_atual %in% unname(ano_choices)) ano_atual else "todos"
      )
      updateSelectInput(
        session, "mes_lacunas",
        choices = MONTH_SELECT_CHOICES,
        selected = if (!is.null(mes_atual) && mes_atual %in% unname(MONTH_SELECT_CHOICES)) mes_atual else "todos"
      )
    }, ignoreNULL = FALSE)

    observeEvent(input$portal_lacunas, {
      cache <- lacunas_cache()
      if (is.null(cache)) return()
      portal_sel <- input$portal_lacunas
      if (is.null(portal_sel) || portal_sel == "Selecione") return()
      portal_years <- cache$portal_years[[portal_sel]]
      default_year <- if (!is.null(portal_years) && length(portal_years) > 0) {
        as.character(max(portal_years, na.rm = TRUE))
      } else {
        "todos"
      }
      updateSelectInput(session, "ano_lacunas", selected = default_year)
      updateSelectInput(session, "mes_lacunas", selected = "todos")
    }, ignoreNULL = TRUE)

    output$lacunas_info <- renderUI({
      cache <- lacunas_cache()
      if (is.null(cache)) {
        return(tags$div(class = "text-muted", "Sem dados carregados."))
      }
      portal_sel <- input$portal_lacunas
      if (is.null(portal_sel) || portal_sel == "Selecione") {
        return(tags$div(class = "text-muted", "Selecione um portal para ver as lacunas."))
      }

      portal_tbl <- cache$portal[[portal_sel]]
      if (is.null(portal_tbl) || nrow(portal_tbl) == 0) {
        return(tags$div(
          class = "text-warning",
          sprintf("Portal %s ainda não possui registros no intervalo carregado.", portal_sel)
        ))
      }

      if (!"fora_intervalo" %in% names(portal_tbl)) {
        portal_tbl$fora_intervalo <- FALSE
      }

      portal_range <- NULL
      if (!is.null(cache$portal_ranges)) {
        portal_range <- cache$portal_ranges[cache$portal_ranges$portal == portal_sel, , drop = FALSE]
      }
      portal_inicio <- if (!is.null(portal_range) && nrow(portal_range) > 0) portal_range$inicio_portal[1] else as.Date(NA)
      portal_fim <- if (!is.null(portal_range) && nrow(portal_range) > 0) portal_range$fim_portal[1] else as.Date(NA)

      portal_tbl_in <- portal_tbl %>% dplyr::filter(!fora_intervalo)
      faltantes_total <- if (nrow(portal_tbl_in) > 0) lengths(portal_tbl_in$faltantes) else integer()
      resumo_total <- list(
        meses_total = nrow(portal_tbl_in),
        meses_completos = sum(faltantes_total == 0),
        dias_pendentes = sum(faltantes_total)
      )

      meses <- portal_tbl
      ano_sel <- input$ano_lacunas
      if (!is.null(ano_sel) && ano_sel != "todos") {
        ano_value <- suppressWarnings(as.integer(ano_sel))
        if (!is.na(ano_value)) {
          meses <- meses %>% dplyr::filter(ano == ano_value)
        } else {
          meses <- meses %>% dplyr::filter(FALSE)
        }
      }

      mes_sel <- input$mes_lacunas
      if (!is.null(mes_sel) && mes_sel != "todos") {
        mes_value <- suppressWarnings(as.integer(mes_sel))
        if (!is.na(mes_value)) {
          meses <- meses %>% dplyr::filter(mes_num == mes_value)
        } else {
          meses <- meses %>% dplyr::filter(FALSE)
        }
      }

      if (nrow(meses) == 0) {
        return(tags$div(
          class = "text-muted",
          "Nenhum mês disponível para o filtro aplicado."
        ))
      }

      meses_in <- meses %>% dplyr::filter(!fora_intervalo)
      faltantes_filtrados <- if (nrow(meses_in) > 0) lengths(meses_in$faltantes) else integer()
      resumo_filtro <- list(
        meses = nrow(meses_in),
        dias_pendentes = sum(faltantes_filtrados)
      )

      tags$div(
        tags$p(
          class = "text-muted",
          sprintf("Intervalo considerado: %s a %s",
                  format(portal_inicio, "%d/%m/%Y"),
                  format(portal_fim, "%d/%m/%Y"))
        ),
        tags$p(
          class = "text-info",
          sprintf(
            "Resumo: %s meses no período (completos: %s) | %s dias pendentes. Filtro atual: %s meses | %s dias pendentes.",
            resumo_total$meses_total,
            resumo_total$meses_completos,
            format(resumo_total$dias_pendentes, big.mark = ".", scientific = FALSE),
            resumo_filtro$meses,
            format(resumo_filtro$dias_pendentes, big.mark = ".", scientific = FALSE)
          )
        ),
        if (!is.null(coleta_timestamp())) {
          tags$p(class = "text-success", sprintf("Última atualização: %s", format(coleta_timestamp(), "%d/%m/%Y %H:%M:%S")))
        } else {
          NULL
        },
        tags$div(
          class = "lacunas-grid",
          style = "display:grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap:12px;",
          lapply(seq_len(nrow(meses)), function(i) {
            row <- meses[i, ]
            dias <- row$faltantes[[1]]
            fora_intervalo <- isTRUE(row$fora_intervalo)
            dias_lista <- if (fora_intervalo) {
              tags$span(class = "text-muted", "Fora do intervalo coletado")
            } else if (length(dias) == 0) {
              tags$span(class = "text-success", "Sem lacunas")
            } else {
              tags$div(
                class = "lacunas-days",
                paste(format(dias, "%d"), collapse = ", ")
              )
            }
            tags$div(
              class = "lacunas-card",
              tags$strong(row$mes_label),
              tags$p(
                class = "text-secondary mb-1",
                if (fora_intervalo) {
                  "Sem coleta nesse periodo"
                } else if (length(dias) == 0) {
                  "Dias completos"
                } else {
                  sprintf("%s dias faltando", length(dias))
                }
              ),
              dias_lista
            )
          })
        )
      )
    })

    log_txt <- reactiveVal("Aguardando execução.")
    output$log_exec <- renderText(log_txt())

    cancelar <- reactiveVal(FALSE)

    observeEvent(input$btn_cancelar, {
      cancelar(TRUE)
      log_txt("Cancelamento solicitado. A coleta atual terminará a etapa em andamento e depois vai parar.")
      showNotification("Cancelamento solicitado. Aguardando fim da etapa corrente.", type = "warning")
      set_status_details("warning", "Cancelamento solicitado; a coleta será interrompida após a etapa atual.", NULL)
    })

    observeEvent(input$btn_executar, {
      cancelar(FALSE)
      req(input$range)
      d1 <- as.Date(input$range[1])
      d2 <- as.Date(input$range[2])
      if (is.na(d1) || is.na(d2) || d1 > d2) {
        showNotification("Intervalo de datas inválido.", type = "error")
        return()
      }

      portais_sel <- input$portais
      if (is.null(portais_sel) || length(portais_sel) == 0 || identical(portais_sel, "Todos")) {
        portais_sel <- NULL
      }

      log_txt("Iniciando execução...")
      set_status_details("running", sprintf("Coleta em andamento (%s a %s) para portais: %s",
                                     format(d1, "%d/%m/%Y"),
                                     format(d2, "%d/%m/%Y"),
                                     if (is.null(portais_sel)) "Todos" else paste(portais_sel, collapse = ", ")), NULL)

      ok <- TRUE
      withProgress(message = "Executando coleta", value = 0, {
        incProgress(0.1, detail = "Carregando scripts de scraping")
        load_err <- NULL
        tryCatch({
          source("scripts/01_scraping.R", local = TRUE)
          source("scripts/02_parse.R", local = TRUE)
          source("scripts/03_cleaning.R", local = TRUE)
        }, error = function(e) {
          load_err <<- conditionMessage(e)
        })
        if (!is.null(load_err)) {
          ok <<- FALSE
          log_txt(paste("Erro carregando scripts:", load_err))
          showNotification(paste("Erro carregando scripts:", load_err), type = "error")
          set_status_details("error", paste("Erro carregando scripts:", load_err), NULL)
          return(NULL)
        }
        if (exists("CRIMES_AM_WORKDIR", inherits = TRUE)) {
          log_txt(paste("Workdir:", get("CRIMES_AM_WORKDIR", inherits = TRUE)))
        }

        incProgress(0.4, detail = "Rodando scraping")
        scrap_err <- NULL
        n_raw <- NA_integer_
        tryCatch({
          df_raw <- rodar_scraping(
            data_inicio = d1,
            data_fim    = d2,
            portais     = if (is.null(portais_sel)) listar_coletores() else portais_sel
          )
          n_raw <- if (is.data.frame(df_raw)) nrow(df_raw) else NA_integer_
        }, error = function(e) {
          scrap_err <<- conditionMessage(e)
        })
        if (!is.null(scrap_err)) {
          ok <<- FALSE
          log_txt(paste("Erro no scraping:", scrap_err))
          showNotification(paste("Erro no scraping:", scrap_err), type = "error")
          set_status_details("error", paste("Erro no scraping:", scrap_err), NULL)
          return(NULL)
        }
        if (!is.na(n_raw)) log_txt(paste("Scraping OK. Registros brutos:", n_raw))
        if (is.na(n_raw) || n_raw == 0) {
          ok <<- FALSE
          log_txt("Scraping não retornou notícias novas. Parse/cleaning não serão executados.")
          showNotification("Scraping concluído sem novas notícias. Nada para processar.", type = "warning")
          set_status_details("warning", "Scraping concluído sem novos registros; nenhuma atualização foi feita.", NULL)
          return()
        }

        if (isTRUE(cancelar())) {
          ok <<- FALSE
          log_txt("Execução interrompida pelo usuário após scraping. Dados coletados foram mantidos.")
          showNotification("Execução interrompida após scraping. Dados brutos mantidos.", type = "warning")
          set_status_details("warning", "Coleta interrompida pelo usuário; os dados brutos foram mantidos.", NULL)
          return()
        }

        if (isTRUE(input$rodar_pipeline)) {
          incProgress(0.7, detail = "Parse + cleaning")
          pipe_err <- NULL
          tryCatch({
            df_parsed <- parse_raw_files()
            clean_and_enrich_data(df_parsed)
          }, error = function(e) {
            pipe_err <<- conditionMessage(e)
          })
          if (!is.null(pipe_err)) {
            ok <<- FALSE
            log_txt(paste("Erro no parse/cleaning:", pipe_err))
            showNotification(paste("Erro no parse/cleaning:", pipe_err), type = "error")
            set_status_details("error", paste("Erro no parse/cleaning:", pipe_err), NULL)
            return(NULL)
          }
        }

        incProgress(1, detail = "Concluido")
      })

      if (!isTRUE(ok)) return()

      if (isTRUE(input$rodar_pipeline)) {
        dados_enr(carregar_principal())
        dados_est(carregar_estaticos())
        coleta_timestamp(Sys.time())
        log_txt("Execução finalizada com sucesso.")
        artifacts <- artifact_info(ARTIFACT_FILES)
        set_status_details(
          "success",
          sprintf(
            "Coleta concluída em %s; lacunas e cobertura atualizadas.",
            format(coleta_timestamp(), "%d/%m/%Y %H:%M:%S")
          ),
          artifacts
        )
        showNotification("Coleta finalizada e lacunas recalculadas.", type = "message")
      } else {
        log_txt("Coleta finalizada (pipeline desativado).")
        set_status_details(
          "warning",
          "Coleta finalizada, mas o pipeline está desativado; lacunas e cobertura não foram recalculadas.",
          NULL
        )
        showNotification("Coleta finalizada, mas pipeline desativado (lacunas não recalculadas).", type = "warning")
      }
    })
  })
}
