############################################################
# mod_controle_coleta.R
# Painel interativo para acionar raspagem e pipeline
############################################################

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
        actionButton(ns("btn_cancelar"), "Interromper (apos etapa atual)", class = "btn btn-danger btn-sm")
      )
    ),
    column(
      width = 8,
      div(
        class = "card-panel",
        h4("Cobertura por portal (dados carregados)"),
        tableOutput(ns("tab_resumo_portal"))
      ),
      div(
        class = "card-panel",
        h4("Lacunas por portal"),
        selectInput(ns("portal_lacunas"), "Portal:", choices = "Selecione", width = "50%"),
        tags$small("Selecionar um portal para ver todos os meses desde janeiro de 2025 e os dias que ainda não foram coletados."),
        htmlOutput(ns("lacunas_info"))
      ),
      div(
        class = "card-panel",
        h4("Log da execucao"),
        verbatimTextOutput(ns("log_exec"))
      )
    )
  )
}

mod_controle_coleta_server <- function(id, dados_enr, dados_est) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    portais_status <- reactiveVal("")
    output$portais_status <- renderText(portais_status())

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
          inicio = format(min(data_pub, na.rm = TRUE), "%d/%m/%Y"),
          fim    = format(max(data_pub, na.rm = TRUE), "%d/%m/%Y"),
          registros = dplyr::n(),
          .groups = "drop"
        ) %>%
        dplyr::arrange(dplyr::desc(registros))
    }, rownames = FALSE)

    lacunas_cache <- reactive({
      df <- dados_enr()
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      inicio <- as.Date("2025-01-01")
      fim <- Sys.Date()
      meses <- seq(lubridate::floor_date(inicio, "month"), lubridate::floor_date(fim, "month"), by = "month")
      if (length(meses) == 0) {
        return(NULL)
      }

      month_ranges <- lapply(meses, function(mes) {
        primeiro_dia <- mes
        ultimo_dia <- min(lubridate::ceiling_date(mes, "month") - 1, fim)
        seq(primeiro_dia, ultimo_dia, by = "day")
      })

      df_proc <- df %>%
        dplyr::mutate(data_pub = as.Date(data_publicacao)) %>%
        dplyr::filter(!is.na(data_pub), data_pub >= inicio, data_pub <= fim)

      portais <- sort(unique(na.omit(df_proc$portal)))
      portal_missing <- lapply(portais, function(portal) {
        presentes <- unique(df_proc$data_pub[df_proc$portal == portal])
        lapply(month_ranges, function(dias) sort(setdiff(dias, presentes)))
      })
      names(portal_missing) <- portais

      list(
        meses = meses,
        inicio = inicio,
        fim = fim,
        portal = portal_missing
      )
    })

    output$lacunas_info <- renderUI({
      cache <- lacunas_cache()
      if (is.null(cache)) {
        return(tags$div(class = "text-muted", "Sem dados carregados."))
      }
      portal_sel <- input$portal_lacunas
      if (is.null(portal_sel) || portal_sel == "Selecione") {
        return(tags$div(class = "text-muted", "Selecione um portal para ver as lacunas."))
      }

      faltantes <- cache$portal[[portal_sel]]
      if (is.null(faltantes)) {
        return(tags$div(
          class = "text-warning",
          sprintf("Ainda não há registros para %s entre %s e %s.", portal_sel, format(cache$inicio, "%d/%m/%Y"), format(cache$fim, "%d/%m/%Y"))
        ))
      }

      tags$div(
        tags$p(
          class = "text-muted",
          sprintf("Intervalo considerado: %s a %s",
                  format(cache$inicio, "%d/%m/%Y"),
                  format(cache$fim, "%d/%m/%Y"))
        ),
        tags$div(
          class = "lacunas-grid",
          style = "display:grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap:12px;",
          lapply(seq_along(cache$meses), function(i) {
            mes <- cache$meses[i]
            dias <- faltantes[[i]]
            dias_lista <- if (length(dias) == 0) {
              tags$span(class = "text-success", "Sem lacunas")
            } else {
              tags$div(
                class = "lacunas-days",
                paste(format(dias, "%d"), collapse = ", ")
              )
            }
            tags$div(
              class = "lacunas-card",
              tags$strong(format(mes, "%B %Y")),
              tags$p(
                class = "text-secondary mb-1",
                if (length(dias) == 0) "Dias completos" else sprintf("%s dias faltando", length(dias))
              ),
              dias_lista
            )
          })
        )
      )
    })

    log_txt <- reactiveVal("Aguardando execucao.")
    output$log_exec <- renderText(log_txt())

    cancelar <- reactiveVal(FALSE)

    observeEvent(input$btn_cancelar, {
      cancelar(TRUE)
      log_txt("Cancelamento solicitado. A coleta atual terminara a etapa em andamento e depois para.")
      showNotification("Cancelamento solicitado. Aguardando fim da etapa corrente.", type = "warning")
    })

    observeEvent(input$btn_executar, {
      cancelar(FALSE)
      req(input$range)
      d1 <- as.Date(input$range[1])
      d2 <- as.Date(input$range[2])
      if (is.na(d1) || is.na(d2) || d1 > d2) {
        showNotification("Intervalo de datas invalido.", type = "error")
        return()
      }

      portais_sel <- input$portais
      if (is.null(portais_sel) || "Todos" %in% portais_sel) {
        portais_sel <- NULL
      }

      log_txt("Iniciando execucao...")

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
          return(NULL)
        }
        if (!is.na(n_raw)) log_txt(paste("Scraping OK. Registros brutos:", n_raw))
        if (is.na(n_raw) || n_raw == 0) {
          ok <<- FALSE
          log_txt("Scraping nao retornou noticias novas. Parse/cleaning nao serao executados.")
          showNotification("Scraping concluido sem novas noticias. Nada para processar.", type = "warning")
          return()
        }

        if (isTRUE(cancelar())) {
          ok <<- FALSE
          log_txt("Execucao interrompida pelo usuario apos scraping. Dados coletados foram mantidos.")
          showNotification("Execucao interrompida apos scraping. Dados brutos mantidos.", type = "warning")
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
            return(NULL)
          }
        }

        incProgress(1, detail = "Concluido")
      })

      if (!isTRUE(ok)) return()

      dados_enr(carregar_principal())
      dados_est(carregar_estaticos())
      log_txt("Execucao finalizada com sucesso.")
      showNotification("Coleta finalizada.", type = "message")
    })
  })
}
