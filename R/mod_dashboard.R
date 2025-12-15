############################################################
# mod_dashboard.R
# Módulo principal de monitoramento dinâmico
############################################################

mod_dashboard_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h5("Filtros"),
          dateRangeInput(
            ns("filtro_data"),
            "Período:",
            start = Sys.Date() - 30,
            end   = Sys.Date(),
            format = "dd/mm/yyyy",
            language = "pt-BR"
          ),
          selectInput(ns("filtro_portal"), "Portal:", choices = "Todos"),
          div(class = "text-muted small", textOutput(ns("info_portal_periodo"))),
          selectInput(ns("fonte_tipo"), "Fonte de tipo:", choices = c("Heurística" = "heur", "Modelo NLP" = "ml")),
          selectInput(ns("filtro_tipo"), "Tipo de crime:", choices = "Todos"),
          selectInput(ns("filtro_gravidade"), "Gravidade:", choices = c("Todas", "baixa", "média", "alta", "muito alta", "extrema")),
          actionButton(ns("btn_aplicar"), "Aplicar filtros", class = "btn-primary btn-sm"),
          tags$hr(),
          h6("Cobertura por portal"),
          tableOutput(ns("tab_resumo_portal"))

        )
      ),
      column(
        width = 9,
        fluidRow(
          column(
            width = 4,
            div(
              class = "kpi-box",
              div(class = "kpi-title", "Notícias no recorte"),
              div(class = "kpi-value", textOutput(ns("kpi_total"))),
              div(class = "kpi-sub", textOutput(ns("kpi_periodo")))
            )
          ),
          column(
            width = 4,
            div(
              class = "kpi-box",
              div(class = "kpi-title", "Crimes letais violentos"),
              div(class = "kpi-value", textOutput(ns("kpi_letais"))),
              div(class = "kpi-sub", textOutput(ns("kpi_letal_pct")))
            )
          ),
          column(
            width = 4,
            div(
              class = "kpi-box",
              div(class = "kpi-title", "Principais tipos de crime"),
              div(class = "kpi-sub", textOutput(ns("kpi_top_tipos")))
            )
          )
        ),
        tabsetPanel(
          id = ns("tabs_dyn"),
          tabPanel(
            "Série Temporal",
            plotlyOutput(ns("plot_serie"))
          ),
          tabPanel(
            "Por Tipo",
            plotlyOutput(ns("plot_tipo"))
          ),
          tabPanel(
            "Tabela de Notícias",
            DTOutput(ns("tab_todas"))
          )
        )
      )
    )
  )
}

mod_dashboard_server <- function(id, dados_enr) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(dados_enr(), {
      df <- dados_enr()
      if (is.null(df) || nrow(df) == 0) return()

      updateDateRangeInput(
        session,
        "filtro_data",
        start = min(df$data_pub, na.rm = TRUE),
        end   = max(df$data_pub, na.rm = TRUE)
      )

      updateSelectInput(
        session, "filtro_portal",
        choices = c("Todos", sort(unique(df$portal))),
        selected = "Todos"
      )

      updateSelectInput(
        session, "filtro_gravidade",
        choices = c("Todas", sort(unique(df$gravidade))),
        selected = "Todas"
      )

      tipos <- sort(unique(df$tipo_principal))
      updateSelectInput(
        session, "filtro_tipo",
        choices = c("Todos", tipos),
        selected = "Todos"
      )
    }, ignoreNULL = FALSE)


    output$info_portal_periodo <- renderText({
      df <- dados_enr()
      portal_sel <- input$filtro_portal
      if (is.null(df) || nrow(df) == 0) return("Sem dados carregados ainda.")
      if (is.null(portal_sel) || portal_sel == "Todos") {
        return("Selecione um portal para ver o periodo coletado.")
      }
      df_portal <- df %>% filter(portal == portal_sel)
      if (nrow(df_portal) == 0) return("Portal ainda sem noticias coletadas.")
      dmin <- format(min(df_portal$data_pub, na.rm = TRUE), "%d/%m/%Y")
      dmax <- format(max(df_portal$data_pub, na.rm = TRUE), "%d/%m/%Y")
      sprintf("Periodo coletado em %s: %s a %s (total: %s)", portal_sel, dmin, dmax, nrow(df_portal))
    })
    output$tab_resumo_portal <- renderTable({
      df <- dados_enr()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df %>%
        filter(!is.na(data_pub)) %>%
        group_by(portal) %>%
        summarise(
          inicio = format(min(data_pub, na.rm = TRUE), "%d/%m/%Y"),
          fim    = format(max(data_pub, na.rm = TRUE), "%d/%m/%Y"),
          registros = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(registros))
    }, rownames = FALSE)
    dados_filt <- eventReactive(input$btn_aplicar, {
      df <- dados_enr()
      if (is.null(df) || nrow(df) == 0) return(df)

      req(input$filtro_data)
      d1 <- input$filtro_data[1]
      d2 <- input$filtro_data[2]

      df <- df %>%
        filter(
          data_pub >= d1,
          data_pub <= d2
        )

      if (!is.null(input$filtro_portal) && input$filtro_portal != "Todos") {
        df <- df %>% filter(portal == input$filtro_portal)
      }

      if (!is.null(input$filtro_gravidade) && input$filtro_gravidade != "Todas") {
        df <- df %>% filter(gravidade == input$filtro_gravidade)
      }

      if (!is.null(input$filtro_tipo) && input$filtro_tipo != "Todos") {
        if (!is.null(input$fonte_tipo) && input$fonte_tipo == "ml" && "tipo_ml" %in% names(df)) {
          df <- df %>% filter(tipo_ml == input$filtro_tipo)
        } else {
          df <- df %>% filter(tipo_principal == input$filtro_tipo)
        }
      }

      df
    }, ignoreNULL = FALSE)

    output$kpi_total <- renderText({
      df <- dados_filt()
      if (is.null(df)) return("0")
      format(nrow(df), big.mark = ".", decimal.mark = ",")
    })

    output$kpi_periodo <- renderText({
      df <- dados_filt()
      if (is.null(df) || nrow(df) == 0) return("Sem dados no recorte")
      sprintf(
        "De %s a %s",
        format(min(df$data_pub, na.rm = TRUE), "%d/%m/%Y"),
        format(max(df$data_pub, na.rm = TRUE), "%d/%m/%Y")
      )
    })

    output$kpi_letais <- renderText({
      df <- dados_filt()
      if (is.null(df) || nrow(df) == 0 || !"categoria" %in% names(df)) return("0")
      sum(df$categoria == "Crime Letal Violento", na.rm = TRUE)
    })

    output$kpi_letal_pct <- renderText({
      df <- dados_filt()
      if (is.null(df) || nrow(df) == 0 || !"categoria" %in% names(df)) return("")
      n_total <- nrow(df)
      n_letais <- sum(df$categoria == "Crime Letal Violento", na.rm = TRUE)
      if (n_total == 0) return("")
      pct <- round(100 * n_letais / n_total, 1)
      sprintf("%s%% das notícias do recorte", format(pct, decimal.mark = ","))
    })

    output$kpi_top_tipos <- renderText({
      df <- dados_filt()
      if (is.null(df) || nrow(df) == 0) return("Sem tipos predominantes")
      col_tipo <- if (!is.null(input$fonte_tipo) && input$fonte_tipo == "ml" && "tipo_ml" %in% names(df)) {
        "tipo_ml"
      } else {
        "tipo_principal"
      }
      df %>%
        count(.data[[col_tipo]], sort = TRUE) %>%
        head(3) %>%
        pull(.data[[col_tipo]]) %>%
        paste(collapse = " • ")
    })

    output$plot_serie <- renderPlotly({
      df <- dados_filt()
      validate(need(!is.null(df) && nrow(df) > 0, "Sem dados no recorte selecionado."))

      serie <- df %>%
        filter(!is.na(data_pub)) %>%
        count(data_pub)

      p <- ggplot(serie, aes(x = data_pub, y = n)) +
        geom_line(color = "#2980b9") +
        geom_point(color = "#2980b9") +
        labs(
          title = "Notícias por dia",
          x = "Data de publicação",
          y = "Número de notícias"
        ) +
        theme_minimal()

      plotly::ggplotly(p)
    })

    output$plot_tipo <- renderPlotly({
      df <- dados_filt()
      validate(need(!is.null(df) && nrow(df) > 0, "Sem dados no recorte selecionado."))

      col_tipo <- if (!is.null(input$fonte_tipo) && input$fonte_tipo == "ml" && "tipo_ml" %in% names(df)) {
        "tipo_ml"
      } else {
        "tipo_principal"
      }

      top <- df %>%
        count(.data[[col_tipo]], sort = TRUE) %>%
        slice_max(n, n = 15) %>%
        mutate(
          total = sum(n),
          pct = round(100 * n / total, 1),
          tipo_vis = forcats::fct_reorder(.data[[col_tipo]], n)
        )

      p <- ggplot(top, aes(x = tipo_vis, y = n)) +
        geom_col(fill = "#8e44ad") +
        geom_text(
          aes(label = paste0(pct, "%")),
          hjust = -0.1,
          size = 3,
          color = "black"
        ) +
        coord_flip() +
        labs(
          title = "Top tipos de crime no recorte",
          x = "Tipo",
          y = "Número de notícias (e % do total)"
        ) +
        theme_minimal()

      plotly::ggplotly(p)
    })

    output$tab_todas <- renderDT({
      df <- dados_filt()
      validate(need(!is.null(df) && nrow(df) > 0, "Sem dados para exibir."))

      df %>%
        arrange(desc(data_pub)) %>%
        select(
          Data = data_pub,
          Portal = portal,
          Título = titulo,
          Tipo = tipo_principal,
          Gravidade = gravidade,
          Gênero = genero_vitima
        ) %>%
        datatable(
          rownames = FALSE,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = c("copy", "csv"),
            pageLength = 20,
            scrollX = TRUE
          ),
          filter = "top"
        )
    })
  })
}

