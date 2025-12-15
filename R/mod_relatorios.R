############################################################
# mod_relatorios.R
# Módulo de relatórios e auditoria (outputs estáticos)
############################################################

mod_relatorios_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          class = "card-panel",
          h3("Relatórios & Auditoria")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card-panel",
          h4("Resumo geral"),
          DTOutput(ns("tbl_resumo_geral"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card-panel",
          h4("Índice de letalidade (mensal)"),
          plotlyOutput(ns("plot_indice_mensal"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card-panel",
          h4("Anomalias de classificação"),
          p(
            "Notícias rotuladas como 'Outros' ou fora dos padrões principais, ",
            "úteis para auditoria do classificador e ajustes de regras."
          ),
          DTOutput(ns("tbl_anomalias"))
        )
      )
    )
  )
}

mod_relatorios_server <- function(id, dados_est) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    resumo_geral <- reactive({
      est <- dados_est()
      if (is.null(est$resumo)) return(NULL)
      est$resumo
    })

    indice_mensal <- reactive({
      est <- dados_est()
      if (is.null(est$mensal)) return(NULL)
      est$mensal
    })

    anomalias <- reactive({
      est <- dados_est()
      if (is.null(est$anomalias)) return(NULL)
      est$anomalias
    })

    output$tbl_resumo_geral <- renderDT({
      df <- resumo_geral()
      validate(need(!is.null(df), "Resumo geral ainda não foi gerado. Rode o script 04_analysis.R ou a pipeline completa."))
      datatable(
        df,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE)
      )
    })

    output$plot_indice_mensal <- renderPlotly({
      df <- indice_mensal()
      validate(need(!is.null(df), "Índice de letalidade mensal ainda não foi gerado."))

      p <- ggplot(df, aes(x = mes_ano, y = indice_letal)) +
        geom_line(color = "#e74c3c", size = 1) +
        geom_point(color = "#e74c3c") +
        geom_label(
          aes(label = scales::percent(indice_letal, accuracy = 0.1)),
          size = 3,
          vjust = -0.5,
          label.size = 0,
          fill = NA
        ) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme_minimal() +
        labs(
          title = "Índice de letalidade (mensal)",
          x = "Mês/ano",
          y = "Índice"
        )

      plotly::ggplotly(p)
    })

    output$tbl_anomalias <- renderDT({
      df <- anomalias()
      validate(need(!is.null(df), "Arquivo de anomalias ainda não foi gerado."))
      datatable(
        df,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          dom = "Bfrtip",
          buttons = c("copy", "csv"),
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })
  })
}

