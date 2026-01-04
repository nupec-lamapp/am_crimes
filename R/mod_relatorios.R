############################################################
# mod_relatorios.R
# Modulo de relatorios e auditoria (outputs estaticos)
############################################################

mod_relatorios_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          class = "card-panel",
          h3("Relatorios & Auditoria")
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card-panel",
          h4("Resumo geral"),
          p("Responde: quao completa esta a coleta por portal e gravidade no periodo consolidado. Use para priorizar portais com baixa cobertura ou alta proporcao de casos graves."),
          DTOutput(ns("tbl_resumo_geral"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card-panel",
          h4("Indice de letalidade (mensal)"),
          p("Responde: a proporcao de crimes letais esta subindo ou caindo mes a mes? Investigue saltos ou quedas bruscas e alinhe comunicacao com stakeholders."),
          plotlyOutput(ns("plot_indice_mensal"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "card-panel",
          h4("Anomalias de classificacao"),
          p("Responde: quais casos estao fora dos padroes de classificacao. Use para revisar regras heuristicas, rotulos do modelo e decidir quando acionar revisao humana."),
          p(
            "Noticias rotuladas como 'Outros' ou fora dos padroes principais, ",
            "uteis para auditoria do classificador e ajustes de regras."
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
      validate(need(!is.null(df), "Resumo geral ainda nao foi gerado. Rode o script 04_analysis.R ou a pipeline completa."))
      datatable(
        df,
        rownames = FALSE,
        options = list(dom = "t", paging = FALSE)
      )
    })

    output$plot_indice_mensal <- renderPlotly({
      df <- indice_mensal()
      validate(need(!is.null(df), "Indice de letalidade mensal ainda nao foi gerado."))

      meses_abrev_pt <- c("Jan","Fev","Mar","Abr","Mai","Jun","Jul","Ago","Set","Out","Nov","Dez")
      df <- df %>%
        mutate(
          mes_data = suppressWarnings(as.Date(mes_ano)),
          mes_data = dplyr::coalesce(mes_data, lubridate::ymd(mes_ano)),
          mes_ord = lubridate::month(mes_data),
          ano_ord = lubridate::year(mes_data),
          mes_label = sprintf("%s/%s", meses_abrev_pt[mes_ord], ano_ord)
        ) %>%
        arrange(ano_ord, mes_ord) %>%
        mutate(mes_label = factor(mes_label, levels = unique(mes_label)))

      p <- ggplot(df, aes(x = mes_label, y = indice_letal, group = 1)) +
        geom_line(color = "#e74c3c", size = 1) +
        geom_point(color = "#e74c3c") +
        geom_text(
          aes(label = scales::percent(indice_letal, accuracy = 0.1)),
          size = 3,
          vjust = -0.5
        ) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        theme_minimal() +
        labs(
          title = "Indice de letalidade (mensal)",
          x = "Mes/ano",
          y = "Indice"
        )

      plotly::ggplotly(p)
    })

    output$tbl_anomalias <- renderDT({
      df <- anomalias()
      validate(need(!is.null(df), "Arquivo de anomalias ainda nao foi gerado."))
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
