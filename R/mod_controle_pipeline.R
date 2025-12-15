############################################################
# mod_controle_pipeline.R
# Modulo de controle da execucao da pipeline (admin)
############################################################

mod_controle_pipeline_ui <- function(id) {
  ns <- NS(id)

  wellPanel(
    h4("Execucao da pipeline de coleta"),
    p("Este painel dispara a coleta automatica de noticias e a atualizacao dos dados."),
    actionButton(
      ns("btn_modal_scrap"),
      "Nova coleta (admin)",
      class = "btn btn-danger",
      icon = icon("play")
    )
  )
}

mod_controle_pipeline_server <- function(id, dados_enr, dados_est) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$btn_modal_scrap, {
      showModal(modalDialog(
        title = "Executar Pipeline de Coleta",
        "Atencao: Isso vai acionar o robo de scraping. Certifique-se de que os scripts em 'scripts' estao limpos (sem auto-execucao).",
        dateRangeInput(
          ns("dates_scrap"),
          "Intervalo de Coleta:",
          start = Sys.Date() - 7,
          end   = Sys.Date(),
          format = "dd/mm/yyyy",
          language = "pt-BR"
        ),
        footer = tagList(
          modalButton("Cancelar"),
          actionButton(ns("btn_confirmar_scrap"), "INICIAR COLETA", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$btn_confirmar_scrap, {
      removeModal()

      p1 <- file.path(DIR_PIPELINE, "01_scraping.R")
      p2 <- file.path(DIR_PIPELINE, "02_parse.R")
      p3 <- file.path(DIR_PIPELINE, "03_cleaning.R")
      p4 <- file.path(DIR_PIPELINE, "04_analysis.R")

      if (!file.exists(p1) || !file.exists(p2) || !file.exists(p3)) {
        showNotification("Scripts obrigatorios nao encontrados na pasta scripts!", type = "error")
        return()
      }

      withProgress(message = "Executando Pipeline...", value = 0, {
        incProgress(0.1, detail = "Carregando funcoes...")
        source(p1, local = TRUE)
        source(p2, local = TRUE)
        source(p3, local = TRUE)

        incProgress(0.3, detail = "Baixando noticias...")
        tryCatch({
          rodar_scraping(
            data_inicio = input$dates_scrap[1],
            data_fim    = input$dates_scrap[2]
          )
        }, error = function(e) {
          showNotification(paste("Erro Scraping:", e$message), type = "warning")
        })

        incProgress(0.6, detail = "Processando dados (parse + cleaning)...")
        tryCatch({
          df_parsed <- parse_raw_files()
          clean_and_enrich_data(df_parsed)
        }, error = function(e) {
          showNotification(paste("Erro Processamento:", e$message), type = "warning")
        })

        if (file.exists(p4)) {
          incProgress(0.8, detail = "Gerando relatorios estaticos...")
          tryCatch({
            source(p4, local = TRUE)
          }, error = function(e) {
            showNotification(paste("Erro Analises:", e$message), type = "warning")
          })
        }

        incProgress(1, detail = "Concluido!")
        dados_enr(carregar_principal())
        dados_est(carregar_estaticos())
        showNotification("Pipeline finalizado!", type = "success")
      })
    })
  })
}
