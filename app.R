 ############################################################
# APP SHINY - Monitor de Crimes Violentos (Amazonas)
# Versao 0.0.5 - Arquitetura modular
############################################################

source("global.R")
source("R/mod_dashboard.R")
source("R/mod_relatorios.R")
source("R/mod_controle_pipeline.R")
source("R/mod_controle_coleta.R")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#667eea",
    secondary = "#764ba2"
  ),

  tags$head(
    tags$link(rel = "stylesheet", href = "www/style.css")
  ),

  navbarPage(
    title = span(icon("shield-alt"), "Monitor de Crimes Violentos - AM"),
    id = "tabs_main",

    tabPanel(
      title = tagList(icon("info-circle"), "Apresentacao"),
      fluidRow(
        column(
          width = 12,
          div(
            class = "header-nupec",
            div(
              class = "titulo-projeto",
              "Monitor de Crimes Violentos - Amazonas"
            ),
            div(
              class = "subtitulo-projeto",
              "NUPEC / LAMAPP"
            ),
            div(
              class = "subtitulo-meta",
              textOutput("ultima_atualizacao", inline = TRUE)
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "card-panel",
            div(
              class = "logos-row",
              tags$img(src = "logo_lamapp.jpg", class = "logo-parceiro", alt = "LAMAPP"),
              tags$img(src = "logo_nupec.jpg", class = "logo-parceiro", alt = "NuPeC"),
              tags$img(src = "Logo_UFAM.png", class = "logo-parceiro", alt = "UFAM"),
              tags$img(src = "Logomarca ICET sem fundo PNG P.png", class = "logo-parceiro", alt = "ICET"),
              tags$img(src = "logo_tropa_dos_amigos.png", class = "logo-parceiro", alt = "Tropa dos Amigos")
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 8,
          div(
            class = "card-panel",
            uiOutput("apresentacao_md")
          )
        ),
        column(
          width = 4,
          div(
            class = "card-panel",
            h4("Executar pipeline (admin)"),
            mod_controle_pipeline_ui("pipeline")
          )
        )
      )
    ),

    tabPanel(
      title = tagList(icon("tachometer-alt"), "Monitor Dinamico"),
      mod_dashboard_ui("dashboard")
    ),

    tabPanel(
      title = tagList(icon("file-alt"), "Relatorios & Auditoria"),
      mod_relatorios_ui("relatorios")
    ),
    tabPanel(
      title = tagList(icon("tools"), "Controle da Coleta"),
      mod_controle_coleta_ui("coleta")
    )
  )
)

server <- function(input, output, session) {
  dados_enr <- reactiveVal(carregar_principal())
  dados_est <- reactiveVal(carregar_estaticos())

  output$ultima_atualizacao <- renderText({
    paste0("Ultima atualizacao: ",
           format(Sys.time(), "%d/%m/%Y %H:%M"))
  })

  output$apresentacao_md <- renderUI({
    carregar_apresentacao()
  })

  mod_dashboard_server("dashboard", dados_enr)
  mod_relatorios_server("relatorios", dados_est)
  mod_controle_pipeline_server("pipeline", dados_enr, dados_est)
  mod_controle_coleta_server("coleta", dados_enr, dados_est)
}

shinyApp(ui, server)
