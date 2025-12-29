############################################################
# APP SHINY - Monitor de Crimes Violentos (Amazonas)
# Versao 0.0.7 - Arquitetura modular
############################################################

source("global.R")
source("R/mod_dashboard.R")
source("R/mod_relatorios.R")
source("R/mod_controle_pipeline.R")
source("R/mod_controle_coleta.R")
source("R/mod_equipes_e_parcerias.R")

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#667eea",
    secondary = "#764ba2"
  ),

  tags$head(
    tags$link(rel = "stylesheet", href = "style.css")
  ),

  navbarPage(
    title = span(icon("shield-alt"), "Monitor de Crimes Violentos - AM"),
    id = "tabs_main",

    tabPanel(
      title = tagList(icon("info-circle"), "Apresentação"),
      fluidRow(
        column(
          width = 12,
          div(
            class = "header-nupec",
            div(
              class = "titulo-projeto",
              div(
                class = "titulo-projeto-row",
                div(
                  class = "titulo-projeto-logos titulo-projeto-logos-left",
                  tags$img(src = "logo_lamapp.jpg", class = "logo-parceiro", alt = "LAMAPP"),
                  tags$img(src = "logo_nupec.jpg", class = "logo-parceiro", alt = "NuPeC")
                ),
                div(
                  class = "titulo-projeto-text",
                  "Monitor de Crimes Violentos - Amazonas"
                ),
                div(
                  class = "titulo-projeto-logos titulo-projeto-logos-right",
                  tags$img(src = "Logo_UFAM.png", class = "logo-parceiro", alt = "UFAM"),
                  tags$img(src = "Logomarca ICET sem fundo PNG P.png", class = "logo-parceiro", alt = "ICET")
                )
              )
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
            uiOutput("apresentacao_md")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "panel-grid info-grid",
            div(
              class = "blue-panel",
              h5("Contexto institucional"),
              p("Este monitor agrega o Laboratório LAMAPP e o Núcleo NuPeC para colocar ciência de dados aplicada e pesquisa operacional a serviço da segurança pública no Amazonas."),
              p("UFAM e ICET dão musculatura acadêmica e extensão, ampliando a interlocução com políticas públicas e órgãos governamentais."),
              p("A governança do pipeline repousa em transparência, indicadores reprodutíveis e escalabilidade para acompanhar dinâmicas territoriais críticas.")
            ),
            div(
              class = "blue-panel",
              h5("Abordagem metodológica"),
              p("Coletamos portais policiais autorizados com scraping automatizado, aplicamos filtros heurísticos e testamos modelos de classificação para destacar padrões relevantes."),
              p("Auditorias de qualidade e validações humanas fecham o ciclo, enquanto logs e metadados alimentam o artigo em elaboração e os relatórios de escuta."),
              p("Esse arranjo reforça a reprodutibilidade, aproxima pesquisa e extensão e sustenta a base empírica para a tomada de decisão pública.")
            )
          )
        )
      )
    ),

    tabPanel(
      title = tagList(icon("tachometer-alt"), "Monitor Dinâmico"),
      mod_dashboard_ui("dashboard")
    ),

    tabPanel(
      title = tagList(icon("file-alt"), "Relatórios & Auditoria"),
      mod_relatorios_ui("relatorios")
    ),

    tabPanel(
      title = tagList(icon("tools"), "Controle da Coleta"),
      mod_controle_coleta_ui("coleta")
    ),

    tabPanel(
      title = tagList(icon("file-alt"), "Artigo em elaboração"),
      fluidRow(
        column(
          width = 12,
          div(
            class = "card-panel",
            includeMarkdown("ArtigoWebSrapingSegurancapublica.md")
          )
        )
      )
    ),

    tabPanel(
      title = tagList(icon("users"), "Equipe e Parcerias"),
      mod_equipes_e_parcerias_ui("equipes")
    )
  )
)

server <- function(input, output, session) {
  dados_enr <- reactiveVal(carregar_principal())
  dados_est <- reactiveVal(carregar_estaticos())

  stamp_arquivo <- function(path) {
    if (is.null(path) || !file.exists(path)) return("missing")
    info <- file.info(path)
    paste(path, info$mtime, info$size, sep = "|")
  }

  dados_enr_poll <- reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      stamp_arquivo(localizar_arquivo(DIR_PROCESSED, "crimes_classificados.csv"))
    },
    valueFunc = function() {
      carregar_principal()
    }
  )

  observeEvent(dados_enr_poll(), {
    dados_enr(dados_enr_poll())
  }, ignoreInit = TRUE)

  dados_est_poll <- reactivePoll(
    intervalMillis = 5000,
    session = session,
    checkFunc = function() {
      arquivos <- c(
        "04_resumo_geral.csv",
        "04_indice_letal_mensal.csv",
        "04_anomalias_classificacao.csv"
      )
      stamps <- vapply(arquivos, function(nome) {
        stamp_arquivo(localizar_arquivo(DIR_OUTPUTS, nome))
      }, character(1))
      paste(stamps, collapse = "||")
    },
    valueFunc = function() {
      carregar_estaticos()
    }
  )

  observeEvent(dados_est_poll(), {
    dados_est(dados_est_poll())
  }, ignoreInit = TRUE)

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
  mod_equipes_e_parcerias_server("equipes")
}

shinyApp(ui, server)
