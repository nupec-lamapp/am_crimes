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
    tags$link(rel = "stylesheet", href = "style.css")
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
          width = 12,
          div(
            class = "card-panel",
            h5("Contexto Institucional"),
            p("O Laboratório de Monitoramento e Avaliação de Políticas Públicas (LAMAPP) agrega ciência de dados aplicada, inteligência artificial e pesquisa operacional com foco em evidências para a gestão pública amazônica. Seu portfólio inclui monitoramento territorial, dashboards de políticas públicas e formação de equipes multidisciplinares, conforme descrito em ", a(href = "https://lamapp-tec.vercel.app/", "lamapp-tec.vercel.app"), "."),
            p("O Núcleo de Pesquisa em Ciência de Dados e Otimização (NuPeC) da UFAM complementa o trabalho com modelos de otimização, logística e métodos quantitativos voltados para a resolução de problemas complexos na Amazônia e na formação de recursos humanos, apoiado pelo Instituto de Ciências Exatas e Tecnologia (ICET). Veja as iniciativas em ", a(href = "https://www.nupec.ufam.edu.br/", "nupec.ufam.edu.br"), ".")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "card-panel",
            h6("LAMAPP & NuPeC"),
            p("Ambos os centros estão sediados na UFAM/ICET e operam com programas de pesquisa, extensão e parcerias com órgãos públicos, fortalecendo integrações entre ciência de dados, políticas e impacto social."),
            p(tags$b("Equipe acadêmica:"), "Hidelbrando Ferreira Rodrigues e Luiz Antônio Nascimento de Souza são doutores, com experiência em estatística aplicada, políticas públicas e gestão de conflitos territoriais, respectivamente; Thiago dos Santos Mendonça e Gisele Pena da Silva trazem contribuições técnicas em engenharia de software e engenharia de produção para o projeto.")
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
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "card-panel",
            h5("Equipe e parcerias"),
            tags$ul(
              tags$li("Hidelbrando Ferreira Rodrigues"),
              tags$li("Thiago dos Santos Mendonça"),
              tags$li("Gisele Pena da Silva"),
              tags$li("Luiz Antônio Nascimento de Souza (docente)")
            ),
            p("Uma iniciativa coordenada pelo LAMAPP / NuPeC com apoio institucional da UFAM e do ICET.")
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

