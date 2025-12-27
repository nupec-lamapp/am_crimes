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
  tags$style(HTML("
    .institution-card {
      background: #fff;
      border-radius: 12px;
      box-shadow: 0 10px 25px rgba(15, 23, 42, 0.1);
      border: 1px solid #e5e7eb;
      padding: 1.5rem;
      width: 100%;
      max-width: 320px;
      text-align: center;
      display: flex;
      flex-direction: column;
      gap: 0.35rem;
      min-height: 210px;
    }
    .institution-card img {
      max-height: 64px;
      margin: 0 auto;
      object-fit: contain;
    }
    .institution-card a {
      color: #0d9488;
      font-weight: 600;
    }
    .institution-grid {
      display: grid;
      grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
      gap: 1rem;
      justify-items: stretch;
    }
    .institution-grid.text-grid {
      grid-template-columns: repeat(auto-fit, minmax(320px, 1fr));
    }
    .institution-card.text-card {
      text-align: left;
      max-width: 520px;
      width: 100%;
      padding: 2rem;
      min-height: auto;
      justify-content: flex-start;
      line-height: 1.5;
      gap: 0.6rem;
    }
    .institution-card.text-card p {
      margin-bottom: 0.75rem;
    }
    .institution-section-title {
      font-size: 1.4rem;
      font-weight: 600;
      margin-bottom: 0.5rem;
      color: #0f172a;
    }
  ")),

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
      fluidRow(
        column(
          width = 12,
          div(
            class = "card-panel",
            h5("Contexto Institucional"),
            p("O LaboratÃ³rio de Monitoramento e AvaliaÃ§Ã£o de PolÃ­ticas PÃºblicas (LAMAPP) agrega ciÃªncia de dados aplicada, inteligÃªncia artificial e pesquisa operacional com foco em evidÃªncias para a gestÃ£o pÃºblica amazÃ´nica. Seu portfÃ³lio inclui monitoramento territorial, dashboards de polÃ­ticas pÃºblicas e formaÃ§Ã£o de equipes multidisciplinares, conforme descrito em ", a(href = "https://lamapp-tec.vercel.app/", "lamapp-tec.vercel.app"), "."),
            p("O NÃºcleo de Pesquisa em CiÃªncia de Dados e OtimizaÃ§Ã£o (NuPeC) da UFAM complementa o trabalho com modelos de otimizaÃ§Ã£o, logÃ­stica e mÃ©todos quantitativos voltados para a resoluÃ§Ã£o de problemas complexos na AmazÃ´nia e na formaÃ§Ã£o de recursos humanos, apoiado pelo Instituto de CiÃªncias Exatas e Tecnologia (ICET). Veja as iniciativas em ", a(href = "https://www.nupec.ufam.edu.br/", "nupec.ufam.edu.br"), ".")
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "card-panel",
            h6("LAMAPP & NuPeC"),
            p("Ambos os centros estÃ£o sediados na UFAM/ICET e operam com programas de pesquisa, extensÃ£o e parcerias com Ã³rgÃ£os pÃºblicos, fortalecendo integraÃ§Ãµes entre ciÃªncia de dados, polÃ­ticas e impacto social."),
            p(tags$b("Equipe acadÃªmica:"), "Dr. Hidelbrando Ferreira Rodrigues e Dr. Luiz AntÃ´nio Nascimento de Souza sÃ£o doutores, com experiÃªncia em estatÃ­stica aplicada, polÃ­ticas pÃºblicas e gestÃ£o de conflitos territoriais, respectivamente; Thiago dos Santos MendonÃ§a e Gisele Pena da Silva trazem contribuiÃ§Ãµes tÃ©cnicas em engenharia de software e engenharia de produÃ§Ã£o para o projeto.")
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
      title = tagList(icon("users"), "Equipe e Parcerias"),
      fluidRow(
        column(
          width = 12,
          div(
            class = "institution-grid",
            div(
              class = "institution-card",
              tags$img(src = "logo_lamapp.jpg", alt = "LAMAPP"),
              h5("LAMAPP"),
              p("LaboratÃ³rio de Monitoramento e AvaliaÃ§Ã£o de PolÃ­ticas PÃºblicas"),
              tags$a(href = "https://lamapp-tec.vercel.app/", target = "_blank", rel = "noopener", "lamapp-tec.vercel.app")
            ),
            div(
              class = "institution-card",
              tags$img(src = "logo_nupec.jpg", alt = "NuPeC"),
              h5("NuPeC"),
              p("NÃºcleo de Pesquisa em CiÃªncia de Dados e OtimizaÃ§Ã£o"),
              tags$a(href = "https://www.nupec.ufam.edu.br/", target = "_blank", rel = "noopener", "nupec.ufam.edu.br")
            ),
            div(
              class = "institution-card",
              tags$img(src = "logo_tropa_dos_amigos.png", alt = "Tropa dos Amigos"),
              h5("Tropa dos Amigos Podcast"),
              p("Apoio institucional e divulgaÃ§Ã£o"),
              tags$a(href = "https://tropadosamigos.org", target = "_blank", rel = "noopener", "Tropa dos Amigos Podcast")
            ),
            div(
              class = "institution-card",
              tags$img(src = "Logo_UFAM.png", alt = "UFAM"),
              h5("UFAM"),
              p("Universidade Federal do Amazonas"),
              tags$a(href = "https://ufam.edu.br/", target = "_blank", rel = "noopener", "ufam.edu.br")
            ),
            div(
              class = "institution-card",
              tags$img(src = "Logomarca ICET sem fundo PNG P.png", alt = "ICET"),
              h5("ICET"),
              p("Instituto de CiÃªncias Exatas e Tecnologia"),
              tags$a(href = "https://www.icet.ufam.edu.br/", target = "_blank", rel = "noopener", "icet.ufam.edu.br")
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "institution-grid text-grid",
            div(
              class = "institution-card text-card",
              h5("CoordenaÃ§Ã£o cientÃ­fica"),
              p("Dr. Hidelbrando Ferreira Rodrigues (UFAM/ICET) Ã© o coordenador do projeto, com formaÃ§Ã£o em estatÃ­stica aplicada e experiÃªncia em monitoramento de polÃ­ticas pÃºblicas."),
              p("Dr. Luiz AntÃ´nio Nascimento de Souza atua como pesquisador sÃªnior, contribuindo com gestÃ£o de conflitos e mediaÃ§Ã£o territorial."),
              p("Thiago dos Santos MendonÃ§a (Engenharia de Software) e Gisele Pena da Silva (Engenharia de ProduÃ§Ã£o) compÃµem a equipe tÃ©cnica responsÃ¡vel pelas integraÃ§Ãµes e pela operacionalizaÃ§Ã£o do app.")
            ),
            div(
              class = "institution-card text-card",
              h5("Sobre o projeto"),
              p("Ferramenta experimental para monitoramento contÃ­nuo de notÃ­cias policiais no Amazonas. Integra scraping, NLP, classificaÃ§Ã£o heurÃ­stica e Painel Shiny para produzir indicadores reprodutÃ­veis a partir dos portais locais, com documentaÃ§Ã£o cientÃ­fica pronta para submissÃ£o.")
            )
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
    ),

    tabPanel(
      title = tagList(icon("users"), "Equipe e Parcerias"),
      fluidRow(
        column(
          width = 12,
          div(
            class = "institution-grid",
            div(
              class = "institution-card",
              tags$img(src = "logo_lamapp.jpg", alt = "LAMAPP"),
              h5("LAMAPP"),
              p("LaboratÃ³rio de Monitoramento e AvaliaÃ§Ã£o de PolÃ­ticas PÃºblicas"),
              tags$a(href = "https://lamapp-tec.vercel.app/", target = "_blank", rel = "noopener", "lamapp-tec.vercel.app")
            ),
            div(
              class = "institution-card",
              tags$img(src = "logo_nupec.jpg", alt = "NuPeC"),
              h5("NuPeC"),
              p("NÃºcleo de Pesquisa em CiÃªncia de Dados e OtimizaÃ§Ã£o"),
              tags$a(href = "https://www.nupec.ufam.edu.br/", target = "_blank", rel = "noopener", "nupec.ufam.edu.br")
            ),
            div(
              class = "institution-card",
              tags$img(src = "logo_tropa_dos_amigos.png", alt = "Tropa dos Amigos"),
              h5("Tropa dos Amigos Podcast"),
              p("Apoio institucional e divulgaÃ§Ã£o"),
              tags$a(href = "https://tropadosamigos.org", target = "_blank", rel = "noopener", "Tropa dos Amigos Podcast")
            ),
            div(
              class = "institution-card",
              tags$img(src = "Logo_UFAM.png", alt = "UFAM"),
              h5("UFAM"),
              p("Universidade Federal do Amazonas"),
              tags$a(href = "https://ufam.edu.br/", target = "_blank", rel = "noopener", "ufam.edu.br")
            ),
            div(
              class = "institution-card",
              tags$img(src = "Logomarca ICET sem fundo PNG P.png", alt = "ICET"),
              h5("ICET"),
              p("Instituto de CiÃªncias Exatas e Tecnologia"),
              tags$a(href = "https://www.icet.ufam.edu.br/", target = "_blank", rel = "noopener", "icet.ufam.edu.br")
            )
          )
        )
      ),
      ),
      fluidRow(
        column(
          width = 12,
          div(
            class = "institution-grid text-grid",
            div(
              class = "institution-card text-card",
              h5("CoordenaÃ§Ã£o cientÃ­fica"),
              p("Dr. Hidelbrando Ferreira Rodrigues (UFAM/ICET) coordena o projeto com expertise em estatÃ­stica aplicada e ciÃªncia de dados para seguranÃ§a pÃºblica."),
              p("Dr. Luiz AntÃ´nio Nascimento de Souza atua como pesquisador sÃªnior, adicionando experiÃªncia em conflitos territoriais e gestÃ£o estratÃ©gica."),
              p("Thiago dos Santos MendonÃ§a (Engenharia de Software) e Gisele Pena da Silva (Engenharia de ProduÃ§Ã£o) sustentam a implementaÃ§Ã£o tÃ©cnica e operacional.")
            ),
            div(
              class = "institution-card text-card",
              h5("Sobre o projeto"),
              p("Ferramenta experimental para monitoramento contÃ­nuo de notÃ­cias policiais no Amazonas; combina scraping, NLP, heurÃ­sticas de classificaÃ§Ã£o e um painel Shiny para gerar indicadores replicÃ¡veis e documentados.")
            )
          )
        )
      )
    ),
    tabPanel(
      title = tagList(icon("file-alt"), "Artigo em elaboraÃ§Ã£o"),
      fluidRow(
        column(
          width = 12,
          div(
            class = "card-panel",
            includeMarkdown("ArtigoWebSrapingSegurancapublica.md")
          )
        )
      )
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

