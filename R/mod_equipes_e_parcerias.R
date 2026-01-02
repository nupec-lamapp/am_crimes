############################################################
# mod_equipes_e_parcerias.R
# Módulo com as informações institucionais, equipe e créditos
############################################################

mod_equipes_e_parcerias_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        div(
          class = "institution-grid logo-grid",
          div(
            class = "institution-card logo-card",
            tags$img(src = "logo_lamapp.jpg", alt = "LAMAPP"),
            h5("LAMAPP"),
            p("Laboratório de Monitoramento e Avaliação de Políticas Públicas"),
            tags$a(href = "https://lamapp-tec.vercel.app/", target = "_blank", rel = "noopener", "lamapp-tec.vercel.app")
          ),
          div(
            class = "institution-card logo-card",
            tags$img(src = "logo_nupec.jpg", alt = "NuPeC"),
            h5("NuPeC"),
            p("Núcleo de Pesquisa em Ciências de Dados e Otimização"),
            tags$a(href = "https://www.nupec.ufam.edu.br/", target = "_blank", rel = "noopener", "nupec.ufam.edu.br")
          ),
          div(
            class = "institution-card logo-card",
            tags$img(src = "logo_tropa_dos_amigos.png", alt = "Tropa dos Amigos"),
            h5("Tropa dos Amigos Podcast"),
            p("Apoio institucional e divulgação"),
            tags$a(href = "https://tropadosamigos.org", target = "_blank", rel = "noopener", "Tropa dos Amigos Podcast")
          ),
          div(
            class = "institution-card logo-card",
            tags$img(src = "Logo_UFAM.png", alt = "UFAM"),
            h5("UFAM"),
            p("Universidade Federal do Amazonas"),
            tags$a(href = "https://ufam.edu.br/", target = "_blank", rel = "noopener", "ufam.edu.br")
          ),
          div(
            class = "institution-card logo-card",
            tags$img(src = "Logomarca ICET sem fundo PNG P.png", alt = "ICET"),
            h5("ICET"),
            p("Instituto de Ciências Exatas e Tecnologia"),
            tags$a(href = "https://www.icet.ufam.edu.br/", target = "_blank", rel = "noopener", "icet.ufam.edu.br")
          )
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
            h5("Sobre o projeto"),
            p("Ferramenta experimental de monitoramento de notícias policiais no Amazonas voltada a dados reprodutíveis, ética e alinhamento com academia e poder público."),
            p("O artigo em elaboração registra coleta, classificação e validação, oferecendo lições para políticas de segurança territorial e metodologias híbridas.")
          ),
          div(
            class = "blue-panel",
            h5("Equipe e parcerias institucionais"),
            p("Coordenação científica: Dr. Hidelbrando Ferreira Rodrigues (UFAM/ICET) e Dr. Luiz Antônio Nascimento de Souza (UFAM/NuPeC)."),
            p("Equipe técnica: Thiago dos Santos Mendonça (Engenharia de Software) e Gisele Pena da Silva (Engenharia de Produção)."),
            p("Parcerias institucionais: LAMAPP, NuPeC, UFAM, ICET e o podcast Tropa dos Amigos ampliam a visibilidade do projeto.")
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        div(
          class = "blue-panel credits-panel",
          h5("Créditos"),
          p("Projeto sustentado pelos laboratórios LAMAPP e NuPeC, com apoio institucional da UFAM e do ICET."),
          p("Coordenação e supervisão científica: Dr. Hidelbrando Ferreira Rodrigues e Dr. Luiz Antônio Nascimento de Souza."),
          p("Thiago dos Santos Mendonça e Gisele Pena da Silva conduzem a engenharia e a produção; agradecimentos ao podcast Tropa dos Amigos pela divulgação.")
        )
      )
    )
  )
}

mod_equipes_e_parcerias_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    invisible(NULL)
  })
}
