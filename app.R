############################################################
# APP SHINY – Monitor de Crimes Violentos (Amazonas)
# Versão 0.0.1 - Layout de Logos Horizontal + Apresentação Integrada
############################################################

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(DT)
library(readr)
library(lubridate)
library(stringr)
library(fs)
library(plotly)
library(reactable)

# --- CONFIGURAÇÃO DE CAMINHOS ---
DIR_PIPELINE  <- "scripts_pipeline"
DIR_PROCESSED <- "data/processed"
DIR_OUTPUTS   <- "outputs"

# Função auxiliar para localizar arquivos
localizar_arquivo <- function(pasta, nome) {
  caminho <- file.path(pasta, nome)
  if (file.exists(caminho)) return(caminho)
  if (file.exists(nome)) return(nome)
  return(NULL)
}

############################################################
# 1. FUNÇÕES DE ENRIQUECIMENTO (LÓGICA DE NEGÓCIO)
############################################################

# Tenta extrair gênero pelo título
extrair_genero <- function(texto) {
  if(is.na(texto)) return("indefinido")
  texto <- tolower(texto)
  if (str_detect(texto, "\\b(mulher|esposa|namorada|jovem|menina|senhora|mãe|filha|adolescente)\\b")) return("feminino")
  if (str_detect(texto, "\\b(homem|marido|namorado|rapaz|menino|senhor|pai|filho|suspeito)\\b")) return("masculino")
  return("indefinido")
}

# Tenta classificar faixa etária
classificar_faixa <- function(texto) {
  if(is.na(texto)) return("idade não informada")
  match <- str_extract(texto, "\\b\\d{1,2}(?=\\s?anos)\\b")
  if (is.na(match)) return("idade não informada")
  idade <- as.integer(match)
  
  if (idade <= 11) return("0–11 (criança)")
  if (idade <= 17) return("12–17 (adolescente)")
  if (idade <= 29) return("18–29 (jovem)")
  if (idade <= 59) return("30–59 (adulto)")
  return("60+ (idoso)")
}

############################################################
# 2. FUNÇÕES DE CARREGAMENTO
############################################################

# Carrega a base principal (Dinâmica) + Enriquecimento on-the-fly
carregar_principal <- function() {
  arquivo <- localizar_arquivo(DIR_PROCESSED, "crimes_classificados.csv")
  if (is.null(arquivo)) return(NULL)
  
  tryCatch({
    df <- read_csv(arquivo, show_col_types = FALSE) %>%
      mutate(data_publicacao = as.Date(data_publicacao))
    
    # Garante colunas básicas
    if(!"tipo_principal" %in% names(df)) df$tipo_principal <- "Outros"
    if(!"gravidade" %in% names(df)) df$gravidade <- "indefinida"
    if(!"crime_violento" %in% names(df)) df$crime_violento <- FALSE
    if(!"categoria" %in% names(df)) df$categoria <- "Geral"
    
    # Aplica enriquecimento se as colunas não existirem
    if(!"genero_vitima" %in% names(df)) df$genero_vitima <- sapply(df$titulo, extrair_genero)
    if(!"faixa_etaria" %in% names(df))  df$faixa_etaria  <- sapply(df$titulo, classificar_faixa)
    
    # Alias de data para compatibilidade com código visual
    df$data_pub <- df$data_publicacao 
    
    return(df)
  }, error = function(e) NULL)
}

# Carrega arquivos estáticos (Outputs do script 04)
carregar_estaticos <- function() {
  lista <- list()
  f_anom <- localizar_arquivo(DIR_OUTPUTS, "04_anomalias_classificacao.csv")
  if(!is.null(f_anom)) lista$anomalias <- read_csv(f_anom, show_col_types = FALSE)
  
  f_ind <- localizar_arquivo(DIR_OUTPUTS, "04_indice_letal_mensal.csv")
  if(!is.null(f_ind)) lista$mensal <- read_csv(f_ind, show_col_types = FALSE)
  
  f_res <- localizar_arquivo(DIR_OUTPUTS, "04_resumo_geral.csv")
  if(!is.null(f_res)) lista$resumo <- read_csv(f_res, show_col_types = FALSE)
  return(lista)
}

# Carrega e renderiza apresentação markdown
carregar_apresentacao <- function() {
  arquivo <- "APRESENTACAO.md"
  if (!file.exists(arquivo)) return(HTML("<p>Arquivo de apresentação não encontrado.</p>"))
  
  tryCatch({
    linhas <- readLines(arquivo, encoding = "UTF-8", warn = FALSE)
    html_parts <- character()
    in_list <- FALSE
    
    for (i in seq_along(linhas)) {
      linha <- linhas[i]
      
      # Separadores
      if (grepl("^---$", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        html_parts <- c(html_parts, "<hr style='margin:20px 0;' />")
        next
      }
      
      # Títulos
      if (grepl("^### ", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        titulo <- gsub("^### ", "", linha)
        html_parts <- c(html_parts, paste0("<h3 style='margin-top:20px; color:#2c3e50;'>", titulo, "</h3>"))
        next
      }
      if (grepl("^## ", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        titulo <- gsub("^## ", "", linha)
        html_parts <- c(html_parts, paste0("<h2 style='margin-top:25px; color:#2c3e50; border-bottom:2px solid #667eea; padding-bottom:5px;'>", titulo, "</h2>"))
        next
      }
      if (grepl("^# ", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        titulo <- gsub("^# ", "", linha)
        html_parts <- c(html_parts, paste0("<h1 style='margin-top:30px; color:#667eea;'>", titulo, "</h1>"))
        next
      }
      
      # Listas
      if (grepl("^- ", linha)) {
        if (!in_list) {
          html_parts <- c(html_parts, "<ul style='margin-left:20px;'>")
          in_list <- TRUE
        }
        item <- gsub("^- ", "", linha)
        # Processa negrito dentro do item
        item <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", item)
        html_parts <- c(html_parts, paste0("<li style='margin:5px 0;'>", item, "</li>"))
        next
      }
      
      # Linha em branco
      if (trimws(linha) == "") {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        html_parts <- c(html_parts, "<br />")
        next
      }
      
      # Imagens
      if (grepl("^!\\[", linha)) {
        if (in_list) {
          html_parts <- c(html_parts, "</ul>")
          in_list <- FALSE
        }
        # Extrai alt e src
        match <- regmatches(linha, regexpr("!\\[([^\\]]+)\\]\\(([^)]+)\\)", linha))
        if (length(match) > 0) {
          alt <- gsub("!\\[([^\\]]+)\\]\\(.*\\)", "\\1", match)
          src <- gsub("!\\[.*\\]\\(([^)]+)\\)", "\\1", match)
          html_parts <- c(html_parts, paste0("<div style='text-align:center; margin:15px 0;'><img src='", src, "' alt='", alt, "' style='max-width:250px; height:auto; margin:10px;' /></div>"))
        }
        next
      }
      
      # Parágrafos normais
      if (in_list) {
        html_parts <- c(html_parts, "</ul>")
        in_list <- FALSE
      }
      
      # Processa negrito
      linha <- gsub("\\*\\*(.*?)\\*\\*", "<strong>\\1</strong>", linha)
      
      # Processa links (se houver)
      linha <- gsub("\\[([^\\]]+)\\]\\(([^)]+)\\)", "<a href='\\2' target='_blank'>\\1</a>", linha)
      
      html_parts <- c(html_parts, paste0("<p style='text-align:justify; line-height:1.6; margin:10px 0;'>", linha, "</p>"))
    }
    
    if (in_list) {
      html_parts <- c(html_parts, "</ul>")
    }
    
    html_final <- paste(html_parts, collapse = "\n")
    return(HTML(html_final))
  }, error = function(e) {
    return(HTML(paste0("<p>Erro ao carregar apresentação: ", e$message, "</p>")))
  })
}

############################################################
# 3. UI (VISUAL PROFISSIONAL)
############################################################

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .header-nupec {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        padding: 20px;
        text-align: center;
        margin-bottom: 15px;
        border-radius: 8px;
        box-shadow: 0 4px 6px rgba(0,0,0,0.1);
      }
      .titulo-projeto { font-size: 24px; font-weight: 700; margin-bottom: 5px; letter-spacing: 0.5px; }
      .subtitulo-projeto { font-size: 14px; opacity: 0.9; font-weight: 300; }
      .kpi-box {
        background: white;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.05);
        border-left: 5px solid #667eea;
        transition: transform 0.2s;
      }
      .kpi-box:hover { transform: translateY(-2px); }
      .kpi-title { font-size: 12px; text-transform: uppercase; color: #7f8c8d; margin-bottom: 5px; font-weight: 600; }
      .kpi-value { font-size: 24px; font-weight: 700; color: #2c3e50; }
      .kpi-sub { font-size: 11px; color: #95a5a6; margin-top: 3px; }
      .nav-tabs > li > a { color: #2c3e50; font-weight: 600; }
      .status-list { list-style: none; padding-left: 0; font-size: 0.85em; color: #666; margin-top: 10px; }
      .credit-wrapper { background:#f7f9fc; border-radius:12px; padding:30px; margin-bottom:20px; box-shadow:0 8px 20px rgba(0,0,0,0.05); }
      .credit-card { background:white; border-radius:12px; padding:20px; box-shadow:0 6px 14px rgba(0,0,0,0.08); margin-bottom:20px; }
      .credit-card img { max-width:160px; object-fit:contain; margin-bottom:10px; }
      .credit-section h4 { font-weight:600; color:#2c3e50; }
      .insight-text { font-size:0.95em; margin-top:6px; color:#2c3e50; background:#f4f6fb; border-left:4px solid #667eea; padding:10px 12px; border-radius:8px; }
    "))
  ),
  
  div(
    class = "header-nupec",
    fluidRow(
      column(
        3,
        div(style="text-align:left; display:flex; align-items:center; gap:10px;",
            tags$img(src = "Logo_UFAM.png", height = "60px", style = "max-width:100%; object-fit:contain;"),
            tags$img(src = "Logomarca ICET sem fundo PNG P.png", height = "60px", style = "max-width:100%; object-fit:contain;"))
      ),
      column(
        6,
        div(class = "titulo-projeto", icon("chart-line"), " Monitor de Crimes Violentos - Amazonas"),
        div(class = "subtitulo-projeto", "NuPeC / LAMAPP - Classificação automatizada de tipologias criminais")
      ),
      column(
        3,
        div(style="text-align:right; display:flex; align-items:center; gap:10px; justify-content:flex-end;",
            tags$img(src = "logo_lamapp.jpg", height = "60px", style = "max-width:100%; object-fit:contain;"),
            tags$img(src = "logo_nupec.jpg", height = "60px", style = "max-width:100%; object-fit:contain;"))
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4(icon("calendar-alt"), " Coleta"),
      dateRangeInput("datas", "Intervalo:", start = Sys.Date() - 30, end = Sys.Date(), format = "dd/mm/yyyy", language = "pt-BR"),
      
      # actionButton("btn_modal_scrap", " Nova Coleta (Pipeline)", class = "btn-primary w-100", icon = icon("sync")),
      
      tags$hr(),
      
      h4(icon("filter"), " Filtros Visuais"),
      radioButtons("fonte_tipo", "Fonte da tipologia:",
                   choices = c("Heurística" = "heuristica",
                               "Modelo NLP (se disponível)" = "ml"),
                   selected = "heuristica"),
      uiOutput("aviso_nlp"),
      selectInput("filtro_tipo",      "Tipo de Crime:",  choices = c("Todos"), selected = "Todos"),
      selectInput("filtro_gravidade", "Gravidade:",      choices = c("Todos", "extrema", "muito alta", "alta", "indefinida"), selected = "Todos"),
      selectInput("filtro_genero",    "Gênero Vítima:",  choices = c("Todos", "feminino", "masculino", "indefinido"), selected = "Todos"),
      selectInput("filtro_faixa",     "Faixa Etária:",   choices = c("Todas", "0–11 (criança)", "12–17 (adolescente)", "18–29 (jovem)", "30–59 (adulto)", "60+ (idoso)"), selected = "Todas"),
      
      tags$hr(),
      actionButton("btn_reload", " Recarregar CSVs", icon = icon("hdd"), class = "btn-light btn-sm w-100"),
      br(),
      uiOutput("status_arquivos"),
      br(),
      downloadButton("download_crimes",     " Baixar CSV Filtrado", class = "btn-success w-100")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs_main",
        
        # --- ABA 1: Apresentação ---
        tabPanel(
          tagList(icon("info-circle"), "Apresentação"),
          fluidPage(
            div(style="padding:20px;",
                uiOutput("apresentacao_html")
            )
          )
        ),
        
        # --- ABA 2: Visão Geral ---
        tabPanel(
          title = tagList(icon("tachometer-alt"), "Visão Geral"),
          br(),
          fluidRow(
            column(3, div(class = "kpi-box",
                          div(class = "kpi-title", "Ocorrências"),
                          div(class = "kpi-value", textOutput("kpi1")),
                          div(class = "kpi-sub", "no período selecionado"))),
            column(3, div(class = "kpi-box",
                          div(class = "kpi-title", "% Feminino"),
                          div(class = "kpi-value", textOutput("kpi2")),
                          div(class = "kpi-sub", "vítimas identificadas"))),
            column(3, div(class = "kpi-box",
                          div(class = "kpi-title", "Crime + Comum"),
                          div(class = "kpi-value", textOutput("kpi3"), style="font-size: 18px;"),
                          div(class = "kpi-sub", "tipologia frequente"))),
            column(3, div(class = "kpi-box",
                          div(class = "kpi-title", "Gravidade"),
                          div(class = "kpi-value", textOutput("kpi4"), style="font-size: 18px;"),
                          div(class = "kpi-sub", "nível predominante")))
          ),
          hr(),
          fluidRow(
            column(8, h5("Evolução Temporal"), plotlyOutput("plot_temp", height = "320px")),
            column(4, h5("Distribuição por Gênero"), plotlyOutput("plot_vit", height = "320px"))
          ),
          br(),
          h5("Últimas Notícias Coletadas"),
          DTOutput("tab_resumo_head")
        ),
        
        # --- ABA 3: Análise Detalhada ---
        tabPanel(tagList(icon("chart-bar"), "Análise Detalhada"), 
                 br(), 
                 fluidRow(
                   column(6, plotlyOutput("plot_tipo", height = "400px")),
                   column(6, plotlyOutput("plot_grav", height = "400px"))
                 ),
                 hr(),
                 plotlyOutput("plot_faixa", height = "300px"),
                 hr(),
                 h5("Resumo de Tipologias (interativo)"),
                 reactableOutput("tbl_resumo_tipo")
        ),
        
        # --- ABA 4: Cruzamentos ---
        tabPanel(tagList(icon("layer-group"), "Cruzamentos"),
                 br(),
                 h4("Tipologia x Gravidade"),
                 plotlyOutput("plot_tipo_grav", height = "360px"),
                 uiOutput("txt_tipo_grav"),
                 hr(),
                 h4("Portal x Gravidade"),
                 plotlyOutput("plot_portal_grav", height = "360px"),
                 uiOutput("txt_portal_grav"),
                 hr(),
                 h4("Composição por Gênero"),
                 plotlyOutput("plot_tipo_genero", height = "360px"),
                 uiOutput("txt_tipo_genero"),
                 hr(),
                 h4("Composição por Faixa Etária"),
                 plotlyOutput("plot_tipo_faixa_cross", height = "360px"),
                 uiOutput("txt_tipo_faixa"),
                 hr(),
                 h4("Divergências Heurística x Modelo NLP"),
                 reactableOutput("tbl_divergencias")
        ),
        
        # --- ABA 5: Relatórios Oficiais (NOVA ABA INTEGRADA) ---
        tabPanel(tagList(icon("file-alt"), "Relatórios & Auditoria"),
                 br(),
                 div(class="alert alert-light", "Dados gerados pelo script 04 (análises estáticas)."),
                 tabsetPanel(
                   tabPanel("Série Mensal", br(), plotlyOutput("plot_estatico_mensal", height="300px"), DTOutput("tbl_estatico_mensal")),
                   tabPanel("Auditoria (Anomalias)", br(), DTOutput("tbl_anomalias"))
                 )
        ),
        
        # --- ABA 6: Base Completa ---
        tabPanel(tagList(icon("table"), "Base de Dados"), 
                 br(), 
                 DTOutput("tab_todas")
        ),
        
        # --- ABA 7: Créditos - Inlcuir o email do Nupec ---
        tabPanel(
          "Créditos",
          fluidPage(
            div(
              class = "credit-wrapper",
              h3(icon("users"), " Equipe e Parcerias"),
              fluidRow(column(
                4, div(
                  class = "credit-card text-center",
                  img(src = "logo_lamapp.jpg", height = "110px"),
                  h4("LAMAPP"),
                  p("Laboratório de Monitoramento e Avaliação de Políticas Públicas."),
                  p(
                    a("https://lamapp-tec.vercel.app/", href = "https://lamapp-tec.vercel.app/")
                  ),
                  
                )
              ), column(
                4, div(
                  class = "credit-card text-center",
                  img(src = "logo_nupec.jpg", height = "120px"),
                  h4("NuPeC"),
                  p("Núcleo de Pesquisa em Ciências de Dados e Otimização."),
                  p(
                    a("www.nupec.ufam.edu.br", href = "http://www.nupec.ufam.edu.br")
                  )
                )
              ), column(
                4, div(
                  class = "credit-card text-center",
                  img(src = "logo_tropa_dos_amigos.png", height =
                        "110px"),
                  h4("Tropa dos Amigos Podcast"),
                  p("Apoio institucional e divulgação."),
                  p(
                    a("Tropa dos Amigos Podcast", 
                      href = "https://www.instagram.com/tropadosamigospodcast/")
                  )
                )
              )),
              
                fluidRow(
                  column(
                    6,
                    div(class="credit-card text-center",
                        img(src="Logo_UFAM.png", height="110px"),
                        h4("UFAM"),
                        p("Universidade Federal do Amazonas"),
                        p(
                          a("UFAM", 
                            href = "https://ufam.edu.br/")
                        )
                    )
                  ),
                  column(
                    6,
                    div(class="credit-card text-center",
                        tags$img(src="Logomarca ICET sem fundo PNG P.png", height="110px"),
                        h4("ICET"),
                        p("Instituto de Ciências Exatas e Tecnologia"),
                        p(
                          a("ICET", 
                            href = "https://icet.ufam.edu.br/")
                        )
                    )
                  )
                ),
                fluidRow(
                  column(
                    6,
                    div(class="credit-card",
                        h4("Coordenação científica"),
                        p(strong("Prof. Dr. Hidelbrando Ferreira Rodrigues"), " (UFAM/ICET)", "hrodrigues@ufam.edu.br"),
                        p("Pesquisador responsável: Dr. Luiz Antônio Nascimento de Souza"),
                        p("Acadêmico de Engenharia de Software: Tiago dos Santos Mendonça")
                    )
                  ),
                  column(
                    6,
                    div(class="credit-card",
                        h4("Sobre o projeto"),
                        p("Ferramenta experimental de monitoramento de notícias policiais em portais do Amazonas, ",
                          "voltada à pesquisa em ciência de dados e avaliação de políticas públicas. ",
                          "Integra o ecossistema NuPeC/LAMAPP e está em contínua evolução.")
                    )
                  )
                )
            )
          )
        )
      )
    )
  )
)

############################################################
# 4. SERVER
############################################################

server <- function(input, output, session) {
  
  # Dados Reativos (Inicializa sem rodar scripts)
  dados_enr <- reactiveVal({
    df_base <- carregar_principal()
    if (is.null(df_base)) return(NULL)
    
    # Tenta mesclar colunas do modelo NLP, se existirem
    arq_nlp <- localizar_arquivo(DIR_PROCESSED, "crimes_classificados_nlp.csv")
    if (!is.null(arq_nlp)) {
      df_nlp <- tryCatch(
        read_csv(arq_nlp, show_col_types = FALSE),
        error = function(e) NULL
      )
      if (!is.null(df_nlp) && "tipo_ml" %in% names(df_nlp)) {
        sufixos <- setdiff(c("tipo_ml", "prob_tipo_ml", "tipo_ml_classe"), names(df_base))
        df_base <- df_base %>%
          left_join(df_nlp %>% select(any_of(c("url", "titulo", sufixos))),
                    by = c("url", "titulo"))
      }
    }
    df_base
  })
  dados_est <- reactiveVal(carregar_estaticos())
  
  # --- AÇÃO: Botão Recarregar ---
  observeEvent(input$btn_reload, {
    dados_enr(carregar_principal())
    dados_est(carregar_estaticos())
    showNotification("Dados recarregados do disco.", type="message")
  })
  
  # --- AÇÃO: Botão Modal de Coleta ---
  observeEvent(input$btn_modal_scrap, {
    showModal(modalDialog(
      title = "Executar Pipeline de Coleta",
      "Atenção: Isso irá acionar o robô de scraping. Certifique-se de que os scripts em 'scripts_pipeline' estão limpos (sem auto-execução).",
      dateRangeInput("dates_scrap", "Intervalo de Coleta:", start = Sys.Date()-7, end = Sys.Date(), language = "pt-BR"),
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("btn_confirmar_scrap", "INICIAR COLETA", class = "btn-danger")
      )
    ))
  })
  
  # --- AÇÃO: Executar Coleta (Backend Blindado) ---
  observeEvent(input$btn_confirmar_scrap, {
    removeModal()
    
    p1 <- file.path(DIR_PIPELINE, "01_scraping.R")
    p2 <- file.path(DIR_PIPELINE, "02_processamento.R")
    
    if(!file.exists(p1) || !file.exists(p2)) {
      showNotification("Scripts não encontrados na pasta scripts_pipeline!", type="error")
      return()
    }
    
    withProgress(message = "Executando Pipeline...", value = 0, {
      incProgress(0.1, detail = "Carregando funções...")
      source(p1, local = TRUE)
      source(p2, local = TRUE)
      
      incProgress(0.3, detail = "Baixando notícias...")
      tryCatch({
        rodar_scraping(data_inicio = input$dates_scrap[1], data_fim = input$dates_scrap[2])
      }, error = function(e) showNotification(paste("Erro Scraping:", e$message), type="warning"))
      
      incProgress(0.6, detail = "Processando dados...")
      tryCatch({
        if(exists("processar_dados_brutos")) processar_dados_brutos()
        else if(exists("processar_dados_raw")) processar_dados_raw()
      }, error = function(e) showNotification(paste("Erro Processamento:", e$message), type="warning"))
      
      # Script 04 (Opcional)
      p4 <- file.path(DIR_PIPELINE, "04_analise_crimes_violentos.R")
      if(file.exists(p4)) {
        incProgress(0.8, detail = "Gerando relatórios estáticos...")
        source(p4, local = TRUE)
      }
      
      incProgress(1, detail = "Concluído!")
      dados_enr(carregar_principal())
      dados_est(carregar_estaticos())
      showNotification("Pipeline finalizado!", type="success")
    })
  })
  
  # Atualiza Dropdowns
  observe({
    df <- dados_enr()
    if (!is.null(df)) {
      if (!is.null(input$fonte_tipo) && input$fonte_tipo == "ml" && "tipo_ml" %in% names(df)) {
        tipos <- sort(unique(df$tipo_ml))
      } else {
        tipos <- sort(unique(df$tipo_principal))
      }
      updateSelectInput(session, "filtro_tipo", choices = c("Todos", tipos), selected = "Todos")
    }
  })
  
  # Status Arquivos (Sidebar)
  output$status_arquivos <- renderUI({
    est <- dados_est()
    tags$ul(class="status-list",
            tags$li(if(!is.null(est$mensal)) HTML("<span style='color:green'>&#10004;</span> Série Mensal") else HTML("<span style='color:red'>&#10006;</span> Série Mensal")),
            tags$li(if(!is.null(est$anomalias)) HTML("<span style='color:green'>&#10004;</span> Anomalias") else HTML("<span style='color:red'>&#10006;</span> Anomalias"))
    )
  })
  
  # Aviso sobre uso do modelo NLP
  output$aviso_nlp <- renderUI({
    df <- dados_enr()
    if (!is.null(df) && "tipo_ml" %in% names(df)) {
      tags$small(style = "color:#16a085;",
                 "Modelo NLP disponível (tipologia sugerida em tipo_ml).")
    } else {
      tags$small(style = "color:#7f8c8d;",
                 "Modelo NLP não disponível; usando apenas tipologia heurística.")
    }
  })
  
  # Renderiza apresentação
  output$apresentacao_html <- renderUI({
    carregar_apresentacao()
  })
  
  # Filtragem Reativa
  dados_filt <- reactive({
    df <- dados_enr()
    req(df)
    df <- df %>% filter(data_pub >= input$datas[1], data_pub <= input$datas[2])
    if (nrow(df) == 0) return(NULL)
    
    # Escolhe coluna de tipo conforme fonte selecionada
    if (!is.null(input$fonte_tipo) && input$fonte_tipo == "ml" && "tipo_ml" %in% names(df)) {
      df <- df %>% mutate(tipo_vis = ifelse(is.na(tipo_ml), tipo_principal, tipo_ml))
      if (input$filtro_tipo != "Todos") df <- df %>% filter(tipo_vis == input$filtro_tipo)
    } else {
      df <- df %>% mutate(tipo_vis = tipo_principal)
      if (input$filtro_tipo != "Todos") df <- df %>% filter(tipo_principal == input$filtro_tipo)
    }
    if (input$filtro_gravidade != "Todos") df <- df %>% filter(gravidade      == input$filtro_gravidade)
    if (input$filtro_genero    != "Todos") df <- df %>% filter(genero_vitima  == input$filtro_genero)
    if (input$filtro_faixa     != "Todas") df <- df %>% filter(faixa_etaria   == input$filtro_faixa)
    df
  })
  
  serie_mensal <- reactive({
    df <- dados_filt()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    df_filtrado <- df %>%
      filter(!is.na(data_pub))
    if (nrow(df_filtrado) == 0) return(NULL)
    resultado <- df_filtrado %>%
      mutate(mes = lubridate::floor_date(data_pub, "month")) %>%
      group_by(mes) %>%
      summarise(
        total = n(),
        letal = sum(categoria == "Crime Letal Violento", na.rm = TRUE),
        indice_letal = ifelse(total > 0, letal / total, NA_real_),
        .groups = "drop"
      ) %>%
      arrange(mes)
    if (nrow(resultado) == 0) return(NULL)
    resultado
  })
  
  cross_tipo_grav <- reactive({
    df <- dados_filt(); req(nrow(df)>0)
    if(!"tipo_vis" %in% names(df)) df$tipo_vis <- df$tipo_principal
    df %>% filter(!is.na(gravidade)) %>%
      count(tipo_vis, gravidade) %>%
      group_by(tipo_vis) %>%
      mutate(total = sum(n)) %>%
      ungroup()
  })
  
  cross_tipo_genero <- reactive({
    df <- dados_filt(); req(nrow(df)>0)
    if(!"tipo_vis" %in% names(df)) df$tipo_vis <- df$tipo_principal
    df %>% filter(!is.na(genero_vitima)) %>%
      count(tipo_vis, genero_vitima) %>%
      group_by(tipo_vis) %>%
      mutate(pct = n / sum(n)) %>%
      ungroup()
  })
  
  cross_tipo_faixa <- reactive({
    df <- dados_filt(); req(nrow(df)>0)
    if(!"tipo_vis" %in% names(df)) df$tipo_vis <- df$tipo_principal
    df %>% filter(!is.na(faixa_etaria), faixa_etaria != "idade não informada") %>%
      count(tipo_vis, faixa_etaria) %>%
      group_by(tipo_vis) %>%
      mutate(total = sum(n)) %>%
      ungroup()
  })
  
  cross_portal_grav <- reactive({
    df <- dados_filt(); req(nrow(df)>0)
    df %>% filter(!is.na(gravidade)) %>%
      count(portal, gravidade) %>%
      group_by(portal) %>%
      mutate(total = sum(n)) %>%
      ungroup()
  })
  
  # --- KPIs & GRÁFICOS (VISUAL) ---
  output$kpi1 <- renderText({ df <- dados_filt(); if(is.null(df)) "0" else format(nrow(df), big.mark=".") })
  output$kpi2 <- renderText({ 
    df <- dados_filt(); if(is.null(df)) return("-")
    df_g <- df %>% filter(genero_vitima %in% c("feminino", "masculino"))
    if(nrow(df_g)==0) "0%" else paste0(round(mean(df_g$genero_vitima == "feminino")*100,1),"%")
  })
  output$kpi3 <- renderText({
    df <- dados_filt()
    if (is.null(df) || nrow(df) == 0) return("-")
    if (!"tipo_vis" %in% names(df)) df$tipo_vis <- df$tipo_principal
    names(sort(table(df$tipo_vis), decreasing = TRUE))[1]
  })
  output$kpi4 <- renderText({ df<-dados_filt(); if(is.null(df)||nrow(df)==0) "-" else names(sort(table(df$gravidade), decreasing=TRUE))[1] })
  
  output$plot_temp <- renderPlotly({
    df <- dados_filt(); req(nrow(df)>0)
    p <- df %>%
      count(data_pub) %>%
      ggplot(aes(x=data_pub, y=n)) +
      geom_area(fill="#667eea", alpha=0.5) +
      geom_line(color="#764ba2", size=1) +
      geom_point(color="#2c3e50") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x="Data de publicação", y="Notícias")
    ggplotly(p)
  })
  
  output$plot_vit <- renderPlotly({
    df <- dados_filt(); req(nrow(df)>0)
    p <- ggplot(df, aes(x=genero_vitima, fill=genero_vitima)) +
      geom_bar() +
      scale_fill_manual(values=c("feminino"="#e84393", "masculino"="#0984e3", "indefinido"="#b2bec3")) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="none") +
      labs(x="Gênero estimado", y="Qtd")
    ggplotly(p)
  })
  
  output$plot_tipo <- renderPlotly({
    df <- dados_filt(); req(nrow(df)>0)
    if (!"tipo_vis" %in% names(df)) df$tipo_vis <- df$tipo_principal
    p <- df %>% count(tipo_vis, sort=TRUE) %>% head(15) %>%
      ggplot(aes(x=reorder(tipo_vis, n), y=n)) +
      geom_col(fill="#e74c3c") +
      coord_flip() +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = element_text(size = 11)) +
      labs(title="Top Tipologias", x="", y="Qtd")
    ggplotly(p)
  })
  
  output$plot_grav <- renderPlotly({
    df <- dados_filt(); req(nrow(df)>0)
    p <- ggplot(df, aes(x=gravidade, fill=gravidade)) +
      geom_bar() +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette="OrRd") +
      labs(title="Severidade", x="Gravidade", y="Qtd")
    ggplotly(p)
  })
  
  output$plot_faixa <- renderPlotly({
    df <- dados_filt(); req(nrow(df)>0)
    df_id <- df %>% filter(faixa_etaria != "idade não informada")
    if(nrow(df_id)==0) return(NULL)
    p <- ggplot(df_id, aes(x=faixa_etaria, fill=genero_vitima)) +
      geom_bar(position="dodge") +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values=c("feminino"="#e84393", "masculino"="#0984e3", "indefinido"="#b2bec3")) +
      labs(title="Faixa Etária", x="Faixa", y="Qtd")
    ggplotly(p)
  })
  
  output$plot_tipo_grav <- renderPlotly({
    cross <- cross_tipo_grav(); req(nrow(cross)>0)
    p <- ggplot(cross, aes(x=gravidade, y=reorder(tipo_vis, total), fill=n)) +
      geom_tile(color="white") +
      scale_fill_gradient(low="#dfe6e9", high="#d63031") +
      scale_y_discrete(labels = function(x) stringr::str_trunc(x, 40)) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x="Gravidade", y="Tipo de crime", fill="Qtd")
    ggplotly(p)
  })
  
  output$plot_tipo_genero <- renderPlotly({
    cross <- cross_tipo_genero(); req(nrow(cross)>0)
    p <- ggplot(cross, aes(x=reorder(tipo_vis, n), y=pct, fill=genero_vitima)) +
      geom_col(position="stack") +
      coord_flip() +
      scale_y_continuous(labels=scales::percent_format(accuracy = 1)) +
      theme_minimal(base_size = 13) +
      theme(axis.text.y = element_text(size = 11)) +
      labs(x="", y="% dentro do tipo", fill="Gênero")
    ggplotly(p)
  })
  
  output$plot_tipo_faixa_cross <- renderPlotly({
    cross <- cross_tipo_faixa(); req(nrow(cross)>0)
    p <- ggplot(cross, aes(x=faixa_etaria, y=reorder(tipo_vis, total), fill=n)) +
      geom_tile(color="white") +
      scale_fill_gradient(low="#e0f7fa", high="#006064") +
      scale_y_discrete(labels = function(x) stringr::str_trunc(x, 40)) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x="Faixa etária", y="Tipo de crime", fill="Qtd")
    ggplotly(p)
  })
  
  output$plot_portal_grav <- renderPlotly({
    cross <- cross_portal_grav(); req(nrow(cross)>0)
    p <- ggplot(cross, aes(x=gravidade, y=reorder(portal, total), fill=n)) +
      geom_tile(color="white") +
      scale_fill_gradient(low="#f1f2f6", high="#636e72") +
      scale_y_discrete(labels = function(x) stringr::str_trunc(x, 25)) +
      theme_minimal(base_size = 13) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(x="Gravidade", y="Portal", fill="Qtd")
    ggplotly(p)
  })
  
  output$txt_tipo_grav <- renderUI({
    cross <- cross_tipo_grav(); req(nrow(cross)>0)
    top <- cross %>% arrange(desc(n)) %>% slice(1)
    share <- scales::percent(top$n / sum(cross$n), accuracy = 0.1)
    tags$div(class="insight-text",
             HTML(paste0("<strong>Destaque:</strong> ", top$tipo_vis,
                         " aparece com gravidade <strong>", top$gravidade,
                         "</strong> em ", top$n, " notícias (", share, ").")))
  })
  
  output$txt_portal_grav <- renderUI({
    cross <- cross_portal_grav(); req(nrow(cross)>0)
    top <- cross %>% arrange(desc(n)) %>% slice(1)
    share <- scales::percent(top$n / sum(cross$n), accuracy = 0.1)
    tags$div(class="insight-text",
             HTML(paste0("O portal <strong>", top$portal,
                         "</strong> concentra a maioria na gravidade <strong>", top$gravidade,
                         "</strong> (", top$n, " notícias, ", share, ").")))
  })
  
  output$txt_tipo_genero <- renderUI({
    cross <- cross_tipo_genero(); req(nrow(cross)>0)
    top <- cross %>% arrange(desc(pct)) %>% slice(1)
    pct_txt <- scales::percent(top$pct, accuracy = 0.1)
    tags$div(class="insight-text",
             HTML(paste0("Dentro de <strong>", top$tipo_vis, "</strong>, o gênero ",
                         "<strong>", top$genero_vitima, "</strong> responde por ",
                         pct_txt, " das notícias do recorte.")))
  })
  
  output$txt_tipo_faixa <- renderUI({
    cross <- cross_tipo_faixa(); req(nrow(cross)>0)
    top <- cross %>% arrange(desc(n)) %>% slice(1)
    share <- scales::percent(top$n / sum(cross$n), accuracy = 0.1)
    tags$div(class="insight-text",
             HTML(paste0("Faixa etária mais frequente: <strong>", top$faixa_etaria,
                         "</strong> dentro de <strong>", top$tipo_vis, "</strong> (",
                         top$n, " notícias, ", share, ").")))
  })
  
  # --- TABELAS ---
  output$tab_resumo_head <- renderDT({
    df <- dados_filt(); req(nrow(df)>0)
    if(!"tipo_vis" %in% names(df)) df$tipo_vis <- df$tipo_principal
    df %>% arrange(desc(data_pub)) %>% head(5) %>%
      select(Data=data_pub, Título=titulo, Tipo=tipo_vis) %>%
      datatable(options=list(dom='Bfrtip', buttons=c('copy','csv'), pageLength=5, scrollX=TRUE), rownames=FALSE)
  })
  
  output$tab_todas <- renderDT({
    df <- dados_filt(); req(nrow(df)>0)
    if(!"tipo_vis" %in% names(df)) df$tipo_vis <- df$tipo_principal
    df %>% select(Data=data_pub, Portal=portal, Título=titulo, Tipo=tipo_vis, Gravidade=gravidade, Gênero=genero_vitima) %>%
      datatable(rownames=FALSE, extensions='Buttons',
                options=list(dom='Bfrtip', buttons=c('copy','csv'), pageLength=20, scrollX=TRUE),
                filter="top")
  })
  
  output$tbl_divergencias <- renderReactable({
    df <- dados_filt(); req(nrow(df)>0)
    if(!"tipo_ml" %in% names(df)) {
      return(reactable(tibble(Mensagem = "Modelo NLP não disponível no conjunto atual."), searchable = FALSE))
    }
    diverg <- df %>%
      filter(!is.na(tipo_ml), tipo_ml != tipo_principal) %>%
      mutate(prob_display = ifelse(!is.na(prob_tipo_ml), sprintf("%.1f%%", prob_tipo_ml*100), "-")) %>%
      arrange(desc(prob_tipo_ml)) %>%
      select(Data=data_pub, Portal=portal, Título=titulo,
             Heurística=tipo_principal, `Modelo NLP`=tipo_ml, `Confiança NLP`=prob_display)
    if(nrow(diverg)==0) {
      diverg <- tibble(Mensagem = "Sem divergências entre heurística e modelo NLP no recorte atual.")
    }
    reactable(
      diverg,
      searchable = TRUE,
      defaultPageSize = 10,
      columns = list(
        `Confiança NLP` = colDef(align="right")
      ),
      highlight = TRUE,
      striped = TRUE,
      compact = TRUE
    )
  })
  
  output$tbl_resumo_tipo <- renderReactable({
    df <- dados_filt(); req(nrow(df)>0)
    if(!"tipo_vis" %in% names(df)) df$tipo_vis <- df$tipo_principal
    resumo <- df %>% count(tipo_vis, sort=TRUE) %>% mutate(pct = round(100 * n / sum(n), 1))
    reactable(
      resumo,
      searchable = TRUE,
      sortable = TRUE,
      defaultPageSize = 15,
      columns = list(
        tipo_vis = colDef(name = "Tipo de crime"),
        n = colDef(name = "Qtd", align = "right"),
        pct = colDef(name = "%", align = "right")
      ),
      highlight = TRUE,
      striped = TRUE,
      compact = TRUE
    )
  })
  
  # --- RELATÓRIOS ESTÁTICOS (ABA 3) ---
  output$plot_estatico_mensal <- renderPlotly({
    df <- serie_mensal()
    if (is.null(df) || nrow(df) == 0) {
      p <- ggplot(tibble(x=1, y=1), aes(x=x, y=y)) +
        geom_blank() +
        theme_minimal() +
        labs(title = "Sem dados disponíveis para o período selecionado",
             x = "", y = "")
      return(ggplotly(p))
    }
    p <- ggplot(df, aes(x=mes, y=indice_letal)) +
      geom_line(color="#e74c3c", size=1) +
      geom_point() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
      theme_minimal(base_size = 13) +
      labs(title="Índice de letalidade (mensal)", x="Mês/ano", y="Índice")
    ggplotly(p)
  })
  output$tbl_estatico_mensal <- renderDT({
    d <- serie_mensal()
    if (is.null(d) || nrow(d) == 0) {
      return(datatable(tibble(Mensagem = "Sem dados disponíveis para o período selecionado"),
                       options=list(dom='t'), rownames=FALSE))
    }
    datatable(d %>% mutate(indice_letal = scales::percent(indice_letal, accuracy = 0.1)),
              options=list(dom='Bfrtip', buttons=c('copy','csv'), pageLength=12),
              rownames=FALSE)
  })
  output$tbl_anomalias <- renderDT({
    d<-dados_est()$anomalias; validate(need(!is.null(d),"Sem anomalias detectadas"))
    datatable(d,options=list(dom='Bfrtip', buttons=c('copy','csv')),rownames=FALSE) %>%
      formatStyle('tipo_principal', fontWeight='bold')
  })
  
  # Download
  output$download_crimes <- downloadHandler(
    filename = function() paste0("crimes_am_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      df <- dados_filt()
      if (is.null(df) || nrow(df) == 0) {
        showNotification("Sem dados filtrados para exportar.", type = "warning")
        readr::write_csv(tibble(), file)
        return(invisible(NULL))
      }
      readr::write_csv(df, file)
    }
  )
}

shinyApp(ui, server)
