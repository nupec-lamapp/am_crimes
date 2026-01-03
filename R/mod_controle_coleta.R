############################################################
# mod_controle_coleta.R
# Painel interativo para acionar raspagem e pipeline
############################################################

MES_PT <- c(
  "Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
  "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro"
)
MONTH_SELECT_CHOICES <- c(
  "Todos" = "todos",
  setNames(sprintf("%02d", seq_along(MES_PT)), sprintf("%02d - %s", seq_along(MES_PT), MES_PT))
)
ANO_MINIMO <- 2000
ARTIFACT_FILES <- c(
  "data/processed/crimes_classificados.csv",
  "outputs/04_resumo_geral.csv",
  "outputs/04_indice_letal_mensal.csv",
  "outputs/04_anomalias_classificacao.csv"
)

mod_controle_coleta_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tags$head(tags$style(HTML("#tabs_controle li:nth-child(2), #tabs_controle li:nth-child(4){display:none;}"))),
    fluidRow(
    column(
      width = 4,
      wellPanel(
        h4("Coleta de portais"),
        actionButton(ns("btn_refresh_portais"), "Atualizar lista de portais", class = "btn btn-secondary btn-sm"),
        textOutput(ns("portais_status")),
        uiOutput(ns("storage_info")),
        selectInput(ns("portais"), "Portais:", choices = "Todos", multiple = TRUE),
        dateRangeInput(
          ns("range"),
          "Intervalo:",
          start = Sys.Date() - 7,
          end   = Sys.Date(),
          format = "dd/mm/yyyy",
          language = "pt-BR"
        ),
        div(
          class = "btn-group btn-group-sm",
          actionButton(ns("preset_3"), "3 dias", class = "btn btn-outline-secondary"),
          actionButton(ns("preset_7"), "7 dias", class = "btn btn-outline-secondary"),
          actionButton(ns("preset_30"), "30 dias", class = "btn btn-outline-secondary")
        ),
        checkboxInput(ns("rodar_pipeline"), "Rodar parse + cleaning + analises", value = TRUE),
        actionButton(ns("btn_executar"), "Executar coleta", class = "btn btn-primary"),
        actionButton(ns("btn_cancelar"), "Interromper (após etapa atual)", class = "btn btn-danger btn-sm")
      )
    ),
    column(
      width = 8,
      tabsetPanel(
        id = ns("tabs_controle"),
        type = "tabs",
        
        # Aba 1: Visão Geral e Cobertura
        tabPanel(
          title = tagList(icon("chart-line"), "Visão Geral"),
          div(
            class = "card-panel",
            h4("Cobertura por portal (dados carregados)"),
            div(
              class = "status-summary",
              textOutput(ns("controle_info")),
              textOutput(ns("controle_periodo")),
              uiOutput(ns("status_widget")),
              htmlOutput(ns("artifact_list"))
            ),
            DT::DTOutput(ns("tab_resumo_portal")),
            tags$hr(),
            tags$small(class = "text-muted", "Detalhe mensal ajuda a ver volumes e dias ativos por mês."),
            selectInput(ns("portal_cobertura"), "Detalhar portal:", choices = "Todos", width = "50%"),
            DT::DTOutput(ns("tab_resumo_portal_mensal"))
          )
        ),
        
        # Aba 2: Inventário de Arquivos (NOVA)
        tabPanel(
          title = tagList(icon("folder-open"), "Inventário"),
          div(
            class = "card-panel",
            h4("Inventário de Arquivos Coletados"),
            tags$p(
              class = "text-muted",
              "Lista completa de arquivos raw coletados, com informações sobre portal, período, tamanho e status de processamento."
            ),
            fluidRow(
              column(
                width = 6,
                selectInput(
                  ns("inventario_portal_filtro"),
                  "Filtrar por portal:",
                  choices = "Todos",
                  width = "100%"
                )
              ),
              column(
                width = 6,
                actionButton(ns("btn_atualizar_inventario"), "Atualizar inventário", 
                           class = "btn btn-sm btn-secondary", icon = icon("refresh"))
              )
            ),
            tags$hr(),
            h5("Resumo por Portal"),
            tags$p(
              class = "text-muted",
              tags$small("Múltiplos arquivos por portal são normais - cada coleta gera um arquivo separado. ",
                        "O sistema consolida automaticamente todos os arquivos durante o processamento.")
            ),
            DT::DTOutput(ns("tab_inventario_resumo")),
            tags$hr(),
            h5("Detalhamento de Arquivos"),
            tags$p(
              class = "text-muted",
              tags$small("Lista completa de arquivos coletados. Arquivos com períodos sobrepostos podem conter dados duplicados, ",
                        "mas o sistema remove duplicatas automaticamente durante o processamento.")
            ),
            DT::DTOutput(ns("tab_inventario_detalhes")),
            tags$hr(),
            h5("Sobreposições Detectadas"),
            tags$p(
              class = "text-muted",
              tags$small("Arquivos que coletam o mesmo período (sobreposição). Isso é normal em coletas incrementais.")
            ),
            DT::DTOutput(ns("tab_inventario_sobreposicoes")),
            tags$hr(),
            h5("Gestão de Arquivos (Melhores Práticas)"),
            tags$p(
              class = "text-info",
              tags$strong("Política de Retenção:"),
              tags$br(),
              tags$small("• Manter sempre: últimos 3 meses + arquivos não processados"),
              tags$br(),
              tags$small("• Consolidar em backup: arquivos antigos (>6 meses) já processados"),
              tags$br(),
              tags$small("• Nunca remover automaticamente - sempre fazer backup primeiro")
            ),
            actionButton(ns("btn_analisar_gestao"), "Analisar Arquivos", 
                        class = "btn btn-sm btn-info", icon = icon("search")),
            tags$br(), tags$br(),
            DT::DTOutput(ns("tab_gestao_arquivos"))
          )
        ),
        
        # Aba 3: Lacunas
        tabPanel(
          title = tagList(icon("calendar-times"), "Lacunas"),
          div(
            class = "card-panel",
            h4("Lacunas por portal"),
            fluidRow(
              column(
                width = 6,
                selectInput(ns("portal_lacunas"), "Portal:", choices = "Selecione", width = "100%")
              ),
              column(
                width = 6,
                selectInput(ns("visualizacao_tipo"), "Visualização:", 
                           choices = c("Tabela Mensal" = "tabela", "Calendário" = "calendario"),
                           selected = "tabela", width = "100%")
              )
            ),
            dateRangeInput(
              ns("lacunas_range"),
              "Intervalo:",
              start = as.Date("2025-01-01"),
              end   = Sys.Date(),
              format = "dd/mm/yyyy",
              language = "pt-BR",
              width = "100%"
            ),
            tags$small("Use o intervalo para filtrar os meses. Meses fora do intervalo coletado aparecem como \"Fora do intervalo coletado\"."),
            tags$hr(),
            # Estatísticas de cobertura
            uiOutput(ns("estatisticas_cobertura")),
            tags$hr(),
            h5("Meses sem dados (alerta)"),
            tags$p(class = "text-muted", tags$small("Meses com zero dias coletados no intervalo selecionado.")),
            DT::DTOutput(ns("tab_meses_sem_dados")),
            tags$hr(),
            # Visualização de calendário
            conditionalPanel(
              condition = paste0("input['", ns("visualizacao_tipo"), "'] == 'calendario'"),
              h5("Visualização de Calendário"),
              tags$p(class = "text-muted", 
                    tags$small("Verde = Coletado | Vermelho = Faltando | Cinza = Fora do intervalo")),
              DT::DTOutput(ns("tab_calendario_lacunas"))
            ),
            # Tabela mensal (padrão)
            conditionalPanel(
              condition = paste0("input['", ns("visualizacao_tipo"), "'] == 'tabela'"),
              h5("Resumo Mensal"),
              DT::DTOutput(ns("tab_lacunas_mensal"))
            ),
            tags$hr(),
            # Períodos críticos
            h5("Períodos Críticos (Lacunas Longas)"),
            DT::DTOutput(ns("tab_periodos_criticos")),
            tags$hr(),
            # Sugestões Inteligentes de Coleta
            h5("Sugestões Inteligentes de Coleta"),
            tags$p(
              class = "text-info",
              tags$small("O sistema analisa lacunas e sugere períodos prioritários para coleta, ",
                        "agrupando lacunas próximas e estimando tempo necessário.")
            ),
            fluidRow(
              column(
                width = 6,
                actionButton(ns("btn_gerar_sugestoes"), "Gerar Sugestões", 
                           class = "btn btn-primary", icon = icon("lightbulb"))
              ),
              column(
                width = 6,
                checkboxInput(ns("agrupar_sugestoes"), "Agrupar lacunas próximas", value = TRUE)
              )
            ),
            tags$br(),
            # Estratégia recomendada
            uiOutput(ns("estrategia_coleta")),
            tags$hr(),
            # Tabela de sugestões
            DT::DTOutput(ns("tab_sugestoes_coleta")),
            tags$hr(),
            # Análise de Cobertura Temporal
            h5("Análise de Cobertura Temporal"),
            tags$p(
              class = "text-muted",
              tags$small("Evolução da cobertura ao longo do tempo e comparação entre portais.")
            ),
            plotly::plotlyOutput(ns("grafico_cobertura_temporal"), height = "400px"),
            tags$hr(),
            # Informações detalhadas (mantém o original - versão simplificada)
            h5("Detalhamento Mensal (Legado)"),
            htmlOutput(ns("lacunas_info"))
          )
        ),
        
        # Aba 5: Métricas e KPIs
        tabPanel(
          title = tagList(icon("chart-bar"), "Métricas"),
          div(
            class = "card-panel",
            h4("Dashboard de Métricas e KPIs"),
            tags$p(
              class = "text-muted",
              tags$small("Indicadores principais de cobertura e qualidade dos dados.")
            ),
            tags$hr(),
            # KPIs principais
            uiOutput(ns("kpis_principais")),
            tags$hr(),
            # Métricas por portal
            h5("Métricas por Portal"),
            DT::DTOutput(ns("tab_metricas_portal")),
            tags$hr(),
            # Métricas de qualidade
            h5("Qualidade dos Dados"),
            uiOutput(ns("metricas_qualidade")),
            tags$hr(),
            # Comparação entre portais
            h5("Comparação entre Portais"),
            plotly::plotlyOutput(ns("grafico_comparacao_portais"), height = "300px")
          )
        ),
        
        # Aba 6: Relatórios e Exportação
        tabPanel(
          title = tagList(icon("file-export"), "Relatórios"),
          div(
            class = "card-panel",
            h4("Exportação de Relatórios e Alertas"),
            tags$p(
              class = "text-muted",
              tags$small("Gere relatórios e verifique alertas de lacunas críticas.")
            ),
            tags$hr(),
            # Alertas
            h5("Alertas de Lacunas Críticas"),
            uiOutput(ns("alertas_lacunas")),
            tags$br(),
            DT::DTOutput(ns("tab_alertas_lacunas")),
            tags$hr(),
            # Exportação
            h5("Exportar Relatórios"),
            fluidRow(
              column(
                width = 6,
                selectInput(ns("formato_exportacao"), "Formato:", 
                           choices = c("CSV" = "csv", "Excel (em breve)" = "excel"),
                           selected = "csv", width = "100%")
              ),
              column(
                width = 6,
                actionButton(ns("btn_exportar_relatorio"), "Exportar Relatório", 
                           class = "btn btn-primary", icon = icon("download"))
              )
            ),
            tags$br(),
            uiOutput(ns("status_exportacao")),
            tags$hr(),
            # Histórico de coletas
            h5("Histórico de Coletas"),
            DT::DTOutput(ns("tab_historico_coletas"))
          )
        ),
        
        # Aba 4: Log
        tabPanel(
          title = tagList(icon("file-alt"), "Log"),
          div(
            class = "card-panel",
            h4("Log da execução"),
            verbatimTextOutput(ns("log_exec"))
          )
        )
      )
    )
  )
  )
}

mod_controle_coleta_server <- function(id, dados_enr, dados_est) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    portais_status <- reactiveVal("")
    status_summary <- reactiveVal("Pronto para iniciar coleta.")
    coleta_timestamp <- reactiveVal(NULL)
    status_details <- reactiveVal(list(state = "idle", message = "Pronto para iniciar coleta.", time = NULL, artifacts = NULL))

    set_status_details <- function(state, message, artifacts = NULL) {
      status_summary(message)
      status_details(list(state = state, message = message, time = Sys.time(), artifacts = artifacts))
    }
    artifact_info <- function(paths) {
      info <- file.info(paths)
      info <- info[!is.na(info$mtime), , drop = FALSE]
      if (nrow(info) == 0) return(NULL)
      tibble::tibble(
        path = rownames(info),
        modified = info$mtime
      )[order(info$mtime, decreasing = TRUE), , drop = FALSE]
    }
    set_status_details("idle", "Pronto para iniciar coleta.", NULL)
    
    # Carregar funções de inventário, gestão e visualização
    if (file.exists("R/inventario_coleta.R")) {
      source("R/inventario_coleta.R", local = TRUE)
    }
    if (file.exists("R/gestao_arquivos_raw.R")) {
      source("R/gestao_arquivos_raw.R", local = TRUE)
    }
    if (file.exists("R/visualizacao_lacunas.R")) {
      source("R/visualizacao_lacunas.R", local = TRUE)
    }
    if (file.exists("R/sugestoes_coleta.R")) {
      source("R/sugestoes_coleta.R", local = TRUE)
    }
    if (file.exists("R/analise_cobertura_temporal.R")) {
      source("R/analise_cobertura_temporal.R", local = TRUE)
    }
    if (file.exists("R/dashboard_metricas.R")) {
      source("R/dashboard_metricas.R", local = TRUE)
    }
    if (file.exists("R/exportacao_relatorios.R")) {
      source("R/exportacao_relatorios.R", local = TRUE)
    }
    
    # Reactive para sugestões
    sugestoes_coleta <- reactiveVal(NULL)
    
    # Reactive para análise de gestão
    analise_gestao <- reactiveVal(NULL)
    
    # Reactive para inventário
    inventario_raw <- reactiveVal(NULL)
    inventario_atualizar <- reactiveVal(0)
    
    # Função para atualizar inventário
    atualizar_inventario <- function() {
      dados_proc <- dados_enr()
      dir_raw <- if (exists("DIR_RAW", inherits = TRUE)) {
        get("DIR_RAW", inherits = TRUE)
      } else {
        file.path("data", "raw")
      }
      
      inventario <- tryCatch({
        inventariar_arquivos_raw(dir_raw = dir_raw, dados_processados = dados_proc)
      }, error = function(e) {
        message("Erro ao inventariar arquivos: ", e$message)
        NULL
      })
      
      inventario_raw(inventario)
      inventario_atualizar(inventario_atualizar() + 1)
    }
    
    # Atualizar inventário inicialmente e quando dados mudarem
    observeEvent(dados_enr(), {
      atualizar_inventario()
    }, ignoreInit = FALSE)
    
    # Atualizar quando botão for clicado
    observeEvent(input$btn_atualizar_inventario, {
      atualizar_inventario()
      showNotification("Inventário atualizado", type = "message")
    })

    output$portais_status <- renderText(portais_status())
    output$storage_info <- renderUI({
      workdir <- if (exists("CRIMES_AM_WORKDIR", inherits = TRUE)) {
        get("CRIMES_AM_WORKDIR", inherits = TRUE)
      } else {
        NA_character_
      }
      mode <- if (exists("CRIMES_AM_STORAGE_MODE", inherits = TRUE)) {
        get("CRIMES_AM_STORAGE_MODE", inherits = TRUE)
      } else {
        "unknown"
      }
      source <- if (exists("CRIMES_AM_WORKDIR_SOURCE", inherits = TRUE)) {
        get("CRIMES_AM_WORKDIR_SOURCE", inherits = TRUE)
      } else {
        "unknown"
      }
      if (identical(mode, "temp")) {
        tags$div(
          class = "text-danger",
          "Armazenamento temporario: dados coletados serao perdidos ao reiniciar.",
          tags$br(),
          if (!is.na(workdir) && nzchar(workdir)) {
            sprintf("Workdir atual: %s", workdir)
          } else {
            NULL
          },
          tags$br(),
          "Defina CRIMES_AM_WORKDIR para um caminho persistente."
        )
      } else if (!is.na(workdir) && nzchar(workdir)) {
        tags$small(class = "text-muted", sprintf("Armazenamento: %s (%s)", workdir, source))
      } else {
        NULL
      }
    })
    output$controle_info <- renderText(status_summary())
    output$controle_periodo <- renderText({
      rng <- input$range
      if (is.null(rng) || any(is.na(rng))) {
        return("")
      }
      start <- as.Date(rng[1])
      end <- as.Date(rng[2])
      sprintf("Período selecionado: %s a %s", format(start, "%d/%m/%Y"), format(end, "%d/%m/%Y"))
    })
    output$status_widget <- renderUI({
      detail <- status_details()
      state <- if (!is.null(detail$state) && nzchar(detail$state)) detail$state else "idle"
      badge_text <- switch(
        state,
        success = "OK",
        error = "ERRO",
        warning = "ATENÇÃO",
        running = "EM ANDAMENTO",
        idle = "PRONTO",
        "INFO"
      )
      badge_class <- switch(
        state,
        success = "text-success",
        error = "text-danger",
        warning = "text-warning",
        running = "text-warning",
        idle = "text-muted",
        "text-muted"
      )
      badge <- tags$span(class = badge_class, style = "font-weight:bold; margin-right:8px;", badge_text)
      message <- tags$span(detail$message)
      last_time <- detail$time
      time_note <- if (!is.null(last_time)) {
        tags$span(sprintf("(%s)", format(last_time, "%d/%m/%Y %H:%M:%S")), class = "text-muted", style = "margin-left:6px;")
      } else NULL
      tags$div(class = "status-widget", badge, message, time_note)
    })
    output$artifact_list <- renderUI({
      detail <- status_details()
      arts <- detail$artifacts
      if (is.null(arts) || nrow(arts) == 0) {
        return(NULL)
      }
      tags$div(
        class = "artifact-list",
        tags$small("Arquivos atualizados:"),
        tags$ul(
          lapply(seq_len(nrow(arts)), function(i) {
            tags$li(sprintf("%s (%s)", basename(arts$path[i]), format(arts$modified[i], "%d/%m/%Y %H:%M:%S")))
          })
        )
      )
    })

    listar_portais_seguro <- function() {
      portais_status("")
      out <- tryCatch({
        source("scripts/01_scraping.R", local = TRUE)
        list(portais = listar_coletores(), err = NULL)
      }, error = function(e) {
        list(portais = character(), err = conditionMessage(e))
      })
      if (!is.null(out$err) && nzchar(out$err)) {
        portais_status(paste0("Falha ao carregar coletores: ", out$err))
      }
      out$portais
    }

    atualizar_portais <- function() {
      portais <- listar_portais_seguro()
      if (length(portais) == 0) {
        portais <- "Todos"
        if (!nzchar(isolate(portais_status()))) {
          portais_status("Nenhum coletor disponivel. Verifique dependencias/pacotes do scraper.")
        }
      }
      updateSelectInput(
        session, "portais",
        choices = c("Todos", portais),
        selected = "Todos"
      )
      updateSelectInput(
        session, "portal_lacunas",
        choices = c("Selecione", portais),
        selected = if (length(portais) > 0) portais[[1]] else "Selecione"
      )
      updateSelectInput(
        session, "portal_cobertura",
        choices = c("Todos", portais),
        selected = "Todos"
      )
    }

    atualizar_portais()

    observeEvent(input$btn_refresh_portais, {
      atualizar_portais()
    })

    observeEvent(input$portais, {
      sel <- input$portais
      if (is.null(sel)) return()
      if (length(sel) == 0) {
        updateSelectInput(session, "portais", selected = "Todos")
        return()
      }
      if (length(sel) > 1 && "Todos" %in% sel) {
        updateSelectInput(session, "portais", selected = setdiff(sel, "Todos"))
      }
    }, ignoreInit = TRUE)

    observeEvent(input$preset_3, {
      updateDateRangeInput(session, "range", start = Sys.Date() - 3, end = Sys.Date())
    })
    observeEvent(input$preset_7, {
      updateDateRangeInput(session, "range", start = Sys.Date() - 7, end = Sys.Date())
    })
    observeEvent(input$preset_30, {
      updateDateRangeInput(session, "range", start = Sys.Date() - 30, end = Sys.Date())
    })

    output$tab_resumo_portal <- DT::renderDT({
      df <- dados_enr()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      resumo <- df %>%
        dplyr::filter(!is.na(data_pub)) %>%
        dplyr::group_by(portal) %>%
        dplyr::summarise(
          inicio = min(data_pub, na.rm = TRUE),
          fim    = max(data_pub, na.rm = TRUE),
          registros = dplyr::n(),
          dias_unicos = dplyr::n_distinct(data_pub),
          meses_com_dados = dplyr::n_distinct(lubridate::floor_date(data_pub, "month")),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          dias_periodo = as.integer(fim - inicio) + 1L,
          dias_faltando = pmax(dias_periodo - dias_unicos, 0L),
          cobertura_pct = ifelse(dias_periodo > 0, round(100 * dias_unicos / dias_periodo, 1), NA_real_),
          registros_por_dia = ifelse(dias_unicos > 0, round(registros / dias_unicos, 1), NA_real_),
          periodo = sprintf("%s - %s", format(inicio, "%d/%m/%Y"), format(fim, "%d/%m/%Y")),
          ultima_coleta = format(fim, "%d/%m/%Y")
        ) %>%
        dplyr::select(
          portal,
          periodo,
          inicio,
          fim,
          meses_com_dados,
          dias_unicos,
          dias_faltando,
          cobertura_pct,
          registros,
          registros_por_dia,
          ultima_coleta
        ) %>%
        dplyr::arrange(dplyr::desc(registros)) %>%
        dplyr::rename(
          "Portal" = portal,
          "Periodo" = periodo,
          "Inicio" = inicio,
          "Fim" = fim,
          "Meses com dados" = meses_com_dados,
          "Dias unicos" = dias_unicos,
          "Dias faltando" = dias_faltando,
          "Cobertura (%)" = cobertura_pct,
          "Registros" = registros,
          "Registros/dia" = registros_por_dia,
          "Ultima coleta" = ultima_coleta
        )

      DT::datatable(
        resumo,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(8, "desc"))
        )
      )
    })

    output$tab_resumo_portal_mensal <- DT::renderDT({
      df <- dados_enr()
      if (is.null(df) || nrow(df) == 0) return(NULL)

      df_proc <- df %>%
        dplyr::filter(!is.na(data_pub)) %>%
        dplyr::mutate(mes = lubridate::floor_date(data_pub, "month"))

      portal_sel <- input$portal_cobertura
      if (!is.null(portal_sel) && portal_sel != "Todos") {
        df_proc <- df_proc %>% dplyr::filter(portal == portal_sel)
      }
      if (nrow(df_proc) == 0) return(NULL)

      mensal <- df_proc %>%
        dplyr::group_by(portal, mes) %>%
        dplyr::summarise(
          inicio = min(data_pub, na.rm = TRUE),
          fim = max(data_pub, na.rm = TRUE),
          registros = dplyr::n(),
          dias_unicos = dplyr::n_distinct(data_pub),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          mes_label = format(mes, "%Y-%m"),
          dias_periodo = as.integer(fim - inicio) + 1L,
          cobertura_pct = ifelse(dias_periodo > 0, round(100 * dias_unicos / dias_periodo, 1), NA_real_),
          registros_por_dia = ifelse(dias_unicos > 0, round(registros / dias_unicos, 1), NA_real_)
        ) %>%
        dplyr::arrange(dplyr::desc(mes), portal) %>%
        dplyr::select(
          portal,
          mes_label,
          inicio,
          fim,
          dias_unicos,
          cobertura_pct,
          registros,
          registros_por_dia
        ) %>%
        dplyr::rename(
          "Portal" = portal,
          "Mes" = mes_label,
          "Inicio" = inicio,
          "Fim" = fim,
          "Dias unicos" = dias_unicos,
          "Cobertura (%)" = cobertura_pct,
          "Registros" = registros,
          "Registros/dia" = registros_por_dia
        )

      DT::datatable(
        mensal,
        rownames = FALSE,
        options = list(
          pageLength = 12,
          scrollX = TRUE,
          order = list(list(1, "desc"))
        )
      )
    })

    lacunas_cache <- reactive({
      df <- dados_enr()
      if (is.null(df) || nrow(df) == 0) {
        return(NULL)
      }

      inicio <- as.Date("2025-01-01")
      fim <- Sys.Date()
      if (inicio > fim) return(NULL)

      months_seq <- seq(lubridate::floor_date(inicio, "month"), lubridate::floor_date(fim, "month"), by = "month")
      if (length(months_seq) == 0) {
        return(NULL)
      }

      anos <- lubridate::year(months_seq)
      meses_num <- lubridate::month(months_seq)
      month_info <- tibble::tibble(
        mes = months_seq,
        ano = anos,
        mes_num = meses_num,
        mes_label = sprintf("%s %d", MES_PT[meses_num], anos),
        inicio = months_seq,
        fim = pmin(lubridate::ceiling_date(months_seq, "month") - 1, fim)
      )

      df_proc <- df %>%
        dplyr::mutate(data_pub = as.Date(data_publicacao)) %>%
        dplyr::filter(!is.na(data_pub), data_pub >= inicio, data_pub <= fim)

      if (nrow(df_proc) == 0) {
        return(NULL)
      }

      portal_ranges <- df_proc %>%
        dplyr::group_by(portal) %>%
        dplyr::summarise(
          inicio_portal = min(data_pub, na.rm = TRUE),
          fim_portal = max(data_pub, na.rm = TRUE),
          .groups = "drop"
        )

      portal_list <- sort(unique(df_proc$portal))
      portal_missing <- lapply(portal_list, function(portal) {
        presentes <- unique(df_proc$data_pub[df_proc$portal == portal])
        portal_range <- portal_ranges[portal_ranges$portal == portal, , drop = FALSE]
        portal_inicio <- portal_range$inicio_portal[1]
        portal_fim <- portal_range$fim_portal[1]

        calc <- mapply(function(primeiro, ultimo) {
          if (is.na(portal_inicio) || is.na(portal_fim)) {
            return(list(faltantes = as.Date(character()), fora_intervalo = TRUE))
          }
          calc_inicio <- max(primeiro, portal_inicio)
          calc_fim <- min(ultimo, portal_fim)
          if (is.na(calc_inicio) || is.na(calc_fim) || calc_inicio > calc_fim) {
            list(faltantes = as.Date(character()), fora_intervalo = TRUE)
          } else {
            dias_mes <- seq(calc_inicio, calc_fim, by = "day")
            list(faltantes = sort(setdiff(dias_mes, presentes)), fora_intervalo = FALSE)
          }
        }, month_info$inicio, month_info$fim, SIMPLIFY = FALSE)

        faltantes <- lapply(calc, `[[`, "faltantes")
        fora_intervalo <- vapply(calc, `[[`, logical(1), "fora_intervalo")
        month_info %>%
          dplyr::mutate(faltantes = faltantes, fora_intervalo = fora_intervalo)
      })
      names(portal_missing) <- portal_list

      portal_years <- df_proc %>%
        dplyr::group_by(portal) %>%
        dplyr::summarise(years = list(sort(unique(lubridate::year(data_pub)))), .groups = "drop")

      list(
        mes_info = month_info,
        year_span = sort(unique(month_info$ano)),
        year_domain = seq(ANO_MINIMO, lubridate::year(fim)),
        portal = portal_missing,
        portal_ranges = portal_ranges,
        portal_years = setNames(portal_years$years, portal_years$portal),
        inicio = min(month_info$inicio),
        fim = max(month_info$fim)
      )
    })

    observeEvent(lacunas_cache(), {
      cache <- lacunas_cache()
      if (is.null(cache)) return()
      range_atual <- input$lacunas_range
      min_date <- as.Date(sprintf("%d-01-01", ANO_MINIMO))
      max_date <- cache$fim
      start_date <- if (is.null(range_atual) || any(is.na(range_atual))) cache$inicio else as.Date(range_atual[1])
      end_date <- if (is.null(range_atual) || any(is.na(range_atual))) cache$fim else as.Date(range_atual[2])
      if (is.na(start_date) || is.na(end_date) || start_date > end_date) {
        start_date <- cache$inicio
        end_date <- cache$fim
      }
      updateDateRangeInput(
        session,
        "lacunas_range",
        start = start_date,
        end = end_date,
        min = min_date,
        max = max_date
      )
    }, ignoreNULL = FALSE)

    observeEvent(input$portal_lacunas, {
      cache <- lacunas_cache()
      if (is.null(cache)) return()
      portal_sel <- input$portal_lacunas
      if (is.null(portal_sel) || portal_sel == "Selecione") return()
      portal_range <- NULL
      if (!is.null(cache$portal_ranges)) {
        portal_range <- cache$portal_ranges[cache$portal_ranges$portal == portal_sel, , drop = FALSE]
      }
      portal_inicio <- if (!is.null(portal_range) && nrow(portal_range) > 0) portal_range$inicio_portal[1] else cache$inicio
      portal_fim <- if (!is.null(portal_range) && nrow(portal_range) > 0) portal_range$fim_portal[1] else cache$fim
      if (is.na(portal_inicio) || is.na(portal_fim) || portal_inicio > portal_fim) {
        portal_inicio <- cache$inicio
        portal_fim <- cache$fim
      }
      updateDateRangeInput(session, "lacunas_range", start = portal_inicio, end = portal_fim)
    }, ignoreNULL = TRUE)

    # Reactive para calendário de lacunas
    calendario_lacunas <- reactive({
      dados <- dados_enr()
      if (is.null(dados) || nrow(dados) == 0) {
        return(NULL)
      }
      
      portal_sel <- input$portal_lacunas
      if (is.null(portal_sel) || portal_sel == "Selecione") {
        return(NULL)
      }
      
      range_sel <- input$lacunas_range
      if (is.null(range_sel) || any(is.na(range_sel))) {
        return(NULL)
      }
      
      tryCatch({
        preparar_calendario_lacunas(
          dados_processados = dados,
          portal = portal_sel,
          data_inicio = as.Date(range_sel[1]),
          data_fim = as.Date(range_sel[2])
        )
      }, error = function(e) {
        message("Erro ao preparar calendário: ", e$message)
        NULL
      })
    })
    
    # Output: Estatísticas de cobertura
    output$estatisticas_cobertura <- renderUI({
      calendario <- calendario_lacunas()
      if (is.null(calendario) || nrow(calendario) == 0) {
        return(tags$div(class = "text-muted", "Selecione um portal e intervalo para ver estatísticas."))
      }
      
      stats <- calcular_estatisticas_cobertura(calendario)
      
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = "text-primary", stats$total_dias),
            tags$small("Total de Dias")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = "text-success", stats$dias_coletados),
            tags$small("Dias Coletados")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = "text-danger", stats$dias_faltando),
            tags$small("Dias Faltando")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = ifelse(stats$cobertura_pct >= 80, "text-success", 
                                 ifelse(stats$cobertura_pct >= 50, "text-warning", "text-danger")),
                   sprintf("%.1f%%", stats$cobertura_pct)),
            tags$small("Cobertura")
          )
        )
      )
    })
    
    # Output: Tabela de calendário (visualização dia a dia)
    output$tab_calendario_lacunas <- DT::renderDT({
      calendario <- calendario_lacunas()
      if (is.null(calendario) || nrow(calendario) == 0) {
        return(DT::datatable(tibble(Mensagem = "Selecione um portal e intervalo para ver o calendário."), rownames = FALSE))
      }
      
      # Agrupar por mês para visualização
      calendario_display <- calendario %>%
        group_by(.data$mes_ano, .data$ano, .data$mes) %>%
        summarise(
          dias_total = n(),
          dias_coletados = sum(.data$tem_dados),
          dias_faltando = .data$dias_total - .data$dias_coletados,
          cobertura_pct = round(100 * .data$dias_coletados / .data$dias_total, 1),
          dias_lista = paste(
            ifelse(.data$tem_dados, 
                   sprintf("%02d", .data$dia), 
                   sprintf("(%02d)", .data$dia)),
            collapse = " "
          ),
          .groups = "drop"
        ) %>%
        arrange(desc(.data$ano), desc(.data$mes)) %>%
        mutate(
          status = case_when(
            cobertura_pct == 100 ~ "Completo",
            cobertura_pct >= 80 ~ "Quase completo",
            cobertura_pct >= 50 ~ "Parcial",
            cobertura_pct > 0 ~ "Incompleto",
            TRUE ~ "Sem dados"
          )
        ) %>%
        select(mes_ano, dias_total, dias_coletados, dias_faltando, cobertura_pct, status, dias_lista) %>%
        rename(
          "Mês" = mes_ano,
          "Total" = dias_total,
          "Coletados" = dias_coletados,
          "Faltando" = dias_faltando,
          "Cobertura %" = cobertura_pct,
          "Status" = status,
          "Dias (coletados) / (faltando)" = dias_lista
        )
      
      DT::datatable(
        calendario_display,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 12,
          scrollX = TRUE,
          order = list(list(0, "desc"))
        )
      ) %>%
        DT::formatStyle("Status",
                       backgroundColor = DT::styleEqual(
                         c("Completo", "Quase completo", "Parcial", "Incompleto", "Sem dados"),
                         c("#ccffcc", "#e6ffcc", "#fff4cc", "#ffe6cc", "#ffcccc")
                       )) %>%
        DT::formatStyle("Cobertura %",
                       backgroundColor = DT::styleInterval(
                         c(50, 80, 100),
                         c("#ffcccc", "#fff4cc", "#e6ffcc", "#ccffcc")
                       ))
    })
    
    # Reactive para calendário de lacunas
    calendario_lacunas <- reactive({
      dados <- dados_enr()
      if (is.null(dados) || nrow(dados) == 0) {
        return(NULL)
      }
      
      portal_sel <- input$portal_lacunas
      if (is.null(portal_sel) || portal_sel == "Selecione") {
        return(NULL)
      }
      
      range_sel <- input$lacunas_range
      if (is.null(range_sel) || any(is.na(range_sel))) {
        return(NULL)
      }
      
      tryCatch({
        preparar_calendario_lacunas(
          dados_processados = dados,
          portal = portal_sel,
          data_inicio = as.Date(range_sel[1]),
          data_fim = as.Date(range_sel[2])
        )
      }, error = function(e) {
        message("Erro ao preparar calendário: ", e$message)
        NULL
      })
    })
    
    # Output: Estatísticas de cobertura
    output$estatisticas_cobertura <- renderUI({
      calendario <- calendario_lacunas()
      if (is.null(calendario) || nrow(calendario) == 0) {
        return(tags$div(class = "text-muted", "Selecione um portal e intervalo para ver estatísticas."))
      }
      
      stats <- calcular_estatisticas_cobertura(calendario)
      
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = "text-primary", stats$total_dias),
            tags$small("Total de Dias")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = "text-success", stats$dias_coletados),
            tags$small("Dias Coletados")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = "text-danger", stats$dias_faltando),
            tags$small("Dias Faltando")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = ifelse(stats$cobertura_pct >= 80, "text-success", 
                                 ifelse(stats$cobertura_pct >= 50, "text-warning", "text-danger")),
                   sprintf("%.1f%%", stats$cobertura_pct)),
            tags$small("Cobertura")
          )
        )
      )
    })
    
    # Output: Tabela de calendário (visualização dia a dia)
    output$tab_calendario_lacunas <- DT::renderDT({
      calendario <- calendario_lacunas()
      if (is.null(calendario) || nrow(calendario) == 0) {
        return(DT::datatable(tibble(Mensagem = "Selecione um portal e intervalo para ver o calendário."), rownames = FALSE))
      }
      
      # Agrupar por mês para visualização
      calendario_display <- calendario %>%
        group_by(.data$mes_ano, .data$ano, .data$mes) %>%
        summarise(
          dias_total = n(),
          dias_coletados = sum(.data$tem_dados),
          dias_faltando = .data$dias_total - .data$dias_coletados,
          cobertura_pct = round(100 * .data$dias_coletados / .data$dias_total, 1),
          dias_lista = paste(
            ifelse(.data$tem_dados, 
                   sprintf("%02d", .data$dia), 
                   sprintf("(%02d)", .data$dia)),
            collapse = " "
          ),
          .groups = "drop"
        ) %>%
        arrange(desc(.data$ano), desc(.data$mes)) %>%
        mutate(
          status = case_when(
            cobertura_pct == 100 ~ "Completo",
            cobertura_pct >= 80 ~ "Quase completo",
            cobertura_pct >= 50 ~ "Parcial",
            cobertura_pct > 0 ~ "Incompleto",
            TRUE ~ "Sem dados"
          )
        ) %>%
        select(mes_ano, dias_total, dias_coletados, dias_faltando, cobertura_pct, status, dias_lista) %>%
        rename(
          "Mês" = mes_ano,
          "Total" = dias_total,
          "Coletados" = dias_coletados,
          "Faltando" = dias_faltando,
          "Cobertura %" = cobertura_pct,
          "Status" = status,
          "Dias (coletados) / (faltando)" = dias_lista
        )
      
      DT::datatable(
        calendario_display,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 12,
          scrollX = TRUE,
          order = list(list(0, "desc"))
        )
      ) %>%
        DT::formatStyle("Status",
                       backgroundColor = DT::styleEqual(
                         c("Completo", "Quase completo", "Parcial", "Incompleto", "Sem dados"),
                         c("#ccffcc", "#e6ffcc", "#fff4cc", "#ffe6cc", "#ffcccc")
                       )) %>%
        DT::formatStyle("Cobertura %",
                       backgroundColor = DT::styleInterval(
                         c(50, 80, 100),
                         c("#ffcccc", "#fff4cc", "#e6ffcc", "#ccffcc")
                       ))
    })
    
    # Output: Resumo mensal de lacunas
    output$tab_lacunas_mensal <- DT::renderDT({
      calendario <- calendario_lacunas()
      if (is.null(calendario) || nrow(calendario) == 0) {
        return(DT::datatable(tibble(Mensagem = "Selecione um portal e intervalo para ver o resumo mensal."), rownames = FALSE))
      }
      
      resumo_mensal <- resumir_lacunas_mensal(calendario) %>%
        select(mes_ano, dias_total, dias_coletados, dias_faltando, cobertura_pct, status_mes) %>%
        rename(
          "Mês" = mes_ano,
          "Total" = dias_total,
          "Coletados" = dias_coletados,
          "Faltando" = dias_faltando,
          "Cobertura %" = cobertura_pct,
          "Status" = status_mes
        )
      
      DT::datatable(
        resumo_mensal,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 12,
          scrollX = TRUE,
          order = list(list(0, "desc"))
        )
      ) %>%
        DT::formatStyle("Status",
                       backgroundColor = DT::styleEqual(
                         c("Completo", "Quase completo", "Parcial", "Incompleto", "Sem dados"),
                         c("#ccffcc", "#e6ffcc", "#fff4cc", "#ffe6cc", "#ffcccc")
                       )) %>%
        DT::formatStyle("Cobertura %",
                      backgroundColor = DT::styleInterval(
                        c(50, 80, 100),
                        c("#ffcccc", "#fff4cc", "#e6ffcc", "#ccffcc")
                      ))
    })

    # Output: Meses sem dados (alerta)
    output$tab_meses_sem_dados <- DT::renderDT({
      calendario <- calendario_lacunas()
      if (is.null(calendario) || nrow(calendario) == 0) {
        return(DT::datatable(tibble(Mensagem = "Selecione um portal e intervalo para ver meses sem dados."), rownames = FALSE))
      }

      resumo_mensal <- resumir_lacunas_mensal(calendario)
      sem_dados <- resumo_mensal %>%
        filter(.data$dias_coletados == 0) %>%
        select(mes_ano, dias_total) %>%
        rename("Mês" = mes_ano, "Dias no mês" = dias_total)

      if (nrow(sem_dados) == 0) {
        return(DT::datatable(tibble(Mensagem = "Nenhum mês sem dados no intervalo selecionado."), rownames = FALSE))
      }

      DT::datatable(
        sem_dados,
        rownames = FALSE,
        options = list(
          paging = FALSE,
          searching = FALSE,
          info = FALSE
        )
      )
    })
    
    # Output: Períodos críticos
    output$tab_periodos_criticos <- DT::renderDT({
      calendario <- calendario_lacunas()
      if (is.null(calendario) || nrow(calendario) == 0) {
        return(DT::datatable(tibble(Mensagem = "Selecione um portal e intervalo para ver períodos críticos."), rownames = FALSE))
      }
      
      periodos <- identificar_periodos_criticos(calendario, min_dias_sequencia = 7)
      
      if (nrow(periodos) == 0) {
        return(DT::datatable(
          tibble(Mensagem = "Nenhum período crítico detectado (lacunas de 7+ dias consecutivos)."), 
          rownames = FALSE
        ))
      }
      
      periodos_display <- periodos %>%
        select(periodo, dias_sequencia, criticidade) %>%
        rename(
          "Período" = periodo,
          "Dias Consecutivos" = dias_sequencia,
          "Criticidade" = criticidade
        )
      
      DT::datatable(
        periodos_display,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(1, "desc"))
        )
      ) %>%
        DT::formatStyle("Criticidade",
                       backgroundColor = DT::styleEqual(
                         c("Crítico", "Alto", "Médio"),
                         c("#ffcccc", "#ffe6cc", "#fff4cc")
                       ))
    })
    
    # Observador para gerar sugestões
    observeEvent(input$btn_gerar_sugestoes, {
      dados <- dados_enr()
      if (is.null(dados) || nrow(dados) == 0) {
        showNotification("Nenhum dado disponível para gerar sugestões", type = "warning")
        return()
      }
      
      portal_sel <- input$portal_lacunas
      if (is.null(portal_sel) || portal_sel == "Selecione") {
        showNotification("Selecione um portal primeiro", type = "warning")
        return()
      }
      
      range_sel <- input$lacunas_range
      if (is.null(range_sel) || any(is.na(range_sel))) {
        showNotification("Selecione um intervalo de datas", type = "warning")
        return()
      }
      
      sugestoes <- tryCatch({
        sugestoes_raw <- gerar_sugestoes_coleta(
          dados_processados = dados,
          portal = portal_sel,
          data_inicio = as.Date(range_sel[1]),
          data_fim = as.Date(range_sel[2]),
          max_sugestoes = 20
        )
        
        if (isTRUE(input$agrupar_sugestoes) && nrow(sugestoes_raw) > 0) {
          agrupar_sugestoes_proximas(sugestoes_raw, max_distancia_dias = 7)
        } else {
          sugestoes_raw
        }
      }, error = function(e) {
        showNotification(paste("Erro ao gerar sugestões:", e$message), type = "error")
        NULL
      })
      
      if (!is.null(sugestoes)) {
        sugestoes_coleta(sugestoes)
        showNotification("Sugestões geradas com sucesso", type = "message")
      }
    })
    
    # Output: Estratégia de coleta
    output$estrategia_coleta <- renderUI({
      sugestoes <- sugestoes_coleta()
      if (is.null(sugestoes) || nrow(sugestoes) == 0) {
        return(tags$div(
          class = "alert alert-info",
          tags$strong("Instruções:"),
          tags$p("Clique em 'Gerar Sugestões' para analisar lacunas e obter recomendações de coleta.")
        ))
      }
      
      estrategia <- sugerir_estrategia_coleta(sugestoes)
      resumo <- calcular_resumo_sugestoes(sugestoes)
      
      cor_alerta <- switch(estrategia$estrategia,
                          "Urgente" = "danger",
                          "Importante" = "warning",
                          "Manutenção" = "info",
                          "success")
      
      tags$div(
        class = sprintf("alert alert-%s", cor_alerta),
        tags$h5(tags$strong(sprintf("Estratégia: %s", estrategia$estrategia))),
        tags$p(estrategia$recomendacao),
        tags$p(
          tags$strong("Resumo: "),
          sprintf("%d sugestões | %d dias faltando no total", 
                 resumo$total_sugestoes, 
                 resumo$total_dias)
        ),
        tags$hr(),
        tags$p(tags$strong("Próximos passos:")),
        tags$ul(
          lapply(estrategia$proximos_passos, function(passo) {
            tags$li(passo)
          })
        )
      )
    })
    
    # Output: Tabela de sugestões
    output$tab_sugestoes_coleta <- DT::renderDT({
      sugestoes <- sugestoes_coleta()
      if (is.null(sugestoes) || nrow(sugestoes) == 0) {
        return(DT::datatable(
          tibble(Mensagem = "Clique em 'Gerar Sugestões' para ver recomendações de coleta."), 
          rownames = FALSE
        ))
      }
      
      sugestoes_display <- sugestoes %>%
        select(prioridade, periodo, `Dias Faltando`, motivo, estimativa_tempo) %>%
        rename(
          "Prioridade" = prioridade,
          "Período Sugerido" = periodo,
          "Dias" = `Dias Faltando`,
          "Motivo" = motivo,
          "Tempo Estimado" = estimativa_tempo
        )
      
      DT::datatable(
        sugestoes_display,
        rownames = FALSE,
        filter = "top",
        selection = "single",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(0, "asc"), list(2, "desc"))  # Ordenar por prioridade e dias
        )
      ) %>%
        DT::formatStyle("Prioridade",
                       backgroundColor = DT::styleEqual(
                         c("Crítica", "Alta", "Média", "Baixa"),
                         c("#ffcccc", "#ffe6cc", "#fff4cc", "#e6f3ff")
                       ))
    })
    
    # Observador para aplicar sugestão selecionada
    observeEvent(input$tab_sugestoes_coleta_rows_selected, {
      sugestoes <- sugestoes_coleta()
      if (is.null(sugestoes) || nrow(sugestoes) == 0) return()
      
      linha_selecionada <- input$tab_sugestoes_coleta_rows_selected
      if (length(linha_selecionada) == 0) return()
      
      sugestao_selecionada <- sugestoes[linha_selecionada, ]
      
      # Atualizar intervalo de coleta com a sugestão
      updateDateRangeInput(
        session,
        "range",
        start = sugestao_selecionada$data_inicio,
        end = sugestao_selecionada$data_fim
      )
      
      # Atualizar portal se necessário
      portal_sel <- input$portal_lacunas
      if (!is.null(portal_sel) && portal_sel != "Selecione") {
        updateSelectInput(session, "portais", selected = portal_sel)
      }
      
      showNotification(
        sprintf("Intervalo atualizado para: %s", sugestao_selecionada$periodo),
        type = "message"
      )
    })
    
    # Reactive para série temporal de cobertura
    serie_temporal_cobertura <- reactive({
      dados <- dados_enr()
      if (is.null(dados) || nrow(dados) == 0) {
        return(NULL)
      }
      
      range_sel <- input$lacunas_range
      if (is.null(range_sel) || any(is.na(range_sel))) {
        return(NULL)
      }
      
      tryCatch({
        preparar_serie_temporal_cobertura(
          dados_processados = dados,
          data_inicio = as.Date(range_sel[1]),
          data_fim = as.Date(range_sel[2]),
          agrupamento = "mes"
        )
      }, error = function(e) {
        message("Erro ao preparar série temporal: ", e$message)
        NULL
      })
    })
    
    # Output: Gráfico de cobertura temporal
    output$grafico_cobertura_temporal <- plotly::renderPlotly({
      serie <- serie_temporal_cobertura()
      if (is.null(serie) || nrow(serie) == 0) {
        return(plotly::plotly_empty() %>%
               plotly::layout(title = "Selecione um intervalo para ver a análise temporal"))
      }
      
      # Criar gráfico de linha
      p <- serie %>%
        plotly::plot_ly(
          x = ~periodo,
          y = ~cobertura_pct,
          color = ~portal,
          type = "scatter",
          mode = "lines+markers",
          hovertemplate = "<b>%{fullData.name}</b><br>%{x|%d/%m/%Y}<br>Cobertura: %{y:.1f}%<extra></extra>"
        ) %>%
        plotly::layout(
          title = "Evolução da Cobertura ao Longo do Tempo",
          xaxis = list(title = "Período"),
          yaxis = list(title = "Cobertura (%)", range = c(0, 105)),
          hovermode = "x unified",
          legend = list(orientation = "h", y = -0.2)
        )
      
      # Adicionar linha de referência (80% e 100%)
      p <- p %>%
        plotly::layout(
          shapes = list(
            list(
              type = "line",
              xref = "paper", x0 = 0, x1 = 1,
              yref = "y", y0 = 80, y1 = 80,
              line = list(color = "orange", dash = "dash")
            ),
            list(
              type = "line",
              xref = "paper", x0 = 0, x1 = 1,
              yref = "y", y0 = 100, y1 = 100,
              line = list(color = "green", dash = "dash")
            )
          ),
          annotations = list(
            list(
              xref = "paper", x = 1, xanchor = "left",
              yref = "y", y = 80,
              text = "80% (Meta)",
              showarrow = FALSE,
              font = list(color = "orange")
            ),
            list(
              xref = "paper", x = 1, xanchor = "left",
              yref = "y", y = 100,
              text = "100% (Ideal)",
              showarrow = FALSE,
              font = list(color = "green")
            )
          )
        )
      
      p
    })
    
    # Reactive para KPIs
    kpis_cobertura <- reactive({
      dados <- dados_enr()
      if (is.null(dados) || nrow(dados) == 0) {
        return(NULL)
      }
      
      range_sel <- input$lacunas_range
      if (is.null(range_sel) || any(is.na(range_sel))) {
        return(NULL)
      }
      
      tryCatch({
        calcular_kpis_cobertura(
          dados_processados = dados,
          data_inicio = as.Date(range_sel[1]),
          data_fim = as.Date(range_sel[2])
        )
      }, error = function(e) {
        message("Erro ao calcular KPIs: ", e$message)
        NULL
      })
    })
    
    # Output: KPIs principais
    output$kpis_principais <- renderUI({
      kpis <- kpis_cobertura()
      if (is.null(kpis)) {
        return(tags$div(class = "text-muted", "Selecione um intervalo para ver os KPIs."))
      }
      
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h3(class = "text-primary", sprintf("%.1f%%", kpis$cobertura_media_geral)),
            tags$small("Cobertura Média Geral")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h3(class = "text-success", kpis$total_dias_coletados),
            tags$small("Dias Coletados")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h3(class = "text-info", format(kpis$total_registros, big.mark = ".", scientific = FALSE)),
            tags$small("Total de Registros")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h3(class = "text-warning", kpis$portais_ativos),
            tags$small("Portais Ativos")
          )
        )
      )
    })
    
    # Output: Métricas por portal
    output$tab_metricas_portal <- DT::renderDT({
      kpis <- kpis_cobertura()
      if (is.null(kpis) || nrow(kpis$cobertura_media_por_portal) == 0) {
        return(DT::datatable(tibble(Mensagem = "Selecione um intervalo para ver as métricas."), rownames = FALSE))
      }
      
      metricas <- kpis$cobertura_media_por_portal %>%
        rename(
          "Portal" = portal,
          "Cobertura %" = cobertura_pct,
          "Dias Coletados" = dias_coletados,
          "Registros" = registros
        )
      
      DT::datatable(
        metricas,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(1, "desc"))
        )
      ) %>%
        DT::formatStyle("Cobertura %",
                       backgroundColor = DT::styleInterval(
                         c(50, 80, 100),
                         c("#ffcccc", "#fff4cc", "#e6ffcc", "#ccffcc")
                       ))
    })
    
    # Output: Métricas de qualidade
    output$metricas_qualidade <- renderUI({
      dados <- dados_enr()
      if (is.null(dados) || nrow(dados) == 0) {
        return(tags$div(class = "text-muted", "Nenhum dado disponível."))
      }
      
      qualidade <- tryCatch({
        calcular_metricas_qualidade(dados)
      }, error = function(e) {
        message("Erro ao calcular qualidade: ", e$message)
        NULL
      })
      
      if (is.null(qualidade)) {
        return(tags$div(class = "text-muted", "Erro ao calcular métricas de qualidade."))
      }
      
      tags$div(
        class = "row",
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = ifelse(qualidade$pct_com_titulo >= 95, "text-success", "text-warning"),
                   sprintf("%.1f%%", qualidade$pct_com_titulo)),
            tags$small("Com Título")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = ifelse(qualidade$pct_com_texto >= 95, "text-success", "text-warning"),
                   sprintf("%.1f%%", qualidade$pct_com_texto)),
            tags$small("Com Texto")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = ifelse(qualidade$pct_com_data >= 95, "text-success", "text-warning"),
                   sprintf("%.1f%%", qualidade$pct_com_data)),
            tags$small("Com Data")
          )
        ),
        tags$div(
          class = "col-md-3",
          tags$div(
            class = "well well-sm text-center",
            tags$h4(class = ifelse(qualidade$registros_duplicados == 0, "text-success", "text-warning"),
                   qualidade$registros_duplicados),
            tags$small("Duplicados")
          )
        )
      )
    })
    
    # Output: Gráfico de comparação entre portais
    output$grafico_comparacao_portais <- plotly::renderPlotly({
      dados <- dados_enr()
      if (is.null(dados) || nrow(dados) == 0) {
        return(plotly::plotly_empty() %>%
               plotly::layout(title = "Selecione um intervalo para ver a comparação"))
      }
      
      range_sel <- input$lacunas_range
      if (is.null(range_sel) || any(is.na(range_sel))) {
        return(plotly::plotly_empty())
      }
      
      comparacao <- tryCatch({
        comparar_portais(
          dados_processados = dados,
          data_inicio = as.Date(range_sel[1]),
          data_fim = as.Date(range_sel[2])
        )
      }, error = function(e) {
        message("Erro ao comparar portais: ", e$message)
        return(tibble())
      })
      
      if (nrow(comparacao) == 0) {
        return(plotly::plotly_empty() %>%
               plotly::layout(title = "Nenhum dado disponível para comparação"))
      }
      
      p <- comparacao %>%
        plotly::plot_ly(
          x = ~portal,
          y = ~cobertura_pct,
          type = "bar",
          text = ~sprintf("%.1f%%", cobertura_pct),
          textposition = "outside",
          marker = list(
            color = ~cobertura_pct,
            colorscale = list(c(0, "#ffcccc"), c(0.5, "#fff4cc"), c(1, "#ccffcc")),
            showscale = TRUE
          )
        ) %>%
        plotly::layout(
          title = "Cobertura por Portal",
          xaxis = list(title = "Portal"),
          yaxis = list(title = "Cobertura (%)", range = c(0, 105))
        )
      
      p
    })
    
    # Reactive para alertas
    alertas_lacunas <- reactive({
      dados <- dados_enr()
      if (is.null(dados) || nrow(dados) == 0) {
        return(NULL)
      }
      
      range_sel <- input$lacunas_range
      if (is.null(range_sel) || any(is.na(range_sel))) {
        return(NULL)
      }
      
      tryCatch({
        verificar_alertas_lacunas(
          dados_processados = dados,
          portal = NULL,
          data_inicio = as.Date(range_sel[1]),
          data_fim = as.Date(range_sel[2]),
          limite_dias_critico = 7
        )
      }, error = function(e) {
        message("Erro ao verificar alertas: ", e$message)
        NULL
      })
    })
    
    # Output: Alertas de lacunas
    output$alertas_lacunas <- renderUI({
      alertas <- alertas_lacunas()
      if (is.null(alertas) || alertas$total_alertas == 0) {
        return(tags$div(
          class = "alert alert-success",
          tags$strong("✓ Nenhum alerta crítico!"),
          tags$p("Não foram detectadas lacunas de 7+ dias consecutivos no período selecionado.")
        ))
      }
      
      cor_alerta <- ifelse(alertas$criticidade_alta > 0, "danger", "warning")
      
      tags$div(
        class = sprintf("alert alert-%s", cor_alerta),
        tags$strong(sprintf("⚠ %d alerta(s) detectado(s)", alertas$total_alertas)),
        tags$p(sprintf("%d com criticidade alta ou crítica", alertas$criticidade_alta))
      )
    })
    
    # Output: Tabela de alertas
    output$tab_alertas_lacunas <- DT::renderDT({
      alertas <- alertas_lacunas()
      if (is.null(alertas) || alertas$total_alertas == 0) {
        return(DT::datatable(tibble(Mensagem = "Nenhum alerta crítico detectado."), rownames = FALSE))
      }
      
      alertas_display <- alertas$alertas %>%
        mutate(
          periodo = sprintf("%s a %s", 
                           format(data_inicio, "%d/%m/%Y"),
                           format(data_fim, "%d/%m/%Y"))
        ) %>%
        select(portal, periodo, dias_consecutivos, criticidade) %>%
        rename(
          "Portal" = portal,
          "Período" = periodo,
          "Dias Consecutivos" = dias_consecutivos,
          "Criticidade" = criticidade
        )
      
      DT::datatable(
        alertas_display,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(2, "desc"))
        )
      ) %>%
        DT::formatStyle("Criticidade",
                       backgroundColor = DT::styleEqual(
                         c("Crítica", "Alta", "Média"),
                         c("#ffcccc", "#ffe6cc", "#fff4cc")
                       ))
    })
    
    # Observador para exportar relatório
    observeEvent(input$btn_exportar_relatorio, {
      dados <- dados_enr()
      if (is.null(dados) || nrow(dados) == 0) {
        showNotification("Nenhum dado disponível para exportar", type = "warning")
        return()
      }
      
      range_sel <- input$lacunas_range
      if (is.null(range_sel) || any(is.na(range_sel))) {
        showNotification("Selecione um intervalo de datas", type = "warning")
        return()
      }
      
      formato <- input$formato_exportacao
      
      if (formato == "csv") {
        arquivo <- tryCatch({
          gerar_relatorio_cobertura_csv(
            dados_processados = dados,
            inventario = inventario_raw(),
            sugestoes = sugestoes_coleta(),
            arquivo_saida = NULL,
            data_inicio = as.Date(range_sel[1]),
            data_fim = as.Date(range_sel[2])
          )
        }, error = function(e) {
          showNotification(paste("Erro ao exportar:", e$message), type = "error")
          NULL
        })
        
        if (!is.null(arquivo)) {
          showNotification(
            sprintf("Relatório exportado com sucesso: %s", basename(arquivo)),
            type = "message"
          )
        }
      } else {
        showNotification("Exportação para Excel ainda não implementada", type = "info")
      }
    })
    
    # Output: Status de exportação
    output$status_exportacao <- renderUI({
      # Placeholder - pode ser expandido para mostrar histórico de exportações
      NULL
    })
    
    # Output: Histórico de coletas
    output$tab_historico_coletas <- DT::renderDT({
      inventario <- inventario_raw()
      if (is.null(inventario) || nrow(inventario) == 0) {
        return(DT::datatable(tibble(Mensagem = "Nenhum arquivo coletado encontrado."), rownames = FALSE))
      }
      
      historico <- tryCatch({
        gerar_historico_coletas(inventario)
      }, error = function(e) {
        message("Erro ao gerar histórico: ", e$message)
        return(tibble())
      })
      
      if (nrow(historico) == 0) {
        return(DT::datatable(tibble(Mensagem = "Nenhum histórico disponível."), rownames = FALSE))
      }
      
      historico_display <- historico %>%
        mutate(
          mes_label = sprintf("%s/%04d", sprintf("%02d", month(mes_coleta)), ano_coleta),
          tamanho_total_mb = sprintf("%.2f MB", tamanho_total_mb)
        ) %>%
        select(portal, mes_label, n_coletas, total_registros_estimado, tamanho_total_mb) %>%
        rename(
          "Portal" = portal,
          "Mês" = mes_label,
          "Coletas" = n_coletas,
          "Registros (est.)" = total_registros_estimado,
          "Tamanho" = tamanho_total_mb
        )
      
      DT::datatable(
        historico_display,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 12,
          scrollX = TRUE,
          order = list(list(1, "desc"))
        )
      )
    })

    output$lacunas_info <- renderUI({
      cache <- lacunas_cache()
      if (is.null(cache)) {
        return(tags$div(class = "text-muted", "Sem dados carregados."))
      }
      portal_sel <- input$portal_lacunas
      if (is.null(portal_sel) || portal_sel == "Selecione") {
        return(tags$div(class = "text-muted", "Selecione um portal para ver as lacunas."))
      }

      portal_tbl <- cache$portal[[portal_sel]]
      if (is.null(portal_tbl) || nrow(portal_tbl) == 0) {
        return(tags$div(
          class = "text-warning",
          sprintf("Portal %s ainda não possui registros no intervalo carregado.", portal_sel)
        ))
      }

      if (!"fora_intervalo" %in% names(portal_tbl)) {
        portal_tbl$fora_intervalo <- FALSE
      }

      portal_range <- NULL
      if (!is.null(cache$portal_ranges)) {
        portal_range <- cache$portal_ranges[cache$portal_ranges$portal == portal_sel, , drop = FALSE]
      }
      portal_inicio <- if (!is.null(portal_range) && nrow(portal_range) > 0) portal_range$inicio_portal[1] else as.Date(NA)
      portal_fim <- if (!is.null(portal_range) && nrow(portal_range) > 0) portal_range$fim_portal[1] else as.Date(NA)

      portal_tbl_in <- portal_tbl %>% dplyr::filter(!fora_intervalo)
      faltantes_total <- if (nrow(portal_tbl_in) > 0) lengths(portal_tbl_in$faltantes) else integer()
      resumo_total <- list(
        meses_total = nrow(portal_tbl_in),
        meses_completos = sum(faltantes_total == 0),
        dias_pendentes = sum(faltantes_total)
      )

      meses <- portal_tbl
      range_sel <- input$lacunas_range
      if (is.null(range_sel) || any(is.na(range_sel))) {
        range_start <- cache$inicio
        range_end <- cache$fim
      } else {
        range_start <- as.Date(range_sel[1])
        range_end <- as.Date(range_sel[2])
      }
      if (is.na(range_start) || is.na(range_end) || range_start > range_end) {
        return(tags$div(
          class = "text-warning",
          "Intervalo de datas invalido para filtrar as lacunas."
        ))
      }
      meses <- meses %>%
        dplyr::filter(fim >= range_start, inicio <= range_end) %>%
        dplyr::mutate(
          faltantes_range = lapply(faltantes, function(dias) {
            dias[dias >= range_start & dias <= range_end]
          })
        )

      if (nrow(meses) == 0) {
        return(tags$div(
          class = "text-muted",
          "Nenhum mês disponível para o filtro aplicado."
        ))
      }

      meses_in <- meses %>% dplyr::filter(!fora_intervalo)
      faltantes_filtrados <- if (nrow(meses_in) > 0) lengths(meses_in$faltantes_range) else integer()
      resumo_filtro <- list(
        meses = nrow(meses_in),
        dias_pendentes = sum(faltantes_filtrados)
      )

      intervalo_portal_texto <- if (!is.na(portal_inicio) && !is.na(portal_fim)) {
        sprintf("Intervalo considerado: %s a %s",
                format(portal_inicio, "%d/%m/%Y"),
                format(portal_fim, "%d/%m/%Y"))
      } else {
        "Intervalo considerado: indisponivel."
      }

      lacunas_tbl <- meses %>%
        dplyr::mutate(
          dias_faltando = lengths(faltantes_range),
          dias_faltando_lista = vapply(faltantes_range, function(dias) {
            if (length(dias) == 0) return("—")
            paste(format(dias, "%d/%m"), collapse = ", ")
          }, character(1)),
          status_mes = dplyr::if_else(
            fora_intervalo,
            "Fora do intervalo coletado",
            dplyr::if_else(dias_faltando == 0, "Completo", "Pendente")
          )
        ) %>%
        dplyr::select(mes_label, status_mes, dias_faltando, dias_faltando_lista) %>%
        dplyr::rename(
          "Mes" = mes_label,
          "Status" = status_mes,
          "Dias faltando" = dias_faltando,
          "Dias nao coletados (dd/mm)" = dias_faltando_lista
        )

      tags$div(
        tags$p(
          class = "text-muted",
          intervalo_portal_texto
        ),
        tags$p(
          class = "text-info",
          sprintf(
            "Resumo: %s meses no periodo (completos: %s) | %s dias pendentes. Filtro atual (%s a %s): %s meses | %s dias pendentes.",
            resumo_total$meses_total,
            resumo_total$meses_completos,
            format(resumo_total$dias_pendentes, big.mark = ".", scientific = FALSE),
            format(range_start, "%d/%m/%Y"),
            format(range_end, "%d/%m/%Y"),
            resumo_filtro$meses,
            format(resumo_filtro$dias_pendentes, big.mark = ".", scientific = FALSE)
          )
        ),
        if (!is.null(coleta_timestamp())) {
          tags$p(class = "text-success", sprintf("Última atualização: %s", format(coleta_timestamp(), "%d/%m/%Y %H:%M:%S")))
        } else {
          NULL
        },
        DT::datatable(
          lacunas_tbl,
          rownames = FALSE,
          options = list(
            dom = "tip",
            pageLength = min(24, nrow(lacunas_tbl)),
            scrollX = TRUE
          )
        )
      )
    })

    # Atualizar filtro de portal do inventário quando portais mudarem
    observeEvent(input$portais, {
      portais <- input$portais
      if (!is.null(portais) && length(portais) > 0) {
        choices <- c("Todos", portais)
        updateSelectInput(session, "inventario_portal_filtro", choices = choices)
      }
    }, ignoreInit = TRUE)
    
    # Output: Resumo do inventário por portal
    output$tab_inventario_resumo <- DT::renderDT({
      inventario <- inventario_raw()
      if (is.null(inventario) || nrow(inventario) == 0) {
        return(DT::datatable(tibble(Mensagem = "Nenhum arquivo coletado encontrado."), rownames = FALSE))
      }
      
      resumo <- resumir_inventario_por_portal(inventario)
      if (nrow(resumo) == 0) {
        return(DT::datatable(tibble(Mensagem = "Nenhum arquivo válido encontrado."), rownames = FALSE))
      }
      
      resumo_display <- resumo %>%
        select(
          portal,
          periodo,
          n_arquivos,
          tamanho_total_mb,
          dias_coletados_total,
          n_processados,
          n_nao_processados,
          pct_processado,
          ultima_coleta
        ) %>%
        mutate(
          ultima_coleta = format(ultima_coleta, "%d/%m/%Y %H:%M"),
          tamanho_total_mb = sprintf("%.2f MB", tamanho_total_mb),
          pct_processado = sprintf("%.1f%%", pct_processado)
        ) %>%
        rename(
          "Portal" = portal,
          "Período" = periodo,
          "Arquivos" = n_arquivos,
          "Tamanho Total" = tamanho_total_mb,
          "Dias Coletados" = dias_coletados_total,
          "Processados" = n_processados,
          "Não Processados" = n_nao_processados,
          "% Processado" = pct_processado,
          "Última Coleta" = ultima_coleta
        )
      
      DT::datatable(
        resumo_display,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(8, "desc"))  # Ordenar por última coleta
        )
      ) %>%
        DT::formatStyle("Processados", backgroundColor = DT::styleEqual(0, "#ffcccc")) %>%
        DT::formatStyle("Não Processados", backgroundColor = DT::styleInterval(0, c("#ccffcc", "#ffffcc")))
    })
    
    # Output: Detalhamento de arquivos do inventário
    output$tab_inventario_detalhes <- DT::renderDT({
      inventario <- inventario_raw()
      if (is.null(inventario) || nrow(inventario) == 0) {
        return(DT::datatable(tibble(Mensagem = "Nenhum arquivo coletado encontrado."), rownames = FALSE))
      }
      
      # Aplicar filtro de portal
      portal_filtro <- input$inventario_portal_filtro
      inventario_filtrado <- inventario
      if (!is.null(portal_filtro) && portal_filtro != "Todos") {
        inventario_filtrado <- inventario_filtrado %>%
          filter(portal == portal_filtro)
      }
      
      if (nrow(inventario_filtrado) == 0) {
        return(DT::datatable(tibble(Mensagem = "Nenhum arquivo encontrado para o filtro selecionado."), rownames = FALSE))
      }
      
      detalhes <- inventario_filtrado %>%
        mutate(
          periodo = sprintf("%s a %s", 
                           format(data_inicio, "%d/%m/%Y"),
                           format(data_fim, "%d/%m/%Y")),
          tamanho_kb = sprintf("%.2f KB", tamanho_kb),
          data_criacao = format(data_criacao, "%d/%m/%Y %H:%M"),
          status_display = ifelse(processado, "✓ Processado", "○ Não processado")
        ) %>%
        select(
          arquivo,
          portal,
          periodo,
          data_inicio,
          data_fim,
          dias_coletados,
          tamanho_kb,
          data_criacao,
          status_display
        ) %>%
        arrange(desc(data_criacao)) %>%
        rename(
          "Arquivo" = arquivo,
          "Portal" = portal,
          "Período" = periodo,
          "Data Início" = data_inicio,
          "Data Fim" = data_fim,
          "Dias" = dias_coletados,
          "Tamanho" = tamanho_kb,
          "Data Criação" = data_criacao,
          "Status" = status_display
        )
      
      DT::datatable(
        detalhes,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(7, "desc"))  # Ordenar por data de criação
        )
      ) %>%
        DT::formatStyle("Status", 
                       backgroundColor = DT::styleEqual("✓ Processado", "#ccffcc"))
    })
    
    # Output: Sobreposições detectadas
    output$tab_inventario_sobreposicoes <- DT::renderDT({
      inventario <- inventario_raw()
      if (is.null(inventario) || nrow(inventario) == 0) {
        return(DT::datatable(tibble(Mensagem = "Nenhum arquivo coletado encontrado."), rownames = FALSE))
      }
      
      sobreposicoes <- tryCatch({
        detectar_sobreposicoes(inventario)
      }, error = function(e) {
        return(tibble())
      })
      
      if (is.null(sobreposicoes) || nrow(sobreposicoes) == 0) {
        return(DT::datatable(
          tibble(Mensagem = "Nenhuma sobreposição detectada. Todos os arquivos têm períodos distintos."), 
          rownames = FALSE
        ))
      }
      
      sobreposicoes_display <- sobreposicoes %>%
        mutate(
          periodo = sprintf("%s a %s", 
                           format(data_inicio, "%d/%m/%Y"),
                           format(data_fim, "%d/%m/%Y"))
        ) %>%
        select(
          arquivo,
          portal,
          periodo,
          data_inicio,
          data_fim,
          n_sobrepostos,
          arquivos_sobrepostos
        ) %>%
        arrange(portal, desc(data_inicio)) %>%
        rename(
          "Arquivo" = arquivo,
          "Portal" = portal,
          "Período" = periodo,
          "Data Início" = data_inicio,
          "Data Fim" = data_fim,
          "Nº Sobrepostos" = n_sobrepostos,
          "Arquivos Sobrepostos" = arquivos_sobrepostos
        )
      
      DT::datatable(
        sobreposicoes_display,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          order = list(list(2, "asc"), list(3, "desc"))
        )
      ) %>%
        DT::formatStyle("Nº Sobrepostos", 
                       backgroundColor = DT::styleInterval(0, c("#ccffcc", "#fff4cc")))
    })
    
    # Observador para análise de gestão
    observeEvent(input$btn_analisar_gestao, {
      inventario <- inventario_raw()
      dados_proc <- dados_enr()
      
      if (is.null(inventario) || nrow(inventario) == 0) {
        showNotification("Nenhum arquivo para analisar", type = "warning")
        return()
      }
      
      analise <- tryCatch({
        analisar_arquivos_para_gestao(
          inventario = inventario,
          dados_processados = dados_proc,
          dias_retencao = 180,  # 6 meses
          manter_sempre_ultimos_meses = 3
        )
      }, error = function(e) {
        showNotification(paste("Erro na análise:", e$message), type = "error")
        NULL
      })
      
      if (!is.null(analise)) {
        analise_gestao(analise)
        showNotification("Análise concluída", type = "message")
      }
    })
    
    # Output: Tabela de gestão de arquivos
    output$tab_gestao_arquivos <- DT::renderDT({
      analise <- analise_gestao()
      if (is.null(analise)) {
        return(DT::datatable(
          tibble(Mensagem = "Clique em 'Analisar Arquivos' para ver recomendações de gestão."), 
          rownames = FALSE
        ))
      }
      
      # Combinar manter e consolidar
      manter_display <- analise$manter %>%
        mutate(
          periodo_display = sprintf("%s a %s", 
                                   format(.data$data_inicio, "%d/%m/%Y"),
                                   format(.data$data_fim, "%d/%m/%Y")),
          tamanho_display = sprintf("%.2f KB", .data$tamanho_kb),
          categoria = "Manter"
        ) %>%
        select(arquivo, portal, periodo_display, idade_dias, motivo, tamanho_display, categoria)
      
      consolidar_display <- analise$consolidar %>%
        mutate(
          periodo_display = sprintf("%s a %s", 
                                   format(.data$data_inicio, "%d/%m/%Y"),
                                   format(.data$data_fim, "%d/%m/%Y")),
          tamanho_display = sprintf("%.2f KB", .data$tamanho_kb),
          categoria = "Consolidar"
        ) %>%
        select(arquivo, portal, periodo_display, idade_dias, motivo, tamanho_display, categoria)
      
      gestao <- bind_rows(manter_display, consolidar_display) %>%
        arrange(categoria, desc(idade_dias)) %>%
        rename(
          "Categoria" = categoria,
          "Arquivo" = arquivo,
          "Portal" = portal,
          "Período" = periodo_display,
          "Idade (dias)" = idade_dias,
          "Motivo" = motivo,
          "Tamanho" = tamanho_display
        )
      
      DT::datatable(
        gestao,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          order = list(list(0, "asc"), list(4, "desc"))
        )
      ) %>%
        DT::formatStyle("Categoria",
                       backgroundColor = DT::styleEqual(
                         c("Manter", "Consolidar"),
                         c("#ccffcc", "#fff4cc")
                       ))
    })

    log_txt <- reactiveVal("Aguardando execução.")
    output$log_exec <- renderText(log_txt())

    cancelar <- reactiveVal(FALSE)

    observeEvent(input$btn_cancelar, {
      cancelar(TRUE)
      log_txt("Cancelamento solicitado. A coleta atual terminará a etapa em andamento e depois vai parar.")
      showNotification("Cancelamento solicitado. Aguardando fim da etapa corrente.", type = "warning")
      set_status_details("warning", "Cancelamento solicitado; a coleta será interrompida após a etapa atual.", NULL)
    })

    observeEvent(input$btn_executar, {
      cancelar(FALSE)
      req(input$range)
      d1 <- as.Date(input$range[1])
      d2 <- as.Date(input$range[2])
      if (is.na(d1) || is.na(d2) || d1 > d2) {
        showNotification("Intervalo de datas inválido.", type = "error")
        return()
      }

      portais_sel <- input$portais
      if (is.null(portais_sel) || length(portais_sel) == 0 || identical(portais_sel, "Todos")) {
        portais_sel <- NULL
      }

      log_txt("Iniciando execução...")
      set_status_details("running", sprintf("Coleta em andamento (%s a %s) para portais: %s",
                                     format(d1, "%d/%m/%Y"),
                                     format(d2, "%d/%m/%Y"),
                                     if (is.null(portais_sel)) "Todos" else paste(portais_sel, collapse = ", ")), NULL)

      ok <- TRUE
      withProgress(message = "Executando coleta", value = 0, {
        incProgress(0.1, detail = "Carregando scripts de scraping")
        load_err <- NULL
        tryCatch({
          source("scripts/01_scraping.R", local = TRUE)
          source("scripts/02_parse.R", local = TRUE)
          source("scripts/03_cleaning.R", local = TRUE)
        }, error = function(e) {
          load_err <<- conditionMessage(e)
        })
        if (!is.null(load_err)) {
          ok <<- FALSE
          log_txt(paste("Erro carregando scripts:", load_err))
          showNotification(paste("Erro carregando scripts:", load_err), type = "error")
          set_status_details("error", paste("Erro carregando scripts:", load_err), NULL)
          return(NULL)
        }
        if (exists("CRIMES_AM_WORKDIR", inherits = TRUE)) {
          log_txt(paste("Workdir:", get("CRIMES_AM_WORKDIR", inherits = TRUE)))
        }

        incProgress(0.4, detail = "Rodando scraping")
        scrap_err <- NULL
        n_raw <- NA_integer_
        tryCatch({
          df_raw <- rodar_scraping(
            data_inicio = d1,
            data_fim    = d2,
            portais     = if (is.null(portais_sel)) listar_coletores() else portais_sel
          )
          n_raw <- if (is.data.frame(df_raw)) nrow(df_raw) else NA_integer_
        }, error = function(e) {
          scrap_err <<- conditionMessage(e)
        })
        if (!is.null(scrap_err)) {
          ok <<- FALSE
          log_txt(paste("Erro no scraping:", scrap_err))
          showNotification(paste("Erro no scraping:", scrap_err), type = "error")
          set_status_details("error", paste("Erro no scraping:", scrap_err), NULL)
          return(NULL)
        }
        if (!is.na(n_raw)) log_txt(paste("Scraping OK. Registros brutos:", n_raw))
        if (is.na(n_raw) || n_raw == 0) {
          ok <<- FALSE
          log_txt("Scraping não retornou notícias novas. Parse/cleaning não serão executados.")
          showNotification("Scraping concluído sem novas notícias. Nada para processar.", type = "warning")
          set_status_details("warning", "Scraping concluído sem novos registros; nenhuma atualização foi feita.", NULL)
          return()
        }

        if (isTRUE(cancelar())) {
          ok <<- FALSE
          log_txt("Execução interrompida pelo usuário após scraping. Dados coletados foram mantidos.")
          showNotification("Execução interrompida após scraping. Dados brutos mantidos.", type = "warning")
          set_status_details("warning", "Coleta interrompida pelo usuário; os dados brutos foram mantidos.", NULL)
          return()
        }

        if (isTRUE(input$rodar_pipeline)) {
          incProgress(0.7, detail = "Parse + cleaning")
          pipe_err <- NULL
          tryCatch({
            df_parsed <- parse_raw_files()
            clean_and_enrich_data(df_parsed)
          }, error = function(e) {
            pipe_err <<- conditionMessage(e)
          })
          if (!is.null(pipe_err)) {
            ok <<- FALSE
            log_txt(paste("Erro no parse/cleaning:", pipe_err))
            showNotification(paste("Erro no parse/cleaning:", pipe_err), type = "error")
            set_status_details("error", paste("Erro no parse/cleaning:", pipe_err), NULL)
            return(NULL)
          }
        }

        incProgress(1, detail = "Concluido")
      })

      if (!isTRUE(ok)) return()

      # Atualizar inventário após coleta (arquivos raw foram criados)
      atualizar_inventario()
      
      if (isTRUE(input$rodar_pipeline)) {
        dados_enr(carregar_principal())
        dados_est(carregar_estaticos())
        coleta_timestamp(Sys.time())
        log_txt("Execução finalizada com sucesso.")
        artifacts <- artifact_info(ARTIFACT_FILES)
        set_status_details(
          "success",
          sprintf(
            "Coleta concluída em %s; lacunas e cobertura atualizadas.",
            format(coleta_timestamp(), "%d/%m/%Y %H:%M:%S")
          ),
          artifacts
        )
        showNotification("Coleta finalizada e lacunas recalculadas.", type = "message")
      } else {
        log_txt("Coleta finalizada (pipeline desativado).")
        set_status_details(
          "warning",
          "Coleta finalizada, mas o pipeline está desativado; lacunas e cobertura não foram recalculadas.",
          NULL
        )
        showNotification("Coleta finalizada, mas pipeline desativado (lacunas não recalculadas).", type = "warning")
      }
    })
  })
}


