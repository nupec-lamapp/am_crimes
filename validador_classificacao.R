############################################################
# validador_classificacao.R
# App Shiny independente para revisão/validação de tipologias
# - Carrega crimes_classificados.csv (heurística)
# - Opcionalmente mescla crimes_classificados_nlp.csv (sugestão NLP)
# - Permite aceitar heurística, aceitar NLP ou editar manualmente
# - Marca fonte da classificação (automatico/manual)
# - Salva rascunho em data/exports/validacao_editada.csv
# - Aplica correções na base principal (faz backup antes)
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(shiny)
  library(DT)
  library(dplyr)
  library(readr)
})

options(stringsAsFactors = FALSE)

DIR_PROCESSED <- "data/processed"
DIR_EXPORTS   <- "data/exports"

if (!dir.exists(DIR_EXPORTS)) dir.create(DIR_EXPORTS, recursive = TRUE, showWarnings = FALSE)

carregar_base <- function() {
  arq_class <- file.path(DIR_PROCESSED, "crimes_classificados.csv")
  arq_nlp   <- file.path(DIR_PROCESSED, "crimes_classificados_nlp.csv")

  if (!file.exists(arq_class)) {
    stop("Arquivo crimes_classificados.csv não encontrado em data/processed/. Rode a pipeline primeiro.")
  }

  df <- readr::read_csv(arq_class, show_col_types = FALSE)

  if (file.exists(arq_nlp)) {
    df_nlp <- readr::read_csv(arq_nlp, show_col_types = FALSE)
    df <- df %>%
      left_join(
        df_nlp %>% select(any_of(c("url", "titulo", "tipo_ml", "prob_tipo_ml"))),
        by = c("url", "titulo")
      )
  }

  if (!"tipo_ml" %in% names(df))      df$tipo_ml      <- NA_character_
  if (!"prob_tipo_ml" %in% names(df)) df$prob_tipo_ml <- NA_real_

  df %>%
    mutate(
      tipo_corrigido      = tipo_principal,
      gravidade_corrigida = gravidade,
      fonte_classificacao = "automatico",
      validado            = FALSE
    )
}

ui <- fluidPage(
  titlePanel("Validador de Tipologias (heurística vs. NLP vs. manual)"),
  fluidRow(
    column(
      width = 12,
      p("Selecione linhas e clique em um botão de ação ou edite manualmente as colunas 'tipo_corrigido', 'gravidade_corrigida' e 'validado'."),
      p("Somente linhas marcadas em 'validado' serão aplicadas à base principal.")
    )
  ),
  fluidRow(
    column(
      width = 12,
      DTOutput("tbl")
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      actionButton("aceitar_heu", "Aceitar heurística (selecionadas)", class = "btn-primary"),
      actionButton("aceitar_nlp", "Aceitar NLP (selecionadas)", class = "btn-info"),
      actionButton("salvar", "Salvar rascunho", class = "btn-secondary"),
      actionButton("aplicar", "Aplicar na base (backup automático)", class = "btn-success")
    )
  ),
  br(),
  verbatimTextOutput("msg")
)

server <- function(input, output, session) {
  dados <- reactiveVal(carregar_base())

  output$tbl <- renderDT({
    df <- dados()
    datatable(
      df,
      selection = "multiple",
      editable = TRUE,
      options = list(scrollX = TRUE, pageLength = 20)
    )
  }, server = TRUE)

  proxy <- dataTableProxy("tbl")

  # Editar células manualmente
  observeEvent(input$tbl_cell_edit, {
    info <- input$tbl_cell_edit
    df <- dados()
    col <- colnames(df)[info$col + 1]
    df[info$row, col] <- info$value
    if (col %in% c("tipo_corrigido", "gravidade_corrigida", "validado")) {
      df[info$row, "fonte_classificacao"] <- "manual"
    }
    dados(df)
  })

  # Aceitar heurística nas linhas selecionadas
  observeEvent(input$aceitar_heu, {
    idx <- input$tbl_rows_selected
    if (length(idx) == 0) return(NULL)
    df <- dados()
    df[idx, "tipo_corrigido"]      <- df[idx, "tipo_principal", drop = TRUE]
    df[idx, "gravidade_corrigida"] <- df[idx, "gravidade", drop = TRUE]
    df[idx, "fonte_classificacao"] <- "automatico"
    dados(df)
    replaceData(proxy, df, resetPaging = FALSE)
  })

  # Aceitar NLP nas linhas selecionadas (se existir)
  observeEvent(input$aceitar_nlp, {
    idx <- input$tbl_rows_selected
    if (length(idx) == 0) return(NULL)
    df <- dados()
    if (!"tipo_ml" %in% names(df)) return(NULL)
    df[idx, "tipo_corrigido"]      <- df[idx, "tipo_ml", drop = TRUE]
    df[idx, "gravidade_corrigida"] <- df[idx, "gravidade", drop = TRUE]
    df[idx, "fonte_classificacao"] <- "automatico"
    dados(df)
    replaceData(proxy, df, resetPaging = FALSE)
  })

  # Salvar rascunho
  observeEvent(input$salvar, {
    arq <- file.path(DIR_EXPORTS, "validacao_editada.csv")
    readr::write_csv(dados(), arq)
    output$msg <- renderText(paste("Rascunho salvo em:", arq))
  })

  # Aplicar correções na base principal
  observeEvent(input$aplicar, {
    df <- dados()
    flag_validado <- tolower(as.character(df$validado)) %in% c("true", "1", "sim", "ok", "y", "yes")
    if (any(is.logical(df$validado))) flag_validado <- flag_validado | df$validado == TRUE

    if (!any(flag_validado, na.rm = TRUE)) {
      output$msg <- renderText("Nenhuma linha marcada como validada; nada a aplicar.")
      return()
    }

    df_final <- df %>%
      mutate(
        tipo_principal = ifelse(flag_validado & !is.na(tipo_corrigido) & tipo_corrigido != "",
                                tipo_corrigido, tipo_principal),
        gravidade = ifelse(flag_validado & !is.na(gravidade_corrigida) & gravidade_corrigida != "",
                           gravidade_corrigida, gravidade),
        fonte_classificacao = ifelse(flag_validado & !is.na(fonte_classificacao),
                                     fonte_classificacao, "automatico")
      ) %>%
      select(-flag_validado)

    # Backup e escrita
    arq_class <- file.path(DIR_PROCESSED, "crimes_classificados.csv")
    backup <- file.path(DIR_PROCESSED,
                        paste0("crimes_classificados_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
    file.copy(arq_class, backup, overwrite = TRUE)
    readr::write_csv(df_final, arq_class)

    output$msg <- renderText(paste("Correções aplicadas. Backup salvo em:", backup))
  })
}

shinyApp(ui, server)
