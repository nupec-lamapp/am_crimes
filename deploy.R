############################################################
# Script de Deploy - Monitor de Crimes Violentos
# Versão 0.0.1
############################################################

library(rsconnect)

# Configurações
APP_DIR <- getwd()
APP_NAME <- "crimes_am"
ACCOUNT <- "nupec"
SERVER <- "shinyapps.io"
APP_TITLE <- "Monitor de Crimes Violentos - Amazonas"

cat("=== DEPLOY - Monitor de Crimes Violentos ===\n\n")
cat(sprintf("Diretório: %s\n", APP_DIR))
cat(sprintf("Nome da aplicação: %s\n", APP_NAME))
cat(sprintf("Conta: %s\n", ACCOUNT))
cat(sprintf("Servidor: %s\n\n", SERVER))

# Verificar se a aplicação já existe
cat("Verificando aplicações existentes...\n")
tryCatch({
  apps <- rsconnect::applications(account = ACCOUNT, server = SERVER)
  
  if (any(apps$name == APP_NAME)) {
    cat(sprintf("✓ Aplicação '%s' já existe. Fazendo UPDATE...\n\n", APP_NAME))
    force_update <- TRUE
  } else {
    cat(sprintf("✗ Aplicação '%s' não existe. Tentando criar...\n\n", APP_NAME))
    force_update <- FALSE
  }
}, error = function(e) {
  cat(sprintf("Aviso: Não foi possível verificar aplicações: %s\n", e$message))
  cat("Tentando deploy com forceUpdate = TRUE...\n\n")
  force_update <- TRUE
})

# Fazer deploy
cat("Iniciando deploy...\n")
cat("(Isso pode levar alguns minutos)\n\n")

tryCatch({
  rsconnect::deployApp(
    appDir = APP_DIR,
    appName = APP_NAME,
    account = ACCOUNT,
    server = SERVER,
    appTitle = APP_TITLE,
    forceUpdate = force_update,
    launch.browser = function(url) {
      cat(sprintf("\n✓ Deploy concluído com sucesso!\n"))
      cat(sprintf("URL: %s\n\n", url))
    },
    lint = FALSE,
    metadata = list(
      asMultiple = FALSE,
      asStatic = FALSE
    ),
    logLevel = "verbose"
  )
}, error = function(e) {
  cat("\n✗ ERRO NO DEPLOY:\n")
  cat(sprintf("  %s\n\n", e$message))
  
  if (grepl("maximum number of applications", e$message, ignore.case = TRUE)) {
    cat("=== SOLUÇÃO PARA LIMITE DE APLICAÇÕES ===\n\n")
    cat("Execute o seguinte script para ver e gerenciar suas aplicações:\n\n")
    cat("  source('resolver_limite_apps.R')\n\n")
    cat("Este script irá:\n")
    cat("  1. Listar todas as aplicações existentes\n")
    cat("  2. Verificar se 'crimes_am' já existe\n")
    cat("  3. Mostrar opções para resolver o problema\n\n")
    cat("Se 'crimes_am' já existe, use forceUpdate = TRUE:\n")
    cat("  rsconnect::deployApp(..., forceUpdate = TRUE)\n\n")
    cat("Para mais informações, consulte: DEPLOY.md\n")
  } else if (grepl("Application not found", e$message, ignore.case = TRUE)) {
    cat("=== SOLUÇÃO ===\n\n")
    cat("A aplicação não existe e você atingiu o limite.\n")
    cat("Opções:\n")
    cat("  1. Deletar uma aplicação antiga\n")
    cat("  2. Usar um nome diferente (ex: 'crimes_am_v001')\n")
    cat("  3. Fazer upgrade da conta shinyapps.io\n\n")
  } else {
    cat("Consulte DEPLOY.md para mais informações.\n")
  }
  
  stop(e)
})

