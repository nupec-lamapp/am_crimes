############################################################
# Script de Deploy FINAL - Monitor de Crimes Violentos
# Versão 0.0.1 - Com todas as correções
############################################################

library(rsconnect)

cat("=== DEPLOY FINAL - Monitor de Crimes Violentos ===\n\n")

# Configurações
APP_DIR <- getwd()
APP_NAME <- "crimes_am"
ACCOUNT <- "nupec"
SERVER <- "shinyapps.io"
APP_TITLE <- "Monitor de Crimes Violentos - Amazonas"

cat(sprintf("Diretório: %s\n", APP_DIR))
cat(sprintf("Nome da aplicação: %s\n", APP_NAME))
cat(sprintf("Conta: %s\n", ACCOUNT))
cat(sprintf("Servidor: %s\n\n", SERVER))

# Verificar se .rscignore existe
if (!file.exists(".rscignore")) {
  cat("⚠️  AVISO: Arquivo .rscignore não encontrado!\n")
  cat("   Criando .rscignore básico...\n\n")
}

# Verificar se app.R existe
if (!file.exists("app.R")) {
  stop("ERRO: app.R não encontrado no diretório atual!")
}

cat("✓ app.R encontrado\n")

# Verificar aplicações existentes
cat("\nVerificando aplicações existentes...\n")
force_update <- FALSE

tryCatch({
  apps <- rsconnect::applications(account = ACCOUNT, server = SERVER)
  
  if (any(apps$name == APP_NAME)) {
    cat(sprintf("✓ Aplicação '%s' JÁ EXISTE\n", APP_NAME))
    cat("  → Usando forceUpdate = TRUE para atualizar\n\n")
    force_update <- TRUE
  } else {
    cat(sprintf("✗ Aplicação '%s' NÃO existe\n", APP_NAME))
    cat(sprintf("  → Tentando criar nova aplicação\n"))
    cat(sprintf("  → Total de aplicações: %d\n\n", nrow(apps)))
    
    if (nrow(apps) >= 5) {
      cat("⚠️  ATENÇÃO: Você pode ter atingido o limite de 5 aplicações!\n")
      cat("   Se o deploy falhar, você precisará:\n")
      cat("   1. Deletar uma aplicação antiga\n")
      cat("   2. Ou fazer upgrade da conta\n\n")
    }
    force_update <- FALSE
  }
}, error = function(e) {
  cat(sprintf("⚠️  Não foi possível verificar aplicações: %s\n", e$message))
  cat("   Tentando deploy com forceUpdate = TRUE...\n\n")
  force_update <- TRUE
})

# Fazer deploy
cat("═══════════════════════════════════════════════════════\n")
cat("INICIANDO DEPLOY...\n")
cat("═══════════════════════════════════════════════════════\n")
cat("(Isso pode levar vários minutos)\n\n")

tryCatch({
  rsconnect::deployApp(
    appDir = APP_DIR,
    appName = APP_NAME,
    account = ACCOUNT,
    server = SERVER,
    appTitle = APP_TITLE,
    forceUpdate = force_update,
    launch.browser = function(url) {
      cat("\n")
      cat("═══════════════════════════════════════════════════════\n")
      cat("✓ DEPLOY CONCLUÍDO COM SUCESSO!\n")
      cat("═══════════════════════════════════════════════════════\n")
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
  cat("\n")
  cat("═══════════════════════════════════════════════════════\n")
  cat("✗ ERRO NO DEPLOY\n")
  cat("═══════════════════════════════════════════════════════\n")
  cat(sprintf("Mensagem: %s\n\n", e$message))
  
  if (grepl("maximum number of applications", e$message, ignore.case = TRUE)) {
    cat("🔴 PROBLEMA: Limite de aplicações atingido\n\n")
    cat("SOLUÇÕES:\n")
    cat("1. Execute: source('resolver_limite_apps.R')\n")
    cat("2. Delete uma aplicação antiga\n")
    cat("3. Ou use forceUpdate = TRUE se 'crimes_am' já existe:\n\n")
    cat("   rsconnect::deployApp(\n")
    cat("     appDir = getwd(),\n")
    cat("     appName = 'crimes_am',\n")
    cat("     account = 'nupec',\n")
    cat("     server = 'shinyapps.io',\n")
    cat("     forceUpdate = TRUE\n")
    cat("   )\n\n")
  } else if (grepl("Application not found", e$message, ignore.case = TRUE)) {
    cat("🔴 PROBLEMA: Aplicação não existe e limite atingido\n\n")
    cat("SOLUÇÕES:\n")
    cat("1. Deletar uma aplicação antiga\n")
    cat("2. Usar nome diferente (ex: 'crimes_am_v001')\n")
    cat("3. Fazer upgrade da conta\n\n")
  } else {
    cat("Consulte DIAGNOSTICO_DEPLOY.md para mais informações.\n\n")
  }
  
  stop(e)
})


