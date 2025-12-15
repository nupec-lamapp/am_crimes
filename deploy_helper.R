############################################################
# Script de Ajuda para Deploy no shinyapps.io
# Resolve o problema de limite de aplicações
############################################################

library(rsconnect)

# Configurar conta (se necessário)
# rsconnect::setAccountInfo(
#   name = "nupec",
#   token = "SEU_TOKEN",
#   secret = "SEU_SECRET"
# )

cat("=== GERENCIADOR DE APLICAÇÕES SHINYAPPS.IO ===\n\n")

# 1. Listar todas as aplicações
cat("1. Listando todas as aplicações da conta 'nupec'...\n")
tryCatch({
  apps <- rsconnect::applications(account = "nupec", server = "shinyapps.io")
  
  if (length(apps) > 0) {
    cat(sprintf("\nTotal de aplicações: %d\n\n", nrow(apps)))
    cat("Aplicações existentes:\n")
    cat("====================\n")
    for (i in 1:nrow(apps)) {
      cat(sprintf("\n%d. Nome: %s\n", i, apps$name[i]))
      cat(sprintf("   URL: %s\n", apps$url[i]))
      cat(sprintf("   Criada em: %s\n", apps$created_time[i]))
      cat(sprintf("   Última atualização: %s\n", apps$updated_time[i]))
    }
    
    cat("\n\n=== OPÇÕES ===\n\n")
    cat("OPÇÃO 1: Fazer UPDATE de uma aplicação existente\n")
    cat("Se já existe uma aplicação chamada 'crimes_am', use:\n")
    cat("  rsconnect::deployApp(\n")
    cat("    appDir = getwd(),\n")
    cat("    appName = 'crimes_am',\n")
    cat("    account = 'nupec',\n")
    cat("    server = 'shinyapps.io',\n")
    cat("    forceUpdate = TRUE\n")
    cat("  )\n\n")
    
    cat("OPÇÃO 2: Deletar aplicações antigas\n")
    cat("Para deletar uma aplicação, use:\n")
    cat("  rsconnect::terminateApp(\n")
    cat("    appName = 'NOME_DA_APLICACAO',\n")
    cat("    account = 'nupec',\n")
    cat("    server = 'shinyapps.io'\n")
    cat("  )\n\n")
    
    cat("OPÇÃO 3: Usar um nome diferente\n")
    cat("Deploy com um nome diferente:\n")
    cat("  rsconnect::deployApp(\n")
    cat("    appDir = getwd(),\n")
    cat("    appName = 'crimes_am_v2',  # ou outro nome\n")
    cat("    account = 'nupec',\n")
    cat("    server = 'shinyapps.io'\n")
    cat("  )\n\n")
    
  } else {
    cat("Nenhuma aplicação encontrada.\n")
  }
  
}, error = function(e) {
  cat(sprintf("Erro ao listar aplicações: %s\n", e$message))
  cat("\nTente verificar suas credenciais:\n")
  cat("  rsconnect::accounts()\n")
})

cat("\n=== VERIFICAR SE 'crimes_am' JÁ EXISTE ===\n")
tryCatch({
  apps <- rsconnect::applications(account = "nupec", server = "shinyapps.io")
  if (any(apps$name == "crimes_am")) {
    cat("✓ Aplicação 'crimes_am' JÁ EXISTE!\n")
    cat("  Use forceUpdate = TRUE para atualizar em vez de criar nova.\n\n")
    cat("Comando para fazer UPDATE:\n")
    cat("rsconnect::deployApp(\n")
    cat("  appDir = getwd(),\n")
    cat("  appName = 'crimes_am',\n")
    cat("  account = 'nupec',\n")
    cat("  server = 'shinyapps.io',\n")
    cat("  forceUpdate = TRUE\n")
    cat(")\n")
  } else {
    cat("✗ Aplicação 'crimes_am' NÃO existe.\n")
    cat("  Você precisa deletar uma aplicação antiga ou usar um nome diferente.\n")
  }
}, error = function(e) {
  cat(sprintf("Erro: %s\n", e$message))
})


