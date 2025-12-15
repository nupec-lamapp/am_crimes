############################################################
# Script para Resolver Limite de Aplicações no shinyapps.io
# Lista aplicações e ajuda a deletar as antigas
############################################################

library(rsconnect)

cat("=== RESOLVER LIMITE DE APLICAÇÕES ===\n\n")
cat("Listando todas as aplicações da conta 'nupec'...\n\n")

tryCatch({
  apps <- rsconnect::applications(account = "nupec", server = "shinyapps.io")
  
  if (nrow(apps) == 0) {
    cat("Nenhuma aplicação encontrada.\n")
    cat("Você pode criar uma nova aplicação agora.\n")
  } else {
    cat(sprintf("Total de aplicações: %d\n\n", nrow(apps)))
    cat("═══════════════════════════════════════════════════════\n")
    cat("APLICAÇÕES EXISTENTES:\n")
    cat("═══════════════════════════════════════════════════════\n\n")
    
    for (i in 1:nrow(apps)) {
      cat(sprintf("%d. Nome: %s\n", i, apps$name[i]))
      cat(sprintf("   URL: %s\n", apps$url[i]))
      if (!is.null(apps$created_time[i])) {
        cat(sprintf("   Criada: %s\n", apps$created_time[i]))
      }
      if (!is.null(apps$updated_time[i])) {
        cat(sprintf("   Última atualização: %s\n", apps$updated_time[i]))
      }
      cat("\n")
    }
    
    cat("═══════════════════════════════════════════════════════\n")
    cat("VERIFICANDO SE 'crimes_am' JÁ EXISTE...\n")
    cat("═══════════════════════════════════════════════════════\n\n")
    
    if (any(apps$name == "crimes_am")) {
      cat("✓ Aplicação 'crimes_am' JÁ EXISTE!\n\n")
      cat("SOLUÇÃO: Use forceUpdate = TRUE para atualizar em vez de criar nova.\n\n")
      cat("Execute o seguinte comando:\n\n")
      cat("rsconnect::deployApp(\n")
      cat("  appDir = getwd(),\n")
      cat("  appName = 'crimes_am',\n")
      cat("  account = 'nupec',\n")
      cat("  server = 'shinyapps.io',\n")
      cat("  forceUpdate = TRUE\n")
      cat(")\n\n")
      cat("Ou simplesmente: source('deploy.R')\n\n")
    } else {
      cat("✗ Aplicação 'crimes_am' NÃO existe.\n\n")
      cat("═══════════════════════════════════════════════════════\n")
      cat("OPÇÕES PARA RESOLVER O LIMITE:\n")
      cat("═══════════════════════════════════════════════════════\n\n")
      
      cat("OPÇÃO 1: Deletar uma aplicação antiga\n")
      cat("------------------------------------\n")
      cat("Execute o comando abaixo substituindo 'NOME_APP' pelo nome da aplicação:\n\n")
      cat("rsconnect::terminateApp(\n")
      cat("  appName = 'NOME_DA_APLICACAO',\n")
      cat("  account = 'nupec',\n")
      cat("  server = 'shinyapps.io'\n")
      cat(")\n\n")
      
      cat("OPÇÃO 2: Usar um nome diferente\n")
      cat("------------------------------\n")
      cat("Deploy com um nome diferente (ex: crimes_am_v001):\n\n")
      cat("rsconnect::deployApp(\n")
      cat("  appDir = getwd(),\n")
      cat("  appName = 'crimes_am_v001',\n")
      cat("  account = 'nupec',\n")
      cat("  server = 'shinyapps.io'\n")
      cat(")\n\n")
      
      cat("OPÇÃO 3: Fazer upgrade da conta\n")
      cat("-------------------------------\n")
      cat("Acesse: https://www.shinyapps.io/admin/#/billing\n")
      cat("Planos pagos permitem mais aplicações.\n\n")
    }
    
    cat("═══════════════════════════════════════════════════════\n")
    cat("DELETAR APLICAÇÃO (INTERATIVO)\n")
    cat("═══════════════════════════════════════════════════════\n\n")
    cat("Para deletar uma aplicação, descomente e execute:\n\n")
    cat("# rsconnect::terminateApp(\n")
    cat("#   appName = 'NOME_DA_APLICACAO_AQUI',\n")
    cat("#   account = 'nupec',\n")
    cat("#   server = 'shinyapps.io'\n")
    cat("# )\n\n")
  }
  
}, error = function(e) {
  cat(sprintf("Erro ao listar aplicações: %s\n\n", e$message))
  cat("Verifique suas credenciais:\n")
  cat("  rsconnect::accounts()\n\n")
  cat("Se necessário, execute: source('configurar_rsconnect.R')\n")
})


