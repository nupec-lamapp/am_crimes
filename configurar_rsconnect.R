############################################################
# Configuração Inicial do rsconnect
# Execute este script UMA VEZ para configurar suas credenciais
# 
# IMPORTANTE: Este arquivo contém credenciais sensíveis.
# NÃO faça commit deste arquivo no Git!
############################################################

library(rsconnect)

cat("=== CONFIGURAÇÃO DO RSCONNECT ===\n\n")
cat("Configurando conta shinyapps.io...\n\n")

# Obter credenciais de variáveis de ambiente ou solicitar ao usuário
token <- Sys.getenv("RSCONNECT_TOKEN", "")
secret <- Sys.getenv("RSCONNECT_SECRET", "")

if (nchar(token) == 0 || nchar(secret) == 0) {
  cat("Por favor, forneça suas credenciais do shinyapps.io:\n")
  cat("Você pode obtê-las em: https://www.shinyapps.io/admin/#/tokens\n\n")
  
  token <- readline("Token: ")
  secret <- readline("Secret: ")
  
  if (nchar(token) == 0 || nchar(secret) == 0) {
    stop("Token e Secret são obrigatórios!")
  }
}

# Configurar conta
rsconnect::setAccountInfo(
  name = 'nupec',
  token = token,
  secret = secret
)

cat("✓ Conta 'nupec' configurada com sucesso!\n\n")

# Verificar configuração
cat("Verificando configuração...\n")
accounts <- rsconnect::accounts()
print(accounts)

cat("\n✓ Configuração concluída!\n")
cat("Agora você pode usar o script deploy.R para fazer o deploy.\n\n")
cat("DICA: Para não precisar digitar novamente, defina as variáveis de ambiente:\n")
cat("  RSCONNECT_TOKEN=seu_token\n")
cat("  RSCONNECT_SECRET=seu_secret\n")

