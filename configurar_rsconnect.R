############################################################
# Configuração Inicial do rsconnect
# Execute este script UMA VEZ para configurar suas credenciais
############################################################

library(rsconnect)

cat("=== CONFIGURAÇÃO DO RSCONNECT ===\n\n")
cat("Configurando conta shinyapps.io...\n\n")

# Configurar conta
rsconnect::setAccountInfo(
  name = 'nupec',
  token = '4ABCB213E3D7D8D1477BC0D752EF4E02',
  secret = 'XEeRJaE+/2EhskTGm75QS6bKcJ0sZldLbQjZppTk'
)

cat("✓ Conta 'nupec' configurada com sucesso!\n\n")

# Verificar configuração
cat("Verificando configuração...\n")
accounts <- rsconnect::accounts()
print(accounts)

cat("\n✓ Configuração concluída!\n")
cat("Agora você pode usar o script deploy.R para fazer o deploy.\n\n")

