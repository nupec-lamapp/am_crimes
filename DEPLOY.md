# Guia de Deploy - Monitor de Crimes Violentos

## Configuração Inicial (Primeira Vez)

Antes de fazer o primeiro deploy, configure suas credenciais executando:

```r
source("configurar_rsconnect.R")
```

Ou manualmente no console R:

```r
rsconnect::setAccountInfo(
  name = 'nupec',
  token = '4ABCB213E3D7D8D1477BC0D752EF4E02',
  secret = 'XEeRJaE+/2EhskTGm75QS6bKcJ0sZldLbQjZppTk'
)
```

**Nota:** Esta configuração precisa ser feita apenas uma vez. As credenciais serão salvas localmente.

## Problema: Limite de Aplicações Atingido

O erro `HTTP 402 - You have reached the maximum number of applications` indica que sua conta no shinyapps.io atingiu o limite de aplicações permitidas.

## Soluções

### Solução 1: Fazer UPDATE de Aplicação Existente (Recomendado)

Se já existe uma aplicação chamada `crimes_am`, você deve fazer **UPDATE** em vez de criar uma nova:

```r
library(rsconnect)

rsconnect::deployApp(
  appDir = getwd(),
  appName = "crimes_am",
  account = "nupec",
  server = "shinyapps.io",
  forceUpdate = TRUE  # ← IMPORTANTE: força atualização
)
```

### Solução 2: Verificar e Gerenciar Aplicações Existentes (RECOMENDADO)

Execute o script que lista todas as aplicações e mostra opções:

```r
source("resolver_limite_apps.R")
```

Este script irá:
- Listar todas as aplicações existentes
- Verificar se `crimes_am` já existe
- Mostrar comandos para deletar aplicações antigas
- Fornecer soluções específicas para seu caso

Ou manualmente:

```r
library(rsconnect)

# Listar todas as aplicações
apps <- rsconnect::applications(account = "nupec", server = "shinyapps.io")
print(apps)

# Verificar se 'crimes_am' já existe
any(apps$name == "crimes_am")
```

### Solução 3: Deletar Aplicações Antigas

Se você tem aplicações antigas que não usa mais:

```r
library(rsconnect)

# Deletar uma aplicação específica
rsconnect::terminateApp(
  appName = "NOME_DA_APLICACAO_ANTIGA",
  account = "nupec",
  server = "shinyapps.io"
)
```

**Atenção:** Isso deleta permanentemente a aplicação e não pode ser desfeito!

### Solução 4: Usar Nome Diferente

Deploy com um nome diferente (se não houver conflito):

```r
rsconnect::deployApp(
  appDir = getwd(),
  appName = "crimes_am_v001",  # ou outro nome único
  account = "nupec",
  server = "shinyapps.io"
)
```

### Solução 5: Upgrade da Conta (Pago)

Se você precisa de mais aplicações, considere fazer upgrade da conta no shinyapps.io:
- Acesse: https://www.shinyapps.io/admin/#/billing
- Planos pagos permitem mais aplicações

## Comando Completo de Deploy (Recomendado)

### Opção 1: Usar Script Automatizado (Mais Fácil)

```r
source("deploy.R")
```

### Opção 2: Comando Manual

```r
library(rsconnect)

# Deploy com todas as opções recomendadas
rsconnect::deployApp(
  appDir = getwd(),
  appName = "crimes_am",
  account = "nupec",
  server = "shinyapps.io",
  appTitle = "Monitor de Crimes Violentos - Amazonas",
  forceUpdate = TRUE,  # Atualiza se já existir
  launch.browser = TRUE,
  lint = FALSE  # Desabilita lint se houver problemas
)
```

## Verificar Credenciais

Se houver problemas de autenticação:

```r
# Ver contas configuradas
rsconnect::accounts()

# Reconfigurar conta (se necessário)
rsconnect::setAccountInfo(
  name = "nupec",
  token = "SEU_TOKEN",
  secret = "SEU_SECRET"
)
```

## Limites da Conta Gratuita

- **shinyapps.io Free**: 5 aplicações ativas
- **shinyapps.io Starter**: 10 aplicações ativas
- **shinyapps.io Professional**: Aplicações ilimitadas

## Troubleshooting

### Erro: "Application not found"
- Verifique se o nome da aplicação está correto
- Use `forceUpdate = TRUE` para criar se não existir

### Erro: "Authentication failed"
- Verifique suas credenciais com `rsconnect::accounts()`
- Reconfigure se necessário

### Erro: "Package not found"
- Certifique-se de que todos os pacotes estão listados
- Considere usar `renv` para gerenciar dependências

## Arquivos Necessários para Deploy

Certifique-se de que estes arquivos estão presentes:
- `app.R` (aplicativo principal)
- `www/` (arquivos estáticos: logos, imagens)
- `APRESENTACAO.md` (usado pela aba Apresentação)
- Dependências do R (via `renv` ou manualmente)

## Notas Importantes

1. **Dados não são incluídos no deploy**: Arquivos em `data/` e `outputs/` não são enviados por padrão
2. **Tamanho máximo**: Aplicações gratuitas têm limite de tamanho
3. **Timeout**: Scripts longos podem ser interrompidos
4. **Variáveis de ambiente**: Configure se necessário para scraping

