# Solução Rápida - Erro de Limite de Aplicações

## ⚡ Solução Mais Rápida

Execute este comando no R:

```r
source("resolver_limite_apps.R")
```

Este script irá mostrar todas as suas aplicações e as opções para resolver o problema.

## 🔍 Passo a Passo

### 1. Verificar Aplicações Existentes

```r
source("resolver_limite_apps.R")
```

### 2. Se 'crimes_am' JÁ EXISTE

Use `forceUpdate = TRUE`:

```r
rsconnect::deployApp(
  appDir = getwd(),
  appName = "crimes_am",
  account = "nupec",
  server = "shinyapps.io",
  forceUpdate = TRUE  # ← Isso resolve o problema!
)
```

### 3. Se 'crimes_am' NÃO EXISTE

Você precisa deletar uma aplicação antiga primeiro:

```r
# Listar aplicações
apps <- rsconnect::applications(account = "nupec", server = "shinyapps.io")
print(apps)

# Deletar uma aplicação antiga (substitua 'NOME_APP' pelo nome real)
rsconnect::terminateApp(
  appName = "NOME_DA_APLICACAO_ANTIGA",
  account = "nupec",
  server = "shinyapps.io"
)

# Depois faça o deploy normalmente
source("deploy.R")
```

## 📋 Comandos Úteis

```r
# Ver todas as aplicações
rsconnect::applications(account = "nupec", server = "shinyapps.io")

# Verificar se crimes_am existe
apps <- rsconnect::applications(account = "nupec", server = "shinyapps.io")
any(apps$name == "crimes_am")

# Deletar aplicação
rsconnect::terminateApp("NOME_APP", account = "nupec", server = "shinyapps.io")

# Deploy com forceUpdate
rsconnect::deployApp(..., forceUpdate = TRUE)
```

## ⚠️ Importante

- **Conta gratuita**: Limite de 5 aplicações
- **Deletar é permanente**: Não pode ser desfeito
- **forceUpdate**: Atualiza aplicação existente sem criar nova


