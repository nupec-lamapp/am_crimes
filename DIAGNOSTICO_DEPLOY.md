# Diagnóstico Completo - Problemas de Deploy

## 🔴 Problema Principal Identificado

**Erro HTTP 402**: Limite de aplicações atingido no shinyapps.io

## 📋 Problemas Encontrados no Projeto

### 1. **Limite de Aplicações (CRÍTICO)**
- Conta gratuita: máximo de 5 aplicações
- Solução: Deletar aplicações antigas OU usar `forceUpdate = TRUE`

### 2. **Caminho com Espaços e Caracteres Especiais**
- Caminho atual: `G:/Meu Drive/1NUPEC_PESQUISAS/NP001 -WS_AH_TIAGO/crimes_am`
- Problema: Espaços e caracteres especiais podem causar problemas
- Solução: O rsconnect geralmente lida com isso, mas pode ser um fator

### 3. **Dependência de Arquivos Locais**
- O app tenta carregar `data/processed/crimes_classificados.csv`
- Este arquivo pode não existir no deploy
- **Status**: ✅ O código já trata isso (retorna NULL se não existir)

### 4. **Scripts de Pipeline no Deploy**
- O app tenta executar `source()` de scripts de scraping
- Problema: Scraping pode não funcionar no shinyapps.io (sem permissões, sem internet confiável)
- **Status**: ⚠️ Funcionalidade pode não funcionar, mas não impede deploy

### 5. **Arquivos Desnecessários no Deploy**
- Muitos arquivos CSV, logs, outputs que não precisam ser enviados
- Problema: Aumenta tamanho do deploy e pode causar timeout
- Solução: Criar `.rscignore`

### 6. **Renv Pode Causar Problemas**
- Projeto usa `renv` mas pode não estar configurado corretamente
- Problema: Dependências podem não ser detectadas
- Solução: Verificar se `renv.lock` existe ou criar `DESCRIPTION`

### 7. **Falta de Arquivo DESCRIPTION**
- Não há arquivo `DESCRIPTION` listando dependências
- Problema: rsconnect pode não detectar todas as dependências
- Solução: Criar `DESCRIPTION` ou usar `renv`

## ✅ Soluções Implementadas

1. ✅ Script `resolver_limite_apps.R` para gerenciar aplicações
2. ✅ Script `deploy.R` com verificação automática
3. ✅ Documentação completa em `DEPLOY.md`

## 🔧 Soluções Adicionais Necessárias

### Criar `.rscignore`
Para excluir arquivos desnecessários do deploy.

### Criar `DESCRIPTION` ou verificar `renv.lock`
Para garantir que todas as dependências sejam detectadas.

### Verificar se `crimes_am` já existe
Se existir, usar `forceUpdate = TRUE`.

## 📝 Checklist de Deploy

- [ ] Executar `source("resolver_limite_apps.R")` para ver aplicações
- [ ] Se `crimes_am` existe: usar `forceUpdate = TRUE`
- [ ] Se não existe: deletar uma aplicação antiga
- [ ] Criar `.rscignore` (já criado)
- [ ] Verificar dependências
- [ ] Fazer deploy com `source("deploy.R")`


