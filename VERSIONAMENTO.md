# Guia de Versionamento - Monitor de Crimes Violentos

Este documento contém instruções atualizadas para registrar versões no GitHub.

## Versão Atual: 0.0.6

## Comandos para Versionamento

### 1. Verificar Status do Repositório

```bash
git status
```

### 2. Adicionar Arquivos Modificados

```bash
# Liste explicitamente os arquivos-chave antes do commit
git add app.R
git add ArtigoWebSrapingSegurancapublica.Rmd
git add csl/abnt.csl
git add references.bib
git add CHANGELOG.md
git add VERSIONAMENTO.md
git add INSTRUCOES_GIT.md
# Ou adicione tudo de uma vez
git add .
```

### 3. Criar Commit da Versão 0.0.6

```bash
git commit -m "v0.0.6: Template RBSP, créditos e documentação científica

- Artigo em RMarkdown alinhado à Revista Brasileira de Segurança Pública
- Estilo ABNT (`csl/abnt.csl`) e bibliografia (`references.bib`)
- Créditos no app (equipe e instituições parceiras)
- Atualização do changelog e demais instruções de versionamento"
```

### 4. Criar Tag de Versão

```bash
# Tag anotada
git tag -a v0.0.6 -m "Versão 0.0.6 - template RBSP e créditos oficiais"
```

### 5. Enviar para o GitHub

```bash
git push origin develop
git push origin v0.0.6
git push origin --tags
```

## Estrutura de Versionamento

### Formato de Versão: MAJOR.MINOR.PATCH

- **MAJOR**: Mudanças incompatíveis com versões anteriores
- **MINOR**: Novas funcionalidades compatíveis
- **PATCH**: Correções de bugs e ajustes menores

## Histórico de Versões

- **v0.0.6 (2025-12-27)**: Template científico, créditos e documentação de versionamento atualizados.
- **v0.0.5 (2025-12-26)**: Integração com todos os portais monitorados e botão de atualização do dashboard.
- **v0.0.1 (2025-01-XX)**: Versão inicial com layout horizontal e aba Apresentação.

## Arquivos Importantes para Versionamento

- `app.R` – Aplicativo Shiny principal
- `ArtigoWebSrapingSegurancapublica.Rmd` – Template do artigo científico
- `CHANGELOG.md` – Registro de alterações por versão
- `VERSIONAMENTO.md`, `INSTRUCOES_GIT.md` – Guias de versionamento/comandos

## Notas

- Sempre atualize `CHANGELOG.md` antes de rotular uma nova versão
- Mensagens de commit devem ser descritivas e em português
- Crie tags para todas as versões públicas e mantenha o histórico limpo
