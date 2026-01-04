# Guia de Versionamento - Monitor de Crimes Violentos

Este documento contém instruções atualizadas para registrar versões no GitHub.

## Versão Atual: 0.0.9

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

### 3. Criar Commit da Versão 0.0.9

```bash
git commit -m "v0.0.9: Robustez do scraping, artigo em Rmd e melhorias no painel

- scraper com robots.txt (quando disponível), retry/backoff com Retry-After e cache por URL
- app renderiza ArtigoWebSrapingSegurancapublica.Rmd (fallback quando pandoc não existe)
- pipeline registra metadados por run e parametriza deduplicação por CRIMES_AM_DEDUP_POR"
```

### 4. Criar Tag da Versão

```bash
# Tag anotada
git tag -a v0.0.9 -m "Versão 0.0.9 - robustez do scraping e padronização do artigo"
```

### 5. Enviar para o GitHub

```bash
git push origin develop
git push origin v0.0.9
git push origin --tags
```

## Estrutura de Versionamento

### Formato de Versão: MAJOR.MINOR.PATCH

- **MAJOR**: Mudanças incompatíveis com versões anteriores
- **MINOR**: Novas funcionalidades compatíveis
- **PATCH**: Correções de bugs e ajustes menores

## Histórico de Versões

- **v0.0.9 (2026-01-03)**: robustez do scraping (robots.txt, retry/backoff e cache), artigo em `.Rmd` no app e ajustes de reprodutibilidade do pipeline.
- **v0.0.8 (2025-12-29)**: melhorias na aba "Controle de Coleta" (filtros, cobertura e feedback de execução) e ajustes no deploy.
- **v0.0.7 (2025-12-28)**: reorganização da aba "Lacunas por portal" para exibir cada mês desde jan/2025 com os dias faltantes, atualização das instruções e metadados para a nova release.
- **v0.0.6 (2025-12-27)**: Template científico, créditos e documentação de versionamento atualizados.
- **v0.0.5 (2025-12-26)**: Integração com todos os portais monitorados e botão de atualização do dashboard.
- **v0.0.1 (2025-01-XX)**: Versão inicial com layout horizontal e aba Apresentação.

## Arquivos Importantes para Versionamento

- `app.R` - Aplicativo Shiny principal
- `ArtigoWebSrapingSegurancapublica.Rmd` - Template do artigo científico
- `CHANGELOG.md` - Registro de alterações por versão
- `VERSIONAMENTO.md`, `INSTRUCOES_GIT.md` - Guias de versionamento/comandos

## Notas

- Sempre atualize `CHANGELOG.md` antes de rotular uma nova versão
- Mensagens de commit devem ser descritivas e em português
- Crie tags para todas as versões públicas e mantenha o histórico limpo
