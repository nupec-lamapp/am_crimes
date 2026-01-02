# Guia de Versionamento - Monitor de Crimes Violentos

Este documento contém instruções atualizadas para registrar versões no GitHub.

## Versão Atual: 0.0.7

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

### 3. Criar Commit da Versão 0.0.7

```bash
git commit -m "v0.0.7: Lacunas mensais no painel de coleta e ajustes de deploy

- reorganiza a aba de lacunas para mostrar meses desde jan/2025 e os dias faltantes
- inclui arquivos essenciais no bundle e atualiza metas de deploy
- atualiza metadados e documentação para 0.0.7"
```

### 4. Criar Tag da Versão

```bash
# Tag anotada
git tag -a v0.0.7 -m "Versão 0.0.7 - lacunas mensais e ajustes de deploy"
```

### 5. Enviar para o GitHub

```bash
git push origin develop
git push origin v0.0.7
git push origin --tags
```

## Estrutura de Versionamento

### Formato de Versão: MAJOR.MINOR.PATCH

- **MAJOR**: Mudanças incompatíveis com versões anteriores
- **MINOR**: Novas funcionalidades compatíveis
- **PATCH**: Correções de bugs e ajustes menores

## Histórico de Versões

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
