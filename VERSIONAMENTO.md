# Guia de Versionamento - Monitor de Crimes Violentos

Este documento contém instruções para versionar o projeto no GitHub.

## Versão Atual: 0.0.1

## Comandos para Versionamento

### 1. Verificar Status do Repositório

```bash
git status
```

### 2. Adicionar Arquivos Modificados

```bash
# Adicionar todos os arquivos modificados
git add app.R
git add APRESENTACAO.md
git add CHANGELOG.md
git add VERSIONAMENTO.md

# Ou adicionar tudo de uma vez
git add .
```

### 3. Criar Commit da Versão 0.0.1

```bash
git commit -m "v0.0.1: Layout horizontal de logos, aba Apresentação como primeira guia e melhorias na série mensal

- Adicionada aba Apresentação como primeira guia do dashboard
- Logos reposicionadas horizontalmente no cabeçalho (UFAM/ICET à esquerda, LAMAPP/NuPeC à direita)
- Série mensal corrigida para ser reativa aos filtros
- Melhorado tratamento de erros para evitar páginas em branco
- Criado arquivo APRESENTACAO.md com descrição completa do projeto
- Criado CHANGELOG.md para documentação de versões"
```

### 4. Criar Tag de Versão

```bash
# Criar tag anotada
git tag -a v0.0.1 -m "Versão 0.0.1 - Layout horizontal de logos e aba Apresentação integrada"

# Ou criar tag simples
git tag v0.0.1
```

### 5. Enviar para o GitHub

```bash
# Enviar commits
git push origin main

# Ou se a branch for master
git push origin master

# Enviar tags
git push origin v0.0.1

# Ou enviar todas as tags
git push origin --tags
```

## Estrutura de Versionamento

### Formato de Versão: MAJOR.MINOR.PATCH

- **MAJOR**: Mudanças incompatíveis com versões anteriores
- **MINOR**: Novas funcionalidades compatíveis com versões anteriores
- **PATCH**: Correções de bugs

### Histórico de Versões

- **v0.0.1** (2025-01-XX): Versão inicial - Layout horizontal, aba Apresentação, melhorias na série mensal, sistema de versionamento

## Arquivos Importantes para Versionamento

- `app.R` - Aplicativo Shiny principal
- `APRESENTACAO.md` - Documentação do projeto
- `CHANGELOG.md` - Registro de alterações
- `README.md` - Documentação principal
- `VERSIONAMENTO.md` - Este arquivo

## Notas

- Sempre atualize o `CHANGELOG.md` antes de criar uma nova versão
- Use mensagens de commit descritivas e em português
- Crie tags para todas as versões importantes
- Mantenha o histórico de versões atualizado

