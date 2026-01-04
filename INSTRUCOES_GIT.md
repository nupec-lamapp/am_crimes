# Instruções para Versionamento no GitHub

## Configuração Inicial (se ainda não foi feita)

### 1. Inicializar repositório Git

```bash
git init
```

### 2. Configurar usuário Git (se necessário)

```bash
git config user.name "Seu Nome"
git config user.email "seu.email@exemplo.com"
```

### 3. Adicionar repositório remoto

```bash
git remote add origin https://github.com/USUARIO/crimes_am.git
```

## Versionamento da Versão 0.0.9

### Opção 1: Usar script de versionamento

**Windows:**
```cmd
versionar.bat
```

**Linux / macOS:**
```bash
chmod +x versionar.sh
./versionar.sh
```

### Opção 2: Comandos manuais

#### 1. Verificar status

```bash
git status
```

#### 2. Adicionar arquivos

```bash
git add app.R
git add ArtigoWebSrapingSegurancapublica.Rmd
git add csl/abnt.csl
git add references.bib
git add CHANGELOG.md
git add VERSIONAMENTO.md
git add INSTRUCOES_GIT.md
```

#### 3. Criar commit

```bash
git commit -m "v0.0.9: Robustez do scraping, artigo em Rmd e melhorias no painel

- Scraper com robots.txt (quando disponível), retry/backoff com Retry-After e cache por URL
- App renderiza ArtigoWebSrapingSegurancapublica.Rmd (fallback quando pandoc não existe)
- Pipeline registra metadados por run e parametriza deduplicação por CRIMES_AM_DEDUP_POR"
```

#### 4. Criar tag

```bash
git tag -a v0.0.9 -m "Versão 0.0.9 - robustez do scraping e padronização do artigo"
```

#### 5. Enviar para o GitHub

```bash
git push origin develop
git push origin v0.0.9
git push origin --tags
```

## Verificar versões criadas

```bash
git tag
git show v0.0.9
```

## Criar release no GitHub

1. Acesse o repositório e vá em **Releases > Create a new release**
2. Escolha a tag `v0.0.9`
3. Use o título `v0.0.9 - robustez do scraping e padronização do artigo`
4. Copie o conteúdo da seção `[0.0.9]` do `CHANGELOG.md`
5. Clique em **Publish release**

## Estrutura de commits

Utilize o padrão:

```
vVERSÃO: Resumo direto

- Detalhe 1
- Detalhe 2
- Detalhe 3
```

## Troubleshooting

### Se o Git não estiver instalado

**Windows:** `https://git-scm.com/download/win` ou `winget install Git.Git`  
**Linux:** `sudo apt-get install git` (Ubuntu/Debian), `sudo yum install git` (CentOS/RHEL)  
**Mac:** `brew install git`

### Se houver conflitos

```bash
git status
# resolver manualmente
git add .
git commit -m "Resolve conflitos"
```
