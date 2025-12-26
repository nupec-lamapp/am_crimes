# Instruções para Versionamento no GitHub

## Configuração Inicial (Primeira Vez)

### 1. Inicializar Repositório Git (se ainda não foi feito)

```bash
git init
```

### 2. Configurar Usuário Git (se necessário)

```bash
git config user.name "Seu Nome"
git config user.email "seu.email@exemplo.com"
```

### 3. Adicionar Repositório Remoto do GitHub

```bash
# Criar um repositório no GitHub primeiro, depois:
git remote add origin https://github.com/USUARIO/crimes_am.git

# Ou se usar SSH:
git remote add origin git@github.com:USUARIO/crimes_am.git
```

## Versionamento da Versão 0.0.4

### Opção 1: Usar Script Automatizado

**Windows:**
```cmd
versionar.bat
```

**Linux/Mac:**
```bash
chmod +x versionar.sh
./versionar.sh
```

### Opção 2: Comandos Manuais

#### 1. Verificar Status
```bash
git status
```

#### 2. Adicionar Arquivos
```bash
git add DESCRIPTION
git add APRESENTACAO.md
git add CHANGELOG.md
git add VERSIONAMENTO.md
git add app.R
git add deploy.R
git add deploy_final.R
git add versionar.sh
git add versionar.bat
```

#### 3. Criar Commit
```bash
git commit -m "v0.0.4: Atualização de versão

- Versão do projeto atualizada para 0.0.4"
```

#### 4. Criar Tag de Versão
```bash
git tag -a v0.0.4 -m "Versão 0.0.4"
```

#### 5. Enviar para GitHub
```bash
# Enviar commits
git push origin main
# ou se a branch for master:
git push origin master

# Enviar tag
git push origin v0.0.4

# Ou enviar todas as tags de uma vez
git push origin --tags
```

---

## Versionamento da Versão 0.0.1 (Histórico)

### Opção 1: Usar Script Automatizado

**Windows:**
```cmd
versionar.bat
```

**Linux/Mac:**
```bash
chmod +x versionar.sh
./versionar.sh
```

### Opção 2: Comandos Manuais

#### 1. Verificar Status
```bash
git status
```

#### 2. Adicionar Arquivos
```bash
git add app.R
git add APRESENTACAO.md
git add CHANGELOG.md
git add VERSIONAMENTO.md
git add .gitignore
```

#### 3. Criar Commit
```bash
git commit -m "v0.0.1: Versão inicial - Layout horizontal de logos, aba Apresentação como primeira guia e melhorias na série mensal

- Adicionada aba Apresentação como primeira guia do dashboard
- Logos reposicionadas horizontalmente no cabeçalho (UFAM/ICET à esquerda, LAMAPP/NuPeC à direita)
- Série mensal corrigida para ser reativa aos filtros
- Melhorado tratamento de erros para evitar páginas em branco
- Criado arquivo APRESENTACAO.md com descrição completa do projeto
- Criado CHANGELOG.md para documentação de versões"
```

#### 4. Criar Tag de Versão
```bash
git tag -a v0.0.1 -m "Versão 0.0.1 - Layout horizontal de logos e aba Apresentação integrada"
```

#### 5. Enviar para GitHub
```bash
# Enviar commits
git push origin main
# ou se a branch for master:
git push origin master

# Enviar tag
git push origin v0.0.1

# Ou enviar todas as tags de uma vez
git push origin --tags
```

## Verificar Versões Criadas

```bash
# Listar todas as tags
git tag

# Ver detalhes de uma tag
git show v0.0.4
# ou
git show v0.0.1
```

## Criar Release no GitHub

1. Acesse o repositório no GitHub
2. Vá em "Releases" → "Create a new release"
3. Selecione a tag `v0.0.4` (ou a versão desejada)
4. Título: `v0.0.4` (ou conforme a versão)
5. Descrição: Copie o conteúdo da seção `[0.0.4]` do `CHANGELOG.md`
6. Clique em "Publish release"

## Estrutura de Commits

Use mensagens de commit descritivas seguindo o padrão:

```
vVERSÃO: Resumo curto

- Detalhe 1
- Detalhe 2
- Detalhe 3
```

## Troubleshooting

### Se o Git não estiver instalado:

**Windows:**
- Baixe em: https://git-scm.com/download/win
- Ou instale via: `winget install Git.Git`

**Linux:**
```bash
sudo apt-get install git  # Ubuntu/Debian
sudo yum install git      # CentOS/RHEL
```

**Mac:**
```bash
brew install git
```

### Se houver conflitos:

```bash
# Ver conflitos
git status

# Resolver conflitos manualmente nos arquivos indicados
# Depois:
git add .
git commit -m "Resolve conflitos"
```

