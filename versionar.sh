#!/bin/bash
# Script de Versionamento - Monitor de Crimes Violentos v0.0.1
# Execute este script para criar uma nova versão no GitHub

VERSION="0.0.1"
TAG_MESSAGE="Versão 0.0.1 - Layout horizontal de logos e aba Apresentação integrada"

echo "=========================================="
echo "Versionamento - Monitor de Crimes Violentos"
echo "Versão: $VERSION"
echo "=========================================="
echo ""

# Verificar se estamos em um repositório Git
if [ ! -d ".git" ]; then
    echo "❌ Erro: Este diretório não é um repositório Git."
    echo "   Execute: git init"
    exit 1
fi

# Verificar status
echo "📋 Verificando status do repositório..."
git status

echo ""
read -p "Deseja continuar com o versionamento? (s/n) " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Ss]$ ]]; then
    echo "❌ Versionamento cancelado."
    exit 1
fi

# Adicionar arquivos
echo ""
echo "📦 Adicionando arquivos..."
git add app.R
git add APRESENTACAO.md
git add CHANGELOG.md
git add VERSIONAMENTO.md
git add .gitignore

# Criar commit
echo ""
echo "💾 Criando commit..."
git commit -m "v$VERSION: Versão inicial - Layout horizontal de logos, aba Apresentação como primeira guia e melhorias na série mensal

- Adicionada aba Apresentação como primeira guia do dashboard
- Logos reposicionadas horizontalmente no cabeçalho (UFAM/ICET à esquerda, LAMAPP/NuPeC à direita)
- Série mensal corrigida para ser reativa aos filtros
- Melhorado tratamento de erros para evitar páginas em branco
- Criado arquivo APRESENTACAO.md com descrição completa do projeto
- Criado CHANGELOG.md para documentação de versões"

# Criar tag
echo ""
echo "🏷️  Criando tag v$VERSION..."
git tag -a "v$VERSION" -m "$TAG_MESSAGE"

# Mostrar resumo
echo ""
echo "✅ Versionamento concluído!"
echo ""
echo "📝 Próximos passos:"
echo "   1. git push origin main (ou master)"
echo "   2. git push origin v$VERSION"
echo "   ou"
echo "   git push origin --tags"
echo ""

