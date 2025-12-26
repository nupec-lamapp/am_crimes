@echo off
REM Script de Versionamento - Monitor de Crimes Violentos v0.0.4
REM Execute este script para criar uma nova versão no GitHub

set VERSION=0.0.4
set TAG_MESSAGE=Versão 0.0.4

echo ==========================================
echo Versionamento - Monitor de Crimes Violentos
echo Versão: %VERSION%
echo ==========================================
echo.

REM Verificar se estamos em um repositório Git
if not exist ".git" (
    echo [ERRO] Este diretório não é um repositório Git.
    echo        Execute: git init
    exit /b 1
)

REM Verificar status
echo [INFO] Verificando status do repositório...
git status

echo.
set /p CONTINUAR="Deseja continuar com o versionamento? (s/n): "

if /i not "%CONTINUAR%"=="s" (
    echo [ERRO] Versionamento cancelado.
    exit /b 1
)

REM Adicionar arquivos
echo.
echo [INFO] Adicionando arquivos...
git add app.R
git add APRESENTACAO.md
git add CHANGELOG.md
git add VERSIONAMENTO.md
git add .gitignore

REM Criar commit
echo.
echo [INFO] Criando commit...
git commit -m "v%VERSION%: Versão inicial - Layout horizontal de logos, aba Apresentação como primeira guia e melhorias na série mensal

- Adicionada aba Apresentação como primeira guia do dashboard
- Logos reposicionadas horizontalmente no cabeçalho (UFAM/ICET à esquerda, LAMAPP/NuPeC à direita)
- Série mensal corrigida para ser reativa aos filtros
- Melhorado tratamento de erros para evitar páginas em branco
- Criado arquivo APRESENTACAO.md com descrição completa do projeto
- Criado CHANGELOG.md para documentação de versões"

REM Criar tag
echo.
echo [INFO] Criando tag v%VERSION%...
git tag -a "v%VERSION%" -m "%TAG_MESSAGE%"

REM Mostrar resumo
echo.
echo [SUCESSO] Versionamento concluído!
echo.
echo [INFO] Próximos passos:
echo    1. git push origin main (ou master)
echo    2. git push origin v%VERSION%
echo    ou
echo    git push origin --tags
echo.

pause

