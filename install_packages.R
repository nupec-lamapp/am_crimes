############################################################
# install_packages.R
# Script para instalar todos os pacotes necessarios do projeto
# crimes_am - Monitor de Crimes Violentos - Amazonas
# NUPEC / LAMAPP
#
# Uso:
#   source("install_packages.R")
#   Rscript install_packages.R
############################################################

cat("========================================\n")
cat("Instalacao de pacotes - crimes_am\n")
cat("========================================\n\n")

# Lista de pacotes necessarios
# Baseado no DESCRIPTION, scripts do pipeline e app Shiny
pacotes <- c(
  # ===== Core Shiny (App Principal) =====
  "shiny",        # Framework Shiny (>= 1.7.0)
  "bslib",        # Temas Bootstrap para Shiny (>= 0.5.0)
  
  # ===== Manipulacao de Dados =====
  "dplyr",        # Manipulacao de dados (>= 1.1.0)
  "tidyr",        # Reestruturacao de dados (usado em NLP)
  "readr",        # Leitura de arquivos CSV/TSV (>= 2.1.0)
  "tibble",       # Estrutura de dados moderna
  "purrr",        # Programacao funcional
  "forcats",      # Manipulacao de fatores (usado em analises)
  
  # ===== Visualizacao e Tabelas =====
  "ggplot2",      # Graficos (>= 3.4.0)
  "plotly",       # Graficos interativos (>= 4.10.0)
  "DT",           # Tabelas interativas (>= 0.28)
  "reactable",    # Tabelas reativas (>= 0.4.0)
  
  # ===== Datas e Strings =====
  "lubridate",    # Manipulacao de datas (>= 1.9.0)
  "stringr",      # Manipulacao de strings (>= 1.5.0)
  "stringi",      # Funcoes avancadas de string (usado em classification_utils.R)
  
  # ===== Sistema de Arquivos =====
  "fs",           # Operacoes de sistema de arquivos (>= 1.6.0)
  
  # ===== Web Scraping (Pipeline de Coleta) =====
  "httr",         # Cliente HTTP (scripts/01_scraping.R)
  "rvest",        # Web scraping (scripts/01_scraping.R)
  "xml2",         # Parsing XML/HTML (scripts/01_scraping.R)
  
  # ===== Machine Learning / NLP (Pipeline de Classificacao) =====
  "tidymodels",   # Framework de ML (scripts/03_nlp_enrichment.R)
  "textrecipes",  # Pre-processamento de texto (scripts/03_nlp_enrichment.R)
  
  # ===== Utilitarios =====
  "glue",         # Interpolacao de strings (scripts/utils.R)
  
  # ===== Testes =====
  "testthat"      # Framework de testes (opcional, mas recomendado)
)

# Funcao para instalar pacotes (verifica se ja esta instalado)
instalar_pacote <- function(pacote) {
  # Verifica se o pacote ja esta instalado e carregavel
  if (requireNamespace(pacote, quietly = TRUE)) {
    cat(sprintf("  [OK] %s ja esta instalado\n", pacote))
    return(TRUE)
  }
  
  cat(sprintf("Instalando %s...\n", pacote))
  
  # Lista de repositorios CRAN para tentar
  repos_cran <- c(
    "https://cloud.r-project.org",
    "https://cran.rstudio.com",
    "https://cran.r-project.org"
  )
  
  # Tenta instalar de cada repositorio ate conseguir
  for (repo in repos_cran) {
    tryCatch({
      install.packages(
        pacote, 
        dependencies = TRUE, 
        repos = repo,
        quiet = FALSE,
        type = "binary"  # Preferir binarios no Windows
      )
      
      # Verifica se a instalacao foi bem-sucedida
      if (requireNamespace(pacote, quietly = TRUE)) {
        cat(sprintf("  [OK] %s instalado com sucesso (de %s)\n", pacote, repo))
        return(TRUE)
      } else {
        stop("Pacote instalado mas nao carregavel")
      }
    }, error = function(e) {
      # Continua para o proximo repositorio
      return(NULL)
    }, warning = function(w) {
      # Avisos nao impedem a instalacao
      return(NULL)
    })
  }
  
  # Se chegou aqui, nenhum repositorio funcionou
  cat(sprintf("  [ERRO] Falha ao instalar %s de todos os repositorios tentados\n", pacote))
  return(FALSE)
}

# Verificar se o CRAN esta acessivel
cat("Verificando conexao com CRAN...\n")
repos_cran <- c(
  "https://cloud.r-project.org",
  "https://cran.rstudio.com",
  "https://cran.r-project.org"
)

cran_ok <- FALSE
for (repo in repos_cran) {
  tryCatch({
    available.packages(repos = repo)
    cat(sprintf("  [OK] Conexao com CRAN estabelecida (%s)\n\n", repo))
    cran_ok <- TRUE
    break
  }, error = function(e) {
    # Tenta proximo repositorio
    return(NULL)
  })
}

if (!cran_ok) {
  cat("  [AVISO] Nao foi possivel conectar a nenhum repositorio CRAN.\n")
  cat("  Tentando continuar mesmo assim...\n\n")
}

# Instalar pacotes
cat("Iniciando instalacao dos pacotes...\n\n")
resultados <- vapply(pacotes, instalar_pacote, logical(1))
names(resultados) <- pacotes

# Resumo
cat("\n========================================\n")
cat("Resumo da instalacao\n")
cat("========================================\n")
cat(sprintf("Total de pacotes: %d\n", length(pacotes)))
cat(sprintf("Instalados com sucesso: %d\n", sum(resultados)))
cat(sprintf("Falhas: %d\n", sum(!resultados)))

if (any(!resultados)) {
  cat("\nPacotes com falha na instalacao:\n")
  cat(paste(names(resultados)[!resultados], collapse = ", "), "\n")
  cat("\nTente instalar manualmente com:\n")
  cat(
    "install.packages(c('",
    paste(names(resultados)[!resultados], collapse = "', '"),
    "'))\n",
    sep = ""
  )
} else {
  cat("\n[OK] Todos os pacotes foram instalados com sucesso!\n")
}

cat("\n========================================\n")
cat("Instalacao concluida!\n")
cat("========================================\n")

# Nota: Se usar renv, execute renv::restore() para garantir
# que as versoes exatas dos pacotes sejam instaladas
if (file.exists("renv/activate.R")) {
  cat("\nNota: Este projeto usa renv. Para restaurar o ambiente exato, execute:\n")
  cat("  renv::restore()\n\n")
}
