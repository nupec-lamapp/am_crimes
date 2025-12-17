############################################################
# install_packages.R
# Script para instalar todos os pacotes necessários do projeto
# crimes_am - Monitor de Crimes Violentos - Amazonas
# NUPEC / LAMAPP
#
# Uso:
#   source("install_packages.R")
#   Rscript install_packages.R
############################################################

cat("========================================\n")
cat("Instalação de pacotes - crimes_am\n")
cat("========================================\n\n")

# Lista de pacotes necessários
# Baseado no DESCRIPTION, scripts do pipeline e app Shiny
pacotes <- c(
  # ===== Core Shiny (App Principal) =====
  "shiny",        # Framework Shiny (>= 1.7.0)
  "bslib",        # Temas Bootstrap para Shiny (>= 0.5.0)
  
  # ===== Manipulação de Dados =====
  "dplyr",        # Manipulação de dados (>= 1.1.0)
  "tidyr",        # Reestruturação de dados (usado em NLP)
  "readr",        # Leitura de arquivos CSV/TSV (>= 2.1.0)
  "tibble",       # Estrutura de dados moderna
  "purrr",        # Programação funcional
  "forcats",      # Manipulação de fatores (usado em análises)
  
  # ===== Visualização e Tabelas =====
  "ggplot2",      # Gráficos (>= 3.4.0)
  "plotly",       # Gráficos interativos (>= 4.10.0)
  "DT",           # Tabelas interativas (>= 0.28)
  "reactable",    # Tabelas reativas (>= 0.4.0)
  
  # ===== Datas e Strings =====
  "lubridate",    # Manipulação de datas (>= 1.9.0)
  "stringr",      # Manipulação de strings (>= 1.5.0)
  "stringi",      # Funções avançadas de string (usado em classification_utils.R)
  
  # ===== Sistema de Arquivos =====
  "fs",           # Operações de sistema de arquivos (>= 1.6.0)
  
  # ===== Web Scraping (Pipeline de Coleta) =====
  "httr",         # Cliente HTTP (scripts/01_scraping.R)
  "rvest",        # Web scraping (scripts/01_scraping.R)
  "xml2",         # Parsing XML/HTML (scripts/01_scraping.R)
  
  # ===== Machine Learning / NLP (Pipeline de Classificação) =====
  "tidymodels",   # Framework de ML (scripts/03_nlp_enrichment.R)
  "textrecipes",  # Preprocessamento de texto (scripts/03_nlp_enrichment.R)
  
  # ===== Utilitários =====
  "glue",         # Interpolação de strings (scripts/utils.R)
  
  # ===== Testes =====
  "testthat"      # Framework de testes (opcional, mas recomendado)
)

# Função para instalar pacotes (verifica se já está instalado)
instalar_pacote <- function(pacote) {
  if (!requireNamespace(pacote, quietly = TRUE)) {
    cat(sprintf("Instalando %s...\n", pacote))
    tryCatch({
      install.packages(
        pacote, 
        dependencies = TRUE, 
        repos = "https://cloud.r-project.org",
        quiet = FALSE
      )
      cat(sprintf("  [OK] %s instalado com sucesso\n", pacote))
      TRUE
    }, error = function(e) {
      cat(sprintf("  [ERRO] Falha ao instalar %s: %s\n", pacote, e$message))
      FALSE
    })
  } else {
    cat(sprintf("  [OK] %s já está instalado\n", pacote))
    TRUE
  }
}

# Verificar se o CRAN está acessível
cat("Verificando conexão com CRAN...\n")
tryCatch({
  available.packages(repos = "https://cloud.r-project.org")
  cat("  [OK] Conexão com CRAN estabelecida\n\n")
}, error = function(e) {
  cat("  [ERRO] Não foi possível conectar ao CRAN. Verifique sua conexão com a internet.\n")
  stop("Não foi possível conectar ao CRAN")
})

# Instalar pacotes
cat("Iniciando instalação dos pacotes...\n\n")
resultados <- sapply(pacotes, instalar_pacote)

# Resumo
cat("\n========================================\n")
cat("Resumo da instalação\n")
cat("========================================\n")
cat(sprintf("Total de pacotes: %d\n", length(pacotes)))
cat(sprintf("Instalados com sucesso: %d\n", sum(resultados)))
cat(sprintf("Falhas: %d\n", sum(!resultados)))

if (any(!resultados)) {
  cat("\nPacotes com falha na instalação:\n")
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
cat("Instalação concluída!\n")
cat("========================================\n")

# Nota: Se usar renv, execute renv::restore() para garantir
# que as versões exatas dos pacotes sejam instaladas
if (file.exists("renv/activate.R")) {
  cat("\nNota: Este projeto usa renv. Para restaurar o ambiente exato, execute:\n")
  cat("  renv::restore()\n\n")
}
