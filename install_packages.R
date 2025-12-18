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
  # Verifica se o pacote já está instalado e carregável
  if (requireNamespace(pacote, quietly = TRUE)) {
    cat(sprintf("  [OK] %s já está instalado\n", pacote))
    return(TRUE)
  }
  
  cat(sprintf("Instalando %s...\n", pacote))
  
  # Lista de repositórios CRAN para tentar
  repos_cran <- c(
    "https://cloud.r-project.org",
    "https://cran.rstudio.com",
    "https://cran.r-project.org"
  )
  
  # Tenta instalar de cada repositório até conseguir
  for (repo in repos_cran) {
    tryCatch({
      install.packages(
        pacote, 
        dependencies = TRUE, 
        repos = repo,
        quiet = FALSE,
        type = "binary"  # Preferir binários no Windows
      )
      
      # Verifica se a instalação foi bem-sucedida
      if (requireNamespace(pacote, quietly = TRUE)) {
        cat(sprintf("  [OK] %s instalado com sucesso (de %s)\n", pacote, repo))
        return(TRUE)
      } else {
        stop("Pacote instalado mas não carregável")
      }
    }, error = function(e) {
      # Continua para o próximo repositório
      return(NULL)
    }, warning = function(w) {
      # Avisos não impedem a instalação
      return(NULL)
    })
  }
  
  # Se chegou aqui, nenhum repositório funcionou
  cat(sprintf("  [ERRO] Falha ao instalar %s de todos os repositórios tentados\n", pacote))
  return(FALSE)
}

# Verificar se o CRAN está acessível
cat("Verificando conexão com CRAN...\n")
repos_cran <- c(
  "https://cloud.r-project.org",
  "https://cran.rstudio.com",
  "https://cran.r-project.org"
)

cran_ok <- FALSE
for (repo in repos_cran) {
  tryCatch({
    available.packages(repos = repo)
    cat(sprintf("  [OK] Conexão com CRAN estabelecida (%s)\n\n", repo))
    cran_ok <- TRUE
    break
  }, error = function(e) {
    # Tenta próximo repositório
    return(NULL)
  })
}

if (!cran_ok) {
  cat("  [AVISO] Não foi possível conectar a nenhum repositório CRAN.\n")
  cat("  Tentando continuar mesmo assim...\n\n")
}

# Instalar pacotes
cat("Iniciando instalação dos pacotes...\n\n")
resultados <- vapply(pacotes, instalar_pacote, logical(1))
names(resultados) <- pacotes

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
