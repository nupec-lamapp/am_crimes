############################################################
# run_emtempo.R
# Coleta e processamento dedicados ao portal Em Tempo
# Uso: Rscript scripts/run_emtempo.R
############################################################

suppressPackageStartupMessages({
  source("scripts/01_scraping.R")
  source("scripts/02_parse.R")
  source("scripts/03_cleaning.R")
})

# Intervalo de coleta: ajuste conforme necessario
data_inicio <- Sys.Date() - 2
data_fim    <- Sys.Date()

message("Iniciando coleta Em Tempo de ", data_inicio, " a ", data_fim)

# Coleta apenas Em Tempo
rodar_scraping(
  data_inicio = data_inicio,
  data_fim    = data_fim,
  portais     = "emtempo"
)

message("Coleta concluida. Iniciando parse e limpeza.")

# Parse e limpeza para atualizar a base que o app consome
df_parsed <- parse_raw_files()
clean_and_enrich_data(df_parsed)

message("Pipeline Em Tempo finalizada.")
