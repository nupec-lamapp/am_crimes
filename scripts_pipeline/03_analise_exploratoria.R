############################################################
# 03_analise_exploratoria.R
# Análises exploratórias e gráficos padrão
# Projeto: crimes_am - NUPEC / LAMAPP
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(forcats)
})

options(stringsAsFactors = FALSE)

DIR_PROCESSED <- file.path("data", "processed")
DIR_OUTPUTS   <- "outputs"
DIR_FIGS      <- file.path(DIR_OUTPUTS, "figs")

if (!dir.exists(DIR_OUTPUTS)) dir.create(DIR_OUTPUTS, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(DIR_FIGS))   dir.create(DIR_FIGS,   recursive = TRUE, showWarnings = FALSE)

arq_class <- file.path(DIR_PROCESSED, "crimes_classificados.csv")
if (!file.exists(arq_class)) {
  stop("Arquivo não encontrado: ", arq_class,
       "\nRode antes o 02_processamento.R.")
}

df <- readr::read_csv(arq_class, show_col_types = FALSE) %>%
  mutate(
    data_publicacao = as.Date(data_publicacao),
    mes_ano = floor_date(data_publicacao, "month")
  )

############################################################
# 1. Tabelas resumo
############################################################

resumo_categoria <- df %>%
  count(categoria, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

resumo_tipo_principal <- df %>%
  count(tipo_principal, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

resumo_gravidade <- df %>%
  count(gravidade, sort = FALSE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

readr::write_csv(resumo_categoria,      file.path(DIR_OUTPUTS, "resumo_categoria.csv"))
readr::write_csv(resumo_tipo_principal, file.path(DIR_OUTPUTS, "resumo_tipo_principal.csv"))
readr::write_csv(resumo_gravidade,      file.path(DIR_OUTPUTS, "resumo_gravidade.csv"))

############################################################
# 2. Gráficos
############################################################

# 2.1 Série temporal total
p_serie_total <- df %>%
  filter(!is.na(data_publicacao)) %>%
  count(data_publicacao) %>%
  ggplot(aes(x = data_publicacao, y = n)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Série temporal de notícias de crimes",
    x = "Data de publicação",
    y = "Número de notícias"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(DIR_FIGS, "01_serie_temporal_total.png"),
  plot     = p_serie_total,
  width    = 8,
  height   = 4,
  dpi      = 300
)

# 2.2 Série temporal - Crimes Letais Violentos
p_serie_letal <- df %>%
  filter(!is.na(data_publicacao),
         categoria == "Crime Letal Violento") %>%
  count(data_publicacao) %>%
  ggplot(aes(x = data_publicacao, y = n)) +
  geom_line(color = "#E63946") +
  geom_point(color = "#E63946") +
  labs(
    title = "Série temporal – Crimes Letais Violentos",
    x = "Data de publicação",
    y = "Número de notícias"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(DIR_FIGS, "02_serie_temporal_crimes_letais.png"),
  plot     = p_serie_letal,
  width    = 8,
  height   = 4,
  dpi      = 300
)

# 2.3 Top 15 tipos de crime
p_top_tipos <- df %>%
  count(tipo_principal, sort = TRUE) %>%
  slice_max(n, n = 15) %>%
  mutate(tipo_principal = fct_reorder(tipo_principal, n)) %>%
  ggplot(aes(x = tipo_principal, y = n)) +
  geom_col(fill = "#E63946") +
  coord_flip() +
  labs(
    title = "Top 15 tipos de crime",
    x = "Tipo de crime",
    y = "Número de notícias"
  ) +
  theme_minimal()

ggsave(
  filename = file.path(DIR_FIGS, "03_top_tipos_crime.png"),
  plot     = p_top_tipos,
  width    = 8,
  height   = 6,
  dpi      = 300
)

# 2.4 Distribuição por gravidade
p_gravidade <- df %>%
  count(gravidade) %>%
  ggplot(aes(x = gravidade, y = n, fill = gravidade)) +
  geom_col() +
  labs(
    title = "Crimes por nível de gravidade",
    x = "Gravidade",
    y = "Número de notícias"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(
  filename = file.path(DIR_FIGS, "04_distribuicao_gravidade.png"),
  plot     = p_gravidade,
  width    = 6,
  height   = 4,
  dpi      = 300
)

message("Análises exploratórias salvas em outputs/ e outputs/figs/")
