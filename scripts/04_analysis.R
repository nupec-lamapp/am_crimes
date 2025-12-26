############################################################
# 04_analysis.R
# Geração de indicadores e saídas analíticas
# Análises focadas em crimes violentos na mídia (AM)
#
# Pré-requisito:
#   - data/processed/crimes_classificados.csv
#
# Saídas:
#   - outputs/04_resumo_geral.csv
#   - outputs/04_resumo_categoria.csv
#   - outputs/04_resumo_tipo_principal.csv
#   - outputs/04_resumo_portal_categoria.csv
#   - outputs/04_resumo_genero_tipo.csv
#   - outputs/04_resumo_faixa_genero.csv
#   - gráficos em outputs/04_*.png
############################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(ggplot2)
  library(forcats)
})

if (file.exists("R/paths.R")) source("R/paths.R")
if (exists("crimes_am_set_paths")) crimes_am_set_paths()
if (!exists("DIR_OUTPUTS", inherits = TRUE)) DIR_OUTPUTS <- "outputs"
if (!exists("DIR_PROCESSED", inherits = TRUE)) DIR_PROCESSED <- file.path("data", "processed")
dir.create(DIR_OUTPUTS, showWarnings = FALSE, recursive = TRUE)
out_path <- function(x) file.path(DIR_OUTPUTS, x)

############################################################
# 1. Leitura da base classificada
############################################################

cat("Lendo crimes_classificados.csv ...\n")

df <- read_csv(file.path(DIR_PROCESSED, "crimes_classificados.csv"),
               show_col_types = FALSE) %>%
  mutate(
    data_publicacao = as.Date(data_publicacao),
    mes_ano = floor_date(data_publicacao, "month")
  )

stopifnot("coluna crime_violento não encontrada" = "crime_violento" %in% names(df))

############################################################
# 2. Recorte: apenas crimes violentos
############################################################

viol <- df %>% filter(crime_violento == TRUE)

if (nrow(viol) == 0) {
  stop("Nenhum crime violento encontrado em crimes_classificados.csv")
}

cat("Total de notícias violentas:", nrow(viol), "\n")

viol <- viol %>%
  mutate(
    eh_letal = categoria == "Crime Letal Violento"
  )

############################################################
# 3. Resumo geral
############################################################

resumo_geral <- viol %>%
  summarise(
    n_noticias_violentas = n(),
    n_unico_portais = n_distinct(portal),
    data_min = min(data_publicacao, na.rm = TRUE),
    data_max = max(data_publicacao, na.rm = TRUE),
    prop_letal = mean(eh_letal, na.rm = TRUE),
    prop_feminicidio_dos_letais = ifelse(
      any(eh_letal),
      mean(tipo_principal == "Feminicídio" & eh_letal, na.rm = TRUE) /
        max(mean(eh_letal, na.rm = TRUE), 1e-9),
      NA_real_
    )
  )

write_csv(resumo_geral, out_path("04_resumo_geral.csv"))
cat("Resumo geral salvo em ", out_path("04_resumo_geral.csv"), "\n")

############################################################
# 4. Resumos por categoria e tipo de crime
############################################################

resumo_categoria <- viol %>%
  count(categoria, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(resumo_categoria, out_path("04_resumo_categoria.csv"))

resumo_tipo <- viol %>%
  count(tipo_principal, sort = TRUE) %>%
  mutate(pct = round(100 * n / sum(n), 1))

write_csv(resumo_tipo, out_path("04_resumo_tipo_principal.csv"))

cat("Resumos por categoria e tipo salvos em outputs/04_resumo_*.csv\n")

############################################################
# 5. Comparação entre portais (viés de cobertura)
############################################################

resumo_portal_cat <- viol %>%
  count(portal, categoria) %>%
  group_by(portal) %>%
  mutate(
    total_portal = sum(n),
    pct = round(100 * n / total_portal, 1)
  ) %>%
  ungroup()

write_csv(resumo_portal_cat, out_path("04_resumo_portal_categoria.csv"))
cat("Resumo portal x categoria salvo em ", out_path("04_resumo_portal_categoria.csv"), "\n")

top_portais <- viol %>%
  count(portal, sort = TRUE) %>%
  slice_max(n, n = 6) %>%
  pull(portal)

p_portal_cat <- resumo_portal_cat %>%
  filter(portal %in% top_portais) %>%
  ggplot(aes(x = categoria, y = pct, fill = categoria)) +
  geom_col() +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_stack(vjust = 0.5),
            size = 3,
            color = "white") +
  coord_flip() +
  facet_wrap(~ portal, scales = "free_y") +
  labs(
    title = "Composição de categorias de crimes violentos por portal",
    x = "Categoria",
    y = "% das notícias violentas no portal"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

ggsave(out_path("04_portal_categoria.png"),
       p_portal_cat, width = 10, height = 6, dpi = 300)

############################################################
# 6. Perfil aproximado das vítimas
############################################################

resumo_genero_tipo <- viol %>%
  count(genero_vitima, tipo_principal) %>%
  group_by(genero_vitima) %>%
  mutate(
    total_genero = sum(n),
    pct = round(100 * n / total_genero, 1)
  ) %>%
  ungroup()

write_csv(resumo_genero_tipo, out_path("04_resumo_genero_tipo.csv"))

p_genero_tipo <- resumo_genero_tipo %>%
  filter(!is.na(genero_vitima),
         genero_vitima %in% c("feminino", "masculino")) %>%
  ggplot(aes(x = tipo_principal, y = n, fill = genero_vitima)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.9),
            hjust = -0.1,
            size = 3) +
  coord_flip() +
  labs(
    title = "Tipo de crime x gênero estimado da vítima",
    x = "Tipo de crime",
    y = "Número de notícias",
    fill = "Gênero"
  ) +
  theme_minimal()

ggsave(out_path("04_genero_tipo.png"),
       p_genero_tipo, width = 10, height = 6, dpi = 300)

resumo_faixa_genero <- viol %>%
  filter(!is.na(faixa_etaria)) %>%
  count(faixa_etaria, genero_vitima) %>%
  group_by(faixa_etaria) %>%
  mutate(
    total_faixa = sum(n),
    pct = round(100 * n / total_faixa, 1)
  ) %>%
  ungroup()

write_csv(resumo_faixa_genero, out_path("04_resumo_faixa_genero.csv"))

p_faixa_genero <- resumo_faixa_genero %>%
  ggplot(aes(x = faixa_etaria, y = n, fill = genero_vitima)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = paste0(pct, "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 3) +
  labs(
    title = "Faixa etária x gênero estimado da vítima",
    x = "Faixa etária",
    y = "Número de notícias",
    fill = "Gênero"
  ) +
  theme_minimal()

ggsave(out_path("04_faixa_genero.png"),
       p_faixa_genero, width = 9, height = 5, dpi = 300)

############################################################
# 7. Dinâmica temporal focada nos violentos
############################################################

serie_viol <- viol %>%
  filter(!is.na(data_publicacao)) %>%
  count(data_publicacao)

p_serie_viol <- serie_viol %>%
  ggplot(aes(x = data_publicacao, y = n)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Série temporal - notícias de crimes violentos",
    x = "Data de publicação",
    y = "Número de notícias violentas"
  ) +
  theme_minimal()

ggsave(out_path("04_serie_violentos.png"),
       p_serie_viol, width = 9, height = 4, dpi = 300)

serie_letal <- viol %>%
  filter(!is.na(data_publicacao), eh_letal) %>%
  count(data_publicacao)

p_serie_letal <- serie_letal %>%
  ggplot(aes(x = data_publicacao, y = n)) +
  geom_line(color = "#E63946") +
  geom_point(color = "#E63946") +
  labs(
    title = "Série temporal - crimes letais violentos",
    x = "Data de publicação",
    y = "Número de notícias letais"
  ) +
  theme_minimal()

ggsave(out_path("04_serie_letais.png"),
       p_serie_letal, width = 9, height = 4, dpi = 300)

indice_letal_mensal <- viol %>%
  group_by(mes_ano) %>%
  summarise(
    n_violentos = n(),
    n_letais = sum(eh_letal, na.rm = TRUE),
    indice_letal = n_letais / n_violentos,
    .groups = "drop"
  )

write_csv(indice_letal_mensal, out_path("04_indice_letal_mensal.csv"))

p_indice_letal <- indice_letal_mensal %>%
  ggplot(aes(x = mes_ano, y = indice_letal)) +
  geom_line(color = "#1D3557") +
  geom_point(color = "#1D3557") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Índice de letalidade (proporção de crimes letais entre violentos)",
    x = "Mês/ano",
    y = "Índice de letalidade"
  ) +
  theme_minimal()

ggsave(out_path("04_indice_letal_mensal.png"),
       p_indice_letal, width = 9, height = 4, dpi = 300)

############################################################
# 8. Cruzamentos adicionais
############################################################

tipo_gravidade <- viol %>%
  count(tipo_principal, gravidade) %>%
  group_by(tipo_principal) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

write_csv(tipo_gravidade, out_path("04_tipo_gravidade.csv"))

if ("genero_vitima" %in% names(viol)) {
  tipo_genero <- viol %>%
    count(tipo_principal, genero_vitima) %>%
    group_by(tipo_principal) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()
  write_csv(tipo_genero, out_path("04_tipo_genero.csv"))
}

if ("faixa_etaria" %in% names(viol)) {
  tipo_faixa <- viol %>%
    filter(!is.na(faixa_etaria), faixa_etaria != "idade não informada") %>%
    count(tipo_principal, faixa_etaria) %>%
    group_by(tipo_principal) %>%
    mutate(pct = round(100 * n / sum(n), 1)) %>%
    ungroup()
  write_csv(tipo_faixa, out_path("04_tipo_faixa.csv"))
}

portal_gravidade <- viol %>%
  count(portal, gravidade) %>%
  group_by(portal) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  ungroup()

write_csv(portal_gravidade, out_path("04_portal_gravidade.csv"))

cat("Cruzamentos adicionais salvos em outputs/04_*.csv\n")

############################################################
# 9. Anomalias da classificação
############################################################

anomalias <- viol %>%
  filter(categoria == "Outros") %>%
  select(data_publicacao, portal, titulo, tipo_principal, categoria, gravidade)

write_csv(anomalias, out_path("04_anomalias_classificacao.csv"))

cat("Análise concluída.\n")
cat("Arquivos gerados em '", DIR_OUTPUTS, "'.\n", sep = "")
