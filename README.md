# crimes_am

O Monitor de Crimes Violentos - Amazonas e uma ferramenta experimental de monitoramento automatizado de noticias sobre crimes violentos no estado do Amazonas. O sistema integra tecnologias de web scraping, processamento de linguagem natural (NLP) e visualizacao interativa de dados para coletar, classificar e analisar noticias policiais publicadas em portais de noticias locais.

Monitoramento automatizado de noticias sobre crimes violentos no Amazonas, com coleta (web scraping), classificacao/enriquecimento e visualizacao em Shiny.

## Visão geral

- Coleta de notícias em portais de mídia (scraping).
- Pipeline: `raw -> parse -> cleaning -> análises`.
- App Shiny lê principalmente `data/processed/crimes_classificados.csv`.


## Portais monitorados

- A Crítica (`acritica.com/policia`)
- Em Tempo (`emtempo.com.br/category/policia`)
- D24AM (`d24am.com/policia`)
- G1 Amazonas (`g1.globo.com/am/amazonas/`)

## Estrutura do projeto

### Pipeline (scripts)

- `scripts/01_scraping.R`: coletores com registro centralizado (`rodar_scraping()`).
- `scripts/02_parse.R`: leitura/padronização dos CSVs em `data/raw/`.
- `scripts/03_cleaning.R`: limpeza + classificação + enriquecimento (gera `data/processed/crimes_classificados.csv`).
- `scripts/04_analysis.R`: indicadores e saídas em `outputs/`.
- `run_pipeline.R`: orquestrador CLI (`Rscript run_pipeline.R 2025-11-20 2025-11-21`).

### App

- `app.R`: app Shiny principal.
- `R/mod_*`: módulos do app.

## Requisitos

- R 4.x e pacotes: `shiny`, `bslib`, `dplyr`, `ggplot2`, `DT`, `readr`, `lubridate`, `stringr`, `tibble`, `purrr`, `httr`, `rvest`, `xml2`, `testthat`.
- Acesso à internet para o scraper.

## Execução (CLI)

```sh
# Rodar para o intervalo padrão (últimos 7 dias)
Rscript run_pipeline.R

# Rodar para um dia específico
Rscript run_pipeline.R 2025-11-21

# Rodar para um intervalo
Rscript run_pipeline.R 2025-11-15 2025-11-21
```

## Execução (App Shiny)

```r
shiny::runApp("app.R")
```

## Caminhos e permissão de escrita

Por padrão, o projeto grava logs e artefatos em `logs/`, `data/` e `outputs/`.

Em ambientes com filesystem restrito (ex.: deploy), defina `CRIMES_AM_WORKDIR` para um diretório gravável; caso contrário, o projeto tenta usar `tempdir()` automaticamente.

Exemplo:

```sh
set CRIMES_AM_WORKDIR=C:\temp\crimes_am
```

## Deduplicacao de noticias

A deduplicacao ocorre em `scripts/02_parse.R` em duas etapas:

- `distinct(portal, data_publicacao, titulo, url)` remove duplicatas exatas.
- `remover_duplicados_janela()` remove titulos repetidos em uma janela de dias
  (padrao 7) usando titulo normalizado.

O escopo de dedup pode ser ajustado por `dedup_por` (padrao `portal,titulo_norm`)
ou via variavel de ambiente `CRIMES_AM_DEDUP_POR`.

Exemplos (Windows):

```sh
# Dedup por portal + titulo (padrao)
set CRIMES_AM_DEDUP_POR=portal,titulo_norm

# Dedup entre portais (mesmo titulo)
set CRIMES_AM_DEDUP_POR=titulo_norm
```

Se precisar ajustar a janela de dias, use `parse_raw_files(dedup_dias = N)`
em uma execucao manual no R.



