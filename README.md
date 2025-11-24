# crimes_am

O Monitor de Crimes Violentos - Amazonas e uma ferramenta experimental de monitoramento automatizado de noticias sobre crimes violentos no estado do Amazonas. O sistema integra tecnologias de web scraping, processamento de linguagem natural (NLP) e visualizacao interativa de dados para coletar, classificar e analisar noticias policiais publicadas em portais de noticias locais.


Monitoramento automatizado de notícias sobre crimes violentos no Amazonas, com coleta (web scraping), classificação/enriquecimento e visualização em Shiny.

**Nota:** a versão atual (0.0.1) extrai notícias exclusivamente do portal **A Crítica** (acritica.com).

## Estrutura do projeto

### Pipeline oficial (1 → 4)

- `scripts/01_scraping.R`: coletores com registro centralizado (`rodar_scraping()`). Atualmente implementado apenas o coletor do portal A Crítica.
- `scripts/02_parse.R`: leitura e padronização inicial dos dados brutos (data/raw → df parseado).
- `scripts/03_cleaning.R`: limpeza, classificação e enriquecimento (df parseado → crimes_classificados.csv).
- `scripts/04_analysis.R`: análises estáticas (opcional, mas integrada ao app na aba **“Relatórios & Auditoria”**).
- `run_pipeline.R`: orquestrador oficial (`Rscript run_pipeline.R 2025-11-20 2025-11-21`).

### App e relatórios

- `app.R`: dashboard Shiny principal.
- `05_boletim_crimes.Rmd`: relatório RMarkdown (boletim estático baseado em `data/processed/crimes_classificados.csv`).
- `R/classification_utils.R`: funções de classificação e enriquecimento compartilhadas (usadas no processamento e nos testes).

### Scripts auxiliares (uso manual)

- `scripts/05_analise_exploratoria.R`: análises exploratórias e tabelas/figuras auxiliares (uso manual).
- `scripts/06_avaliacao_classificador.R`: avaliação detalhada do classificador com base na validação manual.

### Principais artefatos gerados

- `data/raw/`: dumps por portal/data gerados pelo scraping (ignorado no Git).
- `data/processed/crimes_classificados.csv`: base consolidada para app/relatórios.
- `data/processed/crimes_classificados_nlp.csv`: base consolidada com colunas sugeridas pelo modelo NLP.
- `outputs/`: resumos e gráficos usados na aba “Relatórios & Auditoria” do app.

## Pré-requisitos

- R ≥ 4.2 com pacotes: `shiny`, `bslib`, `dplyr`, `ggplot2`, `DT`, `readr`, `lubridate`,
  `stringr`, `tibble`, `purrr`, `httr`, `rvest`, `xml2`, `testthat`, `tidymodels`,
  `textrecipes`, `forcats`, `tidyr` (recomenda-se uso de `renv`).
- Acesso à internet para o scraper e permissão de escrita em `data/`, `logs/` e `outputs/`.

## Uso rápido

### Pipeline completa

```sh
# Rodar pipeline completa (scraping + processamento + NLP + relatórios) para hoje
Rscript run_pipeline.R

# Especificar intervalo (datas no formato ISO)
Rscript run_pipeline.R 2025-11-15 2025-11-21
```

### Etapas individuais

```sh
# 1) Scraping: gera CSVs em data/raw/
Rscript scripts/01_scraping.R

# 2) Parse: consolida dados brutos e padroniza campos
Rscript -e "source('scripts/02_parse.R'); df_parsed <- parse_raw_files()"

# 3) Cleaning: limpeza + classificação + enriquecimento
Rscript -e "source('scripts/02_parse.R'); source('scripts/03_cleaning.R'); df_parsed <- parse_raw_files(); clean_and_enrich_data(df_parsed)"

# 4) Análises estáticas focadas em crimes violentos (opcional)
Rscript scripts/04_analysis.R
```

### App Shiny

Após gerar `data/processed/crimes_classificados.csv` (e opcionalmente a versão `_nlp`), basta iniciar o app Shiny:

```r
shiny::runApp("app.R")
```

## Testes

Testes unitários básicos garantem que o classificador heurístico continue categorizando corretamente crimes críticos e que o processamento gere a estrutura esperada:

```sh
Rscript -e "testthat::test_dir('tests/testthat')"
```

## Versionamento

O projeto utiliza versionamento semântico (`MAJOR.MINOR.PATCH`). Para criar uma nova versão:

- **Windows**: execute `versionar.bat`
- **Linux/Mac**: execute `./versionar.sh`

Ou siga as instruções em `VERSIONAMENTO.md` para versionamento manual.

Consulte `CHANGELOG.md` para o histórico completo de alterações.

## Deploy no shinyapps.io

Para fazer deploy da aplicação:

1. **Configurar credenciais (primeira vez)**  
   ```r
   source("configurar_rsconnect.R")
   ```

2. **Fazer deploy**  
   ```r
   source("deploy.R")
   ```

Para mais detalhes e troubleshooting, consulte `DEPLOY.md` e `DIAGNOSTICO_DEPLOY.md`.

## Próximos passos sugeridos

- Adicionar novos coletores chamando `registrar_coletor("portal", func)` em `scripts/01_scraping.R`.
- Integrar `renv` com um `renv.lock` versionado.
- Automatizar execuções (cron, GitHub Actions etc.) chamando `run_pipeline.R`.
- Integrar o app Shiny com as colunas `tipo_ml` / `prob_tipo_ml` para comparação visual entre heurística e modelo NLP.


