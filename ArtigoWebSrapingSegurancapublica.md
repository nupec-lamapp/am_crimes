# Artigo em elaboração

## Introdução

O projeto **Monitor de Crimes Violentos – Amazonas** fornece uma base para estudos em segurança pública com coleta automatizada de notícias policias, classificação baseada em heurísticas e NLP e distribuição dos indicadores por meio de um aplicativo Shiny. O objetivo do artigo é mostrar como o pipeline `crimes_am` permite reconstruir metodologicamente cada etapa do monitoramento e favorecer a transparência analítica.

## Materiais e métodos

### Pipeline
- **Scraping**: `scripts/01_scraping.R` realiza requisições seguras, registra logs em `logs/scraping.log` e grava os CSVs brutos em `data/raw/`.  
- **Padronização**: `scripts/02_parse.R` lê todos os arquivos de `data/raw/`, normaliza títulos e elimina duplicados dentro da mesma janela de sete dias.
- **Limpeza e enriquecimento**: `scripts/03_cleaning.R` aplica classificadores heurísticos (`classification_utils.R`), deriva variáveis de gênero/faixa etária e grava `data/processed/crimes_classificados.csv` com dicionários e templates de validação manual.
- **Indicadores**: `scripts/04_analysis.R` calcula resumos gerais, por categoria e por portal, exportando CSVs e gráficos em `outputs/`.
- **Orquestração**: `run_pipeline.R` aceita intervalo de datas, registra cada etapa via `log_pipeline()` e gera métricas de volume/outros artefatos em `logs/`.

### Reprodutibilidade

- Repositório Git com branches `main`, `develop`, `release` e tags semânticas (hoje `v0.0.6`). Guias `VERSIONAMENTO.md` e `INSTRUCOES_GIT.md` descrevem o fluxo total.
- Ambiente isolado via `.Rprofile` e `renv`, com dependências em `DESCRIPTION` (ggplot2, shiny, textrecipes etc.).
- Testes `testthat` garantem utilitários de scraping/classificação e a presença de colunas essenciais na base processada.

## Resultados preliminares

A base `data/processed/crimes_classificados.csv` contém os campos `portal`, `data_publicacao`, `titulo`, `categoria`, `tipo_principal`, `gravidade`, `crime_violento`, `genero`, `idade`, `faixa_etaria`. Os resumos em `outputs/04_resumo_*` indicam o peso relativo de crimes letais, feminicídios e violência sexual em cada portal e permitem comparar viéses de cobertura.

## Discussão e conclusões

O monitoramento automatizado, integrado a documentação científica e à visualização interativa, representa um recurso para avaliação contínua da violência no Amazonas. A combinação de versionamento rigoroso, testes automatizados e interface pública favorece a auditoria, replicabilidade e uso institucional dos dados gerados.
