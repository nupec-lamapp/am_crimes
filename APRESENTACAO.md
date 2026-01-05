# Monitor de Crimes Violentos - Amazonas

**Versão:** 0.0.9 (2026-01-03)

## Sobre o projeto

O **Monitor de Crimes Violentos - Amazonas** é uma ferramenta experimental de monitoramento automatizado de notícias sobre crimes violentos no estado do Amazonas. O sistema integra tecnologias de **web scraping**, **processamento de linguagem natural (NLP)** e **visualização interativa de dados** para coletar, classificar e analisar notícias policiais publicadas em portais de notícias locais.

### Portais monitorados (v0.0.9)

- **A Crítica** ([acritica.com/policia](https://www.acritica.com/policia))
- **Em Tempo** ([emtempo.com.br/category/policia](https://emtempo.com.br/category/policia/))
- **D24AM** ([d24am.com/policia](https://d24am.com/policia/))
- **G1 Amazonas** ([g1.globo.com/am/amazonas](https://g1.globo.com/am/amazonas/))

### Principais funcionalidades

- **Coleta automatizada**: scraping multiportal com requisições mais robustas (respeito a `robots.txt` quando disponível, suporte a `Retry-After` e cache por URL).
- **Deduplicação configurável**: controle por colunas (via `CRIMES_AM_DEDUP_POR`) e janela parametrizada no parse.
- **Classificação e enriquecimento**: tipologias criminais (heurísticas e modelos), além de extração de atributos como gênero, faixa etária e gravidade.
- **Pipeline reprodutível**: scripts `raw -> parse -> cleaning -> análises`, com orquestração opcional via `targets` e metadados de execução por run (JSON).
- **Conteúdo técnico**: artigo científico em `.Rmd` renderizado no app (com fallback quando `rmarkdown/pandoc` não estiver disponível).
- **Dashboard Shiny**: filtros, indicadores e relatórios, incluindo painéis de controle de coleta (lacunas e cobertura por portal) e textos de contexto para interpretação.

### Objetivos

O projeto visa contribuir para a pesquisa em **ciência de dados** e **avaliação de políticas públicas**, fornecendo uma base de dados estruturada e análises que possam apoiar:

- Estudos sobre padrões de criminalidade no Amazonas.
- Avaliação de políticas de segurança pública.
- Pesquisas acadêmicas em ciências sociais e criminologia.
- Monitoramento de tendências e indicadores de violência.

---

## Tecnologias utilizadas

- **R**: linguagem principal para desenvolvimento.
- **Shiny + bslib**: interface web interativa.
- **Tidymodels**: suporte a modelos de machine learning.
- **Web scraping**: `httr`, `rvest`, `xml2` e `robotstxt`.
- **NLP**: rotinas de processamento de texto para classificação e enriquecimento.
- **Visualização**: `ggplot2`, `plotly`, `DT` e `reactable`.

---

## Leituras e cuidados para análise criminológica

- A **unidade de análise** aqui é a *notícia publicada*, não o boletim de ocorrência ou processo judicial.
- Os indicadores refletem a **cobertura midiática**, não a totalidade dos crimes ocorridos (viés de seleção da mídia).
- Mudanças editoriais dos portais podem afetar as séries (por exemplo, novas seções, reestruturação de páginas, alterações de layout).
- As categorias e tipologias buscam dialogar com a literatura em violência/criminalidade, mas são simplificações operacionais.

---

## Status do projeto

O projeto está em **evolução contínua**, com melhorias regulares no pipeline de coleta, deduplicação, classificação e apresentação dos indicadores.

Para histórico de alterações, consulte o arquivo [`CHANGELOG.md`](CHANGELOG.md).

---

## Licença

Este projeto é distribuído sob a licença MIT. Consulte o arquivo [`LICENSE`](LICENSE).
