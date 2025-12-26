# Monitor de Crimes Violentos - Amazonas

## Sobre o Projeto

O **Monitor de Crimes Violentos - Amazonas** é uma ferramenta experimental de monitoramento automatizado de notícias sobre crimes violentos no estado do Amazonas. O sistema integra tecnologias de **web scraping**, **processamento de linguagem natural (NLP)** e **visualização interativa de dados** para coletar, classificar e analisar notícias policiais publicadas em portais de notícias locais.

**Nota:** a versão atual (0.0.1) extrai informações exclusivamente do portal **A Crítica** (acritica.com).

### Principais Funcionalidades

- **Coleta automatizada**: sistema de scraping que monitora o portal **A Crítica** (acritica.com) em tempo quase real.
- **Classificação inteligente**: classificação automática de tipologias criminais utilizando métodos heurísticos e modelos de machine learning.
- **Enriquecimento de dados**: extração automática de informações como gênero da vítima, faixa etária e gravidade do crime.
- **Dashboard interativo**: interface Shiny para visualização e análise dos dados coletados.
- **Análises estatísticas**: geração de relatórios e métricas sobre crimes violentos e letais.

### Objetivos

O projeto visa contribuir para a pesquisa em **ciência de dados** e **avaliação de políticas públicas**, fornecendo uma base de dados estruturada e análises que possam apoiar:

- Estudos sobre padrões de criminalidade no Amazonas.
- Avaliação de políticas de segurança pública.
- Pesquisas acadêmicas em ciências sociais e criminologia.
- Monitoramento de tendências e indicadores de violência.

---

## Tecnologias Utilizadas

- **R**: linguagem principal para desenvolvimento.
- **Shiny**: framework para aplicações web interativas.
- **Tidymodels**: framework para modelos de machine learning.
- **Web scraping**: coleta automatizada de dados de portais de notícias.
- **NLP**: processamento de linguagem natural para classificação de textos.

---

## Leituras e cuidados para análise criminológica

- A **unidade de análise** aqui é a *notícia publicada*, não o boletim de ocorrência ou processo judicial.
- Os indicadores refletem a **cobertura midiática**, não a totalidade dos crimes ocorridos (viés de seleção da mídia).
- Mudanças editoriais dos portais podem afetar as séries (por exemplo, aumento de jornalismo de bairro, novas seções de polícia).
- As categorias e tipologias buscam dialogar com a literatura em violência/criminalidade, mas são simplificações operacionais.

---

## Status do Projeto

O projeto está em **contínua evolução**, com melhorias regulares nos algoritmos de classificação, expansão dos portais monitorados e refinamento das análises estatísticas. A versão atual (0.0.1) monitora exclusivamente o portal **A Crítica**, com planos de expansão para outros portais de notícias do Amazonas. Integra o ecossistema de pesquisa NuPeC/LAMAPP e contribui para o avanço do conhecimento em monitoramento de políticas públicas e análise de dados criminais.

