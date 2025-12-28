# Registro de Alterações - Monitor de Crimes Violentos - Amazonas

Todas as mudanças notáveis neste projeto serão documentadas neste arquivo.

## [0.0.7] - 2025-12-28

### Adicionado
- Painel de "Lacunas por portal" reorganizado para exibir todos os meses desde janeiro de 2025 e os dias que ainda faltam coletar.
### Atualizado
- Metadados e documentação de versionamento ajustados para a nova release.

## [0.0.1] - 2025-01-XX

### Adicionado
- Nova aba "Apresentação" no dashboard Shiny com conteúdo do arquivo `APRESENTACAO.md`
- Função `carregar_apresentacao()` para renderizar markdown como HTML formatado
- Logos da UFAM e ICET adicionadas no cabeçalho do aplicativo
- Arquivo `APRESENTACAO.md` criado com descrição completa do projeto e parcerias
- Sistema de versionamento implementado com scripts automatizados
- Documentação de versionamento (VERSIONAMENTO.md e INSTRUCOES_GIT.md)
- Coletor de notícias do portal **A Crítica** (acritica.com) implementado

### Limitações Conhecidas
- **Fonte de dados:** A versão atual extrai informações exclusivamente do portal A Crítica. Expansão para outros portais está planejada para versões futuras.

### Modificado
- Layout do cabeçalho: logos reposicionadas horizontalmente (lado a lado)
  - Lado esquerdo: UFAM e ICET (dispostas horizontalmente)
  - Lado direito: LAMAPP e NuPeC (dispostas horizontalmente)
- Série mensal corrigida: alterado de `plotOutput` para `plotlyOutput` para reatividade
- Melhorado tratamento de erros na série mensal para evitar páginas em branco
- Aba "Apresentação" posicionada como primeira guia do dashboard
- Arquivo `.gitignore` atualizado com exclusões apropriadas

### Corrigido
- Série mensal agora é reativa aos filtros de data selecionados
- Página não aparece mais em branco quando não há dados disponíveis
- Gráfico da série mensal exibe mensagem informativa quando não há dados

---

## Versões Anteriores

### Características Principais
- Visual Profissional + Backend Blindado
- Dashboard Shiny com múltiplas abas de análise
- Sistema de classificação heurística e NLP (Processamento de Linguagem Natural)
- Pipeline completo de scraping e processamento de dados
- Interface interativa com filtros e visualizações dinâmicas

