## ⚙️ Documentação Detalhada do Script

Esta seção descreve, de forma detalhada, cada etapa do script `script_mestrado.Rmd`, explicando o objetivo, funcionamento e papel de cada bloco dentro da análise.

---

# 🧩 1. Inicialização do Ambiente

### 📌 Objetivo

Preparar o ambiente de trabalho, carregando bibliotecas necessárias para manipulação de dados, análise estatística e modelagem espacial.

### 🔍 O que é feito:

* Carregamento de pacotes para:

  * Manipulação de dados (`tidyverse`)
  * Leitura de arquivos (`readxl`, `openxlsx`)
  * Visualização (`ggplot2`, `corrplot`)
  * Modelagem estatística (`MASS`, `car`, `robustbase`)
  * Dados espaciais (`sf`, `spdep`, `GWmodel`, `mgwnbr`)
  * Relatórios (`knitr`, `kableExtra`)

### 💡 Interpretação

Este bloco garante que todas as dependências estejam disponíveis para execução do pipeline completo. A diversidade de pacotes reflete a natureza multidisciplinar da análise (estatística + geografia + visualização).

---

# 📥 2. Importação das Bases de Dados

### 📌 Objetivo

Carregar os dados brutos necessários para a análise.

### 🔍 O que é feito:

* Leitura de arquivos Excel contendo:

  * Indicadores socioeconômicos
  * Dados populacionais
  * Registros de violência
* Importação de arquivos geográficos (shapefiles)

### 💡 Interpretação

Esta etapa centraliza todas as fontes de informação utilizadas no estudo. A combinação de dados tabulares com dados espaciais é fundamental para permitir análises georreferenciadas.

---

# 🧹 3. Tratamento e Limpeza dos Dados

### 📌 Objetivo

Garantir consistência e compatibilidade entre as diferentes bases de dados.

### 🔍 O que é feito:

* Padronização de nomes de distritos
* Remoção de acentos e caracteres especiais
* Ajustes de encoding
* Tratamento de valores ausentes
* Conversão de tipos de variáveis

### 💡 Interpretação

Essa etapa é crítica, pois inconsistências nos identificadores geográficos podem comprometer completamente a análise espacial. A padronização garante que os merges posteriores sejam corretos.

---

# 🔗 4. Integração das Bases

### 📌 Objetivo

Unificar todas as bases em um único dataset analítico.

### 🔍 O que é feito:

* Junção (join) entre:

  * Dados socioeconômicos
  * Dados de violência
  * Dados populacionais
  * Dados espaciais

### 💡 Interpretação

Aqui é construída a base final que será utilizada nos modelos. Cada linha representa uma unidade espacial (ex: distrito), com todas as variáveis necessárias para análise.

---

# 🧮 5. Criação de Variáveis Derivadas

### 📌 Objetivo

Gerar novas variáveis que melhor representam o fenômeno estudado.

### 🔍 O que é feito:

* Cálculo de taxas (ex: violência por população)
* Transformações de variáveis
* Criação de indicadores compostos

### 💡 Interpretação

Essas variáveis tornam a análise mais robusta, permitindo comparabilidade entre regiões com diferentes tamanhos populacionais e características distintas.

---

# 🗺️ 6. Construção do Objeto Espacial

### 📌 Objetivo

Preparar os dados para análise espacial.

### 🔍 O que é feito:

* Conversão dos dados para objetos do tipo `sf`
* Associação entre dados tabulares e geometria
* Validação das geometrias

### 💡 Interpretação

Esse passo é essencial para habilitar operações espaciais e modelagens geograficamente ponderadas. Sem ele, não seria possível explorar a dimensão espacial do problema.

---

# 📊 7. Análise Exploratória dos Dados

### 📌 Objetivo

Compreender a distribuição e relações entre as variáveis.

### 🔍 O que é feito:

* Estatísticas descritivas
* Visualizações gráficas
* Matriz de correlação
* Identificação de padrões iniciais

### 💡 Interpretação

Essa etapa permite identificar tendências, outliers e possíveis relações entre variáveis, orientando a escolha dos modelos posteriores.

---

# ⚠️ 8. Diagnóstico de Multicolinearidade

### 📌 Objetivo

Evitar problemas de instabilidade nos modelos.

### 🔍 O que é feito:

* Cálculo do VIF (Variance Inflation Factor)
* Avaliação de correlações entre variáveis explicativas

### 💡 Interpretação

Variáveis altamente correlacionadas podem distorcer os coeficientes dos modelos. Essa etapa garante maior confiabilidade nas estimativas.

---

# 📈 9. Ajuste de Modelos Globais

### 📌 Objetivo

Estabelecer uma referência inicial para análise.

### 🔍 O que é feito:

* Ajuste de modelos:

  * Poisson
  * Binomial Negativa
* Avaliação de:

  * Significância dos coeficientes
  * Qualidade do ajuste

### 💡 Interpretação

Os modelos globais assumem que as relações são constantes no espaço. Eles servem como baseline para comparação com modelos espaciais mais sofisticados.

---

# ⚖️ 10. Avaliação de Sobredispersão

### 📌 Objetivo

Verificar adequação do modelo de Poisson.

### 🔍 O que é feito:

* Comparação entre média e variância
* Testes estatísticos para sobredispersão

### 💡 Interpretação

A presença de sobredispersão justifica o uso da regressão binomial negativa, que é mais flexível para dados de contagem com variabilidade elevada.

---


# 🌍 11. Construção da Estrutura de Dependência Espacial

### 📌 Objetivo

Capturar a relação espacial entre as unidades geográficas (ex: distritos), permitindo modelar dependência espacial.

### 🔍 O que é feito:

* Definição da matriz de vizinhança espacial
* Cálculo de pesos espaciais (ex: distância ou contiguidade)
* Padronização da matriz de pesos

### 💡 Interpretação

Esse bloco define como as regiões "interagem" entre si. A escolha da estrutura de vizinhança é crucial, pois influencia diretamente os resultados dos modelos espaciais.

Uma matriz mal especificada pode gerar:

* Falsos padrões espaciais
* Estimativas enviesadas

---

# 📡 12. Testes de Dependência Espacial

### 📌 Objetivo

Verificar se existe autocorrelação espacial nos dados.

### 🔍 O que é feito:

* Cálculo de estatísticas como Moran’s I
* Testes de significância

### 💡 Interpretação

Se houver autocorrelação espacial significativa, isso indica que:

* Regiões próximas apresentam comportamentos semelhantes
* Modelos tradicionais (independentes) são inadequados

Esse resultado justifica o uso de modelos espaciais como o GWR.

---

# 📍 13. Seleção de Bandwidth

### 📌 Objetivo

Determinar o grau de influência espacial no modelo local.

### 🔍 O que é feito:

* Seleção automática de bandwidth
* Uso de critérios como AIC ou validação cruzada

### 💡 Interpretação

O bandwidth controla o equilíbrio entre:

* 🔵 Modelos muito locais (alta variabilidade)
* 🔴 Modelos muito suaves (perda de informação local)

Esse é um dos parâmetros mais importantes do GWR.

---

# 🧠 14. Ajuste do Modelo Geograficamente Ponderado (GWR / GWZINBR)

### 📌 Objetivo

Modelar relações espaciais variáveis entre as variáveis explicativas e a variável resposta.

### 🔍 O que é feito:

* Ajuste do modelo:

  * Geographically Weighted Regression (GWR)
  * Versão com distribuição binomial negativa
  * Possivelmente versão inflacionada de zeros (GWZINBR)
* Estimação de coeficientes locais para cada unidade espacial

### 💡 Interpretação

Diferente dos modelos globais, aqui cada região possui seu próprio conjunto de coeficientes.

Isso permite identificar:

* Onde uma variável é relevante
* Onde ela não tem efeito
* Onde o efeito muda de sinal

Esse é o principal diferencial metodológico do trabalho.

---

# 📊 15. Extração dos Resultados Locais

### 📌 Objetivo

Organizar os resultados do modelo para análise e visualização.

### 🔍 O que é feito:

* Extração dos coeficientes locais
* Cálculo de estatísticas associadas
* Organização em dataframes

### 💡 Interpretação

Essa etapa transforma o output bruto do modelo em informações interpretáveis, permitindo análises posteriores e geração de mapas.

---

# 🗺️ 16. Geração de Mapas Temáticos

### 📌 Objetivo

Visualizar espacialmente os resultados obtidos.

### 🔍 O que é feito:

* Criação de mapas para:

  * Variáveis explicativas
  * Variável resposta
  * Coeficientes locais
* Uso de `ggplot2` com geometria espacial (`geom_sf`)

### 💡 Interpretação

Os mapas são fundamentais para:

* Identificar padrões espaciais
* Detectar clusters
* Comunicar resultados de forma intuitiva

Eles transformam resultados numéricos em insights visuais.

---

# 🎨 17. Personalização das Visualizações

### 📌 Objetivo

Melhorar a clareza e estética dos gráficos.

### 🔍 O que é feito:

* Ajuste de escalas de cor
* Inclusão de legendas
* Uso de rótulos e títulos descritivos

### 💡 Interpretação

Uma boa visualização não é apenas estética — ela é essencial para a interpretação correta dos resultados, especialmente em análises espaciais.

---

# 📈 18. Avaliação do Modelo Espacial

### 📌 Objetivo

Verificar a qualidade e robustez do modelo ajustado.

### 🔍 O que é feito:

* Análise de métricas como:

  * AIC
  * Deviance
* Comparação com modelos globais
* Avaliação de resíduos

### 💡 Interpretação

Essa etapa valida o ganho de performance do modelo espacial em relação aos modelos tradicionais.

Em geral, espera-se:

* Melhor ajuste
* Redução de erros
* Melhor captura de padrões locais

---

# 🔬 19. Análise dos Resíduos

### 📌 Objetivo

Verificar se o modelo capturou adequadamente a estrutura dos dados.

### 🔍 O que é feito:

* Análise da distribuição dos resíduos
* Verificação de autocorrelação residual
* Identificação de padrões não explicados

### 💡 Interpretação

Resíduos com padrão espacial indicam que ainda há estrutura não capturada pelo modelo.

Idealmente, os resíduos devem:

* Ser aleatórios
* Não apresentar autocorrelação

---

# 🧾 20. Consolidação dos Resultados

### 📌 Objetivo

Organizar os outputs finais para interpretação e apresentação.

### 🔍 O que é feito:

* Criação de tabelas resumo
* Exportação de resultados
* Preparação para inclusão em relatórios

### 💡 Interpretação

Esse bloco final transforma a análise em produto final, pronto para:

* Dissertação
* Apresentações
* Publicações

---

# 🔁 21. Reprodutibilidade da Análise

### 📌 Objetivo

Garantir que o estudo possa ser replicado.

### 🔍 O que é feito:

* Organização do código em RMarkdown
* Uso de pipeline estruturado
* Separação entre dados, código e outputs

### 💡 Interpretação

A reprodutibilidade é um dos pilares da pesquisa científica. Este projeto foi estruturado para permitir:

* Execução completa por terceiros
* Auditoria dos resultados
* Extensão da análise

---

