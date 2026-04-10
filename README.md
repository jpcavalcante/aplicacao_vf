# 📊 Aplicação de Modelo Geograficamente Ponderado para Dados de Violência

## 📌 Descrição do Projeto

Este repositório apresenta a aplicação de modelos estatísticos espaciais para análise da distribuição de casos de violência (física e sexual) entre os anos de 2018 e 2022.

O objetivo principal é investigar a relação entre características socioeconômicas e a ocorrência de violência, considerando a heterogeneidade espacial por meio de modelos de regressão geograficamente ponderada.

---

## 🧠 Metodologia

A análise combina técnicas de estatística clássica e espacial, com foco em:

* Modelos de regressão para dados de contagem
* Tratamento de sobredispersão
* Análise espacial exploratória
* Modelagem local via regressão geograficamente ponderada

### 🔍 Principais abordagens:

* Regressão Binomial Negativa
* Modelos robustos
* Avaliação de multicolinearidade (VIF)
* Estrutura de dependência espacial
* Modelos GWR com distribuição binomial negativa

---

## 📂 Estrutura do Projeto

```bash
.
├── script_mestrado.Rmd        # Script principal da análise
├── /dados                     # Bases de dados utilizadas
├── /output
│   └── /plots                # Mapas e visualizações geradas
└── README.md                 # Documentação do projeto
```

---

## 📊 Base de Dados

Foram utilizadas múltiplas fontes de dados:

* Dados populacionais por distrito
* Registros de violência física e sexual (2018–2022)
* Indicadores socioeconômicos
* Dados geográficos (shapefiles)

### 🔧 Pré-processamento:

* Padronização de nomes de distritos
* Tratamento de encoding (remoção de acentos)
* Integração entre bases
* Construção de variáveis derivadas

---

## ⚙️ Pacotes Utilizados

Principais bibliotecas utilizadas na análise:

* Manipulação e visualização:

  * `tidyverse`
  * `ggplot2`
* Dados espaciais:

  * `sf`
  * `spdep`
  * `GWmodel`
  * `mgwnbr`
* Modelagem estatística:

  * `MASS`
  * `robustbase`
  * `car`
  * `broom`
* Relatórios:

  * `knitr`
  * `kableExtra`

---

## ⚙️ Como Executar o Projeto

### 1. Clonar o repositório

```bash
git clone https://github.com/seu-usuario/seu-repositorio.git
```

### 2. Abrir o arquivo no RStudio

Abra o arquivo:

```bash
script_mestrado.Rmd
```

### 3. Instalar dependências

```r
install.packages(c(
  "tidyverse", "sf", "spdep", "GWmodel", 
  "MASS", "robustbase", "car", "broom",
  "readxl", "openxlsx", "corrplot", 
  "ggrepel", "knitr", "kableExtra"
))
```

### 4. Executar a análise

Renderize o documento:

```r
rmarkdown::render("script_mestrado.Rmd")
```

---

## 📊 Resultados

A análise gera diferentes saídas, incluindo:

### 🗺️ Mapas Temáticos

* Distribuição espacial da população
* Indicadores socioeconômicos
* Taxas de violência por distrito

### 📈 Modelos Estatísticos

* Estimativas globais
* Estimativas locais (GWR)
* Avaliação de significância dos coeficientes

### 🔬 Diagnósticos

* Testes de multicolinearidade
* Ajuste dos modelos (AIC, Deviance)
* Análise de resíduos

---

## 📈 Principais Insights

* A relação entre variáveis socioeconômicas e violência varia espacialmente
* Modelos globais não capturam adequadamente essa heterogeneidade
* A abordagem geograficamente ponderada fornece estimativas mais realistas e localizadas
* Existem clusters espaciais com maior concentração de risco

---

## 🚧 Limitações

* Dependência da qualidade dos dados públicos
* Possível subnotificação de casos de violência
* Sensibilidade à escolha de bandwidth no modelo GWR

---

## 🔄 Possíveis Extensões

* Inclusão de novos anos ou variáveis
* Modelos espaço-temporais
* Comparação com modelos bayesianos
* Aplicação em outras regiões

---

## 📚 Referências

* Fotheringham, A. S., Brunsdon, C., & Charlton, M. (2002). *Geographically Weighted Regression*
* Hilbe, J. M. (2011). *Negative Binomial Regression*
* Literatura recente sobre modelos inflacionados de zeros e regressão espacial

---

## 👨‍💻 Autor

Projeto desenvolvido como parte de dissertação de mestrado em Ciência de Dados / Estatística Aplicada.

---

## 📌 Observações

Este repositório tem fins acadêmicos e pode ser utilizado como referência para estudos em:

* Modelagem espacial
* Dados de contagem
* Regressão geograficamente ponderada
* Análise aplicada a políticas públicas

---
