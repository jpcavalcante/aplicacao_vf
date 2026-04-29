# =============================================================================
# PREPARAÇÃO E CONSOLIDAÇÃO DOS DADOS — EXECUTAR UMA VEZ
# =============================================================================
# Este script lê todos os arquivos brutos da pasta dados/, realiza os merges
# e salva um único arquivo GeoPackage (base_mestrado.gpkg) que contém:
#   - Layer "distritos": geometria dos 96 distritos + todas as variáveis
#
# Após rodar este script, o script_mestrado.R usa apenas st_read() para
# carregar tudo de uma vez.
#
# Uso:
#   setwd("caminho/para/aplicacao_vf")  # pasta raiz do projeto
#   source("preparar_dados.R")
# =============================================================================

library(sf)
library(tidyverse)
library(readxl)
library(openxlsx)
library(stringr)
library(stringi)

# Diretório de dados (relativo à raiz do projeto)
dados_dir <- "~/Documents/aplicacao_vf_mestrado/aplicacao_vf/dados/"

# -----------------------------------------------------------------------------
# 1. Shapefile dos distritos (geometria base)
# -----------------------------------------------------------------------------
distritos <- st_read(
  file.path(dados_dir, "Distr_Adm_Munic_SP/Distr_Adm_Munic_SP.shp"),
  quiet = TRUE
)

# -----------------------------------------------------------------------------
# 2. Arquivos tabulares
# -----------------------------------------------------------------------------

# Casos de violência física/sexual 2018–2022 por faixa etária e sexo
vf <- read.csv2(
  file.path(dados_dir, "vf_fx_eta_gen_2018_2022.csv"),
  sep = ",", check.names = FALSE
)
vf$ds_nome <- toupper(
  stringi::stri_trans_general(vf$DistrAdminist, "Latin-ASCII")
)
vf <- vf %>% dplyr::select(-DistrAdminist)

# Variáveis do Censo 2022
senso <- read_xlsx(file.path(dados_dir, "variaveis_distritosMSP.xlsx"))
senso$CD_distrito <- str_sub(senso$CD_DIST, -2)

# População total por distrito
pop_total <- read.xlsx(
  file.path(dados_dir, "dados - Sheet1.xlsx"), sheet = 1
) %>%
  dplyr::select(ds_nome, Pop) %>%
  rename(populacao_total = Pop)

# Renda média
renda <- read.xlsx(
  file.path(dados_dir, "media_renda_distritos_SP.xlsx"), sheet = 1
)
renda$ds_nome <- toupper(
  stringi::stri_trans_general(renda$NM_DIST, "Latin-ASCII")
)
renda <- renda %>% dplyr::select(-NM_DIST)

# População em risco (crianças 0–9 anos)
pop_risc <- read.xlsx(
  file.path(dados_dir, "soma_criancas_distrito.xlsx")
) %>%
  rename(pop_risc_0_9_anos = total)

# Centroides deslocados
centroide <- st_read(
  file.path(dados_dir, "novo_centr_msp_desloc/novo_centr_msp_desloc.shp"),
  quiet = TRUE
) %>% st_drop_geometry()

# Igrejas e bares por distrito
igrejas <- st_read(
  file.path(dados_dir, "bares_igrejas_por_distrito/igrejas_por_distrito.shp"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  rename(igrejas = NUMPOINTS) %>%
  dplyr::select(ds_nome, igrejas)

bares <- st_read(
  file.path(dados_dir, "bares_igrejas_por_distrito/bares_por_distrito.shp"),
  quiet = TRUE
) %>%
  st_drop_geometry() %>%
  rename(bares = NUMPOINTS) %>%
  dplyr::select(ds_nome, bares)

# -----------------------------------------------------------------------------
# 3. Merge sequencial
# -----------------------------------------------------------------------------
message("▶ Consolidando tabelas...")

tabular <- merge(igrejas,    bares,      by = "ds_nome") %>%
           merge(vf,         by = "ds_nome") %>%
           merge(pop_risc,   by = "ds_nome") %>%
           merge(senso,      by = "CD_distrito") %>%
           merge(centroide,  by = "ds_nome") %>%
           merge(pop_total,  by = "ds_nome") %>%
           merge(renda,      by = "ds_nome")

# Join com geometria (left_join preserva todos os distritos)
base_sf <- distritos %>%
  left_join(tabular, by = "ds_nome")

# -----------------------------------------------------------------------------
# 4. Salvar como GeoPackage
# -----------------------------------------------------------------------------
gpkg_path <- file.path(dados_dir, "base_mestrado.gpkg")

message("▶ Salvando GeoPackage em: ", gpkg_path)
st_write(base_sf, gpkg_path, layer = "distritos",
         driver = "GPKG", delete_dsn = TRUE, quiet = TRUE)

message("✔ base_mestrado.gpkg criado com sucesso!")
message("  Linhas: ", nrow(base_sf), " | Colunas: ", ncol(base_sf))
message("")
message("  Você pode deletar os arquivos xlsx/csv/shapefiles individuais")
message("  da pasta dados/ após verificar que o GeoPackage está completo.")

