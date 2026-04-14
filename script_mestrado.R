# =============================================================================
# ANÁLISE ESPACIAL DE VIOLÊNCIA FÍSICA CONTRA CRIANÇAS (0–9 anos)
# Dissertação de Mestrado — Município de São Paulo (2018–2022)
# =============================================================================
# Abordagem: Regressão geográfica ponderada com resposta Binomial Negativa
# Pacote principal: gwzinbr (GWR com NB e Zero-Inflacionado)
# Autor: José Cavalcante
# Data: 2025
# =============================================================================


# =============================================================================
# 1. PACOTES
# =============================================================================

library(tidyverse)    # dplyr, ggplot2, tidyr, purrr, readr, tibble, stringr, forcats
library(readxl)       # leitura de arquivos .xlsx
library(openxlsx)     # escrita de arquivos .xlsx
library(writexl)      # alternativa para escrita .xlsx

# Mapas e análise espacial
library(sf)           # objetos espaciais (substitui sp)
library(spdep)        # dependência espacial
library(GWmodel)      # regressão geograficamente ponderada (GWR clássica)
library(mgwnbr)       # GWR com binomial negativa (versão customizada)
library(gwzinbr)      # GWR com NB / ZINB (pacote principal da análise)
library(sp)
library(ggspatial)    # elementos cartográficos (escala, norte)

# Modelagem e estatística
library(MASS)         # glm.nb (Binomial Negativa)
library(robustbase)   # boxplot robusto (adjbox)
library(car)          # VIF e testes estatísticos
library(lmtest)       # teste de razão de verossimilhança
library(broom)        # modelos em tidy data frames

# Visualização
library(corrplot)     # matriz de correlação
library(ggrepel)      # rótulos em gráficos ggplot2
library(gridExtra)    # múltiplos gráficos na mesma janela
library(scales)

# Tabelas
library(knitr)
library(kableExtra)

# Utilitários
library(stringr)
library(stringi)
library(purrr)


# =============================================================================
# 2. CAMINHOS E DIRETÓRIOS
# =============================================================================

# Defina o diretório de trabalho como a raiz do projeto antes de rodar:
#   setwd("caminho/para/aplicacao_vf")

dados_dir  <- "dados"
output_dir <- "output/plots_output"
aux_dir    <- "arquivos_auxiliares"

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


# =============================================================================
# 3. LEITURA DOS DADOS
# =============================================================================
# Pré-requisito: rodar preparar_dados.R uma vez para gerar base_mestrado.gpkg
# Se o arquivo ainda não existir, o script abaixo o cria automaticamente.

gpkg_path <- file.path(dados_dir, "base_mestrado.gpkg")

if (!file.exists(gpkg_path)) {
  message("GeoPackage não encontrado. Rodando preparar_dados.R...")
  source("preparar_dados.R")
}

dados_completo <- st_read(gpkg_path, layer = "distritos", quiet = TRUE)
dados          <- dados_completo


# =============================================================================
# 4. VISUALIZAÇÃO DA VARIÁVEL RESPOSTA
# =============================================================================

# Identifica distritos com zero casos para anotação nos mapas
dados_zero <- dados %>%
  filter(VF_2018_2022 == 0) %>%
  mutate(centroide = st_centroid(geometry),
         x = st_coordinates(centroide)[, 1],
         y = st_coordinates(centroide)[, 2])

# ---------------------------------------------------------------------------
# 4.1 Mapa coroplético — contagem de casos (2018–2022)
# ---------------------------------------------------------------------------

p_coro <- ggplot(dados) +
  geom_sf(aes(fill = VF_2018_2022)) +
  scale_fill_gradientn(
    colours = c("white", "#2171b5", "#08306b"),
    name    = "Casos notificados",
    oob     = scales::squish
  ) +
  geom_text(data  = dados_zero,
            aes(x = x, y = y),
            label = "", size = 5, color = "black") +
  theme_minimal(base_size = 14) +
  labs(fill = "Casos notificados")

ggsave(file.path(output_dir, "mapa_vf_2018_2022.png"),
       plot = p_coro, width = 9, height = 7, dpi = 150)

# ---------------------------------------------------------------------------
# 4.2 Mapa de círculos proporcionais — contagem de casos
# ---------------------------------------------------------------------------

dados_centroides <- dados %>%
  mutate(centroide = st_centroid(geometry),
         x = st_coordinates(centroide)[, 1],
         y = st_coordinates(centroide)[, 2])

min_val <- min(dados_centroides$VF_2018_2022, na.rm = TRUE)
max_val <- max(dados_centroides$VF_2018_2022, na.rm = TRUE)

p_circ <- ggplot() +
  geom_sf(data = dados, fill = "gray95", color = "grey50", size = 0.25, alpha = 0.8) +
  geom_point(data  = dados_centroides,
             aes(x = x, y = y, size = VF_2018_2022),
             color = "#2171b5", alpha = 0.8) +
  scale_size(name   = "Casos notificados",
             range  = c(0, 10),
             limits = c(min_val, max_val),
             breaks = pretty(c(min_val, max_val), n = 4)) +
  annotation_scale(location = "br", width_hint = 0.25,
                   pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
  theme_minimal() +
  theme(axis.title   = element_blank(),
        axis.text    = element_blank(),
        axis.ticks   = element_blank(),
        panel.grid   = element_line(color = "grey90", size = 0.2),
        plot.title   = element_blank())

ggsave(file.path(output_dir, "mapa_vf_circulos.png"),
       plot = p_circ, width = 9, height = 7, dpi = 150)


# =============================================================================
# 5. TAXA PADRONIZADA DE VIOLÊNCIA FÍSICA (TIP)
# =============================================================================

# ---------------------------------------------------------------------------
# 5.1 Cálculo da taxa padronizada indireta
# ---------------------------------------------------------------------------

# Taxas de referência da cidade (população total de SP como padrão)
taxas_ref <- dados_completo %>%
  summarise(
    casos_m0_4 = sum(VF_masc_0_4_anos, na.rm = TRUE),
    casos_m5_9 = sum(VF_masc_5_9_anos, na.rm = TRUE),
    casos_f0_4 = sum(VF_fem_0_4_anos,  na.rm = TRUE),
    casos_f5_9 = sum(VF_fem_5_9_anos,  na.rm = TRUE),
    pop_m0_4   = sum(meninos0a4,        na.rm = TRUE),
    pop_m5_9   = sum(meninos5a9,        na.rm = TRUE),
    pop_f0_4   = sum(meninas0a4,        na.rm = TRUE),
    pop_f5_9   = sum(meninas5a9,        na.rm = TRUE)
  ) %>%
  mutate(
    taxa_m0_4 = casos_m0_4 / pop_m0_4,
    taxa_m5_9 = casos_m5_9 / pop_m5_9,
    taxa_f0_4 = casos_f0_4 / pop_f0_4,
    taxa_f5_9 = casos_f5_9 / pop_f5_9
  )

# Número esperado de casos por distrito e TIP (Taxa de Incidência Padronizada)
dados <- dados_completo %>%
  mutate(
    casos_esp_vf     = meninos0a4 * taxas_ref$taxa_m0_4 +
                       meninos5a9 * taxas_ref$taxa_m5_9 +
                       meninas0a4 * taxas_ref$taxa_f0_4 +
                       meninas5a9 * taxas_ref$taxa_f5_9,
    taxa_pad_vf      = VF_2018_2022 / casos_esp_vf,
    log_casos_esp_vf = log(casos_esp_vf)
  )

# ---------------------------------------------------------------------------
# 5.2 Mapa coroplético — TIP
# ---------------------------------------------------------------------------

dados_proj      <- st_transform(dados,      31983)  # SIRGAS 2000 / UTM zona 23S
dados_zero_proj <- st_transform(dados_zero, 31983)

p_tip <- ggplot(dados_proj) +
  geom_sf(aes(fill = taxa_pad_vf), color = "grey70", size = 0.2) +
  scale_fill_gradientn(
    colours = c("white", "#2171b5", "#08306b"),
    limits  = c(0, 5),
    oob     = scales::squish,
    name    = NULL
  ) +
  geom_text(data  = dados_zero_proj,
            aes(x = x, y = y),
            label = "*", size = 5, color = "black") +
  annotation_scale(location = "br", width_hint = 0.25) +
  theme_minimal(base_size = 14) +
  theme(axis.title          = element_blank(),
        axis.text           = element_blank(),
        axis.ticks          = element_blank(),
        panel.grid.major    = element_line(color = "grey90", linewidth = 0.2),
        panel.grid.minor    = element_blank(),
        legend.title        = element_blank(),
        legend.key.height   = unit(1.5, "cm"),
        legend.key.width    = unit(0.4, "cm"),
        plot.title          = element_blank())

ggsave(file.path(output_dir, "mapa_taxa_padronizada_vf_2018_2022.png"),
       plot = p_tip, width = 9, height = 7, dpi = 150)

# ---------------------------------------------------------------------------
# 5.3 Medidas resumo da TIP
# ---------------------------------------------------------------------------

summary_tip <- summary(dados$taxa_pad_vf)
summary_df  <- data.frame(
  Estatística = c(names(summary_tip), "Variância"),
  Valor       = c(as.numeric(summary_tip), var(dados$taxa_pad_vf, na.rm = TRUE))
)
print(summary_df)

# ---------------------------------------------------------------------------
# 5.4 Boxplot robusto da TIP (adjbox) com rótulos nos outliers
# ---------------------------------------------------------------------------

output_file <- file.path(output_dir, "boxplot_robusto_taxa_padronizada.png")
png(filename = output_file, width = 900, height = 700, res = 150)

tip_values <- dados$taxa_pad_vf
labels     <- str_to_title(stri_trans_general(dados$ds_nome, "Latin-ASCII"))
labels     <- gsub("Sao Miguel",    "São Miguel",     labels)
labels     <- gsub("Capao Redondo", "Capão Redondo",  labels)
labels     <- gsub("Jardim Angela", "Jardim Ângela",  labels)
labels     <- gsub("Jardim São Luis","Jardim São Luís",labels)
labels     <- gsub("Consolacao",    "Consolação",     labels)

ajuste_box        <- adjbox(tip_values, main = "", ylab = "TIP",
                             col = "#2171b5", border = "#08306b",
                             range = 1.5, doReflect = TRUE,
                             outline = TRUE, notch = FALSE,
                             cex.main = 1.3, cex.lab = 0.8, cex.axis = 1.2)

outliers_ajustados <- ajuste_box$out
if (length(outliers_ajustados) > 0) {
  outlier_indices <- which(tip_values %in% outliers_ajustados)
  y_out           <- tip_values[outlier_indices]
  x_out           <- rep(1, length(y_out))

  points(x = x_out, y = y_out, pch = 16, col = "black", cex = 0.6)

  lado_direito   <- seq(1, length(y_out), by = 2)
  lado_esquerdo  <- seq(2, length(y_out), by = 2)
  x_lab_direita  <- rep(1.05, length(lado_direito))
  x_lab_esquerda <- rep(0.95, length(lado_esquerdo))

  segments(x_out[lado_direito],  y_out[lado_direito],  x_lab_direita,  y_out[lado_direito],  col = "gray30")
  text(x_lab_direita,  y_out[lado_direito],
       labels = labels[outlier_indices[lado_direito]],  pos = 4, col = "black", cex = 0.8)

  segments(x_out[lado_esquerdo], y_out[lado_esquerdo], x_lab_esquerda, y_out[lado_esquerdo], col = "gray30")
  text(x_lab_esquerda, y_out[lado_esquerdo],
       labels = labels[outlier_indices[lado_esquerdo]], pos = 2, col = "black", cex = 0.8)
}
dev.off()

# ---------------------------------------------------------------------------
# 5.5 Índice de Moran global — TIP
# ---------------------------------------------------------------------------

vizinhos_tip          <- poly2nb(dados)
pesos_tip             <- nb2listw(vizinhos_tip, style = "W")
resultado_monte_carlo <- moran.mc(dados$taxa_pad_vf, listw = pesos_tip, nsim = 999)

tabela_moran <- data.frame(
  Estatística = c("Índice de Moran (I)", "Valor-p"),
  Valor       = c(round(resultado_monte_carlo$statistic, 5),
                  resultado_monte_carlo$p.value)
)
print(tabela_moran)
plot(resultado_monte_carlo)


# =============================================================================
# 6. TRANSFORMAÇÃO DAS VARIÁVEIS EXPLICATIVAS
# =============================================================================

# Padronização de renda (0–100)
dados <- dados %>%
  mutate(rendap = 100 * (media_renda_dist - min(media_renda_dist, na.rm = TRUE)) /
                        (max(media_renda_dist, na.rm = TRUE) - min(media_renda_dist, na.rm = TRUE)))

# Densidade de igrejas e bares por mil habitantes (+ escala 0–100)
dados$igreja_mil_hab <- dados$igrejas / (dados$populacao_total / 1000)
dados <- dados %>%
  mutate(igrejap = 100 * (igreja_mil_hab - min(igreja_mil_hab, na.rm = TRUE)) /
                         (max(igreja_mil_hab, na.rm = TRUE) - min(igreja_mil_hab, na.rm = TRUE)))

dados$bares_mil_hab <- dados$bares / (dados$populacao_total / 1000)
dados <- dados %>%
  mutate(baresp = 100 * (bares_mil_hab - min(bares_mil_hab, na.rm = TRUE)) /
                        (max(bares_mil_hab, na.rm = TRUE) - min(bares_mil_hab, na.rm = TRUE)))

# Exportar base final
write_xlsx(dados, path = file.path(dados_dir, "base_final.xlsx"))


# =============================================================================
# 7. ANÁLISE DESCRITIVA DAS VARIÁVEIS EXPLICATIVAS
# =============================================================================

variaveis_descr <- c(
  "alfab", "ate1mora", "mais5", "comodo",
  "ambos", "plmenos1", "composta", "estend", "nuclear", "mulher_resp",
  "banheiro1", "rendap", "igreja_mil_hab", "bares_mil_hab"
)

# ---------------------------------------------------------------------------
# 7.1 Mapas de distribuição espacial (escala 0–100)
# ---------------------------------------------------------------------------

variaveis_mapa1 <- c(
  "alfab", "ate1mora", "mais5", "comodo",
  "ambos", "plmenos1", "composta", "estend", "nuclear", "mulher_resp",
  "banheiro1", "rendap"
)

map2(variaveis_mapa1, variaveis_mapa1, function(var, label) {
  p <- ggplot(dados) +
    geom_sf(aes(fill = .data[[var]])) +
    scale_fill_gradientn(
      colors = c("#deebf7", "#9ecae1", "#3182bd", "#08519c"),
      limits = c(0, 100), oob = scales::squish
    ) +
    annotation_scale(location = "br", width_hint = 0.25,
                     pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
    theme_minimal() +
    theme(panel.grid    = element_line(color = "grey90", size = 0.2),
          legend.title  = element_blank(),
          plot.title    = element_text(hjust = 0.5, size = 14))

  ggsave(file.path(output_dir, paste0("mapa_", var, ".png")),
         plot = p, width = 10, height = 7.5, dpi = 300)
  return(p)
})

# Mapas de densidade (igrejas e bares) — escala 0–12
variaveis_mapa2 <- c("igreja_mil_hab", "bares_mil_hab")

map2(variaveis_mapa2, variaveis_mapa2, function(var, label) {
  p <- ggplot(dados) +
    geom_sf(aes(fill = .data[[var]])) +
    scale_fill_gradientn(
      colors = c("#deebf7", "#9ecae1", "#3182bd", "#08519c"),
      limits = c(0, 12), oob = scales::squish
    ) +
    annotation_scale(location = "br", width_hint = 0.25,
                     pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
    theme_minimal() +
    theme(panel.grid   = element_line(color = "grey90", size = 0.2),
          legend.title = element_blank())

  ggsave(file.path(output_dir, paste0("mapa_", var, ".png")),
         plot = p, width = 10, height = 7.5, dpi = 300)
  return(p)
})

# ---------------------------------------------------------------------------
# 7.2 Medidas resumo
# ---------------------------------------------------------------------------

dados_long <- st_drop_geometry(dados) %>%
  dplyr::select(all_of(variaveis_descr)) %>%
  pivot_longer(cols = everything(), names_to = "Variável", values_to = "Valor")

estatisticas <- dados_long %>%
  group_by(Variável) %>%
  summarise(
    Média          = mean(Valor,                 na.rm = TRUE),
    `Desvio-padrão`= sd(Valor,                  na.rm = TRUE),
    Mínimo         = min(Valor,                  na.rm = TRUE),
    `1º Quartil`   = quantile(Valor, 0.25,       na.rm = TRUE),
    Mediana        = median(Valor,               na.rm = TRUE),
    `3º Quartil`   = quantile(Valor, 0.75,       na.rm = TRUE),
    Máximo         = max(Valor,                  na.rm = TRUE),
    .groups        = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

print(estatisticas)

# ---------------------------------------------------------------------------
# 7.3 Boxplots robustos por grupo de variáveis
# ---------------------------------------------------------------------------

dados_sem_geo <- st_drop_geometry(dados)

grupo1 <- c("alfab", "ate1mora", "mais5", "mulher_resp", "rendap")
grupo2 <- c("banheiro1", "comodo")
grupo3 <- c("ambos", "plmenos1", "composta", "estend", "nuclear")
grupo4 <- c("igreja_mil_hab", "bares_mil_hab")

vars_123   <- c(grupo1, grupo2, grupo3)
range_123  <- range(dados_sem_geo[, vars_123], na.rm = TRUE)
ylim_padrao <- c(floor(range_123[1]), ceiling(range_123[2]))

range_4 <- range(dados_sem_geo[, grupo4], na.rm = TRUE)
ymax4   <- ceiling(range_4[2])

# Estatísticas do adjbox
robust_boxplot_stats <- function(x) {
  ajuste <- adjbox(x, plot = FALSE)
  list(min = ajuste$stats[1], q1 = ajuste$stats[2],
       median = ajuste$stats[3], q3 = ajuste$stats[4],
       max = ajuste$stats[5], outliers = ajuste$out)
}

# Função genérica para criar e salvar um boxplot robusto por grupo
cria_boxplot_grupo <- function(vars, labels, ylim, nome_arquivo, ylab = "Porcentagem") {
  box_stats <- lapply(vars, function(var) robust_boxplot_stats(dados_sem_geo[[var]]))

  png(filename = file.path(output_dir, nome_arquivo), width = 800, height = 900, res = 150)
  par(mar = c(8, 5, 4, 2) + 0.1)
  plot(NA, xlim = c(0.5, length(labels) + 0.5), ylim = ylim,
       xaxt = "n", xlab = "", ylab = ylab, main = "")

  for (i in seq_along(labels)) {
    stat <- box_stats[[i]]
    rect(i - 0.3, stat$q1, i + 0.3, stat$q3, col = "#2171b5", border = "#08306b")
    segments(i - 0.3, stat$median, i + 0.3, stat$median, lwd = 2, col = "#08306b")
    segments(i, stat$min, i, stat$q1, lwd = 1.5, col = "#08306b")
    segments(i, stat$q3, i, stat$max, lwd = 1.5, col = "#08306b")
    segments(i - 0.3, stat$min, i + 0.3, stat$min, lwd = 1.5, col = "#08306b")
    segments(i - 0.3, stat$max, i + 0.3, stat$max, lwd = 1.5, col = "#08306b")
    if (length(stat$outliers) > 0) {
      y_offsets <- seq(-0.1, 0.1, length.out = length(stat$outliers))
      points(rep(i, length(stat$outliers)), stat$outliers + y_offsets,
             col = "black", pch = 16)
    }
  }
  axis(1, at = seq_along(labels), labels = labels, las = 2)
  dev.off()
}

cria_boxplot_grupo(grupo1, grupo1, ylim_padrao, "boxplot_robusto_grupo1.png")
cria_boxplot_grupo(grupo2, grupo2, ylim_padrao, "boxplot_robusto_grupo2.png")
cria_boxplot_grupo(grupo3, grupo3, ylim_padrao, "boxplot_robusto_grupo3.png")
cria_boxplot_grupo(grupo4, c("igrejas", "bares"), c(0, ymax4),
                   "boxplot_robusto_grupo4.png", ylab = "Densidade por mil habitantes")

# ---------------------------------------------------------------------------
# 7.4 Correlação das variáveis explicativas com a resposta (Pearson)
# ---------------------------------------------------------------------------

variaveis_cor <- c(
  "alfab", "ate1mora", "mais5", "comodo",
  "ambos", "plmenos1", "composta", "estend", "nuclear", "mulher_resp",
  "banheiro1", "rede_geral", "rendap", "igreja_mil_hab", "bares_mil_hab"
)
labels_cor <- c(
  "Alfabetiz.", "1 Morador", "5+ Moradores", "Hab. Coletiva",
  "Casal/Filh.", "Casal/Resp.", "Composta", "Estendida", "Nuclear",
  "Resp. Mulher", "Banheiro", "Rede Água", "Renda (%)",
  "Igrejas/1000 hab.", "Bares/1000 hab."
)

cor_tab <- lapply(seq_along(variaveis_cor), function(i) {
  teste <- cor.test(dados$taxa_pad_vf, dados[[variaveis_cor[i]]], method = "pearson")
  data.frame(Variável  = labels_cor[i],
             Pearson   = round(teste$estimate, 3),
             `Valor p` = formatC(teste$p.value, format = "e", digits = 2))
}) %>% bind_rows()

print(cor_tab)

# ---------------------------------------------------------------------------
# 7.5 Matriz de correlação entre variáveis explicativas (corrplot)
# ---------------------------------------------------------------------------

variaveis_corr <- c(
  "alfab", "ate1mora", "mais5", "comodo",
  "ambos", "plmenos1", "composta", "estend", "nuclear", "mulher_resp",
  "banheiro1", "rendap", "igreja_mil_hab", "bares_mil_hab"
)
labels_corr <- c(
  "alfab", "ate1mora", "mais5", "comodo",
  "ambos", "plmenos1", "composta", "estend", "nuclear", "mulher_resp",
  "banheiro1", "rendap", "igrejas", "bares"
)

dados_correlacao  <- st_drop_geometry(dados) %>% dplyr::select(all_of(variaveis_corr))
matriz_correlacao <- cor(dados_correlacao, use = "complete.obs")
colnames(matriz_correlacao) <- rownames(matriz_correlacao) <- labels_corr

png(file.path(output_dir, "matriz_correlacao_variaveis.png"),
    width = 1800, height = 1500, res = 180)
corrplot(matriz_correlacao,
         method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, tl.cex = 0.9, number.cex = 0.7,
         col = colorRampPalette(c("#08306b", "white", "#b30000"))(200),
         mar = c(0, 0, 2, 0))
dev.off()

# ---------------------------------------------------------------------------
# 7.6 Dendrograma de correlação (Ward)
# ---------------------------------------------------------------------------

dados_dend <- st_drop_geometry(dados) %>%
  dplyr::select(intersect(names(dados), variaveis_corr))

cor_pearson   <- cor(dados_dend, method = "pearson", use = "complete.obs")
dissimilarity <- 1 - abs(cor_pearson)
distance      <- as.dist(dissimilarity)

rotulos_claros <- c(
  "alfab" = "alfab", "ate1mora" = "ate1mora", "mais5" = "mais5",
  "comodo" = "comodo", "ambos" = "ambos", "plmenos1" = "plmenos1",
  "composta" = "composta", "estend" = "estend", "nuclear" = "nuclear",
  "mulher_resp" = "mulher_resp", "banheiro1" = "banheiro1",
  "rendap" = "rendap", "igreja_mil_hab" = "igrejas", "bares_mil_hab" = "bares"
)

colnames(cor_pearson) <- rownames(cor_pearson) <- rotulos_claros[colnames(cor_pearson)]
attr(distance, "Labels") <- rotulos_claros[attr(distance, "Labels")]

png(file.path(output_dir, "dendrograma_correlacao.png"),
    width = 900, height = 700, res = 150)
plot(hclust(distance, method = "ward.D"),
     main = "", xlab = "", cex.main = 1.4, cex.lab = 1.2, cex.axis = 0.9)
dev.off()


# =============================================================================
# 8. MODELO DE POISSON (global)
# =============================================================================

dados_modelo <- st_drop_geometry(dados)

modelo_pois <- glm(
  VF_2018_2022 ~ comodo + composta + mais5 + mulher_resp + igreja_mil_hab +
    offset(log_casos_esp_vf),
  data   = dados_modelo,
  family = poisson()
)

# Métricas de ajuste
log_verossim         <- as.numeric(logLik(modelo_pois))
modelo_nulo_pois     <- update(modelo_pois, . ~ 1)
log_verossim_nulo    <- as.numeric(logLik(modelo_nulo_pois))
pseudo_r2_pois       <- 1 - (log_verossim / log_verossim_nulo)
n_pois               <- nobs(modelo_pois)
k_pois               <- length(coef(modelo_pois))
aicc_pois            <- AIC(modelo_pois) + (2 * k_pois * (k_pois + 1)) / (n_pois - k_pois - 1)

metricas_pois <- data.frame(
  Estatística = c("Log-verossimilhança", "Pseudo R² (McFadden)",
                  "Desvio Residual", "Graus de Liberdade", "AICc"),
  Valor       = c(log_verossim, pseudo_r2_pois,
                  deviance(modelo_pois), df.residual(modelo_pois), aicc_pois)
)
print(metricas_pois)

# Mapa dos resíduos (deviance)
dados$residuos_pois <- residuals(modelo_pois, type = "deviance")

p_res_pois <- ggplot(dados) +
  geom_sf(aes(fill = residuos_pois)) +
  scale_fill_gradient2(
    midpoint = median(dados$residuos_pois, na.rm = TRUE),
    low = "blue", mid = "white", high = "red", name = "Resíduo"
  ) +
  theme_minimal() +
  labs(title = "Resíduos por Distrito — Modelo de Poisson")

ggsave(file.path(output_dir, "mapa_residuos_poisson.png"),
       plot = p_res_pois, width = 8, height = 6, dpi = 300, bg = "white")

# Envelope simulado (script auxiliar)
png(file.path(output_dir, "envelope_poisson.png"), width = 800, height = 800, res = 150)
fit.model <- modelo_pois
attach(dados_modelo)
source(file.path(aux_dir, "envel_pois"))
dev.off()
detach(dados_modelo)


# =============================================================================
# 9. MODELO BINOMIAL NEGATIVA (global)
# =============================================================================

dados_modelo <- st_drop_geometry(dados)

modelo_nb <- glm.nb(
  VF_2018_2022 ~ comodo + composta + mais5 + mulher_resp + igreja_mil_hab +
    offset(log_casos_esp_vf),
  data = dados_modelo
)

# Coeficientes
coef_nb <- summary(modelo_nb)$coefficients
coef_table_nb <- data.frame(
  Variável     = rownames(coef_nb),
  Coeficiente  = round(coef_nb[, "Estimate"],    4),
  `Erro Padrão`= round(coef_nb[, "Std. Error"],  4),
  Estatística  = round(coef_nb[, "z value"],     4),
  `Valor-p`    = formatC(coef_nb[, "Pr(>|z|)"], format = "e", digits = 3)
)
print(coef_table_nb)

# Theta (parâmetro de superdispersão)
cat("Theta:", round(modelo_nb$theta, 4), "\n")

# Teste de razão de verossimilhança (Poisson vs NB)
resultado_lrt <- lrtest(modelo_pois, modelo_nb)
print(resultado_lrt)

# Métricas de ajuste
loglik_nb      <- as.numeric(logLik(modelo_nb))
loglik_nulo_nb <- as.numeric(logLik(update(modelo_nb, . ~ 1)))
n_nb           <- nobs(modelo_nb)
k_nb           <- length(coef(modelo_nb))
aicc_nb        <- AIC(modelo_nb) + (2 * k_nb * (k_nb + 1)) / (n_nb - k_nb - 1)

metricas_nb <- data.frame(
  Estatística = c("Log-verossimilhança", "Pseudo R² (McFadden)",
                  "Desvio Residual", "AIC", "AICc"),
  Valor       = round(c(loglik_nb,
                        1 - (loglik_nb / loglik_nulo_nb),
                        sqrt(deviance(modelo_nb) / df.residual(modelo_nb)),
                        AIC(modelo_nb), aicc_nb), 4)
)
print(metricas_nb)

# Envelope simulado — Binomial Negativa
png(file.path(output_dir, "envelope_negbin.png"), width = 800, height = 800, res = 150)
fit.model <- modelo_nb
attach(dados_modelo)
source(file.path(aux_dir, "envel_nbin"))
dev.off()
detach(dados_modelo)

# Mapa dos resíduos (deviance)
dados$residuos <- residuals(modelo_nb, type = "deviance")

p_res_nb <- ggplot(dados) +
  geom_sf(aes(fill = residuos)) +
  scale_fill_gradient2(
    midpoint = median(dados$residuos, na.rm = TRUE),
    low = "blue", mid = "white", high = "red", name = NULL
  ) +
  annotation_scale(location = "br", width_hint = 0.25,
                   pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.grid = element_line(color = "grey90", size = 0.2))

ggsave(file.path(output_dir, "mapa_residuos_negbin.png"),
       plot = p_res_nb, width = 8, height = 6, dpi = 300, bg = "white")

# Índice de Moran dos resíduos (NB global)
coordenadas_nb   <- cbind(dados$long_utm, dados$lat_utm)
vizinhanca_nb    <- knn2nb(knearneigh(coordenadas_nb, k = 4))
lista_pesos_nb   <- nb2listw(vizinhanca_nb, style = "W")
moran_nb         <- moran.mc(dados$residuos, listw = lista_pesos_nb, nsim = 999)

cat("Moran I (NB):", round(moran_nb$statistic, 4),
    "  p-valor:", round(moran_nb$p.value, 4), "\n")

# Excesso de zeros
mu_hat         <- predict(modelo_nb, type = "response")
theta_nb       <- modelo_nb$theta
expected_zeros <- sum((theta_nb / (theta_nb + mu_hat))^theta_nb)
observed_zeros <- sum(dados_modelo$VF_2018_2022 == 0)

cat("Zeros observados:", observed_zeros,
    "  Zeros esperados (NB):", round(expected_zeros, 2), "\n")


# =============================================================================
# 10. ANÁLISE DE AUTOCORRELAÇÃO LOCAL — LISA
# =============================================================================

# LISA sobre a contagem de casos
vizinhos_lisa  <- poly2nb(dados)
lista_pesos_li <- nb2listw(vizinhos_lisa, style = "W")
lisa_count     <- localmoran(dados$VF_2018_2022, lista_pesos_li)

dados$lisa_I      <- lisa_count[, 1]
dados$lisa_pvalue <- lisa_count[, 5]
media_count       <- mean(dados$VF_2018_2022, na.rm = TRUE)

classifica_lisa <- function(x, lisa_I, pval, media, alpha = 0.1) {
  if (pval > alpha)              return("Não significativo")
  if (x >= media && lisa_I > 0) return("High-High")
  if (x  < media && lisa_I > 0) return("Low-Low")
  if (x >= media && lisa_I < 0) return("High-Low")
  if (x  < media && lisa_I < 0) return("Low-High")
  return("Não classificado")
}

dados$lisa_grupo <- mapply(classifica_lisa, dados$VF_2018_2022,
                           dados$lisa_I, dados$lisa_pvalue,
                           MoreArgs = list(media = media_count, alpha = 0.1))

cores_lisa <- c("High-High" = "#b2182b", "Low-Low" = "#2166ac",
                "High-Low"  = "#fddb6d", "Low-High" = "#44ba4a",
                "Não significativo" = "#f7f7f7")

mapa_lisa_count <- ggplot(dados) +
  geom_sf(aes(fill = lisa_grupo), color = "#cccccc", linewidth = 0.3) +
  scale_fill_manual(values = cores_lisa, name = "Grupo LISA") +
  theme_minimal()

ggsave(file.path(output_dir, "mapa_lisa_contagem.png"),
       plot = mapa_lisa_count, width = 8, height = 6, dpi = 300)

# LISA sobre a TIP
lisa_tip   <- localmoran(dados$taxa_pad_vf, lista_pesos_li)
dados$lisa_I      <- lisa_tip[, 1]
dados$lisa_pvalue <- lisa_tip[, 5]
media_tip         <- mean(dados$taxa_pad_vf, na.rm = TRUE)

dados$lisa_grupo <- mapply(classifica_lisa, dados$taxa_pad_vf,
                           dados$lisa_I, dados$lisa_pvalue,
                           MoreArgs = list(media = media_tip, alpha = 0.1))

mapa_lisa_tip <- ggplot(dados) +
  geom_sf(aes(fill = lisa_grupo), color = "#cccccc", linewidth = 0.3) +
  scale_fill_manual(values = cores_lisa, name = "Grupo LISA") +
  theme_minimal()

ggsave(file.path(output_dir, "mapa_lisa_tx_pad.png"),
       plot = mapa_lisa_tip, width = 8, height = 6, dpi = 300)


# =============================================================================
# 11. GWR — BINOMIAL NEGATIVA GEOGRAFICAMENTE PONDERADA (gwzinbr)
# =============================================================================

# ---------------------------------------------------------------------------
# 11.1 Preparação dos dados para o modelo GWR
# ---------------------------------------------------------------------------

dados_modelo_bn <- as.data.frame(dados)
for (var in c("comodo", "plmenos1", "composta", "mais5",
              "mulher_resp", "degradada", "igrejas")) {
  dados_modelo_bn[[var]] <- as.numeric(scale(dados_modelo_bn[[var]]))
}

# ---------------------------------------------------------------------------
# 11.2 Seleção da largura de banda — cenário com igrejas
# ---------------------------------------------------------------------------

formula_com_igrejas <- VF_2018_2022 ~ comodo + composta + mais5 + mulher_resp + igrejas

gss_adaptativo_aic_igrejas <- Golden(
  data = dados_modelo_bn, formula = formula_com_igrejas,
  lat = "lat_utm", long = "long_utm", offset = "log_casos_esp_vf",
  model = "negbin", method = "adaptive_bsq", bandwidth = "aic",
  globalmin = TRUE, distancekm = FALSE, force = TRUE
)

gss_adaptativo_cv_igrejas <- Golden(
  data = dados_modelo_bn, formula = formula_com_igrejas,
  lat = "lat_utm", long = "long_utm", offset = "log_casos_esp_vf",
  model = "negbin", method = "adaptive_bsq", bandwidth = "cv",
  globalmin = TRUE, distancekm = FALSE, force = TRUE
)

gss_fix_aic_igrejas <- Golden(
  data = dados_modelo_bn, formula = formula_com_igrejas,
  lat = "lat_utm", long = "long_utm", offset = "log_casos_esp_vf",
  model = "negbin", method = "fixed_g", bandwidth = "aic",
  globalmin = TRUE, distancekm = FALSE, force = TRUE
)

gss_fix_cv_igrejas <- Golden(
  data = dados_modelo_bn, formula = formula_com_igrejas,
  lat = "lat_utm", long = "long_utm", offset = "log_casos_esp_vf",
  model = "negbin", method = "fixed_g", bandwidth = "cv",
  globalmin = TRUE, distancekm = FALSE, force = TRUE
)

# ---------------------------------------------------------------------------
# 11.3 Seleção da largura de banda — cenário sem igrejas
# ---------------------------------------------------------------------------

formula_sem_igrejas <- VF_2018_2022 ~ comodo + composta + mais5 + mulher_resp

gss_adaptativo_aic <- Golden(
  data = dados_modelo_bn, formula = formula_sem_igrejas,
  lat = "lat_utm", long = "long_utm", offset = "log_casos_esp_vf",
  model = "negbin", method = "adaptive_bsq", bandwidth = "aic",
  globalmin = TRUE, distancekm = FALSE, force = TRUE
)

gss_adaptativo_cv <- Golden(
  data = dados_modelo_bn, formula = formula_sem_igrejas,
  lat = "lat_utm", long = "long_utm", offset = "log_casos_esp_vf",
  model = "negbin", method = "adaptive_bsq", bandwidth = "cv",
  globalmin = TRUE, distancekm = FALSE, force = TRUE
)

gss_fix_aic <- Golden(
  data = dados_modelo_bn, formula = formula_sem_igrejas,
  lat = "lat_utm", long = "long_utm", offset = "log_casos_esp_vf",
  model = "negbin", method = "fixed_g", bandwidth = "aic",
  globalmin = TRUE, distancekm = FALSE, force = TRUE
)

gss_fix_cv <- Golden(
  data = dados_modelo_bn, formula = formula_sem_igrejas,
  lat = "lat_utm", long = "long_utm", offset = "log_casos_esp_vf",
  model = "negbin", method = "fixed_g", bandwidth = "cv",
  globalmin = TRUE, distancekm = FALSE, force = TRUE
)

# ---------------------------------------------------------------------------
# 11.4 Tabela-resumo das larguras de banda selecionadas
# ---------------------------------------------------------------------------

resumo_bandwidth <- tibble::tibble(
  cenario  = c(rep("Com igrejas", 4), rep("Sem igrejas", 4)),
  metodo   = rep(c("Adaptativa", "Adaptativa", "Fixa", "Fixa"), 2),
  criterio = rep(c("CV", "AIC", "CV", "AIC"), 2),
  h_min    = c(
    gss_adaptativo_cv_igrejas$min_bandwidth[1],
    gss_adaptativo_aic_igrejas$min_bandwidth[1],
    gss_fix_cv_igrejas$min_bandwidth[1],
    gss_fix_aic_igrejas$min_bandwidth[1],
    gss_adaptativo_cv$min_bandwidth[1],
    gss_adaptativo_aic$min_bandwidth[1],
    gss_fix_cv$min_bandwidth[1],
    gss_fix_aic$min_bandwidth[1]
  )
)
print(resumo_bandwidth)

# Gráfico de convergência — banda adaptativa AIC (sem igrejas)
iterations      <- gss_adaptativo_aic$iterations
iterations_long <- data.frame(
  iter = rep(iterations$GSS_count, 2),
  h    = c(iterations$h1, iterations$h2),
  cv   = c(iterations$cv1, iterations$cv2)
)

grafico_banda <- ggplot(iterations_long, aes(x = h, y = cv)) +
  geom_point() + geom_line() +
  labs(x = "Largura de banda (h)", y = "AIC") +
  theme_minimal()

ggsave(file.path(output_dir, "grafico_banda_adaptativa_aic.png"),
       plot = grafico_banda, width = 8, height = 6, dpi = 300)

# ---------------------------------------------------------------------------
# 11.5 Ajuste do modelo GWR-NB final
# ---------------------------------------------------------------------------

modelo_negbin <- gwzinbr(
  data    = dados_modelo_bn,
  formula = VF_2018_2022 ~ comodo + composta + mais5 + mulher_resp + igreja_mil_hab,
  lat     = "lat_utm", long = "long_utm",
  offset  = "log_casos_esp_vf",
  method  = "adaptive_bsq",
  model   = "negbin",
  force   = TRUE, distancekm = FALSE,
  h       = 12
)

# Métricas de ajuste do modelo GWR-NB
measures <- modelo_negbin$measures
tabela_ajuste_gwr <- data.frame(
  Estatística = c("Deviance", "AIC", "AICc", "Pseudo R²", "Pseudo R² ajustado"),
  Valor       = round(c(measures["deviance"], measures["AIC"], measures["AICc"],
                        measures["pct_ll"],   measures["adj_pct_ll"]), 3)
)
print(tabela_ajuste_gwr)

# ---------------------------------------------------------------------------
# 11.6 Mapas dos parâmetros estimados — GWR-NB
# ---------------------------------------------------------------------------

param_df   <- as.data.frame(modelo_negbin$parameter_estimates)
parametros <- c("Intercept", "comodo", "composta", "mais5", "mulher_resp")

labels_parametros <- c(
  "Intercept"    = "Intercepto",
  "comodo"       = "Hab. Coletiva",
  "composta"     = "Composta",
  "mais5"        = "5+ Moradores",
  "mulher_resp"  = "Mulher Resp.",
  "alpha"        = "Alpha (Superdispersão)"
)

# Adiciona estimativas e significâncias ao objeto dados
for (param in parametros) {
  dados[[paste0("param_", param)]] <- as.numeric(param_df[[param]])
  dados[[paste0("sig_",   param)]] <- param_df[[paste0("sig_", param)]]
}
alpha_df       <- as.data.frame(modelo_negbin$alpha_estimates)
dados$param_alpha <- as.numeric(alpha_df$alpha)
dados$sig_alpha   <- alpha_df$sig_alpha

# Traduz categorias de significância para português (2 níveis)
traduz_significancia <- function(sig) {
  dplyr::case_when(
    grepl("not significant at 90%", sig) ~ "não significativo a 10%",
    TRUE                                 ~ "significativo a 10%"
  )
}
for (param in parametros) {
  dados[[paste0("sig_", param)]] <- traduz_significancia(dados[[paste0("sig_", param)]])
}
dados$sig_alpha <- traduz_significancia(dados$sig_alpha)

# Gera e salva os mapas: apenas distritos com coeficiente significativo recebem cor
walk(c(parametros, "alpha"), function(param) {
  var_est  <- paste0("param_", param)
  var_sig  <- paste0("sig_",   param)

  dados$cor_fill <- ifelse(
    dados[[var_sig]] == "significativo a 10%",
    dados[[var_est]], NA
  )

  p <- ggplot(dados) +
    geom_sf(aes(fill = cor_fill), color = "#f7f7f7", size = 0.39) +
    scale_fill_gradient2(
      low = "#2166ac", high = "#b2182b", midpoint = 0,
      name = NULL, na.value = "grey90"
    ) +
    annotation_scale(location = "br", width_hint = 0.25,
                     pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
    theme_minimal() +
    theme(panel.grid    = element_line(color = "grey90", size = 0.2),
          legend.title  = element_blank(),
          plot.title    = element_blank())

  ggsave(file.path(output_dir, paste0("mapa_param_gwr_bn_", param, ".png")),
         plot = p, width = 10, height = 7.5, dpi = 300, bg = "white")
  print(p)
})

# ---------------------------------------------------------------------------
# 11.7 Índice de Moran dos resíduos — GWR-NB
# ---------------------------------------------------------------------------

residuos_negbin  <- modelo_negbin$residuals[, 5]
dados$residuos   <- as.numeric(residuos_negbin)

coordenadas_gwr  <- cbind(dados_modelo$long_utm, dados_modelo$lat_utm)
vizinhanca_gwr   <- knn2nb(knearneigh(coordenadas_gwr, k = 4))
lista_pesos_gwr  <- nb2listw(vizinhanca_gwr, style = "W")
moran_gwr        <- moran.mc(residuos_negbin, listw = lista_pesos_gwr, nsim = 999)

cat("Moran I (GWR-NB):", round(moran_gwr$statistic, 4),
    "  p-valor:", round(moran_gwr$p.value, 4), "\n")

# Mapa dos resíduos do GWR-NB
p_res_gwr <- ggplot(dados) +
  geom_sf(aes(fill = residuos)) +
  scale_fill_gradient2(
    midpoint = median(dados$residuos, na.rm = TRUE),
    low = "blue", mid = "white", high = "red", name = NULL
  ) +
  annotation_scale(location = "br", width_hint = 0.25,
                   pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
  theme_minimal() +
  theme(plot.title = element_blank(),
        panel.grid = element_line(color = "grey90", size = 0.2))

ggsave(file.path(output_dir, "mapa_residuos_gwr_negbin.png"),
       plot = p_res_gwr, width = 8, height = 6, dpi = 300, bg = "white")


# =============================================================================
# FIM DO SCRIPT
# =============================================================================
