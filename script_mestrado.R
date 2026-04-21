# =============================================================================
# ANÁLISE ESPACIAL DE VIOLÊNCIA FÍSICA CONTRA CRIANÇAS (0–9 anos)
# Município de São Paulo · 2018–2022 · Dissertação de Mestrado
# =============================================================================
#
# OBJETIVO
#   Identificar fatores socioeconômicos e demográficos associados à incidência
#   de violência física contra crianças e investigar se esses efeitos variam
#   espacialmente entre os 96 distritos administrativos de São Paulo.
#
# ESTRATÉGIA ANALÍTICA (do global ao local)
#   1. TIP + Moran I global  → confirmar autocorrelação espacial na resposta
#   2. Modelo de Poisson     → linha de base; detectar superdispersão
#   3. Binomial Negativa     → corrigir superdispersão; verificar resíduos
#   4. LISA                  → identificar clusters locais de risco
#   5. GWR-NB (gwzinbr)     → modelar efeitos localmente variáveis
#
# DADOS
#   Entrada : dados/base_mestrado.gpkg (96 distritos, geometria + variáveis)
#   Saídas  : _cache/mod_gwr.rds — modelo GWR serializado
#
# USO
#   setwd("caminho/para/aplicacao_vf")
#   source("script_mestrado.R")
#
# Versão do gwzinbr utilizada: 0.1.0 (CRAN, publicada em 2024-06-10)
# Autores: Jéssica Vasconcelos, Juliana Rosa, Alan da Silva
# install.packages("gwzinbr")
#
# AUTOR : José Cavalcante · 2025
# =============================================================================


# =============================================================================
# 1. PACOTES
# =============================================================================

library(tidyverse)   # dplyr, ggplot2, tidyr, purrr, stringr
library(sf)          # dados vetoriais (geometria dos distritos)
library(spdep)       # poly2nb, nb2listw, moran.mc, localmoran
library(ggspatial)   # barra de escala nos mapas
library(gwzinbr)     # Golden() e gwzinbr() — GWR com BN/ZINB
library(MASS)        # glm.nb() — Binomial Negativa global
library(robustbase)  # adjbox() — boxplot robusto (ajustado ao medcouple)
library(lmtest)      # lrtest() — razão de verossimilhança Poisson vs BN
library(corrplot)    # corrplot() — matriz de correlação visual
library(scales)      # squish() — truncamento de escalas de cor
library(stringi)     # stri_trans_general() — remoção de diacríticos


# =============================================================================
# 2. DIRETÓRIOS
# =============================================================================

dir_dados  <- "dados"
dir_aux    <- "arquivos_auxiliares"   # scripts de envelope simulado
dir_cache  <- "_cache"                # modelo GWR serializado
dir_output <- "~/Documents/GitHub/dissertacao/analise/output/plots_output"

if (!dir.exists(dir_cache))  dir.create(dir_cache,  recursive = TRUE)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)


# =============================================================================
# 3. DADOS
# =============================================================================
# base_mestrado.gpkg reúne em uma única camada: geometria dos 96 distritos,
# notificações SINAN por estrato demográfico, variáveis do Censo 2022,
# densidade de igrejas e bares, e coordenadas UTM do centroide ponderado.

distritos <- st_read(
  file.path(dir_dados, "base_mestrado.gpkg"),
  layer = "distritos",
  quiet = TRUE
)


# =============================================================================
# 4. VARIÁVEL RESPOSTA — casos notificados de violência física (2018–2022)
# =============================================================================

# Distritos sem casos: marcados nos mapas para evitar confusão com áreas vazias
distritos_sem_casos <- distritos %>% filter(VF_2018_2022 == 0)
distritos_sem_casos[c("x", "y")] <- st_coordinates(st_centroid(distritos_sem_casos))

# Mapa coroplético — contagem absoluta
p_mapa_vf <- ggplot(distritos) +
  geom_sf(aes(fill = VF_2018_2022)) +
  scale_fill_gradientn(
    colours = c("white", "#2171b5", "#08306b"),
    name    = "Casos notificados",
    oob     = squish
  ) +
  geom_text(data = distritos_sem_casos, aes(x = x, y = y),
            label = "*", size = 5, color = "black") +
  theme_minimal(base_size = 14) +
  labs(title = "", fill = "Casos notificados")
print(p_mapa_vf)
ggsave(file.path(dir_output, "mapa_vf_2018_2022.png"),
       plot = p_mapa_vf, width = 9, height = 7, dpi = 150)

# Mapa de círculos proporcionais
centroides <- distritos
centroides[c("x", "y")] <- st_coordinates(st_centroid(centroides))

p_mapa_circ <- ggplot() +
  geom_sf(data = distritos, fill = "gray95", color = "grey50", linewidth = 0.25,
          alpha = 0.8) +
  geom_point(data = centroides, aes(x = x, y = y, size = VF_2018_2022),
             color = "#2171b5", alpha = 0.8) +
  scale_size(
    name   = "Casos notificados",
    range  = c(0, 10),
    limits = c(min(centroides$VF_2018_2022, na.rm = TRUE),
               max(centroides$VF_2018_2022, na.rm = TRUE)),
    breaks = pretty(range(centroides$VF_2018_2022, na.rm = TRUE), n = 4)
  ) +
  annotation_scale(location = "br", width_hint = 0.25,
                   pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
  theme_minimal() +
  theme(axis.title  = element_blank(),
        axis.text   = element_blank(),
        axis.ticks  = element_blank(),
        panel.grid  = element_line(color = "grey90", linewidth = 0.2),
        plot.title  = element_blank())
print(p_mapa_circ)
ggsave(file.path(dir_output, "mapa_vf_circulos.png"),
       plot = p_mapa_circ, width = 9, height = 7, dpi = 150)


# =============================================================================
# 5. TAXA DE INCIDÊNCIA PADRONIZADA (TIP)
# =============================================================================
# Padronização indireta por faixa etária × sexo (4 estratos):
#   E_i = Σ_j P_ij × t_j  (casos esperados dado a composição demográfica)
#   TIP_i = O_i / E_i      (> 1: incidência acima do esperado)
#
# O offset nos modelos de contagem = log(E_i), de forma que o modelo estima
# log(TIP) como função linear das covariáveis.

# Taxas de referência: São Paulo como padrão (totais da cidade)
taxas_cidade <- st_drop_geometry(distritos) %>%
  summarise(
    taxa_meninos_0_4 = sum(VF_masc_0_4_anos) / sum(meninos0a4),
    taxa_meninos_5_9 = sum(VF_masc_5_9_anos) / sum(meninos5a9),
    taxa_meninas_0_4 = sum(VF_fem_0_4_anos)  / sum(meninas0a4),
    taxa_meninas_5_9 = sum(VF_fem_5_9_anos)  / sum(meninas5a9)
  )

distritos <- distritos %>%
  mutate(
    casos_esperados = meninos0a4 * taxas_cidade$taxa_meninos_0_4 +
                      meninos5a9 * taxas_cidade$taxa_meninos_5_9 +
                      meninas0a4 * taxas_cidade$taxa_meninas_0_4 +
                      meninas5a9 * taxas_cidade$taxa_meninas_5_9,
    tip          = VF_2018_2022 / casos_esperados,
    log_casos_esp = log(casos_esperados)   # offset para os modelos
  )

# Mapa da TIP (UTM zona 23S para escala cartográfica correta)
distritos_utm <- st_transform(distritos, 31983)
sem_casos_utm <- st_transform(distritos_sem_casos, 31983)
sem_casos_utm[c("x", "y")] <- st_coordinates(st_centroid(sem_casos_utm))

p_tip <- ggplot(distritos_utm) +
  geom_sf(aes(fill = tip), color = "grey70", linewidth = 0.2) +
  scale_fill_gradientn(
    colours = c("white", "#2171b5", "#08306b"),
    limits  = c(0, 5), oob = squish, name = NULL
  ) +
  geom_text(data = sem_casos_utm, aes(x = x, y = y),
            label = "*", size = 5, color = "black") +
  annotation_scale(location = "br", width_hint = 0.25) +
  theme_minimal(base_size = 14) +
  theme(axis.title         = element_blank(),
        axis.text          = element_blank(),
        axis.ticks         = element_blank(),
        panel.grid.major   = element_line(color = "grey90", linewidth = 0.2),
        panel.grid.minor   = element_blank(),
        legend.title       = element_blank(),
        legend.key.height  = unit(1.5, "cm"),
        legend.key.width   = unit(0.4, "cm"),
        plot.title         = element_blank())
print(p_tip)
ggsave(file.path(dir_output, "mapa_taxa_padronizada_vf_2018_2022.png"),
       plot = p_tip, width = 9, height = 7, dpi = 150)

# Estatísticas descritivas da TIP
print(summary(distritos$tip))

# Boxplot robusto da TIP com rótulos nos outliers
# adjbox() ajusta os limites pelo medcouple — mais adequado para distribuições
# assimétricas do que o boxplot de Tukey convencional.
nomes_distritos <- str_to_title(stri_trans_general(distritos$ds_nome, "Latin-ASCII")) %>%
  str_replace("Sao Miguel",      "São Miguel")      %>%
  str_replace("Capao Redondo",   "Capão Redondo")   %>%
  str_replace("Jardim Angela",   "Jardim Ângela")   %>%
  str_replace("Jardim Sao Luis", "Jardim São Luís") %>%
  str_replace("Consolacao",      "Consolação")

box <- adjbox(distritos$tip, main = "", ylab = "TIP",
              col = "#2171b5", border = "#08306b",
              range = 1.5, doReflect = TRUE, outline = TRUE)

idx_outliers <- which(distritos$tip %in% box$out)
if (length(idx_outliers) > 0) {
  y_out    <- distritos$tip[idx_outliers]
  direita  <- seq(1, length(y_out), by = 2)
  esquerda <- seq(2, length(y_out), by = 2)

  points(rep(1, length(y_out)), y_out, pch = 16, col = "black", cex = 0.6)
  segments(rep(1, length(direita)),  y_out[direita],  rep(1.05, length(direita)),  y_out[direita],  col = "gray30")
  text(rep(1.05, length(direita)),   y_out[direita],  labels = nomes_distritos[idx_outliers[direita]],  pos = 4, cex = 0.8)
  segments(rep(1, length(esquerda)), y_out[esquerda], rep(0.95, length(esquerda)), y_out[esquerda], col = "gray30")
  text(rep(0.95, length(esquerda)),  y_out[esquerda], labels = nomes_distritos[idx_outliers[esquerda]], pos = 2, cex = 0.8)
}
dev.copy(png, file.path(dir_output, "boxplot_robusto_taxa_padronizada.png"),
         width = 900, height = 700, res = 150)
dev.off()

# Moran I global — TIP
# I > 0 com p < 0,05: distritos próximos têm TIPs semelhantes → dependência
# espacial → modelos globais são insuficientes.
nb_queen <- poly2nb(distritos)
w_queen  <- nb2listw(nb_queen, style = "W")

moran_tip <- moran.mc(distritos$tip, listw = w_queen, nsim = 999)
cat("Moran I (TIP):", round(moran_tip$statistic, 4),
    " p-valor:", moran_tip$p.value, "\n")


# =============================================================================
# 6. VARIÁVEIS EXPLICATIVAS — transformação e descrição
# =============================================================================
# Renda: min-max [0, 100] para comparabilidade entre distritos.
# Igrejas e bares: por mil habitantes para remover efeito do tamanho populacional.

distritos <- distritos %>%
  mutate(
    rendap  = 100 * (media_renda_dist - min(media_renda_dist, na.rm = TRUE)) /
                    (max(media_renda_dist, na.rm = TRUE) - min(media_renda_dist, na.rm = TRUE)),
    igrejas = igrejas / (populacao_total / 1000),   # por mil habitantes
    bares   = bares   / (populacao_total / 1000)    # por mil habitantes
  )

# Tabela sem geometria — usada nos modelos e nas análises descritivas
tab <- st_drop_geometry(distritos)

# Variáveis incluídas na análise (14 covariáveis)
vars_descr <- c(
  "alfab",       # % alfabetizados ≥ 15 anos
  "ate1mora",    # % domicílios com 1 morador
  "mais5",       # % domicílios com 5+ moradores
  "comodo",      # % habitações coletivas informais (cortiços)
  "ambos",       # % domicílios: casal + filhos só do casal
  "plmenos1",    # % domicílios: casal + ≥1 filho só do responsável/cônjuge
  "composta",    # % domicílios compostos (inclui pessoas sem parentesco)
  "estend",      # % domicílios estendidos (outros parentes presentes)
  "nuclear",     # % domicílios nucleares
  "mulher_resp", # % domicílios chefiados por mulher
  "banheiro1",   # % domicílios com banheiro exclusivo
  "rendap",  # renda padronizada [0, 100]
  "igrejas",
  "bares"
)

rotulos_descr <- vars_descr

# Mapas de distribuição espacial por variável
walk(vars_descr, function(v) {
  lim <- if (v %in% c("igrejas", "bares")) c(0, 12) else c(0, 100)
  p <- ggplot(distritos) +
    geom_sf(aes(fill = .data[[v]])) +
    scale_fill_gradientn(
      colors = c("#deebf7", "#9ecae1", "#3182bd", "#08519c"),
      limits = lim, oob = squish
    ) +
    annotation_scale(location = "br", width_hint = 0.25) +
    labs(title = v) +
    theme_minimal() +
    theme(panel.grid   = element_line(color = "grey90", linewidth = 0.2),
          legend.title = element_blank(),
          axis.text    = element_blank(),
          axis.ticks   = element_blank(),
          plot.title   = element_text(hjust = 0.5, size = 14))
  print(p)
  ggsave(file.path(dir_output, paste0("mapa_", v, ".png")),
         plot = p, width = 10, height = 7.5, dpi = 300)
})

# Estatísticas descritivas
tab %>%
  dplyr::select(all_of(vars_descr)) %>%
  pivot_longer(everything(), names_to = "variavel", values_to = "valor") %>%
  group_by(variavel) %>%
  summarise(
    media   = round(mean(valor,   na.rm = TRUE), 2),
    dp      = round(sd(valor,     na.rm = TRUE), 2),
    min     = round(min(valor,    na.rm = TRUE), 2),
    mediana = round(median(valor, na.rm = TRUE), 2),
    max     = round(max(valor,    na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  print()

# Boxplots robustos por grupo temático
# Grupos 1–3 compartilham o mesmo eixo y (variáveis em %); grupo 4 tem escala própria.
grupos <- list(
  grupo1 = c("alfab", "ate1mora", "mais5", "mulher_resp", "rendap"),
  grupo2 = c("banheiro1", "comodo"),
  grupo3 = c("ambos", "plmenos1", "composta", "estend", "nuclear"),
  grupo4 = c("igrejas", "bares")
)

ylim_pct  <- c(0, ceiling(max(tab[, unlist(grupos[1:3])], na.rm = TRUE)))
ylim_dens <- c(0, ceiling(max(tab[, grupos$grupo4],       na.rm = TRUE)))

stats_adjbox <- function(x) {
  s <- adjbox(x, plot = FALSE)
  list(min = s$stats[1], q1 = s$stats[2], med = s$stats[3],
       q3  = s$stats[4], max = s$stats[5], out = s$out)
}

boxplot_grupo <- function(vars, rotulos, ylim, ylab = "%") {
  stats <- lapply(vars, function(v) stats_adjbox(tab[[v]]))
  par(mar = c(8, 5, 2, 2))
  plot(NA, xlim = c(0.5, length(vars) + 0.5), ylim = ylim,
       xaxt = "n", xlab = "", ylab = ylab, main = "")
  for (i in seq_along(vars)) {
    s <- stats[[i]]
    rect(i - 0.3, s$q1, i + 0.3, s$q3, col = "#2171b5", border = "#08306b")
    segments(i - 0.3, s$med, i + 0.3, s$med, lwd = 2,   col = "#08306b")
    segments(i, s$min, i, s$q1,  lwd = 1.5, col = "#08306b")
    segments(i, s$q3,  i, s$max, lwd = 1.5, col = "#08306b")
    segments(i - 0.3, s$min, i + 0.3, s$min, lwd = 1.5, col = "#08306b")
    segments(i - 0.3, s$max, i + 0.3, s$max, lwd = 1.5, col = "#08306b")
    if (length(s$out) > 0)
      points(rep(i, length(s$out)), s$out, pch = 16, col = "black")
  }
  axis(1, at = seq_along(vars), labels = rotulos, las = 2)
}

boxplot_grupo(grupos$grupo1, grupos$grupo1, ylim_pct)
dev.copy(png, file.path(dir_output, "boxplot_robusto_grupo1.png"), width = 800, height = 900, res = 150); dev.off()

boxplot_grupo(grupos$grupo2, grupos$grupo2, ylim_pct)
dev.copy(png, file.path(dir_output, "boxplot_robusto_grupo2.png"), width = 800, height = 900, res = 150); dev.off()

boxplot_grupo(grupos$grupo3, grupos$grupo3, ylim_pct)
dev.copy(png, file.path(dir_output, "boxplot_robusto_grupo3.png"), width = 800, height = 900, res = 150); dev.off()

boxplot_grupo(grupos$grupo4, grupos$grupo4, ylim_dens, ylab = "por mil hab.")
dev.copy(png, file.path(dir_output, "boxplot_robusto_grupo4.png"), width = 800, height = 900, res = 150); dev.off()

# Correlação de Pearson com a TIP
cor_tip <- lapply(seq_along(vars_descr), function(i) {
  t <- cor.test(distritos$tip, tab[[vars_descr[i]]], method = "pearson")
  data.frame(variavel = rotulos_descr[i],
             r        = round(t$estimate, 3),
             p_valor  = formatC(t$p.value, format = "e", digits = 2))
}) %>%
  bind_rows() %>%
  arrange(desc(abs(r)))

print(cor_tip)

# Matriz de correlação entre covariáveis
mat_cor <- cor(tab[, vars_descr], use = "complete.obs")
colnames(mat_cor) <- rownames(mat_cor) <- rotulos_descr

corrplot(mat_cor, method = "color", type = "upper", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, tl.cex = 0.9, number.cex = 0.7,
         col = colorRampPalette(c("#08306b", "white", "#b30000"))(200),
         mar = c(0, 0, 2, 0))
dev.copy(png, file.path(dir_output, "matriz_correlacao_variaveis.png"),
         width = 1800, height = 1500, res = 180)
dev.off()

# Dendrograma de correlação (dissimilaridade = 1 − |ρ|, método Ward)
# Variáveis no mesmo cluster tendem a ser redundantes para os modelos.
dissim <- as.dist(1 - abs(mat_cor))
plot(hclust(dissim, method = "ward.D"), main = "", xlab = "", sub = "",
     cex.axis = 0.9, cex.lab = 1.1)
dev.copy(png, file.path(dir_output, "dendrograma_correlação.png"),
         width = 900, height = 700, res = 150)
dev.off()


# =============================================================================
# 7. MODELO DE POISSON (global)
# =============================================================================
# Linha de base para dados de contagem. A superdispersão é diagnosticada pela
# razão desvio/gl: valores muito acima de 1 indicam Var(Y) >> E(Y), violando
# o pressuposto de Poisson e exigindo migração para Binomial Negativa.

mod_pois <- glm(
  VF_2018_2022 ~ comodo + composta + mais5 + mulher_resp + igrejas +
    offset(log_casos_esp),
  data   = tab,
  family = poisson()
)

ll_p  <- as.numeric(logLik(mod_pois))
ll0_p <- as.numeric(logLik(update(mod_pois, . ~ 1)))

cat("\n--- Poisson ---\n")
print(round(summary(mod_pois)$coefficients, 4))
cat("Pseudo R²:", round(1 - ll_p / ll0_p, 4), "\n")
cat("Desvio residual:", round(deviance(mod_pois), 2),
    "  gl:", df.residual(mod_pois),
    "  Razão:", round(deviance(mod_pois) / df.residual(mod_pois), 2),
    " (>> 1 = superdispersão)\n")

# Mapa e envelope simulado dos resíduos
distritos$resid_pois <- residuals(mod_pois, type = "deviance")

p_resid_pois <- ggplot(distritos) +
  geom_sf(aes(fill = resid_pois)) +
  scale_fill_gradient2(midpoint = median(distritos$resid_pois, na.rm = TRUE),
                       low = "blue", mid = "white", high = "red",
                       name = "Resíduo") +
  theme_minimal() +
  labs(title = "Resíduos por Distrito — Modelo de Poisson")
print(p_resid_pois)
ggsave(file.path(dir_output, "mapa_residuos_poisson.png"),
       plot = p_resid_pois, width = 8, height = 6, dpi = 300, bg = "white")

fit.model <- mod_pois
attach(tab, warn.conflicts = FALSE)
source(file.path(dir_aux, "envel_pois"), local = TRUE)
dev.copy(png, file.path(dir_output, "envelope_poisson.png"),
         width = 800, height = 800, res = 150)
dev.off()
detach(tab)


# =============================================================================
# 8. MODELO BINOMIAL NEGATIVA (global)
# =============================================================================
# A BN adiciona α (= 1/θ), permitindo Var(Y) = μ + αμ² > μ.
# O LRT testa H0: α = 0 (Poisson suficiente).
# Coeficientes estão na escala log; exp(β̂) dá o fator multiplicativo sobre
# a contagem esperada (ex.: exp(0,3) ≈ 1,35 → aumento de 35%).

mod_nb <- glm.nb(
  VF_2018_2022 ~ comodo + composta + mais5 + mulher_resp + igrejas +
    offset(log_casos_esp),
  data  = tab  
)

ll_nb  <- as.numeric(logLik(mod_nb))
ll0_nb <- as.numeric(logLik(update(mod_nb, . ~ 1)))
k_nb   <- length(coef(mod_nb))
n_nb   <- nobs(mod_nb)

cat("\n--- Binomial Negativa ---\n")
coef_nb <- summary(mod_nb)$coefficients
print(data.frame(
  coef     = round(coef_nb[, "Estimate"],      4),
  exp_coef = round(exp(coef_nb[, "Estimate"]), 4),
  ep       = round(coef_nb[, "Std. Error"],    4),
  p_valor  = formatC(coef_nb[, "Pr(>|z|)"], format = "e", digits = 3)
))
cat("Theta:", round(mod_nb$theta, 4),
    " Alpha (= 1/theta):", round(1 / mod_nb$theta, 4), "\n")
cat("Pseudo R²:", round(1 - ll_nb / ll0_nb, 4), "\n")
cat("AICc:", round(AIC(mod_nb) + 2 * k_nb * (k_nb + 1) / (n_nb - k_nb - 1), 2), "\n")

# Mapa e envelope simulado dos resíduos
distritos$resid_nb <- residuals(mod_nb, type = "deviance")

p_resid_nb <- ggplot(distritos) +
  geom_sf(aes(fill = resid_nb)) +
  scale_fill_gradient2(midpoint = median(distritos$resid_nb, na.rm = TRUE),
                       low = "blue", mid = "white", high = "red",
                       name = NULL) +
  annotation_scale(location = "br", width_hint = 0.25,
                   pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
  theme_minimal() +
  theme(plot.title       = element_blank(),
        panel.grid       = element_line(color = "grey90", linewidth = 0.2),
        legend.position  = "right",
        legend.title     = element_blank(),
        legend.text      = element_text(size = 9))
print(p_resid_nb)
ggsave(file.path(dir_output, "mapa_residuos_negbin.png"),
       plot = p_resid_nb, width = 8, height = 6, dpi = 300, bg = "white")

fit.model <- mod_nb
attach(tab, warn.conflicts = FALSE)
source(file.path(dir_aux, "envel_nbin"), local = TRUE)
dev.copy(png, file.path(dir_output, "envelope_negbin.png"),
         width = 800, height = 800, res = 150)
dev.off()
detach(tab)

# Moran I nos resíduos da BN global
# I significativo → estrutura espacial não capturada → necessidade de GWR.
coords <- cbind(tab$long_utm, tab$lat_utm)
nb_knn <- knn2nb(knearneigh(coords, k = 4))
w_knn  <- nb2listw(nb_knn, style = "W")

moran_nb_resid <- moran.mc(distritos$resid_nb, listw = w_knn, nsim = 999)
cat("Moran I (resíduos BN):", round(moran_nb_resid$statistic, 4),
    " p-valor:", moran_nb_resid$p.value, "\n")

# Verificação de excesso de zeros
# Razão próxima de 1 → BN já acomoda os zeros; ZINB desnecessário.
mu_hat    <- predict(mod_nb, type = "response")
zeros_obs <- sum(tab$VF_2018_2022 == 0)
zeros_esp <- sum((mod_nb$theta / (mod_nb$theta + mu_hat))^mod_nb$theta)
cat("Zeros observados:", zeros_obs,
    " Esperados (BN):", round(zeros_esp, 1),
    " Razão:", round(zeros_obs / zeros_esp, 3), "\n")


# =============================================================================
# 9. LISA — Autocorrelação Espacial Local
# =============================================================================
# O LISA decompõe o Moran I por distrito, identificando onde estão os clusters
# e classificando cada unidade em:
#   High-High (hot spot), Low-Low (cold spot), High-Low / Low-High (outlier).

classifica_lisa <- function(x, I_local, p_valor, media_global, alfa = 0.1) {
  if (p_valor > alfa)                   return("Não significativo")
  if (x >= media_global && I_local > 0) return("High-High")
  if (x  < media_global && I_local > 0) return("Low-Low")
  if (x >= media_global && I_local < 0) return("High-Low")
  if (x  < media_global && I_local < 0) return("Low-High")
  return("Não classificado")
}

cores_lisa <- c(
  "High-High"         = "#b2182b",
  "Low-Low"           = "#2166ac",
  "High-Low"          = "#fddb6d",
  "Low-High"          = "#44ba4a",
  "Não significativo" = "#f7f7f7"
)

lisa_tip <- localmoran(distritos$tip, w_queen)
distritos$cluster_lisa <- mapply(
  classifica_lisa,
  distritos$tip, lisa_tip[, 1], lisa_tip[, 5],
  MoreArgs = list(media_global = mean(distritos$tip, na.rm = TRUE))
)

p_lisa <- ggplot(distritos) +
  geom_sf(aes(fill = cluster_lisa), color = "#cccccc", linewidth = 0.3) +
  scale_fill_manual(values = cores_lisa, name = "Grupo LISA") +
  theme_minimal() +
  labs(title = "")
print(p_lisa)
ggsave(file.path(dir_output, "mapa_lisa_tx_pad.png"),
       plot = p_lisa, width = 8, height = 6, dpi = 300)


# =============================================================================
# 10. GWR-NB — Regressão Geograficamente Ponderada (Binomial Negativa)
# =============================================================================
# O GWR estima um vetor de coeficientes β(u_i) para cada distrito i,
# ponderando as observações pelo kernel bisquare adaptativo:
#   w_ij = (1 − (d_ij/h)²)²  se d_ij < h, caso contrário 0.
# h é o número de vizinhos (vizinhança adaptativa): selecionado por Golden().

# ---------------------------------------------------------------------------
# 10.1 Preparação: z-score nas covariáveis
# Torna os coeficientes locais comparáveis entre si independentemente das
# unidades originais. O offset não é padronizado.
# ---------------------------------------------------------------------------

vars_gwr <- c("comodo", "composta", "mais5", "mulher_resp")

tab_gwr <- tab
for (v in vars_gwr) {
  tab_gwr[[v]] <- as.numeric(scale(tab_gwr[[v]]))
}

# ---------------------------------------------------------------------------
# 10.2 Seleção da largura de banda — via Golden Section Search
# Testamos vizinhança adaptativa e fixa, critério AICc e CV,
# com e sem igrejas (para confirmar exclusão da variável do modelo final).
#
# Resultado esperado:
#   Sem igrejas · adaptativa · AICc → h = 62  ← selecionado
#   Com igrejas · adaptativa · AICc → h = 12  (muito baixo: overfitting local)
# ---------------------------------------------------------------------------

formula_final   <- VF_2018_2022 ~ comodo + composta + mais5 + mulher_resp
formula_igrejas <- VF_2018_2022 ~ comodo + composta + mais5 + mulher_resp + igrejas

args_golden <- list(
  data       = tab_gwr,
  lat        = "lat_utm",
  long       = "long_utm",
  offset     = "log_casos_esp",
  model      = "negbin",
  globalmin  = TRUE,
  distancekm = FALSE,
  force      = TRUE
)

gss_ad_aic  <- do.call(Golden, c(args_golden, list(formula = formula_final,   method = "adaptive_bsq", bandwidth = "aic")))
gss_ad_cv   <- do.call(Golden, c(args_golden, list(formula = formula_final,   method = "adaptive_bsq", bandwidth = "cv")))
gss_fix_aic <- do.call(Golden, c(args_golden, list(formula = formula_final,   method = "fixed_g",     bandwidth = "aic")))
gss_fix_cv  <- do.call(Golden, c(args_golden, list(formula = formula_final,   method = "fixed_g",     bandwidth = "cv")))
gss_igr_aic <- do.call(Golden, c(args_golden, list(formula = formula_igrejas, method = "adaptive_bsq", bandwidth = "aic")))
gss_igr_cv  <- do.call(Golden, c(args_golden, list(formula = formula_igrejas, method = "adaptive_bsq", bandwidth = "cv")))

print(tibble::tibble(
  formula  = c(rep("sem igrejas", 4), rep("com igrejas", 2)),
  metodo   = c("adaptativa", "adaptativa", "fixa",  "fixa",  "adaptativa", "adaptativa"),
  criterio = c("AICc",       "CV",         "AICc",  "CV",    "AICc",       "CV"),
  h        = c(gss_ad_aic$min_bandwidth[1],  gss_ad_cv$min_bandwidth[1],
               gss_fix_aic$min_bandwidth[1], gss_fix_cv$min_bandwidth[1],
               gss_igr_aic$min_bandwidth[1], gss_igr_cv$min_bandwidth[1])
))

# Gráfico de convergência — cenário selecionado
iter <- gss_ad_aic$iterations
p_banda <- ggplot(
  data.frame(h = c(iter$h1, iter$h2), aic = c(iter$cv1, iter$cv2)),
  aes(x = h, y = aic)
) +
  geom_point() + geom_line() +
  labs(x = "Largura da banda (h)", y = "AIC") +
  theme_minimal()
print(p_banda)
ggsave(file.path(dir_output, "grafico_banda_adaptative_aic.png"),
       plot = p_banda, width = 8, height = 6, dpi = 300)

# ---------------------------------------------------------------------------
# 10.3 Ajuste do modelo GWR-NB final
# h = 62: selecionado por Golden Section Search (adaptativa, AICc, sem igrejas)
# ---------------------------------------------------------------------------

mod_gwr <- gwzinbr(
  data       = tab_gwr,
  formula    = formula_final,
  lat        = "lat_utm",
  long       = "long_utm",
  offset     = "log_casos_esp",
  method     = "adaptive_bsq",
  model      = "negbin",
  force      = TRUE,
  distancekm = FALSE,
  h          = 62
)

saveRDS(mod_gwr, file.path(dir_cache, "mod_gwr.rds"))
message("Modelo salvo em ", file.path(dir_cache, "mod_gwr.rds"))

# Métricas de ajuste
m <- mod_gwr$measures
cat("\n--- GWR-NB ---\n")
cat("Deviance:", round(m["deviance"], 3),
    " AICc:", round(m["AICc"], 3),
    " Pseudo R²:", round(m["pct_ll"], 3), "\n")

print(mod_gwr$descript_stats_gwr_param_estimates)


# ---------------------------------------------------------------------------
# 10.4 Mapas dos coeficientes locais
# Regiões em cinza = estimativa não significativa a 10% (p > 0,10).
# ---------------------------------------------------------------------------

# parameter_estimates: matriz N × (2 + p + 3p + p) com colunas:
#   x, y | Intercept, <vars> | std_* | tstat_* | probt_* | sig_*
# t_test_gwr_param_estimates: apenas 3 escalares (alfa ajustado, t crítico, gl) — não contém p-valores por observação
# alpha_estimates: colunas id | alpha | std | tstat | probt | sig_alpha
coefs_locais <- as.data.frame(mod_gwr$parameter_estimates)
alpha_local  <- as.numeric(as.data.frame(mod_gwr$alpha_estimates)[, "alpha"])

alpha_df <- as.data.frame(mod_gwr$alpha_estimates)

for (v in vars_gwr) {
  distritos[[paste0("coef_", v)]] <- as.numeric(coefs_locais[[v]])
  distritos[[paste0("sig_",  v)]] <- coefs_locais[[paste0("sig_", v)]]
}
distritos$coef_alpha <- as.numeric(alpha_df[, "alpha"])
distritos$sig_alpha  <- alpha_df[, "sig_alpha"]

mapa_coef <- function(v, titulo) {
  col_coef <- paste0("coef_", v)
  col_sig  <- paste0("sig_",  v)

  # Usa o texto de significância gerado pelo gwzinbr:
  #   "not significant at 90%"  → NA (cinza no mapa)
  #   "significant at 90/95/99%" → exibe o coeficiente
  distritos$fill <- if (v == "alpha") {
    distritos[[col_coef]]
  } else {
    ifelse(grepl("not significant", distritos[[col_sig]]),
           NA_real_, distritos[[col_coef]])
  }

  ggplot(distritos) +
    geom_sf(aes(fill = fill), color = "#f7f7f7", linewidth = 0.35) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                         midpoint = 0, na.value = "grey90", name = NULL) +
    annotation_scale(location = "br", width_hint = 0.25,
                     pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
    theme_minimal() +
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          panel.grid       = element_line(color = "grey90", linewidth = 0.2),
          legend.position  = "right",
          legend.box       = "vertical",
          legend.title     = element_blank(),
          legend.text      = element_text(size = 9),
          plot.title       = element_blank()) +
    guides(fill = guide_colorbar(order = 1, title.position = "top",
                                 title.hjust = 0.5))
}

walk(
  list(
    list(v = "comodo",      titulo = "Coef. local — comodo"),
    list(v = "composta",    titulo = "Coef. local — composta"),
    list(v = "mais5",       titulo = "Coef. local — mais5"),
    list(v = "mulher_resp", titulo = "Coef. local — mulher_resp"),
    list(v = "alpha",       titulo = "Superdispersão local (alpha)")
  ),
  function(x) {
    p <- mapa_coef(x$v, x$titulo)
    print(p)
    ggsave(file.path(dir_output, paste0("mapa_param_gwr_bn_", x$v, ".png")),
           plot = p, width = 10, height = 7.5, dpi = 300, bg = "white")
  }
)

# ---------------------------------------------------------------------------
# 10.5 Resíduos do GWR-NB e diagnóstico espacial
# Moran I ≈ 0 (p > 0,05): GWR removeu a dependência espacial residual.
# Comparar com a BN global (Moran I ≈ 0,155; p = 0,01).
# ---------------------------------------------------------------------------

distritos$resid_gwr <- as.numeric(mod_gwr$residuals[, 5])   # resíduos de trabalho

moran_gwr_resid <- moran.mc(distritos$resid_gwr, listw = w_knn, nsim = 999)
cat("Moran I (resíduos GWR-NB):", round(moran_gwr_resid$statistic, 4),
    " p-valor:", moran_gwr_resid$p.value, "\n")

p_resid_gwr <- ggplot(distritos) +
  geom_sf(aes(fill = resid_gwr)) +
  scale_fill_gradient2(midpoint = median(distritos$resid_gwr, na.rm = TRUE),
                       low = "blue", mid = "white", high = "red",
                       name = "Residuos") +
  annotation_scale(location = "br", width_hint = 0.25,
                   pad_x = unit(0.2, "cm"), pad_y = unit(0.2, "cm")) +
  theme_minimal() +
  theme(plot.title       = element_blank(),
        panel.grid       = element_line(color = "grey90", linewidth = 0.2),
        legend.position  = "right",
        legend.title     = element_blank(),
        legend.text      = element_text(size = 9))
print(p_resid_gwr)
ggsave(file.path(dir_output, "mapa_residuos_gwr_negbin.png"),
       plot = p_resid_gwr, width = 8, height = 6, dpi = 300, bg = "white")


# =============================================================================
# FIM DO SCRIPT
# Saída gerada: _cache/mod_gwr.rds
# Carregar com: mod_gwr <- readRDS("_cache/mod_gwr.rds")
# =============================================================================
