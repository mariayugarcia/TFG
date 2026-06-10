# ==========================================
# ANÁLISIS DE ENCUESTA 
# ==========================================

library(tidyverse)
library(broom)
library(car)
library(ggplot2)
library(patchwork)
library(scales)
library(dplyr)
library(ggthemes)
library(ggrepel)

# 1. Leer con codificación UTF-8 explícita 
df_en <- read_csv("TFG Form.csv", locale = locale(encoding = "UTF-8"))
df_es <- read_csv("Cuestionario TFG.csv", locale = locale(encoding = "UTF-8"))

df_en <- df_en %>% select(1:27)
df_es <- df_es %>% select(1:27)


# 4. Asignar nombres estándar
col_names_shared <- c(
  "Timestamp", "age", "gender", "social_media_freq", "uses_tiktok", "tiktok_hours",
  "prod_content_freq", "discovered_products",
  "fomo_1", "fomo_2", "fomo_3", "fomo_4", "fomo_5",
  "scar_1", "scar_2", "scar_3", "scar_4", "scar_5",
  "imp_1", "imp_2", "imp_3", "imp_4", "imp_5", "imp_6", "imp_7",
  "bought_last_3m", "buy_freq"
)

df_en <- setNames(df_en, col_names_shared)
df_es <- setNames(df_es, col_names_shared)

# 5. Añadir etiqueta de idioma y combinar
df_en <- df_en %>% mutate(language = "Inglés")
df_es <- df_es %>% mutate(language = "Español")
df_combined <- bind_rows(df_en, df_es)


# ==========================================
# ESTANDARIZACIÓN DE CATEGORÍAS (EN/ES)
# ==========================================
df_combined <- df_combined %>%
  mutate(
    # Género
    gender = case_when(
      gender %in% c("Female", "Femenino") ~ "Femenino",
      gender %in% c("Male", "Masculino") ~ "Masculino",
      gender %in% c("Non-binary / Other", "No binario / Otro") ~ "No binario/Otro",
      TRUE ~ gender
    ),
    # Uso TikTok
    uses_tiktok = ifelse(uses_tiktok %in% c("Yes", "Sí"), "Sí", "No"),
    # Horas diarias
    tiktok_hours = case_when(
      tiktok_hours %in% c("Less than 30 minutes", "Menos de 30 minutos") ~ "< 30 min",
      tiktok_hours %in% c("30 min – 1 hour", "30 min - 1 hour", "30 min – 1 hora") ~ "30 min - 1 h",
      tiktok_hours %in% c("1–2 hours", "1-2 hours", "1–2 horas", "1-2 horas") ~ "1 - 2 h",
      tiktok_hours %in% c("More than 2 hours", "Más de 2 horas") ~ "> 2 h",
      TRUE ~ tiktok_hours
    ),
    # Frecuencia contenido productos
    prod_content_freq = case_when(
      prod_content_freq %in% c("Never", "Nunca") ~ "Nunca",
      prod_content_freq %in% c("Rarely", "Rara vez") ~ "Rara vez",
      prod_content_freq %in% c("Sometimes", "A veces") ~ "A veces",
      prod_content_freq %in% c("Frequently", "Frecuentemente") ~ "Frecuentemente",
      prod_content_freq %in% c("Very frequently", "Muy frecuentemente") ~ "Muy frecuentemente",
      TRUE ~ prod_content_freq
    ),
    # Compra últimos 3 meses
    bought_last_3m = ifelse(bought_last_3m %in% c("Yes", "Sí"), "Sí", "No"),
    # Frecuencia compra influenciada
    buy_freq = case_when(
      buy_freq %in% c("Never", "Nunca") ~ "Nunca",
      buy_freq %in% c("Rarely", "Rara vez") ~ "Rara vez",
      buy_freq %in% c("Sometimes", "A veces") ~ "A veces",
      buy_freq %in% c("Frequently", "Frecuentemente") ~ "Frecuentemente",
      buy_freq %in% c("Very frequently", "Muy frecuentemente") ~ "Muy frecuentemente",
      TRUE ~ buy_freq
    )
  )


# 6. Convertir Likert a numérico y crear scores compuestos
likert_cols <- c(paste0("fomo_", 1:5), paste0("scar_", 1:5), paste0("imp_", 1:7))

df_combined <- df_combined %>%
  mutate(across(all_of(likert_cols), ~as.numeric(as.character(.x)))) %>%
  mutate(
    score_fomo = rowMeans(across(starts_with("fomo_")), na.rm = TRUE),
    score_scar = rowMeans(across(starts_with("scar_")), na.rm = TRUE),
    score_imp  = rowMeans(across(starts_with("imp_")),  na.rm = TRUE)
  )

# 7. Contraste de medias (t-test de Student)
constructs <- c("score_fomo", "score_scar", "score_imp")

results <- df_combined %>%
  select(all_of(constructs), language) %>%
  pivot_longer(cols = all_of(constructs), names_to = "construct", values_to = "value") %>%
  group_by(construct) %>%
  summarise(
    mean_en = mean(value[language == "Inglés"], na.rm = TRUE),
    mean_es = mean(value[language == "Español"], na.rm = TRUE),
    sd_en = sd(value[language == "Inglés"], na.rm = TRUE),
    sd_es = sd(value[language == "Español"], na.rm = TRUE),
    n_en = sum(!is.na(value[language == "Inglés"])),
    n_es = sum(!is.na(value[language == "Español"])),
    levene_p = leveneTest(value ~ language, data = cur_data())$`Pr(>F)`[1],
    t_test = list(tidy(t.test(value ~ language, data = cur_data(), 
                              var.equal = levene_p > 0.05))),
    .groups = "drop"
  ) %>%
  unnest(t_test) %>%
  mutate(
    p_adj = p.adjust(p.value, method = "bonferroni"),
    decision = ifelse(p_adj < 0.05, "Segmentar", "Combinar")
  ) %>%
  select(construct, mean_en, mean_es, sd_en, sd_es, n_en, n_es, 
         levene_p, statistic, p.value, p_adj, decision)

# 8. Mostrar resultados

print(results %>% select(construct, mean_en, mean_es, p.value, p_adj, decision))


# ============================================================================
# EDA - EXPLORATORY DATA ANALYSIS
# ============================================================================

# ============================================================================
# 1. DISTRIBUCIÓN DEMOGRÁFICA
# ============================================================================

# 1.1 Edad

p_edad <- df_combined %>%
  count(age) %>%
  mutate(age = factor(age, levels = c("18-21", "22-25", "26-30", "31-40", "Más de 40"))) %>%
  ggplot(aes(x = age, y = n, fill = n)) +
  geom_col(alpha = 0.9, color = "white", linewidth = 0.4) +
  geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    name = "Frecuencia"
  ) +
  guides(
    fill = guide_colorbar(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Distribución por Edad",
    subtitle = "Participantes divididos por rangos de edad",
    x = "Rango de edad",
    y = "Número de participantes"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )

# 1.2 Género

# Calculamos posiciones
df_genero <- df_combined %>%
  count(gender) %>%
  mutate(
    porcentaje = n / sum(n),
    etiqueta = percent(porcentaje, accuracy = 0.1),
    ymax = cumsum(n),
    ymin = lag(ymax, default = 0),
    y = (ymax + ymin) / 2
  )


p_genero <- ggplot(df_genero, aes(x = 0, y = n, fill = gender)) +
  geom_col(width = 1, color = "white", linewidth = 1) +
  coord_polar(theta = "y") +
  geom_segment(
    aes(x = 0.6, xend = 1.2, y = y, yend = y),
    color = "grey50", linewidth = 0.7
  ) +
  geom_text(
    aes(x = 1.4, y = y, label = etiqueta),
    size = 5, fontface = "bold", color = "#2c3e50"
  ) +
  scale_fill_manual(
    values = c(
      "Femenino" = "#DF6F72",
      "Masculino" = "#231E91",
      "No binario/Otro" = "#F1F734"
    ),
    name = "Identidad de género"
  ) +
  # theme_void limpia todo lo que deforma el pastel
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b=10)),
    plot.subtitle = element_text(hjust = 0.5, color = "#7f8c8d", size = 11, margin = margin(b=20)),
    plot.margin = margin(20, 20, 20, 20)
  ) +
  labs(
    title = "Distribución por Género",
    subtitle = "Participantes según identidad de género"
  ) +
  
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5
    ),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 80, 30, 80)
  )


# 1.3 Uso de TikTok (Sí/No)
p_usa_tiktok <- df_combined %>%
  count(uses_tiktok) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    lab = paste0(n, "\n(", pct, "%)")
  ) %>%
  ggplot(aes(x = uses_tiktok, y = n, fill = uses_tiktok)) +
  geom_col(alpha = 0.9, color = "white", linewidth = 0.4) +
  geom_text(aes(label = lab), vjust = -0.2, size = 5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(
    values = c("No" = "#C0392B", "Sí" = "#27AE60"),
    name = "Respuesta"
  ) +
  labs(
    title = "Uso de TikTok",
    subtitle = "Proporción de participantes que declaran utilizar TikTok",
    x = "Respuesta",
    y = "Número de participantes"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )


# ============================================================================
# 2. PATRONES DE USO DE TIKTOK
# ============================================================================

# 2.1 Horas diarias en TikTok
p_horas <- df_combined %>%
  count(tiktok_hours) %>%
  mutate(tiktok_hours = factor(tiktok_hours, 
                               levels = c("< 30 min", "30 min - 1 h", "1 - 2 h", "> 2 h"))) %>%
  ggplot(aes(x = tiktok_hours, y = n, fill = tiktok_hours)) +
  geom_col(alpha = 0.9, color = "white", linewidth = 0.4) +
  geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_viridis_d(
    option = "C",
    direction = 1,
    name = "Horas diarias"
  ) +
  guides(
    fill = guide_legend(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Tiempo Diario en TikTok",
    subtitle = "Entre usuarios de TikTok, distribución de horas diarias",
    x = "Horas diarias",
    y = "Número de usuarios"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )


# 2.2 Frecuencia de contenido de productos
p_contenido_prod <- df_combined %>%
  count(prod_content_freq) %>%
  mutate(prod_content_freq = factor(prod_content_freq,
                                    levels = c("Nunca","Rara vez","A veces",
                                               "Frecuentemente","Muy frecuentemente"))) %>%
  ggplot(aes(x = prod_content_freq, y = n, fill = prod_content_freq)) +
  geom_col(alpha = 0.9, color = "white", linewidth = 0.4) +
  geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_viridis_d(
    option = "C",
    direction = 1,
    name = "Frecuencia"
  ) +
  guides(
    fill = guide_legend(
      direction = "vertical",
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  labs(
    title = "Frecuencia de Contenido de Productos",
    subtitle = "Con qué frecuencia se ven en TikTok publicaciones de productos",
    x = "Frecuencia",
    y = "Número de participantes"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )


# ============================================================================
# 3. DISTRIBUCIÓN DE CONSTRUCTOS PRINCIPALES
# ============================================================================

# 3.1 Distribución de scores (histogramas + densidad)


dist_plot <- function(data, var, title, ...) {
  media_val <- round(mean({{ var }}, na.rm = TRUE), 2)
  
  data %>%
    ggplot(aes(x = {{ var }})) +
    geom_histogram(aes(y = after_stat(density)), bins = 5,
                   fill = "#231E91", alpha = 0.7, color = "white", linewidth = 0.3) +
    geom_density(color = "black", linewidth = 0.8) +
    geom_vline(aes(xintercept = mean({{ var }}, na.rm = TRUE)),
               color = "#D929AA", linetype = "dashed", linewidth = 1.2) +
    scale_y_continuous(limits = c(0, 0.7), expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = title,
      subtitle = paste("Media =", media_val),
      x = "Puntuación (1–5)",
      y = "Densidad"
    ) +
    theme_fivethirtyeight() +
    theme(
      plot.title = element_text(
        size = 16,
        face = "bold",
        color = "#2c3e50",
        hjust = 0.5,
        margin = margin(b = 12)
      ),
      plot.subtitle = element_text(
        size = 11,
        color = "#7f8c8d",
        hjust = 0.5,
        margin = margin(b = 18)
      ),
      axis.text.x = element_text(
        size = 10,
        color = "#34495e",
        margin = margin(t = 12)
      ),
      axis.title.x = element_text(
        size = 11,
        color = "#34495e",
        margin = margin(t = 18),
      ),
      axis.title.y = element_text(
        size = 11,
        color = "#34495e",
        margin = margin(r = 18)
      ),
      axis.text.y = element_text(
        size = 10, 
        color = "#34495e", 
        margin = margin(r = 10)
      ),
      legend.position = "right",
      legend.direction = "vertical",
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(30, 20, 20, 20)
    )
}

# Generar los tres gráficos
p_dist_fomo <- dist_plot(df_combined, df_combined$score_fomo, "Distribución FOMO")
p_dist_scar <- dist_plot(df_combined, df_combined$score_scar, "Distribución Escasez")
p_dist_imp  <- dist_plot(df_combined, df_combined$score_imp,  "Distribución Compra Impulsiva")

# Función para detectar outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# Procesamiento de datos
df_plot <- df_combined %>%
  pivot_longer(cols = c(score_fomo, score_scar, score_imp),
               names_to = "construct", values_to = "score") %>%
  mutate(construct = factor(construct, 
                            levels = c("score_imp", "score_scar", "score_fomo"),
                            labels = c("Compra Impulsiva", "Escasez", "FOMO"))) %>%
  # Agrupamos para calcular outliers por constructo e idioma
  group_by(construct, language) %>%
  mutate(es_outlier = is_outlier(score)) %>%
  ungroup()

# Gráfico
set.seed(123)
p_box_idioma <- ggplot(df_plot, aes(x = construct, y = score, fill = language)) +
  geom_boxplot(alpha = 0.85, width = 0.6, linewidth = 0.8, outlier.shape = NA) +
  geom_jitter(aes(color = es_outlier), 
              position = position_jitterdodge(jitter.width = 0.15), 
              alpha = 0.4, size = 1.2) +
  # Colores: negro para puntos normales, rojo para outliers
  scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red"), guide = "none") +
  scale_fill_manual(
    values = c("Inglés" = "#231E91", "Español" = "#D929AA"),
    name = "Idioma"
  ) +
  labs(
    title = "Comparación de constructos por idioma",
    subtitle = "Análisis comparativo de FOMO, Escasez y Compra Impulsiva",
    x = "Constructo",
    y = "Puntuación (1–5)"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )


# ============================================================================
# 4. COMPRAS REALIZADAS
# ============================================================================

# 4.1 Compra últimos 3 meses
p_compra_3m <- df_combined %>%
  count(bought_last_3m) %>%
  mutate(
    pct = round(n / sum(n) * 100, 1),
    lab = paste0(n, "\n(", pct, "%)")
  ) %>%
  ggplot(aes(x = bought_last_3m, y = n, fill = bought_last_3m)) +
  geom_col(alpha = 0.9, color = "white", linewidth = 0.4) +
  geom_text(aes(label = lab), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(
    values = c("No" = "#C0392B", "Sí" = "#27AE60"),
    name = "Respuesta"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "¿Compras en los últimos 3 meses?",
    subtitle = "Participantes que declaran haber comprado productos vistos en TikTok",
    x = "Respuesta",
    y = "Número de participantes"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )


# 4.2 Frecuencia de compra influenciada
p_freq_compra <- df_combined %>%
  count(buy_freq) %>%
  mutate(buy_freq = factor(buy_freq,
                           levels = c("Nunca","Rara vez","A veces",
                                      "Frecuentemente","Muy frecuentemente"))) %>%
  ggplot(aes(x = buy_freq, y = n, fill = n)) +
  geom_col(alpha = 0.9, color = "white", linewidth = 0.4) +
  geom_text(aes(label = n), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_viridis_c(
    option = "C", 
    direction = -1, 
    name = "Frecuencia"
  ) +
  guides(
    fill = guide_colorbar(direction = "vertical", title.position = "top", title.hjust = 0.5)
  ) +
  labs(
    title = "Frecuencia de Compra Influenciada",
    subtitle = "Frecuencia de compra de productos tras verlos en TikTok",
    x = "Frecuencia",
    y = "Número de participantes"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )

  
# ============================================================================
# GUARDAR GRÁFICOS
# ============================================================================

# Guardar gráficos en formato PNG dentro de "figuras"
ggsave("figuras/p_edad.png", plot = p_edad, width = 10, height = 10, dpi = 300)
ggsave("figuras/p_genero.png", plot = p_genero, width = 10, height = 7, dpi = 300)
ggsave("figuras/p_usa_tiktok.png", plot = p_usa_tiktok, width = 10, height = 7, dpi = 300)

ggsave("figuras/p_horas.png", plot = p_horas, width = 10, height = 10, dpi = 300)
ggsave("figuras/p_contenido_prod.png", plot = p_contenido_prod, width = 10, height = 10, dpi = 300)

ggsave("figuras/p_dist_fomo.png", plot = p_dist_fomo, width = 10, height = 7, dpi = 300)
ggsave("figuras/p_dist_scar.png", plot = p_dist_scar, width = 10, height = 7, dpi = 300)
ggsave("figuras/p_dist_imp.png", plot = p_dist_imp, width = 10, height = 7, dpi = 300)
ggsave("figuras/p_box_idioma.png", plot = p_box_idioma, width = 12, height = 7, dpi = 300)

ggsave("figuras/p_compra_3m.png", plot = p_compra_3m, width = 10, height = 9.5, dpi = 300)
ggsave("figuras/p_freq_compra.png", plot = p_freq_compra, width = 10, height = 10, dpi = 300)

# Guardar el panel de constructos
ggsave("figuras/constructos_triptico.png", 
       plot = p_dist_fomo | p_dist_scar | p_dist_imp, 
       width = 16, height = 6, dpi = 300)


# ==========================================
# ANÁLISIS ESTADÍSTICO
# ==========================================

# ============================================================================
# ESTADÍSTICOS DESCRIPTIVOS
# ============================================================================

# Calculamos medias y desviaciones de los factores de análisis
descriptivos <- df_combined %>%
  summarise(
    n_total = n(),
    # FOMO
    fomo_mean = mean(score_fomo, na.rm = TRUE),
    fomo_sd = sd(score_fomo, na.rm = TRUE),
    # Escasez
    scar_mean = mean(score_scar, na.rm = TRUE),
    scar_sd = sd(score_scar, na.rm = TRUE),
    # Compra Impulsiva
    imp_mean = mean(score_imp, na.rm = TRUE),
    imp_sd = sd(score_imp, na.rm = TRUE)
  )


print(descriptivos)

# ============================================================================
# CORRELACIONES
# ============================================================================

# Matriz de correlación simple
cor_matrix <- df_combined %>%
  select(score_fomo, score_scar, score_imp) %>%
  cor(use = "pairwise")

print(round(cor_matrix, 3))

# ============================================================================
# REGRESIÓN LINEAL MÚLTIPLE
# ============================================================================

# Preparar datos: Convertir variables categóricas a numéricas para la regresión
df_reg <- df_combined %>%
  mutate(
    # Variable dependiente
    compra_impulsiva = score_imp,
    
    # Variables independientes
    fomo = score_fomo,
    escasez = score_scar,
    
    usa_tiktok = ifelse(uses_tiktok %in% "Sí", 1, 0),
    
    # Tiempo en TikTok
    tiempo_tiktok = case_when(
      tiktok_hours == "< 30 min"      ~ 0.5,
      tiktok_hours == "30 min - 1 h"  ~ 1.0,
      tiktok_hours == "1 - 2 h"       ~ 1.5,
      tiktok_hours == "> 2 h"         ~ 2.0,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(compra_impulsiva, fomo, escasez, usa_tiktok, tiempo_tiktok)

# Ejecutar modelo
modelo <- lm(compra_impulsiva ~ fomo + escasez + usa_tiktok + tiempo_tiktok, data = df_reg)

print(summary(modelo))

# Guardar resultados tidy
resultados_reg <- tidy(modelo, conf.int = TRUE)
print(resultados_reg)

# ============================================================================
# D) GRÁFICOS 
# ============================================================================

# Gráfico 1: Relación FOMO -> Compra (con línea de tendencia)
p_regresion_fomo <- ggplot(df_reg, aes(x = fomo, y = compra_impulsiva, color = language)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "black",
    fill = "#F1F734",
    linetype = "dashed",
    alpha = 0.2
  ) +
  scale_color_manual(values = c("Inglés" = "#231E91", "Español" = "#D929AA"), name = "Idioma") +
  labs(
    title = "Relación entre FOMO y Compra Impulsiva",
    subtitle = "Tendencia lineal según el idioma de la encuesta",
    x = "Nivel de FOMO (1–5)",
    y = "Compra Impulsiva (1–5)"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )


# Gráfico 2: Importancia de predictores
p_impacto_variables <- resultados_reg %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Significativo = ifelse(p.value < 0.05, "Significativo", "No significativo"),
    term_bonito = case_when(
      term == "fomo" ~ "FOMO",
      term == "escasez" ~ "Escasez",
      term == "usa_tiktok" ~ "¿Utiliza TikTok?",
      term == "tiempo_tiktok" ~ "Tiempo en TikTok",
      TRUE ~ term
    ),
    term_bonito = factor(term_bonito, levels = term_bonito[order(estimate)])
  ) %>%
  ggplot(aes(x = estimate, y = term_bonito, fill = Significativo)) +
  geom_col(width = 0.6) +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high), width = 0.2, color = "gray40") +
  scale_fill_manual(values = c("Significativo" = "#27AE60", "No significativo" = "#C0392B")) +
  labs(
    title = "Impacto de variables en Compra Impulsiva",
    subtitle = "Coeficientes de regresión con intervalos de confianza del 95%",
    x = "Estimación del coeficiente",
    y = "" 
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )


# GUARDAR GRÁFICOS

ggsave("figuras/p_regresion_fomo.png", 
       plot = p_regresion_fomo, 
       width = 10, height = 7, dpi = 300)

ggsave("figuras/p_impacto_variables.png", 
       plot = p_impacto_variables, 
       width = 10, height = 7, dpi = 300)


# ============================================================================
# ANÁLISIS Impacto de la Exposición (ANOVA)
# ¿Quienes ven más contenido de productos tienen mayor impulso de compra?
# ============================================================================

# 1. Preparar datos: Convertir frecuencia a factor ordenado
df_anova <- df_combined %>%
  mutate(
    prod_content_freq_clean = str_trim(prod_content_freq),
    
    # Creamos el factor ordenado
    freq_nivel = factor(
      prod_content_freq_clean,
      levels = c("Nunca","Rara vez","A veces",
                 "Frecuentemente","Muy frecuentemente"),
      ordered = TRUE
    )
  ) %>%
  drop_na(freq_nivel, score_imp)

# Ejecutar ANOVA
modelo_anova <- aov(score_imp ~ freq_nivel, data = df_anova)
anova_res <- summary(modelo_anova)

print(anova_res)

# Extraer p-valor para interpretación
p_val_anova <- anova_res[[1]][["Pr(>F)"]][1]
p_val_anova

# Gráfico de Medias por Grupo (Boxplot + Medias)

p_anova <- ggplot(df_anova, aes(x = freq_nivel, y = score_imp, fill = freq_nivel)) +
  geom_boxplot(alpha = 0.8, color = "#2c3e50", outlier.color = "#C0392B", outlier.size = 2) +
  stat_summary(fun = mean, geom = "point", shape = 21, size = 4, fill = "white", color = "black") +
  # Línea de tendencia de la media
  stat_summary(fun = mean, geom = "line", aes(group = 1), color = "#34495e", linewidth = 1.2, linetype = "dashed") +
  scale_fill_viridis_d(option = "C", name = "Frecuencia") +
  labs(
    title = "Compra Impulsiva por Frecuencia de Exposición",
    subtitle = paste("Resultado ANOVA (p =", round(p_val_anova, 3), ") -", 
                     ifelse(p_val_anova < 0.05, "Diferencias significativas detectadas", "Sin diferencias significativas")),
    x = "Frecuencia de exposición a contenido",
    y = "Compra Impulsiva (1-5)"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )

ggsave("figuras/p_anova.png", 
       plot = p_anova, 
       width = 10, height = 7, dpi = 300)


# ============================================================================
# MODELO LOGÍSTICO
# ============================================================================


# Preparar datos para modelo logístico
df_logit <- df_combined %>%
  mutate(
    # Variable objetivo: compra binaria
    compra_bin = ifelse(bought_last_3m == "Sí", 1, 0),
    
    # Predictores psicológicos (ya calculados)
    fomo = score_fomo,
    escasez = score_scar,
    
    # Tiempo en TikTok (numérico)
    tiempo_num = case_when(
      tiktok_hours == "< 30 min" ~ 0.5,
      tiktok_hours == "30 min - 1 h" ~ 1.0,
      tiktok_hours == "1 - 2 h" ~ 1.5,
      tiktok_hours == "> 2 h" ~ 2.0,
      TRUE ~ NA_real_
    )
  ) %>%
  drop_na(compra_bin, fomo, escasez, tiempo_num)

# Modelo logístico
modelo_logit <- glm(compra_bin ~ fomo + escasez + tiempo_num, 
                    data = df_logit, family = binomial)

# Resultados interpretables
resultados_logit <- tidy(modelo_logit, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    interpretacion = case_when(
      term == "fomo" ~ "Por cada punto extra en FOMO, la probabilidad de comprar aumenta X%",
      term == "escasez" ~ "Por cada punto extra en Escasez, la probabilidad de comprar aumenta Y%",
      term == "tiempo_num" ~ "Por cada hora extra en TikTok, la probabilidad cambia Z%",
      TRUE ~ "Intercepto"
    )
  )

print(resultados_logit)

# Métricas de ajuste

round(pROC::roc(df_logit$compra_bin, predict(modelo_logit, type = "response"))$auc, 3)
df_logit_plot <- df_logit %>%
  mutate(prob_predicha = predict(modelo_logit, type = "response"))

p_logit <- ggplot(df_logit_plot, aes(x = fomo, y = prob_predicha, color = factor(compra_bin))) +
  geom_jitter(width = 0.1, height = 0.02, alpha = 0.7) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE, color = "black") +
  scale_color_manual(values = c("0" = "red", "1" = "green"), 
                     labels = c("No compró", "Sí compró"), name = "Compra real") +
  labs(
    title = "Probabilidad predicha de compra según nivel de FOMO",
    subtitle = "Modelo logístico: compra_bin ~ FOMO + Escasez + Tiempo TikTok",
    x = "Puntuación FOMO (1-5)", 
    y = "Probabilidad de compra (0-1)"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      color = "#2c3e50",
      hjust = 0.5,
      margin = margin(b = 12)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "#7f8c8d",
      hjust = 0.5,
      margin = margin(b = 18)
    ),
    axis.text.x = element_text(
      size = 10,
      color = "#34495e",
      margin = margin(t = 12)
    ),
    axis.title.x = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(t = 18),
    ),
    axis.title.y = element_text(
      size = 11,
      color = "#34495e",
      margin = margin(r = 18)
    ),
    axis.text.y = element_text(
      size = 10, 
      color = "#34495e", 
      margin = margin(r = 10)
    ),
    legend.position = "right",
    legend.direction = "vertical",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    plot.margin = margin(30, 20, 20, 20)
  )


ggsave("figuras/p_prediccion_logit.png", p_logit, width = 10, height = 7, dpi = 300)


# ============================================================================
# GUARDAR TABLAS Y DATASET FINAL
# ============================================================================

# 1. Tabla de contraste de medias (t-test EN vs ES)
write_csv(results, "Tablas/Tabla_contraste_medias_idiomas.csv")

# 2. Estadísticos descriptivos de constructos
write_csv(descriptivos, "Tablas/Tabla_descriptivos_constructos.csv")

# 3. Matriz de correlaciones (formato largo para Word/Excel)
cor_matrix_long <- as.data.frame(as.table(cor_matrix)) %>%
  rename(Constructo_1 = Var1, Constructo_2 = Var2, Correlacion = Freq)
write_csv(cor_matrix_long, "Tablas/Tabla_correlaciones.csv")

# 4. Resultados de regresión lineal múltiple
resultados_reg <- tidy(modelo, conf.int = TRUE)
write_csv(resultados_reg, "Tablas/Tabla_regresion_lineal.csv")

# 5. Resultados de regresión logística (modelo predictivo)
resultados_logit <- tidy(modelo_logit, conf.int = TRUE, exponentiate = TRUE) %>%
  mutate(
    interpretacion = case_when(
      term == "fomo" ~ "Por cada punto extra en FOMO, la probabilidad de comprar aumenta X%",
      term == "escasez" ~ "Por cada punto extra en Escasez, la probabilidad de comprar aumenta Y%",
      term == "tiempo_num" ~ "Por cada hora extra en TikTok, la probabilidad cambia Z%",
      TRUE ~ "Intercepto"
    )
  )
write_csv(resultados_logit, "Tablas/Tabla_regresion_logistica.csv")

# 6. Resultados del ANOVA (frecuencia de exposición)
anova_tabla <- anova_res[[1]] %>%
  as.data.frame() %>%
  rownames_to_column(var = "Fuente") %>%
  mutate(
    F_value = round(`F value`, 3),
    p_value = round(`Pr(>F)`, 4)
  ) %>%
  select(Fuente, Df, `Sum Sq`, `Mean Sq`, F_value, p_value)

# Guardar con encoding UTF-8 explícito 
write_excel_csv(anova_tabla, "Tablas/Tabla_ANOVA_exposicion.csv", delim = ",")

# 7. Dataset limpio y estandarizado (para análisis futuros o anexos)
write_csv(df_combined, "Tablas/df_combined_limpio.csv")

# 8. Dataset para regresión 
write_csv(df_reg, "Tablas/df_regresion_limpio.csv")

# 9. Dataset para modelo logístico
write_csv(df_logit, "Tablas/df_logit_limpio.csv")

