# ==========================================
# ANÁLISIS DE DATOS TIKTOK 
# ==========================================

library(tidyverse)
library(readxl)
library(ggplot2)
library(patchwork)
library(writexl)
library(ggthemes)
library(sentimentr)
library(tidytext)
library(textdata)
library(syuzhet)
library(wordcloud2)
library(wordcloud)
library(stopwords)
library(htmlwidgets)
library(webshot)
library(quanteda)
library(topicmodels)

# ============================================================================
# 1. IMPORTAR DATOS
# ============================================================================

videos <- read_excel("Videos_TikTok.xlsx", sheet = "Sheet1")
comentarios <- read_excel("Comentarios_traducidos.xlsx", sheet = "Sheet1")

# ============================================================================
# 2. LIMPIEZA Y PREPARACIÓN
# ============================================================================

# Limpiar nombres de sectores para consistencia
videos <- videos %>%
  mutate(
    Sector = case_when(
      Sector == "BookTok" ~ "Editorial",
      Sector == "Labubu" ~ "Blindbox",
      Sector == "Popup" ~ "PopUp",
      TRUE ~ Sector
    ),
    # Calcular Ratio de Engagement: (Likes + Comentarios + Compartidos) / Vistas * 100
    Ratio_Engagement = round((Likes + Comentarios_Total + Compartidos) / Vistas * 100, 2),
    # Convertir variables Likert a numérico si vienen como texto
    across(c(FOMO_Urgencia:FOMO_Popularidad, Escasez_Temporal:Escasez_Simbolica), 
           ~as.numeric(as.character(.)))
  )

# Limpiar comentarios
comentarios <- comentarios %>%
  mutate(
    Tono = ifelse(Tono == "Negativa", "Negativo", Tono),  # Corregir typo
    across(c(Menciona_FOMO, Menciona_Escasez, Intencion_Compra), 
           ~ifelse(. %in% c("Sí", "Si", "si"), "Sí", "No"))  # Estandarizar Sí/No
  )

# ============================================================================
# 3. ANÁLISIS DESCRIPTIVO POR SECTOR
# ============================================================================

# 3.1 Métricas de engagement por sector
engagement_sector <- videos %>%
  group_by(Sector) %>%
  summarise(
    n_videos = n(),
    Vistas_med = median(Vistas),
    Likes_med = median(Likes),
    Comentarios_med = median(Comentarios_Total),
    Engagement_med = median(Ratio_Engagement),
    Vistas_max = max(Vistas),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(.x, 0)))

print(engagement_sector)

# 3.2 FOMO y Escasez por sector (medias de codificación 0-2)
fomo_escasez_sector <- videos %>%
  group_by(Sector) %>%
  summarise(
    FOMO_Urgencia_mean = round(mean(FOMO_Urgencia, na.rm = TRUE), 2),
    FOMO_Popularidad_mean = round(mean(FOMO_Popularidad, na.rm = TRUE), 2),
    Escasez_Temporal_mean = round(mean(Escasez_Temporal, na.rm = TRUE), 2),
    Escasez_Cuantitativa_mean = round(mean(Escasez_Cuantitativa, na.rm = TRUE), 2),
    Escasez_Simbolica_mean = round(mean(Escasez_Simbolica, na.rm = TRUE), 2),
    .groups = "drop"
  )

print(fomo_escasez_sector)

# ============================================================================
# 4. ANÁLISIS DE COMENTARIOS - eWOM 
# ============================================================================

# 4.1 Distribución de tono por sector
tono_sector <- comentarios %>%
  left_join(videos %>% select(ID_Video, Sector), by = "ID_Video") %>%
  group_by(Sector, Tono) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(pct = round(n / sum(n) * 100, 1))

print(tono_sector)

# 4.2 Menciones de FOMO/Escasez/Intención de compra por sector
ewom_sector <- comentarios %>%
  left_join(videos %>% select(ID_Video, Sector), by = "ID_Video") %>%
  group_by(Sector) %>%
  summarise(
    n_total = n(),
    pct_FOMO = round(mean(Menciona_FOMO == "Sí") * 100, 1),
    pct_Escasez = round(mean(Menciona_Escasez == "Sí") * 100, 1),
    pct_Compra = round(mean(Intencion_Compra == "Sí") * 100, 1),
    .groups = "drop"
  )

print(ewom_sector)

# ============================================================================
# 5. GRÁFICOS
# ============================================================================

# 5.1 Engagement por sector
p_engagement <- ggplot(engagement_sector, aes(x = Sector, y = Engagement_med, fill = Sector)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = paste0(Engagement_med, "%")), vjust = -0.5, size = 4) +
  scale_fill_viridis_d(
    option = "C",
    direction = 1,
    name = "Frecuencia"
  ) +
  labs(title = "Ratio de Engagement Medio por Sector",
       subtitle = "(Likes + Comentarios + Compartidos) / Vistas × 100",
       x = "Sector", y = "Engagement (%)") +
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


# 5.2 FOMO vs Escasez por sector
p_fomo_escasez <- videos %>%
  pivot_longer(cols = c(FOMO_Urgencia, FOMO_Popularidad, Escasez_Temporal, 
                        Escasez_Cuantitativa, Escasez_Simbolica),
               names_to = "variable", values_to = "valor") %>%
  mutate(
    variable = case_when(
      str_detect(variable, "FOMO") ~ "FOMO (Urgencia)",
      str_detect(variable, "Temporal") ~ "Escasez Temporal",
      str_detect(variable, "Cuantitativa") ~ "Escasez Cuantitativa",
      str_detect(variable, "Simbolica") ~ "Escasez Simbólica",
      TRUE ~ variable
    )
  ) %>%
  group_by(Sector, variable) %>%
  summarise(media = round(mean(valor, na.rm = TRUE), 2), .groups = "drop") %>%
  ggplot(aes(x = variable, y = media, fill = Sector)) +
  scale_fill_viridis_d(
    option = "C",
    direction = 1,
    name = "Frecuencia"
  ) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "Estrategias de FOMO y Escasez por Sector",
       subtitle = "Escala 0-2 (0=No aparece, 1=Sutil, 2=Explícito)",
       x = "Variable", y = "Puntuación media") +
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

# 5.3 Tono de comentarios por sector
p_tono <- comentarios %>%
  left_join(videos %>% select(ID_Video, Sector), by = "ID_Video") %>%
  group_by(Sector, Tono) %>%
  summarise(n = n(), .groups = "drop") %>%
  ggplot(aes(x = Sector, y = n, fill = Tono)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_viridis_d(
    option = "C",
    direction = 1,
    name = "Frecuencia"
  ) +
  labs(title = "Distribución de Tono en Comentarios por Sector",
       x = "Sector", y = "Número de comentarios") +
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

# 5.4 Intención de compra vs menciones de FOMO
ewom_long <- ewom_sector %>%
  select(Sector, pct_FOMO, pct_Compra) %>%
  pivot_longer(
    cols = c(pct_FOMO, pct_Compra), 
    names_to = "Metrica", 
    values_to = "Porcentaje"
  ) %>%
  mutate(
    Metrica = case_when(
      Metrica == "pct_FOMO" ~ "% Menciones FOMO",
      Metrica == "pct_Compra" ~ "% Intención de Compra"
    )
  )

p_intencion_fomo <- ggplot(ewom_long, aes(x = Sector, y = Porcentaje, fill = Metrica)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  geom_text(
    aes(label = paste0(round(Porcentaje, 1), "%")),
    position = position_dodge(width = 0.8),
    vjust = -0.5,
    size = 3.5,
    fontface = "bold",
    color = "#2c3e50"
  ) +
  scale_fill_manual(values = c("% Menciones FOMO" = "#231E91", "% Intención de Compra" = "#D929AA")) +
  ylim(0, 100) +
  labs(
    title = "Relación: Menciones de FOMO vs Intención de Compra",
    subtitle = "Cada sector representa la agregación de n=150 comentarios",
    x = "Sector Analizado", 
    y = "Porcentaje (%)",
    fill = "Métrica de Análisis"
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



# Crear carpeta llamada "figuras" 
if (!dir.exists("figuras")) dir.create("figuras")

# Guardar gráficos

ggsave("figuras/TikTok_01_engagement_sector.png", p_engagement, width = 8, height = 7, dpi = 300)
ggsave("figuras/TikTok_02_fomo_escasez.png", p_fomo_escasez, width = 10, height = 6, dpi = 300)
ggsave("figuras/TikTok_03_tono_comentarios.png", p_tono, width = 8, height = 5, dpi = 300)
ggsave("figuras/TikTok_04_fomo_vs_compra.png", p_intencion_fomo, width = 8, height = 6, dpi = 300)

# ============================================================================
# 6. TABLA COMPARATIVA
# ============================================================================

tabla_comparativa <- videos %>%
  group_by(Sector) %>%
  summarise(
    n_videos = n(),
    Engagement_med = median(Ratio_Engagement),
    FOMO_total = round(mean(FOMO_Urgencia + FOMO_Popularidad, na.rm = TRUE), 2),
    Escasez_total = round(mean(Escasez_Temporal + Escasez_Cuantitativa + Escasez_Simbolica, na.rm = TRUE), 2),
    Tipo_escasez_dominante = case_when(
      mean(Escasez_Simbolica, na.rm = TRUE) > mean(Escasez_Temporal, na.rm = TRUE) & 
        mean(Escasez_Simbolica, na.rm = TRUE) > mean(Escasez_Cuantitativa, na.rm = TRUE) ~ "Simbólica",
      mean(Escasez_Temporal, na.rm = TRUE) > mean(Escasez_Cuantitativa, na.rm = TRUE) ~ "Temporal",
      TRUE ~ "Cuantitativa"
    ),
    .groups = "drop"
  ) %>%
  left_join(ewom_sector %>% select(Sector, pct_Compra), by = "Sector") %>%
  select(Sector, n_videos, Engagement_med, FOMO_total, Escasez_total, Tipo_escasez_dominante, pct_Compra)

print(tabla_comparativa)


# Crear carpeta "Tablas" si no existe
if (!dir.exists("Tablas")) dir.create("Tablas")
# Guardar tabla para Word/Excel
write_excel_csv(tabla_comparativa, "Tablas/Tabla_comparativa_sectores.csv")



# ============================================================================
#  6. SENTIMENT ANALYSIS 
# ============================================================================

# Análisis de sentimiento en comentarios
comentarios_sentiment <- comentarios %>%
  unnest_tokens(word, Texto_Comentario) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(ID_Video, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(sentiment_score = positive - negative)

# Unir con vídeos
videos_enriched <- videos %>%
  left_join(comentarios_sentiment, by = "ID_Video")

# Correlación: ¿FOMO → sentimiento?
cor.test(videos_enriched$FOMO_Urgencia, videos_enriched$sentiment_score)


# Emociones por sector
comentarios_con_sector <- comentarios %>%
  left_join(videos %>% select(ID_Video, Sector), by = "ID_Video") %>%
  filter(!is.na(Texto_Comentario))

emociones <- get_nrc_sentiment(comentarios_con_sector$Texto_Comentario, language = "spanish")

comentarios_emotions <- comentarios_con_sector %>%
  select(Sector) %>%
  bind_cols(emociones) %>%
  group_by(Sector) %>%
  summarise(across(anger:positive, sum, .names = "{col}")) %>%
  pivot_longer(
    cols = anger:positive, 
    names_to = "sentiment", 
    values_to = "n"
  ) %>%
  group_by(Sector) %>%
  mutate(prop = round(n / sum(n), 4)) %>%
  ungroup() %>%
  # TRADUCCIÓN AL ESPAÑOL
  mutate(sentiment = case_when(
    sentiment == "anger"        ~ "Enfado",
    sentiment == "anticipation" ~ "Anticipación",
    sentiment == "disgust"      ~ "Asco",
    sentiment == "fear"         ~ "Miedo",
    sentiment == "joy"          ~ "Alegría",
    sentiment == "sadness"      ~ "Tristeza",
    sentiment == "surprise"     ~ "Sorpresa",
    sentiment == "trust"        ~ "Confianza",
    sentiment == "negativa"     ~ "Negativa",
    sentiment == "positive"     ~ "Positiva"
  ))

p_emociones <- comentarios_emotions %>%
  filter(!is.na(sentiment)) %>% 
  ggplot(aes(x = sentiment, y = prop, fill = Sector)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
  scale_fill_viridis_d(option = "C", name = "Sector") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribución de Emociones en Comentarios por Sector",
    subtitle = "Análisis de sentimiento multilingüe mediante el léxico NRC",
    x = "Dimensión Emocional", 
    y = "Proporción de Aparición (%)"
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

ggsave("figuras/TikTok_05_emociones_NRC.png", p_emociones, width = 10, height = 7, dpi = 300)

# Nube de palabras

stop_words_es <- tibble(word = stopwords("spanish"))

png("figuras/nube_editorial.png", width = 800, height = 600, res = 150)
comentarios_con_sector %>%
  filter(Sector == "Editorial") %>%
  unnest_tokens(word, Texto_Comentario) %>%
  anti_join(stop_words_es, by = "word") %>%
  count(word, sort = TRUE) %>%
  head(50) %>%
  with(wordcloud(word, n, max.words = 50, colors = brewer.pal(8, "Dark2")))
dev.off()

png("figuras/nube_blindbox.png", width = 800, height = 600, res = 150)
comentarios_con_sector %>%
  filter(Sector == "Blindbox") %>%
  unnest_tokens(word, Texto_Comentario) %>%
  anti_join(stop_words_es, by = "word") %>%
  count(word, sort = TRUE) %>%
  head(50) %>%
  with(wordcloud(word, n, max.words = 50, colors = brewer.pal(8, "Dark2")))
dev.off()

png("figuras/nube_popup.png", width = 800, height = 600, res = 150)
comentarios_con_sector %>%
  filter(Sector == "PopUp") %>%
  unnest_tokens(word, Texto_Comentario) %>%
  anti_join(stop_words_es, by = "word") %>%
  count(word, sort = TRUE) %>%
  head(50) %>%
  with(wordcloud(word, n, max.words = 50, colors = brewer.pal(8, "Dark2")))
dev.off()


stop_es <- c(stopwords("es"), "sí", "no", "ya", "muy", "tan", "pues", "bien", "mal", 
             "voy", "ir", "tener", "haber", "ser", "estar", "comprar", "compro", "compra")
stop_all <- unique(c(stop_es, stop_words_es))

bigramas <- comentarios %>%
  mutate(Texto_Limpio = str_to_lower(Texto_Comentario)) %>%
  unnest_tokens(bigram, Texto_Limpio, token = "ngrams", n = 2) %>%
  separate(bigram, c("w1", "w2"), sep = " ", remove = FALSE) %>%
  filter(!w1 %in% stop_all, !w2 %in% stop_all,
         str_detect(w1, "^[a-záéíóúüñ]+$"), str_detect(w2, "^[a-záéíóúüñ]+$"),
         !str_detect(w1, "\\d"), !str_detect(w2, "\\d")) %>%
  count(w1, w2, sort = TRUE) %>%
  filter(n >= 2)  

write.csv(bigramas, "Tablas/Tabla_bigramas_frecuentes.csv", 
          fileEncoding = "UTF-8", row.names = FALSE)

# Perfil por sector 
perfil_sector <- tabla_comparativa %>%
  select(Sector, FOMO_total, Escasez_total, Engagement_med) %>%
  mutate(across(c(FOMO_total, Escasez_total, Engagement_med), ~scale(.)))

# Función de recomendación 
recomendar_sector <- function(usuario_fomo, usuario_escasez, perfil_sector) {
  
  # Calcular medias y SD de referencia (del perfil de sectores)
  mean_fomo <- mean(perfil_sector$FOMO_total, na.rm = TRUE)
  sd_fomo <- sd(perfil_sector$FOMO_total, na.rm = TRUE)
  mean_esc <- mean(perfil_sector$Escasez_total, na.rm = TRUE)
  sd_esc <- sd(perfil_sector$Escasez_total, na.rm = TRUE)
  
  # Normalizar entrada del usuario
  usuario_fomo_norm <- (usuario_fomo - mean_fomo) / sd_fomo
  usuario_esc_norm <- (usuario_escasez - mean_esc) / sd_esc
  
  # Calcular distancia euclidiana a cada sector
  resultado <- perfil_sector %>%
    mutate(
      distancia = sqrt((FOMO_total - usuario_fomo_norm)^2 + (Escasez_total - usuario_esc_norm)^2),
      similitud = 1 / (1 + distancia),  # Similaridad inversa a distancia
      recomendacion = paste0(round(similitud * 100, 1), "% afinidad")
    ) %>%
    arrange(desc(similitud))
  
  return(resultado)
}

# Ejemplo de uso: usuario con alto FOMO (4/5) y media escasez (2.5/5)
usuario_ejemplo <- list(fomo = 4, escasez = 2.5)
recomendacion <- recomendar_sector(usuario_ejemplo$fomo, usuario_ejemplo$escasez, perfil_sector)

print(recomendacion %>% select(Sector, similitud, recomendacion))

recomendacion_limpio <- recomendacion %>%
  mutate(across(where(is.matrix), as.vector))

# Guardar el archivo a csv
write_excel_csv(recomendacion_limpio, "Tablas/Tabla_sistema_recomendacion.csv")


# Agregar métricas de TikTok por sector
tiktok_agg <- videos %>%
  group_by(Sector) %>%
  summarise(
    engagement_med = median(Ratio_Engagement),
    fomo_video = mean(FOMO_Urgencia + FOMO_Popularidad),
    escasez_video = mean(Escasez_Temporal + Escasez_Cuantitativa + Escasez_Simbolica),
    .groups = "drop"
  )

# Cruzar con comentarios: % intención de compra por sector
ewom_agg <- comentarios %>%
  left_join(videos %>% select(ID_Video, Sector), by = "ID_Video") %>%
  group_by(Sector) %>%
  summarise(pct_compra = mean(Intencion_Compra == "Sí") * 100, .groups = "drop")

# Unir todo
cruzado <- tiktok_agg %>%
  left_join(ewom_agg, by = "Sector")

write_excel_csv(cruzado, "Tablas/Tabla_cruzado_metricas_sectores.csv")

# Correlaciones clave
cat("CORRELACIONES CRUZADAS:")
cat("FOMO en vídeos ↔ % Compra en comentarios: r =", 
    round(cor(cruzado$fomo_video, cruzado$pct_compra), 3), "\n")
cat("Engagement ↔ % Compra en comentarios: r =", 
    round(cor(cruzado$engagement_med, cruzado$pct_compra), 3), "\n")

# Gráfico

p_fomo_compra <- ggplot(cruzado, aes(x = fomo_video, y = pct_compra, label = Sector)) +
  geom_point(aes(color = Sector), size = 4) + 
  geom_text(vjust = -1, size = 4) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "#231E91") + 
  scale_color_manual(values = c(
    "Blindbox" = "#231E91",  
    "Editorial" = "#D929AA",  
    "PopUp" = "#F1F734"   
  )) +
  labs(title = "FOMO en vídeos vs. Intención de compra en comentarios",
       x = "FOMO medio en vídeos (0-2)", y = "% Comentarios con intención de compra") +
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
      margin = margin(t = 18)
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

ggsave("figuras/TikTok_06_fomo_vs_compra_scatter.png", p_fomo_compra, width = 9, height = 7, dpi = 300)

