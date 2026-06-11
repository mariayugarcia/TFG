# 📊 El impacto del FOMO y las estrategias de escasez en la compra impulsiva en TikTok: un análisis comparativo entre sectores
**Autor:** María Yu García Muñoz  
**Trabajo:** Trabajo de Fin de Grado (TFG) – Grado en Inteligencia y Analítica de Negocios (BIA) – Universitat de València  
**Fecha:** Junio 2026


## 📖 Descripción
Este proyecto analiza el efecto del **FOMO (Fear Of Missing Out)** y las **estrategias de escasez** (temporal, cuantitativa y simbólica) sobre la **intención de compra impulsiva** en TikTok, comparando tres sectores de alto impacto: **Editorial (BookTok)**, **Blindbox (Labubu)** y **PopUp Retail**.

El estudio combina dos fuentes de datos complementarias: encuestas a consumidores (bilingüe español/inglés, n=62) para medir constructos psicológicos, y análisis de contenido de TikTok (n = 30 vídeos + n = 150 comentarios) para evaluar métricas de engagement, tono emocional (eWOM) y patrones de sentimiento mediante técnicas de NLP.


## 🏗️ Estructura del Repositorio

```
📦 TFG
├── 📄 README.md
├── 📂 scripts/
│   ├── 01_analisis_encuestas.R      # Procesamiento de cuestionarios y modelos estadísticos
│   └── 02_analisis_tiktok.R         # Análisis de vídeos, comentarios y minería de texto
├── 📂 data/                          # Datos originales (input)
│   ├── TFG Form.csv                 # Encuesta en inglés
│   ├── Cuestionario TFG.csv         # Encuesta en español
│   ├── Videos_TikTok.xlsx           # Métricas y codificación de los 30 vídeos
│   └── Comentarios_traducidos.xlsx  # Comentarios analizados con codificación eWOM
├── 📂 csv/                           # Datasets limpios
│   ├── df_combined_limpio.csv
│   ├── df_regresion_limpio.csv
│   └── df_logit_limpio.csv
├── 📂 tablas/                        # Tablas finales exportadas para el documento
└── 📂 figuras/                       # Visualizaciones generadas (PNG, 300 DPI)

```


## 🔧 Metodología

El análisis se ha estructurado en tres etapas secuenciales:

### 1. Procesamiento de Encuestas y Constructos Psicológicos (01)
**Fuentes:** Cuestionarios bilingües (español/inglés) con 62 participantes.  
**Proceso:** Unificación de categorías demográficas, conversión de escalas Likert a valores numéricos, cálculo de puntuaciones compuestas para FOMO, percepción de escasez y compra impulsiva. 
**Output:** `df_combined_limpio.csv` (dataset unificado y estandarizado).

### 2. Análisis Estadístico Inferencial (01)
**Técnicas:** Contrastes de medias (t-test de Student con corrección de Bonferroni), regresión lineal múltiple, regresión logística binomial, análisis de varianza (ANOVA) y análisis de correlación de Pearson.  
**Objetivo:** Evaluar diferencias entre grupos de idioma, predecir la compra impulsiva en función de FOMO y escasez, y estimar probabilidades de compra mediante modelos logísticos.

**Output:** Tablas de coeficientes de regresión, resultados ANOVA, y figuras de regresión y probabilidad predicha.

### 3. Análisis de Contenido de TikTok y NLP (02)
**Escala:** 30 vídeos (10 por sector) + 150 comentarios analizados.  
**Técnicas:** Cálculo de ratios de engagement, análisis de sentimiento basado en léxicos (Bing Liu, NRC Emotion Lexicon), extracción de bigramas, nubes de palabras, e implementación de un sistema de recomendación de sectores basado en distancia euclidiana.  
**Métricas:** Ratio de engagement (likes + comentarios + compartidos / vistas × 100), distribución emocional por sector, e intención de compra en comentarios.


## 📂 Fuentes de Datos

- **Encuestas:** Cuestionarios ad hoc distribuidos en español e inglés, diseñados para medir constructos psicológicos mediante escalas Likert validadas.
- **TikTok:** Datos extraídos de 30 vídeos virales (10 por sector: Editorial, Blindbox, PopUp) y 150 comentarios analizados manualmente y mediante técnicas de NLP.
- **Léxicos de sentimiento:** Bing Liu Sentiment Lexicon y NRC Emotion Lexicon para análisis de tono y emociones en comentarios.

## 🛠️ Requisitos Técnicos

El análisis ha sido desarrollado en **R (≥ 4.3.0)**. Para ejecutar los scripts de forma reproducible:

```r
# Librerías principales
install.packages(c("tidyverse", "ggplot2", "patchwork", "scales", 
                   "ggthemes", "broom", "car", "ggrepel", "readxl", 
                   "writexl", "pROC"))

# Librerías para NLP y análisis de texto
install.packages(c("tidytext", "textdata", "syuzhet", "sentimentr",
                   "wordcloud", "wordcloud2", "stopwords", "quanteda",
                   "topicmodels", "htmlwidgets", "webshot"))
```

## 📈 Resultados Principales

Los scripts generan automáticamente todos los outputs presentados en el Capítulo 4 de la memoria:

| Output | Descripción |
|--------|-------------|
| **Tabla 3.1** | Variables del estudio, dimensiones e instrumentos de medición |
| **Tabla 4.1** | Métricas comparativas de engagement, FOMO y escasez por sector |
| **Tabla 4.2** | Coeficientes del modelo de regresión lineal múltiple para la compra impulsiva |
| **Figura 4.1** | Relación entre FOMO medio y % de intención de compra en comentarios |
| **Figura 4.2** | Relación entre nivel de FOMO y compra impulsiva por idioma |
| **Figura 4.3** | Curva de probabilidad predicha de compra según nivel de FOMO |

---

## 📜 Licencia

Este repositorio ha sido desarrollado con fines exclusivamente académicos. El código fuente puede ser consultado y utilizado con fines de investigación y reproducción científica, citando debidamente la autoría del trabajo original.

---

*Este proyecto forma parte del Grado en Inteligencia y Analítica de Negocios (BIA) de la Universitat de València.*
```
