# ==========================================================
# GLOBAL.R — LIBRERÍAS, DATOS Y FUNCIONES COMUNES
# ==========================================================

# Carga las claves de .Renviron aunque R no se haya iniciado en esta carpeta
if (file.exists(".Renviron")) readRenviron(".Renviron")

library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(bslib)
library(caret)
library(pROC)
library(randomForest)
library(shinyWidgets)

# DATOS
ruta_datos <- "screen_time_mental_health.csv"
if (!file.exists(ruta_datos)) stop("No se encontró el archivo: ", ruta_datos)
df <- read.csv(ruta_datos, stringsAsFactors = FALSE)

# ENLACES OPCIONALES: escribe las URL en .Renviron o usa Sys.setenv()
libro_url <- Sys.getenv("JUPYTER_BOOK_URL", unset = "https://sjimenezurrea-sudo.github.io/rbook_Dataviz2026/")
if (!nzchar(libro_url)) libro_url <- "https://sjimenezurrea-sudo.github.io/rbook_Dataviz2026/"
github_url <- Sys.getenv("GITHUB_REPO_URL", unset = "")
if (!nzchar(github_url)) github_url <- NULL
kaggle_url <- Sys.getenv("KAGGLE_DATASET_URL", unset = "")
if (!nzchar(kaggle_url)) kaggle_url <- NULL

# PALETA
paleta <- list(
  cuadricula = "#E8E3DC", superficie = "#FFFFFF",
  azul = "#A3C3E5", terracota = "#EFAC91",
  chicos = "#9AC7BD", chicas = "#D6A5C3"
)

# VARIABLES
variables_eda <- c(
  "screen_time_index", "est_leisure_screen_hours",
  "sleep_quality_index", "avg_sleep_hours",
  "midsleep_weekend_hours", "social_jetlag_hours", "bdi_total"
)
faltantes <- setdiff(c(variables_eda, "sex", "depressed", "subject_id"), names(df))
if (length(faltantes)) stop("Columnas ausentes: ", paste(faltantes, collapse = ", "))

# ESTADÍSTICA
estadisticos_variable <- function(x) {
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3 || sd(x) == 0) {
    return(c(media = NA_real_, mediana = NA_real_, sd = NA_real_, asimetria = NA_real_))
  }
  m3 <- mean((x - mean(x))^3)
  m2 <- mean((x - mean(x))^2)
  asimetria <- sqrt(n * (n - 1)) / (n - 2) * m3 / m2^(3/2)
  c(media = mean(x), mediana = median(x), sd = sd(x), asimetria = asimetria)
}

# OUTLIERS
calcular_outliers <- function(x) {
  validos <- x[is.finite(x)]
  if (length(validos) == 0) {
    return(list(q1=NA_real_,q3=NA_real_,riq=NA_real_,lim_inf=NA_real_,lim_sup=NA_real_,indicador=rep(FALSE,length(x))))
  }
  q1 <- unname(quantile(validos, 0.25))
  q3 <- unname(quantile(validos, 0.75))
  riq <- q3 - q1
  lim_inf <- q1 - 1.5 * riq
  lim_sup <- q3 + 1.5 * riq
  list(q1=q1,q3=q3,riq=riq,lim_inf=lim_inf,lim_sup=lim_sup,
       indicador=!is.na(x) & (x < lim_inf | x > lim_sup))
}

# ============================================
# 5. MODELADO - CARGAR FUNCIONES
# ============================================

# Configuración, variables y métricas
source(
  "R/modelado_config.R",
  encoding = "UTF-8"
)

# Interfaz de las cinco pestañas
source(
  "R/modelado_ui.R",
  encoding = "UTF-8"
)

# Funciones del servidor
source(
  "R/modelado_server.R",
  encoding = "UTF-8"
)


# ================================================
# 6. SHAPIRO - ASISTENTE DE AYUDA
# ================================================

prompt_ayuda <- paste(
  "Eres Shapiro, el asistente de ciencia de datos",
  "del dashboard Pantallas, sueño y salud mental.",
  "Responde en español con claridad y rigor estadístico.",
  "Ayuda a interpretar gráficos, correlaciones,",
  "clasificación, regresión y modelos predictivos.",
  "El estudio contiene 4810 adolescentes.",
  "La variable depressed usa el umbral BDI-II >= 14.",
  "No confundas asociación con causalidad.",
  "No presentes el BDI-II como diagnóstico clínico.",
  "No inventes resultados ni métricas.",
  sep = "\n"
)

crear_agente <- function() {

  # Primera opción: Groq
  if (nzchar(Sys.getenv("GROQ_API_KEY"))) {
    return(ellmer::chat_groq(
      model = "openai/gpt-oss-20b",
      system_prompt = prompt_ayuda
    ))
  }

  # Segunda opción: Gemini
  if (nzchar(Sys.getenv("GEMINI_API_KEY")) ||
      nzchar(Sys.getenv("GOOGLE_API_KEY"))) {
    return(ellmer::chat_google_gemini(
      model = "gemini-3.1-flash-lite",
      system_prompt = prompt_ayuda
    ))
  }

  NULL
}
