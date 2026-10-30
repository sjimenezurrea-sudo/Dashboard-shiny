# ==========================================================
# GLOBAL.R — LIBRERÍAS, DATOS Y FUNCIONES COMUNES
# ==========================================================
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

# ENLACE AL LIBRO
libro_url <- Sys.getenv("JUPYTER_BOOK_URL", unset = "")
if (!nzchar(libro_url)) libro_url <- NULL

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
