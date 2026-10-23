library(shiny)
library(tidyverse)
library(DT) # Para tablas interactivas
library(caret) # Para el entrenamiento del modelo
library(plotly) # Para gráficos interactivos
library(pROC) # Para la curva ROC
library(randomForest) # Requerido por caret para el método 'rf'


df <- read.csv("screen_time_mental_health.csv")


