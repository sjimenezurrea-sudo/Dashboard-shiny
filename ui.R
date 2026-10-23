library(shiny)
library(shinydashboard)   # ← IMPORTANTE: sin esto, dashboardPage no existe
# Definición de la Interfaz de Usuario (UI)
ui <- navbarPage(
  title = "Mi Aplicación", # Nombre de la app en la barra superior
  
  # Primera pestaña
  navbarMenu("Proyecto",
             tabPanel("Inicio"),
             tabPanel("Marco Teórico"),
             tabPanel("Objetivos"),
             tabPanel("Metodología")),
  
  # Primera pestaña
  navbarMenu("EDA",
             tabPanel("Calidad de los datos"),
             tabPanel("Variable objetivo"),
             tabPanel("Análisis por sexo"),
             tabPanel("Variables Númericas"),
             tabPanel("Correlaciones"),
             tabPanel("Outliers")),



  # Pestaña con menú desplegable (opcional)
  navbarMenu("Cierre",
             tabPanel("Hallazgos", h4("Contenido A")),
             tabPanel("Limitaciones", h4("Contenido B"))
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)
