
# ============================================================
# UI.R - DASHBOARD BDI
# ============================================================

library(shiny)
library(bslib)


# ============================================================
# 1. FUNCIÓN PARA ENLACES DEL ÍNDICE
# ============================================================

enlace_indice <- function(id, nombre) {
  
  tags$li(
    actionLink(
      inputId = id,
      label = nombre
    )
  )
  
}


# ============================================================
# 2. INTERFAZ PRINCIPAL
# ============================================================

ui <- navbarPage(
  
  # ----------------------------------------------------------
  # LOGO Y TÍTULO
  # ----------------------------------------------------------
  
  title = div(
    style = "display: flex; align-items: center; height: 100%; padding-top: 5px;",
    tags$img(
      src = "logo.png",
      height = "40px", # Ajusta esta altura si lo quieres más grande o pequeño
      width = "auto",
      alt = "Logo Universidad del Norte"
    )
  ),
  # Identificador de la navegación
  id = "navegacion",
  
  # Tema de Bootstrap
  theme = bslib::bs_theme(
    version = 5
  ),
  
  # Archivo CSS
  header = tags$head(
    
    tags$link(
      rel = "stylesheet",
      href = "styles.css"
    )
    
  ),
  
  
  # ==========================================================
  # 3. PROYECTO
  # ==========================================================
  
  navbarMenu(
    "Proyecto",
    
    
    # --------------------------------------------------------
    # 3.1 INICIO
    # --------------------------------------------------------
    
    tabPanel(
      title = "Inicio",
      value = "inicio",
      
      
      # ENCABEZADO DE LA PÁGINA
      
      div(
        class = "encabezado-pagina",
        
        p(
          "Proyecto / Inicio",
          class = "ruta-pagina"
        ),
        
        h2("Inicio")
      ),
      
      
      # ------------------------------------------------------
      # FILA 1: CUATRO INDICADORES KPI
      # ------------------------------------------------------
      
      fluidRow(
        
        
        # KPI 1: ADOLESCENTES
        
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Adolescentes",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "4.810",
              class = "kpi-valor"
            ),
            
            p(
              "Participantes en el dataset",
              class = "kpi-descripcion"
            )
            
          )
        ),
        
        
        # KPI 2: RANGO DE EDAD
        
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Rango de edad",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "12–16",
              class = "kpi-valor"
            ),
            
            p(
              "Edad de los participantes",
              class = "kpi-descripcion"
            )
            
          )
        ),
        
        
        # KPI 3: DISTRIBUCIÓN POR SEXO
        
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Chicas / chicos",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "49 % / 51 %",
              class = "kpi-valor"
            ),
            
            p(
              "Distribución por sexo",
              class = "kpi-descripcion"
            )
            
          )
        ),
        
        
        # KPI 4: DEPRESIÓN
        
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Deprimidos",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "16,4 %",
              class = "kpi-valor kpi-alerta"
            ),
            
            p(
              "787 adolescentes con BDI-II ≥ 14",
              class = "kpi-descripcion"
            )
            
          )
        )
        
      ),
      
      
      # ------------------------------------------------------
      # FILA 2: CONTEXTO + GRÁFICO + ÍNDICE
      # ------------------------------------------------------
      
      fluidRow(
        
        
        # ====================================================
        # COLUMNA IZQUIERDA: CONTEXTO DEL PROYECTO
        # ====================================================
        
        column(
          width = 8,
          
          div(
            class = "tarjeta-contenido",
            
            h4(
              "Contexto del proyecto"
            ),
            
            p(
              "El uso de pantallas (redes sociales, ",
              "videojuegos, streaming, mensajería) ",
              "se ha vuelto una actividad central ",
              "en la vida de los adolescentes, ",
              "y un cuerpo creciente de investigación ",
              "asocia un mayor tiempo de pantalla ",
              "con síntomas depresivos, peor calidad ",
              "de sueño y menor bienestar psicológico."
            ),
            
            p(
              "Sin embargo, la relación no es simple ",
              "ni unidireccional: el sueño parece ",
              "actuar como mediador clave entre ",
              "el tiempo de pantalla y la depresión, ",
              "y el tamaño del efecto varía según ",
              "el tipo de uso, la plataforma y ",
              "el diseño del estudio."
            ),
            
            p(
              "Este proyecto busca aprovechar un ",
              "dataset de 4.810 adolescentes suecos ",
              "de entre 12 y 16 años, participantes ",
              "en un estudio prospectivo realizado ",
              "en centros escolares de Estocolmo ",
              "para explorar y modelar esta relación ",
              "con métodos de ciencia de datos."
            )
            
          )
          
        ),
        
        
        # ====================================================
        # COLUMNA DERECHA: GRÁFICO E ÍNDICE
        # ====================================================
        
        column(
          width = 4,
          
          
          # --------------------------------------------------
          # GRÁFICO DE DEPRESIÓN
          # --------------------------------------------------
          
          div(
            class = "tarjeta-contenido",
            
            h4(
              "Estado depresivo en la muestra"
            ),
            
            p(
              "Proporción de adolescentes según ",
              "el corte clínico del BDI-II."
            ),
            
            plotOutput(
              outputId = "grafico_depresion",
              height = "270px"
            )
            
          ),
          
          
          # --------------------------------------------------
          # TABLA DE CONTENIDO
          # --------------------------------------------------
          
          div(
            class = "tarjeta-contenido indice-dashboard",
            
            h4(
              "Contenido del dashboard"
            ),
            
            
            # PROYECTO
            
            h5(
              "PROYECTO",
              class = "indice-categoria"
            ),
            
            tags$ul(
              
              enlace_indice(
                "link_inicio",
                "Inicio"
              ),
              
              enlace_indice(
                "link_marco",
                "Marco teórico"
              ),
              
              enlace_indice(
                "link_objetivos",
                "Objetivos"
              ),
              
              enlace_indice(
                "link_metodologia",
                "Metodología"
              )
              
            ),
            
            
            # EDA
            
            h5(
              "EDA",
              class = "indice-categoria"
            ),
            
            tags$ul(
              
              enlace_indice(
                "link_calidad",
                "Calidad de los datos"
              ),
              
              enlace_indice(
                "link_objetivo",
                "Variable objetivo"
              ),
              
              enlace_indice(
                "link_sexo",
                "Análisis por sexo"
              ),
              
              enlace_indice(
                "link_numericas",
                "Variables numéricas"
              ),
              
              enlace_indice(
                "link_correlaciones",
                "Correlaciones"
              ),
              
              enlace_indice(
                "link_outliers",
                "Outliers"
              )
              
            ),
            
            
            # CIERRE
            
            h5(
              "CIERRE",
              class = "indice-categoria"
            ),
            
            tags$ul(
              
              enlace_indice(
                "link_hallazgos",
                "Hallazgos"
              ),
              
              enlace_indice(
                "link_limitaciones",
                "Limitaciones"
              )
              
            )
            
          )
          
        )
        
      )
      
    ),
    
    
    # --------------------------------------------------------
    # 3.2 MARCO TEÓRICO
    # --------------------------------------------------------
    
    tabPanel(
      title = "Marco Teórico",
      value = "marco",
      
      h2("Marco teórico")
      
    ),
    
    
    # --------------------------------------------------------
    # 3.3 OBJETIVOS
    # --------------------------------------------------------
    
    tabPanel(
      title = "Objetivos",
      value = "objetivos",
      
      h2("Objetivos")
      
    ),
    
    
    # --------------------------------------------------------
    # 3.4 METODOLOGÍA
    # --------------------------------------------------------
    
    tabPanel(
      title = "Metodología",
      value = "metodologia",
      
      h2("Metodología")
      
    )
    
  ),
  
  
  # ==========================================================
  # 4. ANÁLISIS EXPLORATORIO DE DATOS (EDA)
  # ==========================================================
  
  navbarMenu(
    "EDA",
    
    
    # 4.1 CALIDAD DE LOS DATOS
    
    tabPanel(
      title = "Calidad de los datos",
      value = "calidad",
      
      h2("Calidad de los datos")
      
    ),
    
    
    # 4.2 VARIABLE OBJETIVO
    
    tabPanel(
      title = "Variable objetivo",
      value = "objetivo",
      
      h2("Variable objetivo")
      
    ),
    
    
    # 4.3 ANÁLISIS POR SEXO
    
    tabPanel(
      title = "Análisis por sexo",
      value = "sexo",
      
      h2("Análisis por sexo")
      
    ),
    
    
    # 4.4 VARIABLES NUMÉRICAS
    
    tabPanel(
      title = "Variables numéricas",
      value = "numericas",
      
      h2("Variables numéricas")
      
    ),
    
    
    # 4.5 CORRELACIONES
    
    tabPanel(
      title = "Correlaciones",
      value = "correlaciones",
      
      h2("Correlaciones")
      
    ),
    
    
    # 4.6 OUTLIERS
    
    tabPanel(
      title = "Outliers",
      value = "outliers",
      
      h2("Análisis de valores atípicos")
      
    )
    
  ),
  
  
  # ==========================================================
  # 5. CIERRE
  # ==========================================================
  
  navbarMenu(
    "Cierre",
    
    
    # 5.1 HALLAZGOS
    
    tabPanel(
      title = "Hallazgos",
      value = "hallazgos",
      
      h2("Hallazgos")
      
    ),
    
    
    # 5.2 LIMITACIONES
    
    tabPanel(
      title = "Limitaciones",
      value = "limitaciones",
      
      h2("Limitaciones")
      
    )
    
  )
  
)


