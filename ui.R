library(shiny)
library(bslib)

enlace_indice <- function(id, nombre) {
  
  tags$li(
    actionLink(
      inputId = id,
      label = nombre
    )
  )
  
}
encabezado_pagina <- function(seccion, pagina) {
  
  div(
    class = "encabezado-pagina",
    
    p(
      paste(seccion, "/", pagina),
      class = "ruta-pagina"
    ),
    
    h2(pagina)
  )
  
}
tarjeta_etapa <- function(numero, titulo, resumen, detalle) {
  
  div(
    class = "tarjeta-kpi tarjeta-etapa",
    
    span(numero, class = "numero-etapa"),
    
    h4(strong(titulo)),
    
    p(resumen, class = "descripcion-etapa"),
    
    tags$details(
      class = "detalle-etapa",
      
      tags$summary("Ver metodología"),
      
      p(detalle)
    )
  )
}
ui <- navbarPage(
  
  title = div(
    style = "display: flex; align-items: center; height: 100%; padding-top: 5px;",
    tags$img(
      src = "logo.png",
      height = "40px", 
      width = "auto",
      alt = "Logo Universidad del Norte"
    )
  ),

  id = "navegacion",
  
  # Tema de Bootstrap
  theme = bslib::bs_theme(
    version = 5
  ),
  
  header = tags$head(
    
    tags$link(
      rel = "stylesheet",
      href = "styles.css"
    )
    
  ),

  
  navbarMenu(
    "Proyecto",
    
    tabPanel(
      title = "Inicio",
      value = "inicio",
      
      
      encabezado_pagina("Proyecto","Inicio"),
        
      
      fluidRow(
        
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
      
      
      fluidRow(
        
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
        
        column(
          width = 4,
        
          div(
            class = "tarjeta-contenido",
            
            h4(
              "Estado depresivo en la muestra"
            ),
            
            p(
              "Proporción de adolescentes según ",
              "el corte clínico del BDI-II."
            ),
            
            plotly::plotlyOutput(
              outputId = "grafico_depresion_inicio",
              height = "320px",
              width = "100%"
            )
            
          ),
          
          div(
            class = "tarjeta-contenido indice-dashboard",
            
            h4(
              "Contenido del dashboard"
            ),
            
          #PROYECTO
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
      
      encabezado_pagina("Proyecto","Marco Teórico"),
      
        column(12, 
               div(
                 class = "tarjeta-kpi",
                 h4("Antecedentes"),
                 p("La preocupación de la salud mental referente al uso de pantallas sin control
                   no es novedad, pero ha obtenido mayor importancia en los ultimos años, esto debido
                   al aumento  en las tasas de depresión de la población.",
                   "Diversos estudios han intentado desentrañar esta relación, y aunque muchos encuentran asociaciones positivas,
                   el panorama es más complejo de lo que parece a simple vista. No todo el tiempo de pantalla es igual, 
                   no todos los adolescentes reaccionan igual, y el sueño parece ser una pieza clave que muchos análisis previos 
                   han tratado de manera demasiado simplificada."),
                 tags$ul(
                   enlace_indice ("link_estudios",
                   "Ver estudios y antecedentes científicos")
                    )
                 )
               ),
      h2("Base estadística del estudio"),
      
      withMathJax(
        fluidRow(
          
          #ESTADÍSTICA DESCRIPTIVA
          
          column(
            width = 6,
            
            div(
              class = "tarjeta-kpi formula-card",
              
              h4("1. Estadística descriptiva"),
              
              p("Tendencia central y dispersión"),
              
              div(
                class = "formula-box",
                
                "$$\\bar{x}=\\frac{1}{n}\\sum_{i=1}^{n}x_i$$"
              )
            )
          ),
          
          #VALORES ATÍPICOS
          
          column(
            width = 6,
            
            div(
              class = "tarjeta-kpi formula-card",
              
              h4("2. Detección de valores atípicos"),
              
              p("Método del rango intercuartílico"),
              
              div(
                class = "formula-box",
                
                "$$L_{sup}=Q_3+1.5\\,RIQ$$"
              )
            )
          )
        ),
        
        
        fluidRow(
          
          #VARIABLE OBJETIVO
          
          column(
            width = 6,
            
            div(
              class = "tarjeta-kpi formula-card",
              
              h4("3. Variable objetivo y prevalencia"),
              
              p("Clasificación mediante BDI-II ≥ 14"),
              
              div(
                class = "formula-box",
                
                "$$y_i=\\mathbb{1}(BDI_i\\geq14)$$"
              )
            )
          ),
          
          #CORRELACIONES
          
          column(
            width = 6,
            
            div(
              class = "tarjeta-kpi formula-card",
              
              h4("4. Correlaciones"),
              
              p("Coeficiente de correlación de Pearson"),
              
              div(
                class = "formula-box",
                
                "$$r_{xy}=\\frac{\\operatorname{Cov}(X,Y)}{\\sigma_X\\sigma_Y}$$"
              )
            )
          )
        ),
        

        fluidRow(
          
          #PRUEBAS DE HIPÓTESIS
          
          column(
            width = 6,
            
            div(
              class = "tarjeta-kpi formula-card",
              
              h4("5. Pruebas de hipótesis"),
              
              p("Contraste de hipótesis estadísticas"),
              
              div(
                class = "formula-box",
                
                "$$H_0\\quad\\text{vs.}\\quad H_1$$"
              )
            )
          ),
          
          # MÉTRICAS DE EVALUACIÓN
          
          column(
            width = 6,
            
            div(
              class = "tarjeta-kpi formula-card",
              
              h4("6. Métricas de evaluación"),
              
              p("Evaluación del desempeño predictivo"),
              
              div(
                class = "formula-box",
                
                "$$F_1=2\\frac{P\\cdot R}{P+R}$$"
              )
            )
          )
         )
        )
      ),
    
    
    # --------------------------------------------------------
    # 3.3 OBJETIVOS
    # --------------------------------------------------------
    
    tabPanel(
      title = "Objetivos",
      value = "objetivos",
      
      encabezado_pagina("Proyecto","Objetivos"),
      
      fluidRow(
        column(6, 
               div(
                 class = "tarjeta-kpi",
                 
                 p(
                   "Dominios del sueño",
                   class = "kpi-etiqueta"
                 ),
                 
                 h2(
                   "4",
                   class = "kpi-valor"
                 ),
                 
                 p(
                   "Analizados por separado",
                   class = "kpi-descripcion"
                 )
                 
               )
         ),
        column(6,
               div(
                 class = "tarjeta-kpi",
                 
                 p(
                   "Objetivos específicos",
                   class = "kpi-etiqueta"
                 ),
                 
                 h2(
                   "5",
                   class = "kpi-valor"
                 ),
                 
                 p(
                   "definidos en el proyecto",
                   class = "kpi-descripcion"
                 )
                 
               )
          ),
        column(
          width = 12,
          
          div(
            class = "tarjeta-contenido",
            
            h4("Objetivo general"),
            
            p(
              "Analizar y modelar, mediante técnicas de análisis ",
              "exploratorio de datos y aprendizaje automático ",
              "respectivamente, la relación entre el tiempo de pantalla, ",
              "los cuatro dominios del sueño (calidad, duración, ",
              "cronotipo y jet lag social) y la severidad de los síntomas ",
              "depresivos (BDI-II) en una muestra de adolescentes suecos, ",
              "con el fin de identificar los predictores de mayor peso ",
              "en el riesgo depresivo y evaluar si un enfoque predictivo ",
              "confirma los patrones de mediación reportados en el ",
              "estudio original (Hökby et al., 2025)."
            )
          )
        ),
        column(12,
               
        div(
          class = "tarjeta-contenido objetivos",
          
          h4(strong("Objetivos específicos")),
          
          # OBJETIVO 01
          div(
            class = "objetivo-item",
            
            span("01", class = "objetivo-numero"),
            
            div(
              class = "objetivo-texto",
              
              p(
                "Mediante un análisis exploratorio de datos (EDA) para caracterizar la distribución del tiempo 
                de pantalla, de los cuatro dominios de sueño (SQI, WASD, cronotipo, jet lag social) y del puntaje
                BDI-II (total), incluyendo la proporción de casos sobre el punto de corte clínico de depresión 
                leve (>13 puntos)."
              ),
              
              div(
                class = "objetivo-enlaces",
                
                span("EDA", class = "etiqueta-eda"),
                
                span("Ver: "),
                
                actionLink(
                  "obj_variable",
                  "Variable objetivo"
                ),
                
                span(", "),
                
                actionLink(
                  "obj_numericas",
                  "Variables numéricas"
                ),
                
                span(", "),
                
                actionLink(
                  "obj_calidad",
                  "Calidad de datos"
                )
              )
            )
          ),
          
          tags$hr(),
          
          # OBJETIVO 02
          div(
            class = "objetivo-item",
            
            span("02", class = "objetivo-numero"),
            
            div(
              class = "objetivo-texto",
              
              p(
                "Evaluar la asociación estadística entre el tiempo de pantalla y 
                cada uno de los cuatro dominios de sueño por separado, 
                en lugar de tratar `sueño` como una variable única."
              ),
              
              div(
                class = "objetivo-enlaces",
                
                span("EDA", class = "etiqueta-eda"),
                
                span("Ver: "),
                
                actionLink(
                  "obj_correlaciones",
                  "Correlaciones"
                )
              )
            )
          ),
          
          tags$hr(),
          
          # OBJETIVO 03
          div(
            class = "objetivo-item",
            
            span("03", class = "objetivo-numero"),
            
            div(
              class = "objetivo-texto",
              
              p(
                "Analizar las diferencias por género en las variables de pantalla, sueño y depresión."
              ),
              
              div(
                class = "objetivo-enlaces",
                
                span("EDA", class = "etiqueta-eda"),
                
                span("Ver: "),
                
                actionLink(
                  "obj_sexo",
                  "Análisis por sexo"
                )
              )
            )
          ),
          
          tags$hr(),
          
          # OBJETIVO 04
          div(
            class = "objetivo-item",
            
            span("04", class = "objetivo-numero"),
            
            div(
              class = "objetivo-texto",
              
              p(
                "Comparar modelos de machine learning (p. ej. regresión logística, random forest, XGBoost) 
                para la predicción el nivel de riesgo depresivo (target definido en el dataset) a partir de 
                variables de pantalla, sueño y demográficas. Adicionalmente se plantea el entrenamiento y 
                evaluacion de modelos de forma separada por género para contrastar con el enfoque multigrupo 
                del estudio original (Hökby et al., 2025)."
              ),
              
              span(
                "Fase de modelado",
                class = "etiqueta-eda"
              )
            )
          ),
          
          tags$hr(),
          
          # OBJETIVO 05
          div(
            class = "objetivo-item",
            
            span("05", class = "objetivo-numero"),
            
            div(
              class = "objetivo-texto",
              
              p(
                "Identificar mediante técnicas de importancia de variables si el tiempo en pantalla
                y el sueño emerge como el predictores más relevantes."
              ),
              
              span(
                "Fase de modelado",
                class = "etiqueta-eda"
              )
            )
          )
        )
      )         
    )),
    
    
    # --------------------------------------------------------
    # 3.4 METODOLOGÍA
    # --------------------------------------------------------
    
    tabPanel(
      title = "Metodología",
      value = "metodologia",
      
      encabezado_pagina("Proyecto","Metodología"),
      
      fluidRow(
        
        column(
          width = 4,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Registros",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "4.810",
              class = "kpi-valor"
            ),
            
            p(
              "Un adolescente por fila",
              class = "kpi-descripcion"
            )
            
          )
        ),
        
        
        column(
          width = 4,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Variables",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "10",
              class = "kpi-valor"
            ),
            
            p(
              "2 objetivos, 1 identificador",
              class = "kpi-descripcion"
            )
            
          )
        ),
        
        column(
          width = 4,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Predictores continuos",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "6",
              class = "kpi-valor"
            ),
            
            p(
              "2 de pantalla, 4 de sueño",
              class = "kpi-descripcion"
            )
            
          )
        ),
      ),
      
      h6("A continucación se presenta la metodología que se seguirá en este proyecto"),
      fluidRow(
        
        column(
          width = 6,
          
          tarjeta_etapa(
            "02",
            "Análisis exploratorio de datos",
            "Distribuciones, diferencias por género y correlaciones.",
            paste0(
              "Análisis descriptivo de cada variable y ",
              "comparaciones por género. Se estudiarán ",
              "las variables objetivo depressed y bdi_total, ",
              "la matriz de correlaciones y las pruebas ",
              "estadísticas correspondientes."
            )
          )
        ),
        
        column(
          width = 6,
          
          tarjeta_etapa(
            "03",
            "Preprocesamiento",
            "División de datos, estandarización y balance de clases.",
            paste0(
              "Separación de entrenamiento y prueba. ",
              "Estandarización de predictores continuos ",
              "y tratamiento del desbalance mediante ",
              "SMOTE o class_weight='balanced', ",
              "aplicados exclusivamente al entrenamiento ",
              "para evitar fugas de información."
            )
          )
        ),
        
        column(
          width = 6,
          
          tarjeta_etapa(
            "04",
            "Modelado",
            "Algoritmos predictivos y validación cruzada.",
            paste0(
              "Entrenamiento de modelos de regresión y ",
              "clasificación con tres configuraciones: ",
              "conjunto, chicos y chicas. Optimización ",
              "mediante GridSearchCV o RandomizedSearchCV ",
              "y validación cruzada de cinco particiones. ",
              "Se utilizarán RMSE para regresión y ",
              "PR-AUC para clasificación."
            )
          )
        ),
        
        column(
          width = 6,
          
          tarjeta_etapa(
            "05",
            "Evaluación",
            "Métricas de rendimiento y análisis de errores.",
            paste0(
              "Evaluación de modelos de clasificación ",
              "mediante AUC-ROC, precisión, recall y F1, ",
              "junto con la matriz de confusión y la ",
              "curva Precision-Recall."
            )
          )
        ),
        
        column(
          width = 6,
          
          tarjeta_etapa(
            "06",
            "Interpretabilidad",
            "Importancia de variables y comparación por género.",
            paste0(
              "Se analizará la importancia de variables ",
              "de Random Forest y XGBoost, prestando ",
              "especial atención a sleep_quality_index. ",
              "Se contrastará la jerarquía de predictores ",
              "entre los modelos de chicos y chicas."
            )
          )
        ),
        
        column(
          width = 6,
          
          tarjeta_etapa(
            "07",
            "Herramientas",
            "R y librerías de análisis y modelado",
            paste0(
              "Desarrollo en R con tidyverse para",
              "procesamiento, uso de caret para entrenamiento",
              "de modelos y algoritmo de random forest."
            )
          )
        )
        
      )
      
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
      
      encabezado_pagina("Proyecto","Calidad de los datos"),
      fluidRow(
        
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Filas",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "4.810",
              class = "kpi-valor"
            ),
            
            p(
              "Un registro por adolescente",
              class = "kpi-descripcion"
            )
            
          )
        ),
        
        
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Columnas",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "10",
              class = "kpi-valor"
            ),
            
            p(
              class = "kpi-descripcion",
              "7 numéricas de análisis",
            )
            
          )
        ),
        
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p(
              "Valores nulos",
              class = "kpi-etiqueta"
            ),
            
            h2(
              "0",
              class = "kpi-valor"
            ),
            
            p("en todas las columnas",
              class = "kpi-descripcion"
            )
            
          )
        ),
        column(width=3,
               div(
                 class = "tarjeta-kpi",
                 p(
                   "Duplicados en subject_id",
                   class = "kpi-etiqueta"
                 ),
                 h2("0",
                    class = "kpi-valor"
                    ),
                 p(
                   "0 filas duplicadas completas",
                   class = "kpi-descripcion"
                 )
               )
             ),
        column(8,
               div(
                 class = "tarjeta-contenido",
                 h4("Estadística Descriptiva"),
                 h6("Varibales numéricas de análisis; subject_id y depressed se excluyen."),
                 DT::DTOutput("tabla_descriptiva")
                 
                )
               ),
        column(4,
               div(
                 class ="tarjeta-contenido",
                 h4("Tipos,valores únicos y únicos "),
                 DT::DTOutput("tabla_tipos")
               )
               
               ),
        column(
          width = 8,
          
          div(
            class = "tarjeta-contenido",
            
            h4("Diagnóstico inicial de calidad"),
            
            # INTEGRIDAD
            h5(
              icon("check-circle"),
              strong(" Integridad estructural")
            ),
            
            p(
              "El dataset contiene 4.810 registros y ",
              "10 variables. No se identificaron ",
              "valores nulos ni identificadores ",
              "duplicados en subject_id."
            ),
            
            tags$hr(),
            
            # DURACIÓN DEL SUEÑO
            h5(
              icon("exclamation-triangle"),
              strong(" Duración del sueño")
            ),
            
            p(
              "La variable avg_sleep_hours presenta ",
              "una media de 7,69 h y asimetría ",
              "negativa (-0,92). Su mínimo de ",
              "1,36 h requiere revisión por ",
              "su valor extremo."
            ),
            
            tags$hr(),
            
            # JET LAG SOCIAL
            h5(
              icon("search"),
              strong(" Jet lag social")
            ),
            
            p(
              "social_jetlag_hours presenta un mínimo ",
              "de -3,03 h. Se verificará si este ",
              "resultado es consistente con la ",
              "definición de la variable."
            ),
            
            tags$hr(),
            
            # ASIMETRÍA
            h5(
              icon("bar-chart"),
              strong(" Distribuciones asimétricas")
            ),
            
            p(
              "sleep_quality_index (skew = 1,47) ",
              "y est_leisure_screen_hours ",
              "(skew = 0,87) muestran asimetría ",
              "positiva. Se recomienda complementar ",
              "la media con mediana y cuartiles."
            )
          )
        ),
        column(
          width = 4,
          div(
            class = "tarjeta-kpi",
            h4("Valores extremos"),
            p(
              "Los valores extremos se revisan en detalle en la sección ",
              
              actionLink(
                inputId = "link_outliers",
                label = "Outliers"
              ),
              
              ". Aquí solo se verifica que estén dentro ",
              "del rango válido de cada escala."
            )
            
          )    
        )
      )
      
    ),
    
    
    # 4.2 VARIABLE OBJETIVO
    
    tabPanel(
      title = "Variable objetivo",
      value = "objetivo",
      
      encabezado_pagina("Proyecto","Variable objetivo"),
      
      
      fluidRow(
        
        # KPI 1: DEPRIMIDOS
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p("Deprimidos", class = "kpi-etiqueta"),
            
            h2(
              "16,4 %",
              class = "kpi-valor kpi-terracota"
            ),
            
            p(
              "787 adolescentes con BDI-II ≥ 14",
              class = "kpi-descripcion"
            )
          )
        ),
        
        # KPI 2: NO DEPRIMIDOS
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p("No deprimidos", class = "kpi-etiqueta"),
            
            h2(
              "83,6 %",
              class = "kpi-valor kpi-azul"
            ),
            
            p(
              "4.023 adolescentes",
              class = "kpi-descripcion"
            )
          )
        ),
        
        # KPI 3: MEDIANA
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p("Mediana de BDI-II", class = "kpi-etiqueta"),
            
            h2(
              "5",
              class = "kpi-valor kpi-oscuro"
            ),
            
            p(
              "Media 7,27",
              class = "kpi-descripcion"
            )
          )
        ),
        
        # KPI 4: ASIMETRÍA
        column(
          width = 3,
          
          div(
            class = "tarjeta-kpi",
            
            p("Asimetría de BDI-II", class = "kpi-etiqueta"),
            
            h2(
              "1,89",
              class = "kpi-valor kpi-oscuro"
            ),
            
            p(
              "Positiva: cola hacia la derecha",
              class = "kpi-descripcion"
            )
          )
        )
      ),
      
      
      fluidRow(
        
        # COLUMNA IZQUIERDA
        column(
          width = 8,
          
          div(
            class = "tarjeta-contenido",
            
            h4("Distribución del puntaje BDI-II"),
            
            plotly::plotlyOutput(
              "grafico_bdi",
              height = "540px"
            )
          )
        ),
        
        # COLUMNA DERECHA
        column(
          width = 4,
          
          # GRÁFICO DE DONA
          div(
            class = "tarjeta-contenido",
            
            h4("Estado depresivo"),
            
            plotly::plotlyOutput(
              "grafico_depresion_objetivo",
              height = "300px"
            )
          ),
          
          # INTERPRETACIÓN DEBAJO
          div(
            class = "tarjeta-interpretacion",
            
            h5("Interpretación"),
            
            p(
              tags$code("depressed"),
              " es una simplificación de ",
              tags$code("bdi_total"),
              " mediante el corte clínico ≥ 14. ",
              "Al binarizar se pierde granularidad, ",
              "pero se gana interpretabilidad clínica. ",
              "El desbalance de clases (16,4 % de ",
              "deprimidos) sugiere evaluar estrategias ",
              "de balanceo durante el modelado."
            )
          )
        )
      ),
      fluidRow(
        column(
          width = 8,
          
          div(
            class = "tarjeta-contenido",
            
            h4("Dispersión de BDI-II"),
            
            p("Mediana punteada y corte clínico discontinuo."),
            
            plotly::plotlyOutput(
              outputId = "grafico_boxplot_bdi",
              height = "350px",
              width = "100%"
            )
          )
        ),
        
        column(
          width = 4,
          
          div(
            class = "tarjeta-interpretacion",
            
            h5("Interpretación"),
            
            p(
              tags$code("bdi_total"),
              
              " presenta una distribución fuertemente ",
              "sesgada a la derecha (skew de 1,89, ",
              "media 7,27 y desviación estándar de 7,70). ",
              "La mayoría de los adolescentes reporta ",
              "síntomas mínimos o nulos; el 50 % presenta ",
              "un puntaje menor o igual a 5. Una minoría ",
              "concentra puntajes elevados, generando ",
              "la cola larga y los valores atípicos ",
              "observados en el boxplot."
            )
          )
        )
        
      )
      
      
      
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
    
  ),
  
  navbarMenu(
    "Estudios",
    tabPanel(
      title = "Estudios",
      value = "estudios",
      
      encabezado_pagina("Proyecto","Estudios")
    
            
    )
  )
)


