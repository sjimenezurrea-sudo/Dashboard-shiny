
library(shiny)
library(ggplot2)

server <- function(input, output, session) {
  
  # =========================================
  # 1. NAVEGACIÓN DEL DASHBOARD
  # =========================================
  
  destinos <- c(
    link_inicio        = "inicio",
    link_marco         = "marco",
    link_objetivos     = "objetivos",
    link_metodologia   = "metodologia",
    link_calidad       = "calidad",
    link_objetivo      = "objetivo",
    link_sexo          = "sexo",
    link_numericas     = "numericas",
    link_correlaciones = "correlaciones",
    link_outliers      = "outliers",
    link_hallazgos     = "hallazgos",
    link_limitaciones  = "limitaciones"
  )
  
  # Conectar automáticamente los enlaces
  for (id in names(destinos)) {
    
    local({
      
      link_id <- id
      destino <- unname(destinos[[id]])
      
      observeEvent(
        input[[link_id]],
        {
          
          updateNavbarPage(
            session = session,
            inputId = "navegacion",
            selected = destino
          )
          
          # Mensaje temporal para comprobar clics
          showNotification(
            paste("Abriendo:", destino),
            type = "message",
            duration = 2
          )
          
        },
        ignoreInit = TRUE
      )
      
    })
    
  }
  
  
  # =========================================
  # 2. PROYECTO
  # =========================================
  
  # -----------------------------------------
  # 2.1 INICIO
  # -----------------------------------------
  
  output$grafico_depresion <- renderPlot({
    
    resumen <- data.frame(
      estado = c(
        "No deprimido",
        "Deprimido"
      ),
      cantidad = c(4023, 787)
    )
    
    resumen$estado <- factor(
      resumen$estado,
      levels = c(
        "No deprimido",
        "Deprimido"
      )
    )
    
    total <- sum(resumen$cantidad)
    
    porcentaje <- 100 * 787 / total
    
    etiqueta <- paste0(
      format(
        round(porcentaje, 1),
        decimal.mark = ",",
        nsmall = 1
      ),
      " %"
    )
    
    ggplot(
      resumen,
      aes(
        x = 2,
        y = cantidad,
        fill = estado
      )
    ) +
      
      geom_col(
        width = 0.7,
        color = "white"
      ) +
      
      coord_polar(theta = "y") +
      
      xlim(0.5, 2.5) +
      
      scale_fill_manual(
        values = c(
          "No deprimido" = "#A3C3E5",
          "Deprimido" = "#EFAC91"
        )
      ) +
      
      annotate(
        "text",
        x = 0.9,
        y = total / 2,
        label = etiqueta,
        size = 6,
        fontface = "bold"
      ) +
      
      theme_void() +
      
      theme(
        legend.position = "top",
        legend.title = element_blank()
      )
    
  })
  
  
  # -----------------------------------------
  # 2.2 MARCO TEÓRICO
  # -----------------------------------------
  
  # Reservado para cálculos dinámicos
  
  
  # -----------------------------------------
  # 2.3 OBJETIVOS
  # -----------------------------------------
  
  # Reservado para cálculos dinámicos
  
  
  # -----------------------------------------
  # 2.4 METODOLOGÍA
  # -----------------------------------------
  
  # Reservado para cálculos dinámicos
  
  
  # =========================================
  # 3. EDA
  # =========================================
  
  # 3.1 Calidad de los datos
  
  
  # 3.2 Variable objetivo
  
  
  # 3.3 Análisis por sexo
  
  
  # 3.4 Variables numéricas
  
  
  # 3.5 Correlaciones
  
  
  # 3.6 Outliers
  
  
  # =========================================
  # 4. CIERRE
  # =========================================
  
  # 4.1 Hallazgos
  
  
  # 4.2 Limitaciones
  
}

