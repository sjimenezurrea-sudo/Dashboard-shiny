library(shiny)
library(tidyverse)
library(DT) # Para tablas interactivas
library(caret) # Para el entrenamiento del modelo
library(plotly) # Para gráficos interactivos
library(pROC) # Para la curva ROC
library(randomForest)
library(ggplot2)

server <- function(input, output, session) {
  
  # =========================================
  # 1. NAVEGACIÓN DEL DASHBOARD
  # =========================================
  
  destinos <- c(
    link_inicio        = "inicio",
    link_marco         = "marco",
    link_estudios      = "estudios",
    link_objetivos     = "objetivos",
    link_metodologia   = "metodologia",
    link_calidad       = "calidad",
    link_objetivo      = "objetivo",
    link_sexo          = "sexo",
    link_numericas     = "numericas",
    link_correlaciones = "correlaciones",
    link_outliers      = "outliers",
    link_hallazgos     = "hallazgos",
    link_limitaciones  = "limitaciones",
    obj_variable      = "objetivo",
    obj_numericas     = "numericas",
    obj_calidad       = "calidad",
    obj_correlaciones = "correlaciones",
    obj_sexo          = "sexo"
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
  
  
  
  # Función que construye el gráfico
  
  # ============================================
  # FUNCIÓN: GRÁFICO DE DONA INTERACTIVO
  # ============================================
  
  crear_grafico_depresion <- function() {
    
    # DATOS
    resumen <- data.frame(
      estado = c("No deprimido", "Deprimido"),
      cantidad = c(4023, 787)
    )
    
    # PORCENTAJE
    total <- sum(resumen$cantidad)
    
    porcentaje <- 100 *
      resumen$cantidad[resumen$estado == "Deprimido"] /
      total
    
    etiqueta <- paste0(
      format(
        round(porcentaje, 1),
        decimal.mark = ",",
        nsmall = 1
      ),
      " %"
    )
    
    # CONSTRUCCIÓN DEL GRÁFICO
    grafico <- plotly::plot_ly(
      data = resumen,
      
      labels = ~estado,
      values = ~cantidad,
      
      type = "pie",
      hole = 0.65,
      sort = FALSE,
      direction = "clockwise",
      
      # No mostrar números sobre las porciones
      textinfo = "none",
      
      # Tooltip interactivo
      hovertemplate = paste0(
        "<b>%{label}</b>",
        "<br>Adolescentes: %{value}",
        "<br>Porcentaje: %{percent:.1%}",
        "<extra></extra>"
      ),
      
      hoverlabel = list(
        bgcolor = "#FFFFFF",
        bordercolor = "#E7E1DC",
        font = list(
          color = "#202936",
          size = 12
        )
      ),
      
      # COLORES
      marker = list(
        colors = c(
          "#A3C3E5",
          "#EFAC91"
        ),
        line = list(
          color = "#FFFFFF",
          width = 2
        )
      ),
      
      showlegend = TRUE
    ) %>%
      
      plotly::layout(
        
        # TEXTO CENTRAL
        annotations = list(
          list(
            x = 0.5,
            y = 0.5,
            xref = "paper",
            yref = "paper",
            
            text = paste0(
              "<b>", etiqueta, "</b>",
              "<br><span style='font-size:12px'>",
              "deprimidos</span>"
            ),
            
            showarrow = FALSE,
            
            font = list(
              size = 23,
              color = "#202936"
            )
          )
        ),
        
        # LEYENDA HORIZONTAL SUPERIOR
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 1.05,
          yanchor = "bottom",
          font = list(
            size = 12,
            color = "#202936"
          )
        ),
        
        # ESPACIOS
        margin = list(
          l = 15,
          r = 15,
          t = 60,
          b = 10
        ),
        
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    # OCULTAR HERRAMIENTAS
    grafico <- plotly::config(
      grafico,
      displayModeBar = FALSE,
      responsive = TRUE
    )
    
    return(grafico)
  }
  
  
  # Dentro de server <- function(...) {...}
  
  output$grafico_depresion_inicio <- plotly::renderPlotly({
    crear_grafico_depresion()
  })
  
  output$grafico_depresion_objetivo <- plotly::renderPlotly({
    crear_grafico_depresion()
  })
  
  

  
  # =========================================
  # 3. EDA
  # =========================================
  
  # 3.1 Calidad de los datos
  tabla_num <- df %>%
    summarise(
      across(
        where(is.numeric) & !any_of(c("subject_id","depressed")),
        list(
          N       = ~sum(!is.na(.x)),
          Media   = ~mean(.x, na.rm = TRUE),
          Mediana = ~median(.x, na.rm = TRUE),
          SD      = ~sd(.x, na.rm = TRUE),
          Min     = ~min(.x, na.rm = TRUE),
          Q1      = ~quantile(.x, 0.25, na.rm = TRUE),
          Q3      = ~quantile(.x, 0.75, na.rm = TRUE),
          Max     = ~max(.x, na.rm = TRUE)
        )
      )
    ) %>%
    pivot_longer(
      everything(),
      names_to = c("Variable", ".value"),
      names_pattern = "(.+)_(N|Media|Mediana|SD|Min|Q1|Q3|Max)$"
    ) %>%
    mutate(
      across(
        where(is.numeric),
        ~round(.x, 2)
      )
    )
  
  
  output$tabla_descriptiva <- DT::renderDT({
    
    DT::datatable(
      tabla_num,
      
      rownames = FALSE,
      
      class = "display compact tabla-eda",
      
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        
        # Sin flechas de ordenación
        ordering = FALSE,
        
        # Ajuste de ancho
        scrollX = TRUE,
        autoWidth = FALSE,
        
        columnDefs = list(
          list(
            targets = 0,
            width = "245px",
            className = "dt-left"
          ),
          list(
            targets = 1:8,
            className = "dt-right"
          )
        )
      )
    ) %>%
      
      DT::formatRound(
        columns = c(
          "Media", "Mediana", "SD",
          "Min", "Q1", "Q3", "Max"
        ),
        digits = 2,
        mark = ".",
        dec.mark = ","
      ) %>%
      
      DT::formatRound(
        columns = "N",
        digits = 0,
        mark = "."
      )
    
  })
  
##tipos
  
  tipo_datos <- df %>%
    summarise(
      across(
        everything(),
        list(
          Tipo = ~class(.x)[1],
          Unicos = ~n_distinct(.x, na.rm = TRUE),
          Nulos = ~sum(is.na(.x))
        )
      )
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Variable", ".value"),
      names_pattern = "^(.*)_(Tipo|Unicos|Nulos)$"
    ) %>%
    mutate(
      Tipo = case_when(
        Tipo == "integer" ~ "Entero (int)",
        Tipo == "numeric" ~ "Numérico (dbl)",
        Tipo == "factor" ~ "Categórico (factor)",
        Tipo == "character" ~ "Texto (chr)",
        Tipo == "logical" ~ "Lógico (bool)",
        TRUE ~ Tipo
      )
    ) %>%
    rename(
      `Únicos` = Unicos
    )
  
  
  output$tabla_tipos <- DT::renderDT({
    
    DT::datatable(
      tipo_datos,
      
      rownames = FALSE,
      class = "compact hover tabla-eda",
      
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    ) %>%
      DT::formatRound(
        columns = c("Únicos", "Nulos"),
        digits = 0,
        mark = "."
      )
    
  })
  
  
  # 3.2 Variable objetivo
  
  ## Distribución interactiva BDI-II
  
  
  output$grafico_bdi <- plotly::renderPlotly({
    
    # 1. Preparar datos
    x <- df$bdi_total
    x <- x[is.finite(x)]
    
    req(length(x) > 1)
    
    # Los puntajes BDI-II son enteros
    puntajes <- 0:ceiling(max(x))
    
    frecuencias <- data.frame(
      puntaje = puntajes,
      frecuencia = tabulate(
        match(as.integer(x), puntajes),
        nbins = length(puntajes)
      )
    )
    
    # 2. Separar los dos grupos
    no_deprimidos <- subset(
      frecuencias,
      puntaje < 14
    )
    
    deprimidos <- subset(
      frecuencias,
      puntaje >= 14
    )
    
    # 3. Calcular mediana y densidad
    mediana_bdi <- median(x)
    
    densidad <- density(
      x,
      from = min(puntajes),
      to = max(puntajes),
      na.rm = TRUE
    )
    
    # Escalar densidad a frecuencias
    densidad_frecuencia <- densidad$y * length(x)
    
    altura <- max(
      frecuencias$frecuencia,
      densidad_frecuencia
    )
    
    # 4. Crear gráfico Plotly
    grafico <- plotly::plot_ly() %>%
      
      # Barras azules
      plotly::add_bars(
        data = no_deprimidos,
        x = ~puntaje,
        y = ~frecuencia,
        name = "No deprimido",
        marker = list(color = "#A3C3E5"),
        width = 0.94,
        hovertemplate = paste0(
          "Puntaje: %{x}",
          "<br>Adolescentes: %{y}",
          "<extra></extra>"
        )
      ) %>%
      
      # Barras terracota
      plotly::add_bars(
        data = deprimidos,
        x = ~puntaje,
        y = ~frecuencia,
        name = "Deprimido",
        marker = list(color = "#EFAC91"),
        width = 0.94,
        hovertemplate = paste0(
          "Puntaje: %{x}",
          "<br>Adolescentes: %{y}",
          "<extra></extra>"
        )
      ) %>%
      
      # Curva KDE
      plotly::add_lines(
        x = densidad$x,
        y = densidad_frecuencia,
        name = "Densidad (KDE)",
        line = list(
          color = "#67645F",
          width = 2.5
        ),
        hoverinfo = "skip"
      ) %>%
      
      # Línea de la mediana
      plotly::add_segments(
        x = mediana_bdi,
        xend = mediana_bdi,
        y = 0,
        yend = altura * 1.05,
        line = list(
          color = "#67645F",
          dash = "dot",
          width = 2
        ),
        showlegend = FALSE,
        inherit = FALSE,
        hoverinfo = "skip"
      ) %>%
      
      # Línea del corte clínico
      plotly::add_segments(
        x = 13.5,
        xend = 13.5,
        y = 0,
        yend = altura * 1.05,
        line = list(
          color = "#526785",
          dash = "dash",
          width = 2
        ),
        showlegend = FALSE,
        inherit = FALSE,
        hoverinfo = "skip"
      ) %>%
      
      # Diseño general
      plotly::layout(
        barmode = "overlay",
        
        xaxis = list(
          title = "Puntaje BDI-II",
          range = c(-0.5, max(puntajes) + 1),
          zeroline = FALSE
        ),
        
        yaxis = list(
          title = "Adolescentes",
          range = c(0, altura * 1.17),
          gridcolor = "#E7E1DC",
          zeroline = FALSE
        ),
        
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.16
        ),
        
        annotations = list(
          list(
            x = mediana_bdi,
            y = altura * 1.09,
            text = paste0("Mediana · ", mediana_bdi),
            showarrow = FALSE,
            xanchor = "left"
          ),
          list(
            x = 13.5,
            y = altura * 1.05,
            text = "Corte clínico · 14",
            showarrow = FALSE,
            xanchor = "left"
          )
        ),
        
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(color = "#202936"),
        
        margin = list(
          l = 65,
          r = 30,
          t = 85,
          b = 60
        )
      )
    
    grafico
  })
  #dispersion
  output$grafico_boxplot_bdi <- plotly::renderPlotly({
    
    # 1. Preparar datos
    datos_box <- data.frame(
      bdi_total = df$bdi_total,
      grupo = "BDI-II"
    )
    
    datos_box <- datos_box[
      is.finite(datos_box$bdi_total),
    ]
    
    req(nrow(datos_box) > 0)
    
    # 2. Estadísticos
    mediana <- median(datos_box$bdi_total)
    q1 <- unname(quantile(datos_box$bdi_total, 0.25))
    q3 <- unname(quantile(datos_box$bdi_total, 0.75))
    riq <- IQR(datos_box$bdi_total)
    
    # 3. Construir gráfico
    grafico <- plotly::plot_ly(
      data = datos_box,
      
      x = ~bdi_total,
      y = ~grupo,
      
      type = "box",
      orientation = "h",
      
      name = "BDI-II",
      
      # Mostrar solo puntos atípicos
      boxpoints = "outliers",
      
      fillcolor = "#E6E2DD",
      
      line = list(
        color = "#67645F",
        width = 1.8
      ),
      
      marker = list(
        color = "#67645F",
        size = 6,
        opacity = 0.8
      ),
      
      hovertemplate = paste0(
        "Puntaje BDI-II: %{x}",
        "<extra></extra>"
      ),
      
      showlegend = FALSE
    ) %>%
      
      plotly::layout(
        
        # EJE X
        xaxis = list(
          title = "Puntaje BDI-II",
          range = c(-3, 54),
          dtick = 10,
          gridcolor = "#E7E1DC",
          zeroline = FALSE
        ),
        
        # OCULTAR EJE Y
        yaxis = list(
          title = "",
          showticklabels = FALSE,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        
        # LÍNEAS VERTICALES
        shapes = list(
          
          # Mediana
          list(
            type = "line",
            x0 = mediana,
            x1 = mediana,
            y0 = 0,
            y1 = 1,
            xref = "x",
            yref = "paper",
            
            line = list(
              color = "#67645F",
              width = 2,
              dash = "dot"
            )
          ),
          
          # Corte clínico
          list(
            type = "line",
            x0 = 13.5,
            x1 = 13.5,
            y0 = 0,
            y1 = 1,
            xref = "x",
            yref = "paper",
            
            line = list(
              color = "#526785",
              width = 2,
              dash = "dash"
            )
          )
        ),
        
        # ETIQUETAS SUPERIORES
        annotations = list(
          
          list(
            x = mediana,
            y = 1.08,
            xref = "x",
            yref = "paper",
            text = paste("Mediana:", mediana),
            showarrow = FALSE,
            xanchor = "left",
            font = list(
              color = "#67645F",
              size = 12
            )
          ),
          
          list(
            x = 13.5,
            y = 0.92,
            xref = "x",
            yref = "paper",
            text = "Corte clínico: 14",
            showarrow = FALSE,
            xanchor = "left",
            font = list(
              color = "#526785",
              size = 12
            )
          )
        ),
        
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        
        font = list(
          color = "#202936",
          size = 12
        ),
        
        margin = list(
          l = 30,
          r = 30,
          t = 55,
          b = 55
        )
      )
    
    plotly::config(
      grafico,
      displayModeBar = FALSE,
      responsive = TRUE
    )
    
  })
  
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

