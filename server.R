library(shiny)
library(tidyverse)
library(DT) # Para tablas interactivas
library(caret) # Para el entrenamiento del modelo
library(plotly) # Para gráficos interactivos
library(pROC) # Para la curva ROC
library(randomForest)
library(ggplot2)

server <- function(input, output, session) {
  
  output$inicio_registros <- renderText({
    format(nrow(df), big.mark = ".", decimal.mark = ",", scientific = FALSE)
  })
  # ---------- Valores derivados (una sola vez) ----------
  fmt_n   <- function(x) format(x, big.mark = ".", decimal.mark = ",", scientific = FALSE)
  fmt_pct <- function(x) paste0(formatC(x, digits = 1, format = "f", decimal.mark = ","), " %")
  
  dep_txt   <- as.character(df$depressed)
  valido_dep <- dep_txt %in% c("0", "1", "No deprimido", "Deprimido")
  es_dep    <- dep_txt %in% c("1", "Deprimido")
  sex_txt   <- as.character(df$sex)
  es_chica  <- sex_txt %in% c("Girl", "Chicas", "Female")
  es_chico  <- sex_txt %in% c("Boy", "Chicos", "Male")
  
  n_total   <- nrow(df)
  n_dep     <- sum(es_dep)
  pct_dep   <- if (any(valido_dep)) 100 * n_dep / sum(valido_dep) else NA_real_
  n_chicas  <- sum(es_chica); n_chicos <- sum(es_chico)
  dep_chicas <- sum(es_dep & es_chica); dep_chicos <- sum(es_dep & es_chico)
  pct_dep_chicas <- if (n_chicas > 0) 100 * dep_chicas / n_chicas else NA_real_
  pct_dep_chicos <- if (n_chicos > 0) 100 * dep_chicos / n_chicos else NA_real_
  
  # Portada
  output$pv_n   <- renderText(fmt_n(n_total))
  output$pv_dep <- renderText(if (is.na(pct_dep)) "Sin datos" else fmt_pct(pct_dep))
  
  # Indicadores dinámicos (antes estaban escritos a mano)
  output$n_deprimidos      <- renderText(fmt_n(n_dep))
  output$n_deprimidos_obj  <- renderText(fmt_n(n_dep))
  output$sx_chicas_n       <- renderText(fmt_n(n_chicas))
  output$sx_chicos_n       <- renderText(fmt_n(n_chicos))
  output$sx_chicas_pct     <- renderText(fmt_pct(100 * n_chicas / (n_chicas + n_chicos)))
  output$sx_chicos_pct     <- renderText(fmt_pct(100 * n_chicos / (n_chicas + n_chicos)))
  output$sx_dep_chicas_pct <- renderText(fmt_pct(pct_dep_chicas))
  output$sx_dep_chicos_pct <- renderText(fmt_pct(pct_dep_chicos))
  output$sx_dep_chicas_txt <- renderText(paste0(fmt_n(dep_chicas), " de ", fmt_n(n_chicas), " chicas"))
  output$sx_dep_chicos_txt <- renderText(paste0(fmt_n(dep_chicos), " de ", fmt_n(n_chicos), " chicos"))
  
  # ---------- Hallazgos (calculados desde df) ----------
  tabla_hz <- reactive({
    pred <- setdiff(variables_eda, "bdi_total")
    rho  <- sapply(pred, function(v) suppressWarnings(
      cor(df[[v]], df$bdi_total, method = "spearman", use = "pairwise.complete.obs")))
    magnitud <- cut(abs(rho), c(-Inf, .10, .30, .50, Inf),
                    labels = c("Despreciable", "Débil", "Moderada", "Fuerte"), right = FALSE)
    data.frame(Variable = pred, rho = round(rho, 3), Magnitud = as.character(magnitud),
               stringsAsFactors = FALSE) %>% arrange(desc(abs(rho)))
  })
  output$hz_prevalencia <- renderText(if (is.na(pct_dep)) "—" else fmt_pct(pct_dep))
  output$hz_brecha <- renderText({
    if (is.na(pct_dep_chicas) || is.na(pct_dep_chicos)) return("—")
    paste0(formatC(pct_dep_chicas - pct_dep_chicos, digits = 1, format = "f", decimal.mark = ","), " pp")
  })
  output$hz_top_var <- renderText(tabla_hz()$Variable[1])
  output$hz_top_rho <- renderText(formatC(tabla_hz()$rho[1], digits = 3, format = "f", decimal.mark = ","))
  output$tabla_hallazgos <- DT::renderDT({
    DT::datatable(tabla_hz(), rownames = FALSE, class = "tabla-eda",
                  options = list(dom = "t", paging = FALSE, ordering = FALSE))
  })
  
  output$inicio_edad <- renderText({
    if (!"age" %in% names(df)) return("12–16")
    paste0(min(df$age, na.rm=TRUE), "–", max(df$age, na.rm=TRUE))
  })
  output$inicio_sexo <- renderText({
    valores <- as.character(df$sex)
    chicas <- sum(valores %in% c("Girl", "Chicas", "Female"), na.rm=TRUE)
    chicos <- sum(valores %in% c("Boy", "Chicos", "Male"), na.rm=TRUE)
    if (chicas + chicos == 0) return("—")
    paste0(round(100*chicas/(chicas+chicos)), " % / ",
           round(100*chicos/(chicas+chicos)), " %")
  })
  output$inicio_deprimidos <- renderText({
    x <- as.character(df$depressed)
    validos <- x %in% c("0","1","No deprimido","Deprimido")
    if (!any(validos)) return("—")
    paste0(formatC(100*sum(x %in% c("1","Deprimido"))/sum(validos),
                   digits=1,format="f",decimal.mark=",")," %")
  })
  
  
  
  
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
    link_cluster       = "cluster",
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
          session$sendCustomMessage("cerrar_menu", list())
          
        },
        ignoreInit = TRUE
      )
      
    })
    
  }
  
  
  
  # 2. PROYECTO
  crear_grafico_depresion <- function() {
    
    # DATOS
    estados <- as.character(df$depressed)
    resumen <- data.frame(
      estado = c("No deprimido", "Deprimido"),
      cantidad = c(
        sum(estados %in% c("0", "No deprimido")),
        sum(estados %in% c("1", "Deprimido"))
      )
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
  
  # GRÁFICO 1: ESTADO DEPRESIVO POR SEXO
  
  datos_depresion_sexo <- df%>%
    filter(!is.na(sex), !is.na(depressed)) %>%
    mutate(
      sexo = factor(
        as.character(sex),
        levels = c("Boy", "Girl"),
        labels = c("Chicos", "Chicas")
      ),
      
      estado = case_when(
        as.character(depressed) %in%
          c("0", "No deprimido") ~ "No deprimido",
        
        as.character(depressed) %in%
          c("1", "Deprimido") ~ "Deprimido",
        
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(sexo), !is.na(estado)) %>%
    count(sexo, estado, name = "n") %>%
    group_by(sexo) %>%
    mutate(
      porcentaje = n / sum(n) * 100,
      
      etiqueta = paste0(
        formatC(
          porcentaje,
          format = "f",
          digits = 1,
          decimal.mark = ","
        ),
        " %"
      ),
      
      texto = paste0(
        "<b>", sexo, " · ", estado, "</b>",
        "<br>Adolescentes: ", n,
        "<br>Porcentaje: ", etiqueta
      )
    ) %>%
    ungroup()
  
  
  # GRÁFICO 2: COMPOSICIÓN DE LA MUESTRA
  
  datos_sexo <- df %>%
    filter(!is.na(sex)) %>%
    count(sex, name = "n") %>%
    mutate(
      sexo = factor(
        as.character(sex),
        levels = c("Boy", "Girl"),
        labels = c("Chicos", "Chicas")
      ),
      
      porcentaje = n / sum(n) * 100,
      
      etiqueta = paste0(
        format(n, big.mark = ".", decimal.mark = ",", scientific = FALSE),
        " · ",
        formatC(
          porcentaje,
          format = "f",
          digits = 1,
          decimal.mark = ","
        ),
        " %"
      ),
      
      texto = paste0(
        "<b>", sexo, "</b>",
        "<br>Adolescentes: ", n,
        "<br>Porcentaje: ",
        round(porcentaje, 1),
        " %"
      )
    )
  
  #depresion sexo
  output$grafico_depresion_sexo <- plotly::renderPlotly({
    
    plot_ly(
      data = datos_depresion_sexo,
      
      x = ~sexo,
      y = ~porcentaje,
      
      type = "bar",
      
      color = ~estado,
      
      colors = c(
        "No deprimido" = "#A3C3E5",
        "Deprimido" = "#EFAC91"
      ),
      
      text = ~etiqueta,
      textposition = "outside",
      
      hovertext = ~texto,
      hoverinfo = "text",
      
      marker = list(
        line = list(
          color = "white",
          width = 1.5
        )
      )
    ) %>%
      layout(
        barmode = "group",
        
        xaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = c("Chicos", "Chicas")
        ),
        
        yaxis = list(
          title = "% dentro del grupo",
          range = c(0, 110),
          ticksuffix = " %",
          gridcolor = "#E7E1DC"
        ),
        
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.12
        ),
        
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        
        font = list(
          color = "#202936"
        ),
        
        margin = list(
          l = 65,
          r = 20,
          t = 65,
          b = 50
        )
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
    
  })
  
  #composicion sexo
  output$grafico_composicion_sexo <- plotly::renderPlotly({
    
    plot_ly(
      data = datos_sexo,
      
      x = ~sexo,
      y = ~n,
      
      type = "bar",
      
      color = ~sexo,
      
      colors = c(
        "Chicos" = "#9AC7BD",
        "Chicas" = "#D6A5C3"
      ),
      
      text = ~etiqueta,
      textposition = "outside",
      
      hovertext = ~texto,
      hoverinfo = "text",
      
      marker = list(
        line = list(
          color = "white",
          width = 2
        )
      ),
      
      showlegend = FALSE
    ) %>%
      layout(
        
        xaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = c("Chicos", "Chicas")
        ),
        
        yaxis = list(
          title = "Adolescentes",
          range = c(0, max(datos_sexo$n) * 1.18),
          gridcolor = "#E7E1DC"
        ),
        
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)",
        
        font = list(
          color = "#202936"
        ),
        
        margin = list(
          l = 65,
          r = 20,
          t = 65,
          b = 50
        )
      ) %>%
      config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
    
  })
  
  #tabla_analisis
  
  datos_analisis_sexo <- df %>%
    mutate(
      sexo = case_when(
        as.character(sex) == "Boy" ~ "Chicos",
        as.character(sex) == "Girl" ~ "Chicas",
        TRUE ~ NA_character_
      ),
      
      estado = case_when(
        as.character(depressed) %in%
          c("0", "No deprimido") ~ "No deprimido",
        
        as.character(depressed) %in%
          c("1", "Deprimido") ~ "Deprimido",
        
        TRUE ~ NA_character_
      ),
      
      sexo = factor(
        sexo,
        levels = c("Chicas", "Chicos")
      ),
      
      estado = factor(
        estado,
        levels = c("No deprimido", "Deprimido")
      )
    ) %>%
    filter(
      !is.na(sexo),
      !is.na(estado)
    )
  
  # Tabla cruzada de frecuencias
  tabla_cruzada <- table(
    datos_analisis_sexo$sexo,
    datos_analisis_sexo$estado
  )
  
  
  #Boxplot bdi
  output$boxplot_bdi_sexo <- plotly::renderPlotly({
    
    datos_box <- datos_analisis_sexo %>%
      filter(
        !is.na(bdi_total),
        is.finite(bdi_total)
      )
    
    req(nrow(datos_box) > 0)
    
    # Distribuciones separadas
    chicos <- datos_box %>%
      filter(sexo == "Chicos")
    
    chicas <- datos_box %>%
      filter(sexo == "Chicas")
    
    grafico <- plotly::plot_ly() %>%
      
      # BOXPLOT CHICOS
      plotly::add_trace(
        data = chicos,
        
        x = ~sexo,
        y = ~bdi_total,
        
        type = "box",
        name = "Chicos",
        
        boxpoints = "outliers",
        jitter = 0.15,
        pointpos = 0,
        
        fillcolor = "#9AC7BD",
        
        line = list(
          color = "#7FAFA5",
          width = 1.5
        ),
        
        marker = list(
          color = "#9AC7BD",
          size = 5,
          opacity = 0.65
        ),
        
        hovertemplate = paste0(
          "<b>Chicos</b>",
          "<br>BDI-II: %{y}",
          "<extra></extra>"
        ),
        
        showlegend = FALSE
      ) %>%
      
      # BOXPLOT CHICAS
      plotly::add_trace(
        data = chicas,
        
        x = ~sexo,
        y = ~bdi_total,
        
        type = "box",
        name = "Chicas",
        
        boxpoints = "outliers",
        jitter = 0.15,
        pointpos = 0,
        
        fillcolor = "#D6A5C3",
        
        line = list(
          color = "#C995B7",
          width = 1.5
        ),
        
        marker = list(
          color = "#D6A5C3",
          size = 5,
          opacity = 0.65
        ),
        
        hovertemplate = paste0(
          "<b>Chicas</b>",
          "<br>BDI-II: %{y}",
          "<extra></extra>"
        ),
        
        showlegend = FALSE
      ) %>%
      
      plotly::layout(
        
        xaxis = list(
          title = "",
          categoryorder = "array",
          categoryarray = c("Chicos", "Chicas")
        ),
        
        yaxis = list(
          title = "Puntaje BDI-II",
          rangemode = "tozero",
          gridcolor = "#E7E1DC",
          zeroline = FALSE
        ),
        
        # LÍNEA HORIZONTAL: CORTE CLÍNICO
        shapes = list(
          list(
            type = "line",
            
            x0 = 0,
            x1 = 1,
            xref = "paper",
            
            y0 = 14,
            y1 = 14,
            yref = "y",
            
            line = list(
              color = "#526785",
              width = 2,
              dash = "dash"
            )
          )
        ),
        
        # ETIQUETA DEL CORTE CLÍNICO
        annotations = list(
          list(
            x = 0,
            xref = "paper",
            
            y = 14,
            yref = "y",
            
            text = "Corte clínico · 14",
            
            showarrow = FALSE,
            xanchor = "left",
            yanchor = "bottom",
            
            font = list(
              color = "#526785",
              size = 12
            )
          )
        ),
        
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        
        font = list(
          color = "#202936"
        ),
        
        margin = list(
          l = 60,
          r = 25,
          t = 35,
          b = 50
        )
      )
    
    plotly::config(
      grafico,
      displayModeBar = FALSE,
      responsive = TRUE
    )
    
  })
  
  #tablas
  tabla_cont <- as.data.frame.matrix(
    tabla_cruzada
  )
  
  tabla_cont$Grupo <- rownames(tabla_cont)
  
  tabla_cont <- tabla_cont %>%
    select(
      Grupo,
      `No deprimido`,
      `Deprimido`
    ) %>%
    mutate(
      Total = `No deprimido` + `Deprimido`,
      
      `% deprimido` = ifelse(
        Total > 0,
        100 * Deprimido / Total,
        NA_real_
      )
    )
  
  # Fila de totales
  fila_total <- data.frame(
    Grupo = "Total",
    
    `No deprimido` = sum(
      tabla_cont$`No deprimido`
    ),
    
    Deprimido = sum(
      tabla_cont$Deprimido
    ),
    
    check.names = FALSE
  )
  
  fila_total$Total <-
    fila_total$`No deprimido` +
    fila_total$Deprimido
  
  fila_total$`% deprimido` <-
    100 * fila_total$Deprimido /
    fila_total$Total
  
  tabla_cont <- bind_rows(
    tabla_cont,
    fila_total
  )
  
  # CONEXIÓN CON DT
  output$tabla_contingencia <- DT::renderDT({
    
    DT::datatable(
      tabla_cont,
      
      rownames = FALSE,
      class = "compact tabla-eda",
      
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      )
    ) %>%
      
      DT::formatRound(
        columns = c(
          "No deprimido",
          "Deprimido",
          "Total"
        ),
        digits = 0,
        mark = "."
      ) %>%
      
      DT::formatRound(
        columns = "% deprimido",
        digits = 1,
        dec.mark = ","
      ) %>%
      
      DT::formatStyle(
        columns = "Grupo",
        target = "row",
        valueColumns = "Grupo",
        
        fontWeight = DT::styleEqual(
          "Total",
          "bold"
        )
      )
    
  })
  
  prueba_chi <- chisq.test(
    tabla_cruzada,
    correct = FALSE
  )
  
  # Tamaño de muestra
  n_total <- sum(tabla_cruzada)
  
  # V de Cramér
  v_cramer <- sqrt(
    as.numeric(prueba_chi$statistic) /
      (
        n_total *
          min(
            nrow(tabla_cruzada) - 1,
            ncol(tabla_cruzada) - 1
          )
      )
  )
  
  # Frecuencias de cada sexo
  dep_chicas <- tabla_cruzada["Chicas", "Deprimido"]
  
  no_dep_chicas <- tabla_cruzada[
    "Chicas", "No deprimido"
  ]
  
  dep_chicos <- tabla_cruzada["Chicos", "Deprimido"]
  
  no_dep_chicos <- tabla_cruzada[
    "Chicos", "No deprimido"
  ]
  
  # Prevalencias
  prev_chicas <- dep_chicas /
    (dep_chicas + no_dep_chicas)
  
  prev_chicos <- dep_chicos /
    (dep_chicos + no_dep_chicos)
  
  # Razón de prevalencias
  rp <- prev_chicas / prev_chicos
  
  # Odds ratio
  or <- (dep_chicas * no_dep_chicos) /
    (no_dep_chicas * dep_chicos)
  
  # Tabla de resultados
  tabla_chi <- data.frame(
    
    Indicador = c(
      "χ² de Pearson",
      "Grados de libertad",
      "Valor p",
      "V de Cramér",
      "Razón de prevalencias (chicas / chicos)",
      "Odds ratio (chicas / chicos)"
    ),
    
    Valor = c(
      formatC(
        unname(prueba_chi$statistic),
        format = "f", digits = 2,
        decimal.mark = ","
      ),
      
      as.character(
        unname(prueba_chi$parameter)
      ),
      
      if (
        prueba_chi$p.value < 0.001
      ) {
        "< 0,001"
      } else {
        formatC(
          prueba_chi$p.value,
          format = "f", digits = 3,
          decimal.mark = ","
        )
      },
      
      formatC(
        v_cramer,
        format = "f", digits = 3,
        decimal.mark = ","
      ),
      
      formatC(
        rp,
        format = "f", digits = 2,
        decimal.mark = ","
      ),
      
      formatC(
        or,
        format = "f", digits = 2,
        decimal.mark = ","
      )
    )
  )
  
  # CONEXIÓN CON DT
  output$tabla_chi_sexo <- DT::renderDT({
    
    DT::datatable(
      tabla_chi,
      
      rownames = FALSE,
      class = "compact tabla-eda",
      
      colnames = c(
        "Prueba de independencia",
        "Valor"
      ),
      
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      )
    )
    
  })
  
  
  # MANN-WHITNEY U: VARIABLES NUMÉRICAS POR SEXO
  
  # 1. Variables que se van a comparar
  
  variables_mw <- c(
    "bdi_total",
    "sleep_quality_index",
    "midsleep_weekend_hours",
    "social_jetlag_hours",
    "avg_sleep_hours",
    "screen_time_index",
    "est_leisure_screen_hours"
  )
  
  # 2. Calcular la prueba para cada variable
  
  tabla_mw <- lapply(variables_mw, function(variable) {
    
    # Extraer observaciones por sexo
    chicas <- df %>%
      filter(sex == "Girl") %>%
      pull(all_of(variable))
    
    chicos <- df %>%
      filter(sex == "Boy") %>%
      pull(all_of(variable))
    
    # Eliminar valores faltantes
    chicas <- chicas[!is.na(chicas)]
    chicos <- chicos[!is.na(chicos)]
    
    # Prueba de Mann-Whitney
    prueba <- wilcox.test(
      chicas,
      chicos,
      alternative = "two.sided",
      exact = FALSE,
      correct = FALSE
    )
    
    # Tamaños muestrales
    n_chicas <- length(chicas)
    n_chicos <- length(chicos)
    
    # Estadístico U para chicas
    U <- as.numeric(prueba$statistic)
    
    # Correlación rango-biserial
    # Negativo = valores mayores en chicas
    r_rb <- 1 - (2 * U) / (n_chicas * n_chicos)
    
    # Resultado por variable
    data.frame(
      Variable = variable,
      `Mediana chicas` = median(chicas),
      `Mediana chicos` = median(chicos),
      U = U,
      `Valor p` = prueba$p.value,
      `r rango-biserial` = r_rb,
      check.names = FALSE
    )
    
  }) %>%
    dplyr::bind_rows()
  
  output$tabla_mann_whitney <- DT::renderDT({
    
    # Copia para presentación
    tabla_visual <- tabla_mw %>%
      mutate(
        `Valor p` = ifelse(
          `Valor p` < 0.001,
          "< 0,001",
          formatC(
            `Valor p`,
            format = "f",
            digits = 3,
            decimal.mark = ","
          )
        )
      )
    
    DT::datatable(
      tabla_visual,
      
      rownames = FALSE,
      class = "compact hover tabla-eda",
      
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        scrollX = TRUE,
        autoWidth = TRUE,
        
        columnDefs = list(
          list(
            targets = 0,
            width = "250px",
            className = "dt-left"
          ),
          list(
            targets = 1:5,
            className = "dt-right"
          )
        )
      )
    ) %>%
      
      DT::formatRound(
        columns = c(
          "Mediana chicas",
          "Mediana chicos"
        ),
        digits = 2,
        mark = ".",
        dec.mark = ","
      ) %>%
      
      DT::formatRound(
        columns = "U",
        digits = 1,
        mark = ".",
        dec.mark = ","
      ) %>%
      
      DT::formatRound(
        columns = "r rango-biserial",
        digits = 3,
        dec.mark = ","
      )
    
  })
  
  
  # 3.4 Variables numéricas
  
  # ============================================
  # 3.4 VARIABLES NUMÉRICAS
  # ============================================
  
  datos_numericas <- reactive({
    
    req(input$variable, input$separador)
    
    # La base original df no se modifica; el grupo se construye en memoria.
    grupo_eda <- switch(
      input$separador,
      "ninguno" = rep("Todos", nrow(df)),
      "sexo" = dplyr::recode(
        as.character(df$sex),
        "Boy" = "Chicos",
        "Girl" = "Chicas"
      ),
      "depresion" = dplyr::case_when(
        as.character(df$depressed) %in% c("0", "No deprimido") ~ "No deprimido",
        as.character(df$depressed) %in% c("1", "Deprimido") ~ "Deprimido",
        TRUE ~ NA_character_
      )
    )
    
    df %>%
      mutate(
        valor = .data[[input$variable]],
        grupo_eda = factor(grupo_eda)
      ) %>%
      filter(
        is.finite(valor),
        !is.na(grupo_eda)
      )
  })
  
  # KPI calculados sobre la variable completa
  resumen_numericas <- reactive({
    req(input$variable)
    
    estadisticos_variable(
      df[[input$variable]]
    )
  })
  
  formato_num <- function(x) {
    if (is.na(x)) return("—")
    
    formatC(
      x,
      format = "f",
      digits = 2,
      decimal.mark = ","
    )
  }
  
  output$num_media <- renderText({
    formato_num(resumen_numericas()["media"])
  })
  
  output$num_mediana <- renderText({
    formato_num(resumen_numericas()["mediana"])
  })
  
  output$num_sd <- renderText({
    formato_num(resumen_numericas()["sd"])
  })
  
  output$num_skew <- renderText({
    formato_num(resumen_numericas()["asimetria"])
  })
  
  # HISTOGRAMA
  output$histograma_numericas <- plotly::renderPlotly({
    
    df <- datos_numericas()
    
    p <- ggplot(
      df,
      aes(
        x = valor,
        fill = grupo_eda
      )
    ) +
      geom_histogram(
        bins = 25,
        color = "white",
        alpha = 0.85,
        position = "identity"
      ) +
      geom_vline(
        xintercept = median(df$valor),
        linetype = "dotted",
        color = "#67645F"
      ) +
      labs(
        x = input$variable,
        y = "Adolescentes",
        fill = "Grupo"
      ) +
      theme_minimal()
    
    if (input$separador == "ninguno") {
      p <- p +
        scale_fill_manual(
          values = c("Todos" = "#6A7594")
        ) +
        theme(legend.position = "none")
    } else {
      p <- p +
        scale_fill_manual(
          values = c(
            "Chicos" = "#9AC7BD",
            "Chicas" = "#D6A5C3",
            "No deprimido" = "#A3C3E5",
            "Deprimido" = "#EFAC91"
          )
        )
    }
    
    plotly::ggplotly(p)
  })
  
  # BOXPLOT
  output$boxplot_numericas <- plotly::renderPlotly({
    
    df <- datos_numericas()
    
    grafico <- plotly::plot_ly(
      data = df,
      
      x = ~valor,
      y = ~grupo_eda,
      
      type = "box",
      orientation = "h",
      
      boxpoints = "outliers",
      
      marker = list(
        color = "#6A7594",
        size = 5
      ),
      
      line = list(
        color = "#6A7594"
      ),
      
      showlegend = FALSE
    ) %>%
      plotly::layout(
        xaxis = list(title = input$variable),
        yaxis = list(title = ""),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      )
    
    plotly::config(
      grafico,
      displayModeBar = FALSE
    )
  })
  
  
  
  # 3.5 Correlaciones
  
  # ============================================
  # 3.5 CORRELACIONES
  # ============================================
  
  # Matriz general
  matriz_cor <- cor(
    df[, variables_eda],
    method = "spearman",
    use = "pairwise.complete.obs"
  )
  
  # KPI
  rho_bdi <- function(variable) {
    formato_num(
      matriz_cor[variable, "bdi_total"]
    )
  }
  
  output$rho_calidad <- renderText({
    rho_bdi("sleep_quality_index")
  })
  
  output$rho_horas <- renderText({
    rho_bdi("avg_sleep_hours")
  })
  
  output$rho_cronotipo <- renderText({
    rho_bdi("midsleep_weekend_hours")
  })
  
  output$rho_pantalla <- renderText({
    rho_bdi("screen_time_index")
  })
  
  # MATRIZ INTERACTIVA
  output$matriz_spearman <- plotly::renderPlotly({
    
    # 1. Orden único de variables
    variables <- variables_eda
    
    # 2. Calcular matriz de Spearman
    matriz <- cor(
      df[, variables],
      method = "spearman",
      use = "pairwise.complete.obs"
    )
    
    # 3. Convertir matriz a formato largo
    tabla_cor <- as.data.frame(
      as.table(matriz)
    )
    
    names(tabla_cor) <- c(
      "Variable_Y",
      "Variable_X",
      "Rho"
    )
    
    # 4. Conservar triángulo inferior
    tabla_cor <- tabla_cor %>%
      mutate(
        fila = match(Variable_Y, variables),
        columna = match(Variable_X, variables)
      ) %>%
      filter(fila > columna) %>%
      mutate(
        Variable_X = factor(
          Variable_X,
          levels = variables
        ),
        
        Variable_Y = factor(
          Variable_Y,
          levels = rev(variables)
        ),
        
        etiqueta = formatC(
          Rho,
          format = "f",
          digits = 2,
          decimal.mark = ","
        ),
        
        tooltip = paste0(
          "<b>", Variable_Y, "</b>",
          "<br>con ", Variable_X,
          "<br>Spearman: ", etiqueta
        )
      )
    
    # 5. Gráfico con orden fijo
    grafico <- ggplot(
      tabla_cor,
      aes(
        x = Variable_X,
        y = Variable_Y,
        fill = Rho,
        text = tooltip
      )
    ) +
      
      geom_tile(
        width = 0.98,
        height = 0.98,
        color = "#FFFFFF",
        linewidth = 0.8
      ) +
      
      geom_text(
        aes(label = etiqueta),
        size = 3.2,
        color = "#202936"
      ) +
      
      scale_fill_gradient2(
        low = "#A3C3E5",
        mid = "#FFFFFF",
        high = "#EFAC91",
        midpoint = 0,
        limits = c(-1, 1),
        name = "ρ"
      ) +
      
      scale_x_discrete(
        drop = FALSE
      ) +
      
      scale_y_discrete(
        drop = FALSE
      ) +
      
      
      labs(
        x = NULL,
        y = NULL
      ) +
      
      theme_minimal(base_size = 12) +
      
      theme(
        panel.grid = element_blank(),
        
        axis.text.x = element_text(
          angle = 40,
          hjust = 1,
          color = "#202936"
        ),
        
        axis.text.y = element_text(
          color = "#202936"
        ),
        
        legend.position = "right"
      )
    
    # 6. Convertir en interactivo
    plotly::ggplotly(
      grafico,
      tooltip = "text"
    ) %>%
      
      plotly::layout(
        margin = list(
          l = 180,
          r = 30,
          t = 25,
          b = 155
        ),
        
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      
      plotly::config(
        displayModeBar = FALSE,
        responsive = TRUE
      )
    
  })
  # ASOCIACIÓN CON BDI-II
  output$asociacion_bdi <- plotly::renderPlotly({
    
    tabla <- data.frame(
      Variable = setdiff(
        rownames(matriz_cor),
        "bdi_total"
      )
    ) %>%
      mutate(
        rho = matriz_cor[
          Variable,
          "bdi_total"
        ],
        
        color = ifelse(
          rho >= 0,
          "#EFAC91",
          "#A3C3E5"
        )
      )
    
    plotly::plot_ly(
      data = tabla,
      
      x = ~rho,
      y = ~Variable,
      
      type = "bar",
      orientation = "h",
      
      marker = list(
        color = tabla$color
      ),
      
      hovertemplate = paste0(
        "%{y}",
        "<br>Spearman: %{x:.3f}",
        "<extra></extra>"
      ),
      
      text = ~formatC(rho, digits = 3, format = "f", decimal.mark = ","),
      textposition = "outside",
      textfont = list(size = 11, color = "#202936"),
      cliponaxis = FALSE,
      showlegend = FALSE
    ) %>%
      plotly::layout(
        xaxis = list(
          title = "ρ de Spearman",
          zeroline = TRUE
        ),
        yaxis = list(title = ""),
        margin = list(
          l = 165,
          r = 65,
          t = 20,
          b = 55
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
  })
  
  # TABLAS DE ASOCIACIONES
  dominios_sueno <- c(
    "sleep_quality_index",
    "avg_sleep_hours",
    "midsleep_weekend_hours",
    "social_jetlag_hours"
  )
  
  crear_tabla_cor <- function(variable_base) {
    
    dplyr::bind_rows(
      lapply(dominios_sueno, function(variable) {
        
        pares <- df %>%
          select(all_of(
            c(variable_base, variable)
          )) %>%
          drop_na()
        
        prueba <- cor.test(
          pares[[variable_base]],
          pares[[variable]],
          method = "spearman",
          exact = FALSE
        )
        
        data.frame(
          Variable = variable,
          N = nrow(pares),
          Rho = unname(prueba$estimate),
          p = prueba$p.value
        )
      })
    )
  }
  
  tabla_pantalla <- crear_tabla_cor(
    "screen_time_index"
  )
  
  tabla_sueno <- crear_tabla_cor(
    "bdi_total"
  )
  
  mostrar_cor <- function(tabla) {
    
    tabla %>%
      mutate(
        Rho = round(Rho, 3),
        p = ifelse(
          p < 0.001,
          "< 0,001",
          formatC(
            p,
            digits = 3,
            format = "f",
            decimal.mark = ","
          )
        )
      ) %>%
      rename(
        `Spearman r` = Rho,
        `Valor p` = p
      )
  }
  
  output$tabla_pantalla_sueno <- DT::renderDT({
    DT::datatable(
      mostrar_cor(tabla_pantalla),
      rownames = FALSE,
      class = "compact tabla-eda",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      )
    )
  })
  
  output$tabla_sueno_bdi <- DT::renderDT({
    DT::datatable(
      mostrar_cor(tabla_sueno),
      rownames = FALSE,
      class = "compact tabla-eda",
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      )
    )
  })
  
  
  # 3.6 Outliers
  
  # ============================================
  # 3.6 OUTLIERS
  # ============================================
  
  # Identificación por variable
  tabla_outliers <- bind_rows(
    lapply(variables_eda, function(variable) {
      
      x <- df[[variable]]
      res <- calcular_outliers(x)
      
      data.frame(
        Variable = variable,
        Q1 = res$q1,
        Q3 = res$q3,
        IQR = res$riq,
        `Lím. inf.` = res$lim_inf,
        `Lím. sup.` = res$lim_sup,
        Bajo = sum(
          x < res$lim_inf,
          na.rm = TRUE
        ),
        Alto = sum(
          x > res$lim_sup,
          na.rm = TRUE
        ),
        `% outliers` =
          100 * sum(res$indicador) /
          sum(!is.na(x)),
        check.names = FALSE
      )
    })
  )
  
  # Matriz lógica: TRUE si hay outlier
  indicadores_out <- sapply(
    variables_eda,
    function(variable) {
      calcular_outliers(
        df[[variable]]
      )$indicador
    }
  )
  
  # Predictores únicamente
  predictores_out <- setdiff(
    variables_eda,
    "bdi_total"
  )
  
  # Filas con outliers en predictores
  filas_out <- rowSums(
    indicadores_out[, predictores_out,
                    drop = FALSE]
  ) > 0
  
  # Clasificación de depresión
  estado_out <- as.character(
    df$depressed
  )
  
  es_deprimido <- estado_out %in%
    c("1", "Deprimido")
  
  # INDICADORES
  output$out_filas <- renderText({
    format(
      sum(filas_out),
      big.mark = ".",
      decimal.mark = ","
    )
  })
  
  output$out_depresion <- renderText({
    if (sum(filas_out) == 0) return("—")
    
    paste0(
      formato_num(
        100 * sum(
          filas_out & es_deprimido
        ) / sum(filas_out)
      ),
      " %"
    )
  })
  
  output$out_bdi <- renderText({
    sum(
      indicadores_out[, "bdi_total"]
    )
  })
  
  output$out_sin <- renderText({
    sum(
      tabla_outliers$Bajo +
        tabla_outliers$Alto == 0
    )
  })
  
  # TABLA RIQ
  output$tabla_outliers <- DT::renderDT({
    
    DT::datatable(
      tabla_outliers,
      rownames = FALSE,
      class = "compact tabla-eda",
      options = list(
        dom = "t",
        paging = FALSE,
        searching = FALSE,
        ordering = FALSE,
        scrollX = TRUE
      )
    ) %>%
      DT::formatRound(
        columns = c(
          "Q1", "Q3", "IQR",
          "Lím. inf.", "Lím. sup.",
          "% outliers"
        ),
        digits = 2,
        dec.mark = ","
      )
  })
  
  # GRÁFICO POR ESTADO DEPRESIVO
  tabla_out_grupos <- bind_rows(
    lapply(predictores_out, function(variable) {
      
      data.frame(
        Variable = variable,
        Estado = estado_out,
        Atipico = indicadores_out[, variable]
      ) %>%
        filter(
          Estado %in% c(
            "0", "1",
            "No deprimido", "Deprimido"
          )
        ) %>%
        mutate(
          Estado = case_when(
            Estado %in% c(
              "0", "No deprimido"
            ) ~ "No deprimido",
            TRUE ~ "Deprimido"
          )
        ) %>%
        group_by(Variable, Estado) %>%
        summarise(
          porcentaje = 100 * mean(Atipico),
          .groups = "drop"
        )
    })
  )
  
  output$grafico_outliers <- plotly::renderPlotly({
    
    # Porcentaje visible junto a cada barra
    datos_barras <- tabla_out_grupos %>%
      mutate(etiqueta = paste0(
        formatC(porcentaje, format = "f", digits = 1, decimal.mark = ","),
        " %"
      ))
    
    plotly::plot_ly(
      data = datos_barras,
      x = ~porcentaje,
      y = ~Variable,
      
      type = "bar",
      orientation = "h",
      color = ~Estado,
      
      colors = c(
        "No deprimido" = "#A3C3E5",
        "Deprimido" = "#EFAC91"
      ),
      
      text = ~etiqueta,
      textposition = "outside",
      cliponaxis = FALSE,
      textfont = list(size = 11, color = "#202936"),
      hovertemplate = paste0(
        "%{y}",
        "<br>Outliers: %{x:.2f} %",
        "<extra></extra>"
      )
    ) %>%
      plotly::layout(
        barmode = "group",
        
        xaxis = list(
          title = "% de outliers dentro del grupo",
          ticksuffix = " %",
          range = c(0, max(1, max(datos_barras$porcentaje, na.rm = TRUE) * 1.18))
        ),
        
        yaxis = list(
          title = ""
        ),
        
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.12
        ),
        
        margin = list(
          l = 185,
          r = 60,
          t = 55,
          b = 55
        ),
        
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
      plotly::config(
        displayModeBar = FALSE
      )
  })
  
  observeEvent(input$entrar_dashboard, {
    
    message("Portada: clic recibido por Shiny")
    
    # Cerrar portada
    session$sendCustomMessage(
      type = "cerrar_portada",
      message = list()
    )
    
    # Navegar a Inicio
    updateNavbarPage(
      session = session,
      inputId = "navegacion",
      selected = "inicio"
    )
    
  })
  
  
  # ==========================================================
  # 4.7 SEGMENTACIÓN: K-MEANS Y DEPRESIÓN POR CLÚSTER
  # ==========================================================
  variables_cluster <- setdiff(variables_eda, "bdi_total")
  filas_cluster <- which(stats::complete.cases(df[, variables_cluster, drop = FALSE]))
  datos_cluster <- df[filas_cluster, variables_cluster, drop = FALSE]
  matriz_cluster <- scale(datos_cluster)
  if (any(!is.finite(matriz_cluster))) {
    stop("No es posible estandarizar las variables de segmentación; revisa columnas constantes o valores no finitos.")
  }
  pca_cluster <- stats::prcomp(matriz_cluster, center = FALSE, scale. = FALSE)
  
  modelo_cluster <- reactive({
    req(input$numero_clusters)
    validate(need(nrow(matriz_cluster) > input$numero_clusters,
                  "Se necesitan más registros que clústeres."))
    set.seed(123)
    stats::kmeans(matriz_cluster, centers = input$numero_clusters, nstart = 25)
  })
  
  asignaciones_cluster <- reactive({
    modelo <- modelo_cluster()
    data.frame(
      Cluster = factor(paste0("Perfil ", modelo$cluster),
                       levels = paste0("Perfil ", seq_len(input$numero_clusters))),
      PC1 = pca_cluster$x[, 1], PC2 = pca_cluster$x[, 2],
      Estado = dplyr::case_when(
        as.character(df$depressed[filas_cluster]) %in% c("1", "Deprimido") ~ "Deprimido",
        as.character(df$depressed[filas_cluster]) %in% c("0", "No deprimido") ~ "No deprimido",
        TRUE ~ NA_character_
      )
    )
  })
  
  resumen_clusters <- reactive({
    asignaciones_cluster() %>%
      dplyr::group_by(Cluster) %>%
      dplyr::summarise(N = dplyr::n(), .groups = "drop") %>%
      dplyr::mutate(Porcentaje = round(100 * N / sum(N), 1))
  })
  
  resumen_depresion_cluster <- reactive({
    asignaciones_cluster() %>%
      dplyr::filter(!is.na(Estado)) %>%
      dplyr::group_by(Cluster) %>%
      dplyr::summarise(
        N = dplyr::n(), Deprimidos = sum(Estado == "Deprimido"),
        `No deprimidos` = sum(Estado == "No deprimido"),
        Prevalencia = 100 * Deprimidos / N, .groups = "drop"
      )
  })
  
  output$cluster_n <- renderText(fmt_n(nrow(datos_cluster)))
  output$cluster_k <- renderText(input$numero_clusters)
  output$cluster_pca <- renderText({
    v <- pca_cluster$sdev^2
    fmt_pct(100 * sum(v[1:2]) / sum(v))
  })
  
  output$grafico_clusters <- plotly::renderPlotly({
    datos <- asignaciones_cluster()
    graf <- ggplot2::ggplot(datos, ggplot2::aes(x = PC1, y = PC2, color = Cluster,
                                                text = paste("Perfil:", Cluster))) +
      ggplot2::geom_point(alpha = .58, size = 1.6) +
      ggplot2::labs(x="Componente principal 1", y="Componente principal 2") +
      ggplot2::theme_minimal(base_size = 12)
    plotly::ggplotly(graf, tooltip = "text") %>%
      plotly::layout(paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$tabla_clusters <- DT::renderDT({
    DT::datatable(resumen_clusters(), rownames=FALSE, class="compact tabla-eda",
                  options=list(dom="t", paging=FALSE, ordering=FALSE))
  })
  
  output$grafico_depresion_cluster <- plotly::renderPlotly({
    resumen <- resumen_depresion_cluster()
    validate(need(nrow(resumen)>0, "No hay estados depresivos válidos."))
    resumen <- resumen %>% dplyr::mutate(
      Etiqueta = paste0(formatC(Prevalencia, format="f", digits=1,
                                decimal.mark=",", big.mark="."), " %"),
      Tooltip = paste0("<b>", Cluster, "</b><br>Adolescentes: ", N,
                       "<br>Deprimidos: ", Deprimidos,
                       "<br>No deprimidos: ", `No deprimidos`,
                       "<br>Prevalencia: ", Etiqueta))
    plotly::plot_ly(resumen, x=~Cluster, y=~Prevalencia, type="bar",
                    marker=list(color="#EFAC91", line=list(color="#FFFFFF",width=1)),
                    text=~Etiqueta, textposition="outside", cliponaxis=FALSE,
                    hovertext=~Tooltip, hoverinfo="text") %>%
      plotly::layout(xaxis=list(title=""), yaxis=list(title="% con BDI-II ≥ 14",
                                                      ticksuffix=" %", range=c(0,max(10,resumen$Prevalencia*1.25))),
                     margin=list(l=65,r=25,t=50,b=55), showlegend=FALSE,
                     paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)") %>%
      plotly::config(displayModeBar=FALSE, responsive=TRUE)
  })
  
  output$tabla_depresion_cluster <- DT::renderDT({
    tabla <- resumen_depresion_cluster()
    DT::datatable(tabla, rownames=FALSE, class="compact tabla-eda",
                  options=list(dom="t",paging=FALSE,scrollX=TRUE)) %>%
      DT::formatRound("Prevalencia", 1, mark=".",dec.mark=",")
  })
  
  output$perfil_clusters <- plotly::renderPlotly({
    modelo <- modelo_cluster()
    perfil <- as.data.frame(matriz_cluster) %>%
      dplyr::mutate(Cluster = factor(paste0("Perfil ", modelo$cluster))) %>%
      dplyr::group_by(Cluster) %>%
      dplyr::summarise(dplyr::across(dplyr::all_of(variables_cluster), mean), .groups="drop") %>%
      tidyr::pivot_longer(-Cluster, names_to="Variable",values_to="Valor Z")
    graf <- ggplot2::ggplot(perfil,
                            ggplot2::aes(x=Variable, y=Cluster, fill=`Valor Z`)) +
      ggplot2::geom_tile(color="white") +
      ggplot2::scale_fill_gradient2(low="#A3C3E5",mid="white",high="#EFAC91") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=35,hjust=1))
    plotly::ggplotly(graf) %>%
      plotly::layout(margin=list(l=90,b=145,t=20,r=20),
                     paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)")
  })
  
  output$tabla_chi_clusters <- DT::renderDT({
    tabla <- table(asignaciones_cluster()$Cluster, asignaciones_cluster()$Estado)
    validate(need(nrow(tabla)>=2 && ncol(tabla)>=2,
                  "No es posible comparar los estados depresivos."))
    prueba <- suppressWarnings(stats::chisq.test(tabla, correct=FALSE))
    v <- sqrt(as.numeric(prueba$statistic)/
                (sum(tabla)*min(nrow(tabla)-1,ncol(tabla)-1)))
    res <- data.frame(
      Indicador=c("χ² de Pearson", "Valor p", "V de Cramér", "Mín. esperado"),
      Valor=c(formatC(as.numeric(prueba$statistic), digits=2,format="f",decimal.mark=","),
              format.pval(prueba$p.value, digits=3),
              formatC(v,digits=3,format="f",decimal.mark=","),
              formatC(min(prueba$expected),digits=1,format="f",decimal.mark=",")))
    DT::datatable(res, rownames=FALSE, options=list(dom="t",paging=FALSE))
  })
  
}