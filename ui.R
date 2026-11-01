
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
  encabezado_eda(seccion, pagina)
}

encabezado_eda <- function(seccion, pagina, etiquetas = character(), url_libro = NULL) {
  if (identical(seccion, "EDA")) {
    etiquetas <- unique(c(
      paste(format(nrow(df), big.mark = ".", decimal.mark = ","), "filas"),
      paste(ncol(df), "variables"), etiquetas
    ))
  }
  div(
    class = "encabezado-eda encabezado-unificado",
    div(class = "encabezado-titulos",
        p(paste(seccion, "/", pagina), class = "ruta-pagina"),
        h2(pagina)),
    div(class = "acciones-eda",
        lapply(etiquetas, function(x) span(x, class = "etiqueta-eda")),
        boton_libro())
  )
}

# ENLACE DIRECTO AL LIBRO EN TODAS LAS PESTAÑAS
boton_libro <- function() {
  tags$a(
    tagList(icon("book-open"), " Ver en el R Bookdown"),
    href = libro_url, target = "_blank", rel = "noopener noreferrer",
    class = "boton-libro boton-libro-global"
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

tarjeta_kpi <- function(etiqueta, salida, descripcion, clase_valor = "") {
  div(
    class = "tarjeta-kpi",
    p(etiqueta, class = "kpi-etiqueta"),
    h2(textOutput(salida, inline = TRUE), class = paste("kpi-valor", clase_valor)),
    p(descripcion, class = "kpi-descripcion")
  )
}
# FICHAS DE ESTUDIOS Y RECURSOS
estudio_card <- function(autor, titulo, muestra, diseno, hallazgo, url = NULL, base = FALSE) {
  tags$article(class="estudio-card",
               div(class="estudio-titulo", tags$strong(autor),
                   if (base) span("Estudio base", class="etiqueta-eda") else NULL),
               p(class="estudio-nombre", titulo),
               span(class="estudio-label", "MUESTRA"), p(muestra),
               span(class="estudio-label", "DISEÑO"), p(diseno),
               span(class="estudio-label", "HALLAZGO CLAVE"), p(hallazgo),
               tags$a(if (is.null(url)) "Buscar publicación" else "Ver artículo",
                      href = if (is.null(url)) paste0("https://scholar.google.com/scholar?q=",
                                                      utils::URLencode(titulo, reserved=TRUE)) else url,
                      target="_blank", rel="noopener noreferrer", class="enlace-estudio")
  )
}

integrante_card <- function(nombre, semestre, carrera, inicial, foto) {
  tags$article(class="integrante-card",
               if (file.exists(file.path("www", foto))) {
                 tags$img(src=foto, alt=paste("Retrato de", nombre), class="equipo-foto")
               } else {
                 div(class="equipo-inicial", tags$span("Espacio para foto", class="foto-placeholder"))
               },
               h3(nombre), span(class="etiqueta-eda", semestre),
               span(class="estudio-label", "CARRERA"), p(carrera)
  )
}

recurso_card <- function(titulo, descripcion, url, etiqueta) {
  tags$article(class="recurso-card", h4(titulo), p(descripcion),
               if (!is.null(url) && nzchar(url)) {
                 tags$a(etiqueta, href=url, target="_blank", rel="noopener noreferrer", class="boton-libro")
               } else {
                 span("Enlace por configurar", class="texto-metodo")
               }
  )
}

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", href = "styles.css?v=20261101c"),
    tags$style(HTML(paste(readLines("www/styles.css", warn = FALSE), collapse = "\n"))),
    tags$script(src = "theme-plotly.js"),
    tags$script(HTML("
(function() {
  function setTheme(mode) {
    document.documentElement.setAttribute('data-bs-theme', mode);
    document.body && document.body.setAttribute('data-bs-theme', mode);
    var toggle = document.getElementById('theme-switch');
    if (toggle) {
      toggle.setAttribute('aria-checked', mode === 'dark' ? 'true' : 'false');
      toggle.classList.toggle('is-dark', mode === 'dark');
    }
    try { localStorage.setItem('dashboard-theme', mode); } catch (e) {}
    if (window.Plotly) {
      document.querySelectorAll('.js-plotly-plot').forEach(function(graph) {
        var col = mode === 'dark' ? '#EDF0F5' : '#202936';
        var grid = mode === 'dark' ? '#3B4759' : '#E7E1DC';
        try {
          window.Plotly.relayout(graph, {
            'font.color': col,
            'xaxis.gridcolor': grid,
            'yaxis.gridcolor': grid,
            'paper_bgcolor': 'rgba(0,0,0,0)',
            'plot_bgcolor': 'rgba(0,0,0,0)'
          });
        } catch (e) {}
      });
    }
    window.dispatchEvent(new Event('resize'));
  }
  var starting = 'light';
  try { starting = localStorage.getItem('dashboard-theme') || 'light'; } catch (e) {}
  document.documentElement.setAttribute('data-bs-theme', starting);
  function positionThemeSwitch() {
    var toggle = document.getElementById('theme-switch');
    var nav = document.querySelector('.navbar .container-fluid');
    if (toggle && nav && toggle.parentNode !== nav) {
      nav.appendChild(toggle);
    }
  }
  document.addEventListener('DOMContentLoaded', function() {
    positionThemeSwitch();
    setTheme(starting);
    window.setTimeout(positionThemeSwitch, 250);
  });
  document.addEventListener('click', function(e) {
    var toggle = e.target.closest('#theme-switch');
    if (toggle) {
      e.preventDefault();
      setTheme(document.documentElement.getAttribute('data-bs-theme') === 'dark' ? 'light' : 'dark');
      return;
    }
    var entry = e.target.closest('#entrar_dashboard');
    if (entry) {
      var cover = document.getElementById('portada-dashboard');
      if (cover) {
        cover.classList.add('portada-oculta');
        window.setTimeout(function() { if (cover.isConnected) cover.remove(); }, 650);
      }
    }
    var link = e.target.closest('.navbar .dropdown-menu .dropdown-item, .navbar .dropdown-menu a');
    if (link) {
      var menu = link.closest('.dropdown-menu');
      var container = link.closest('.dropdown');
      var btn = container && container.querySelector('.dropdown-toggle');
      if (btn && window.bootstrap && window.bootstrap.Dropdown) {
        try { window.bootstrap.Dropdown.getOrCreateInstance(btn).hide(); } catch(err) {}
      }
      if (menu) menu.classList.remove('show');
      if (btn) { btn.classList.remove('show'); btn.setAttribute('aria-expanded','false'); }
      if (container) container.classList.remove('show');
    }
  });
  if (window.Shiny && Shiny.addCustomMessageHandler) {
    Shiny.addCustomMessageHandler('cerrar_portada', function() {
      var cover = document.getElementById('portada-dashboard');
      if (cover) { cover.classList.add('portada-oculta'); window.setTimeout(function(){cover.remove();},650); }
    });
    Shiny.addCustomMessageHandler('cerrar_menu', function() {
      document.querySelectorAll('.navbar .dropdown-menu.show').forEach(function(m){m.classList.remove('show');});
      document.querySelectorAll('.navbar .dropdown-toggle.show').forEach(function(b){b.classList.remove('show');b.setAttribute('aria-expanded','false');});
    });
  }
})();
    "))
  ),
  
  # PORTADA DE BIENVENIDA (fuera de navbarPage)
  div(
    id = "portada-dashboard",
    role = "dialog",
    `aria-label` = "Portada del dashboard",
    
    div(
      class = "portada-marco",
      
      div(
        class = "portada-top",
        span(class = "portada-etiqueta", span(class = "portada-punto"), "INVESTIGACIÓN · CIENCIA DE DATOS"),
        span(class = "portada-ruta", "EDA / Salud mental")
      ),
      
      div(
        class = "portada-contenedor",
        
        # TEXTO
        div(
          class = "portada-texto",
          h1("Pantallas, sueño y salud mental"),
          p("Exploración de la relación entre el uso de pantallas, los dominios del sueño y los síntomas depresivos en adolescentes."),
          div(
            class = "portada-indicadores",
            span(textOutput("pv_n", inline = TRUE), " adolescentes"),
            span(ncol(df), " variables")
          ),
          
          # BOTÓN DE ENTRADA AL DASHBOARD
          
          actionButton(
            inputId = "entrar_dashboard",
            
            label = tagList(
              "Explorar dashboard ",
              icon("arrow-right")
            ),
            
            class = "boton-entrada",
            
            onclick = "
    var portada = document.getElementById('portada-dashboard');
    if (portada) {
      portada.classList.add('portada-oculta');
      window.setTimeout(function(){ if (portada.isConnected) portada.remove(); }, 650);
    }
  "
          )
          
        ),
        
        # CELULAR INTERACTIVO
        div(
          class = "portada-visual",
          div(
            class = "celular",
            div(
              class = "celular-pantalla",
              div(class = "celular-camara"),
              div(class = "celular-cabecera", span("Bienestar"), span(class = "celular-ico")),
              div(
                class = "celular-grafico",
                p("Tiempo de pantalla"),
                div(
                  class = "celular-barras",
                  lapply(seq_along(c(35, 48, 70, 45, 90, 60, 100)), function(i) {
                    span(style = paste0("--h:", c(35, 48, 70, 45, 90, 60, 100)[i], "%;--i:", i))
                  })
                )
              ),
              div(
                class = "celular-notificacion",
                div(class = "notif-titulo", icon("bell"), span("Salud mental")),
                strong(textOutput("pv_dep", inline = TRUE)),
                tags$small("Clasificación BDI-II ≥ 14")
              ),
              div(class = "celular-home")
            )
          )
        )
      )
    )
  ),
  
  # INTERRUPTOR CLARO / OSCURO (UBICADO DENTRO DE LA BARRA)
  tags$button(
    id = "theme-switch", type = "button", class = "theme-switch",
    role = "switch", `aria-label` = "Alternar modo claro y oscuro",
    `aria-checked` = "false",
    tags$span("Claro", class = "switch-label switch-light"),
    tags$span("Oscuro", class = "switch-label switch-dark"),
    tags$span(class = "switch-thumb")
  ),
  
  # NAVEGACIÓN ORIGINAL
  navbarPage(
    
    title = div(
      style = "display: flex; align-items: center; height: 100%; padding-top: 5px;",
      tags$img(
        src = "logo.png",
        height = "40px",
        width = "auto",
        alt = "Logo Universidad del Norte"
      ),
      tags$span("", class = "marca-espaciador")
    ),
    
    id = "navegacion",
    selected = "inicio",
    # Tema de Bootstrap
    theme = bslib::bs_theme(
      version = 5
    ),
    
    # Las páginas se conservan en navbarPage
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
                textOutput("inicio_registros", inline = TRUE),
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
                textOutput("inicio_edad", inline = TRUE),
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
                textOutput("inicio_sexo", inline = TRUE),
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
                textOutput("inicio_deprimidos", inline = TRUE),
                class = "kpi-valor kpi-alerta"
              ),
              
              p(
                textOutput("n_deprimidos", inline = TRUE), " adolescentes con BDI-II ≥ 14",
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
                ),
                enlace_indice("link_cluster", "Segmentación")
                
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
                   no es novedad, pero ha obtenido mayor importancia en los últimos años, esto debido
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
                para predecir el nivel de riesgo depresivo (target definido en el dataset) a partir de 
                variables de pantalla, sueño y demográficas. Adicionalmente se plantea el entrenamiento y 
                evaluación de modelos de forma separada por género para contrastar con el enfoque multigrupo 
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
                y el sueño emergen como los predictores más relevantes."
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
          )
        ),
        
        h6("A continuación se presenta la metodología que se seguirá en este proyecto"),
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
        
        encabezado_eda("EDA", "Calidad de los datos"),
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
                "7 numéricas de análisis"
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
        
        encabezado_eda("EDA", "Variable objetivo"),
        
        
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
                textOutput("n_deprimidos_obj", inline = TRUE), " adolescentes con BDI-II ≥ 14",
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
                height = "540px"
              )
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            
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
          )
          
        )
        
        
        
      ),
      
      
      # 4.3 ANÁLISIS POR SEXO
      
      tabPanel(
        title = "Análisis por sexo",
        value = "sexo",
        
        encabezado_eda("EDA", "Análisis por sexo"),
        
        fluidRow(
          
          column(
            width = 3,
            
            div(
              class = "tarjeta-kpi",
              
              p("Chicas", class = "kpi-etiqueta"),
              
              h2(
                textOutput("sx_chicas_n", inline = TRUE),
                class = "kpi-valor"
              ),
              
              p(
                textOutput("sx_chicas_pct", inline = TRUE), " de la muestra",
                class = "kpi-descripcion"
              )
            )
          ),
          
          
          column(
            width = 3,
            
            div(
              class = "tarjeta-kpi",
              
              p("Chicos", class = "kpi-etiqueta"),
              
              h2(
                textOutput("sx_chicos_n", inline = TRUE),
                class = "kpi-valor"
              ),
              
              p(
                textOutput("sx_chicos_pct", inline = TRUE), " de la muestra",
                class = "kpi-descripcion"
              )
            )
          ),
          
          
          column(
            width = 3,
            
            div(
              class = "tarjeta-kpi",
              
              p("Deprimidas: chicas ", class = "kpi-etiqueta"),
              
              h2(
                textOutput("sx_dep_chicas_pct", inline = TRUE),
                class = "kpi-valor kpi-terracota"
              ),
              
              p(
                textOutput("sx_dep_chicas_txt", inline = TRUE),
                class = "kpi-descripcion"
              )
            )
          ),
          
          
          column(
            width = 3,
            
            div(
              class = "tarjeta-kpi",
              
              p("Deprimidos: Chicos", class = "kpi-etiqueta"),
              
              h2(
                textOutput("sx_dep_chicos_pct", inline = TRUE),
                class = "kpi-valor kpi-terracota"
              ),
              
              p(
                textOutput("sx_dep_chicos_txt", inline = TRUE),
                class = "kpi-descripcion"
              )
            )
          ),
        ),
        
        fluidRow(
          
          # IZQUIERDA: DEPRESIÓN POR SEXO
          column(
            width = 8,
            
            div(
              class = "tarjeta-contenido",
              
              h4("Estado depresivo por sexo"),
              
              p("Porcentaje dentro de cada sexo."),
              
              plotly::plotlyOutput(
                "grafico_depresion_sexo",
                height = "420px",
                width = "100%"
              )
            )
          ),
          
          # DERECHA: COMPOSICIÓN DE LA MUESTRA
          column(
            width = 4,
            
            div(
              class = "tarjeta-contenido",
              
              h4("Composición de la muestra"),
              
              p(
                "Distribución de ",
                tags$code("sex")
              ),
              
              plotly::plotlyOutput(
                "grafico_composicion_sexo",
                height = "420px",
                width = "100%"
              )
            )
          )
          
        ),
        fluidRow(
          
          # COLUMNA IZQUIERDA: BOXPLOT
          column(
            width = 8,
            
            div(
              class = "tarjeta-contenido",
              
              h4("Puntaje BDI-II por sexo"),
              
              p(
                "Distribución de ",
                tags$code("bdi_total"),
                " con el corte clínico."
              ),
              
              plotly::plotlyOutput(
                outputId = "boxplot_bdi_sexo",
                height = "610px",
                width = "100%"
              )
            )
          ),
          
          # COLUMNA DERECHA: DOS TABLAS
          column(
            width = 4,
            
            # TABLA 1: CONTINGENCIA
            div(
              class = "tarjeta-contenido",
              
              h4("Tabla de contingencia"),
              
              DT::DTOutput(
                outputId = "tabla_contingencia"
              )
            ),
            
            # TABLA 2: CONTRASTES ESTADÍSTICOS
            div(
              class = "tarjeta-contenido",
              
              h4(
                "Test chi-cuadrado: estado depresivo vs. sexo"
              ),
              
              DT::DTOutput(
                outputId = "tabla_chi_sexo"
              )
            )
          )
          
        ),
        fluidRow(
          column(12,
                 div(
                   class = "tarjeta-contenido",
                   
                   h4("Mann-Whitney U: variables numéricas por sexo"),
                   
                   p(
                     "Mann-Whitney U bilateral. ",
                     "r negativo: valores mayores en las chicas."
                   ),
                   
                   DT::DTOutput("tabla_mann_whitney")
                 )
          )
          
          
        )
      ),
      
      
      # 4.4 VARIABLES NUMÉRICAS
      
      
      tabPanel(
        title = "Variables numéricas",
        value = "numericas",
        
        encabezado_eda(
          "EDA", "Variables numéricas", c("7 variables"), url_libro = libro_url
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "tarjeta-contenido",
              
              fluidRow(
                column(
                  8,
                  
                  selectInput(
                    "variable",
                    "Variable",
                    
                    choices = c(
                      "Índice de calidad del sueño" =
                        "sleep_quality_index",
                      
                      "Horas promedio de sueño" =
                        "avg_sleep_hours",
                      
                      "Cronotipo" =
                        "midsleep_weekend_hours",
                      
                      "Jet lag social" =
                        "social_jetlag_hours",
                      
                      "Índice de pantalla" =
                        "screen_time_index",
                      
                      "Horas de pantalla recreativa" =
                        "est_leisure_screen_hours",
                      
                      "Puntaje BDI-II" =
                        "bdi_total"
                    ),
                    
                    selected = "sleep_quality_index",
                    width = "100%"
                  )
                ),
                
                column(
                  4,
                  
                  shinyWidgets::radioGroupButtons(
                    inputId = "separador",
                    label = "Separar",
                    choices = c(
                      "Sin separar" = "ninguno",
                      "Por sexo" = "sexo",
                      "Por estado depresivo" = "depresion"
                    ),
                    selected = "ninguno",
                    justified = TRUE,
                    size = "sm"
                  )
                )
              )
            )
          )
        ),
        
        
        fluidRow(
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Media", class = "kpi-etiqueta"),
              h2(textOutput("num_media", inline = TRUE),
                 class = "kpi-valor")
            )
          ),
          
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Mediana", class = "kpi-etiqueta"),
              h2(textOutput("num_mediana", inline = TRUE),
                 class = "kpi-valor")
            )
          ),
          
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Desviación estándar",
                class = "kpi-etiqueta"),
              h2(textOutput("num_sd", inline = TRUE),
                 class = "kpi-valor")
            )
          ),
          
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Asimetría", class = "kpi-etiqueta"),
              h2(textOutput("num_skew", inline = TRUE),
                 class = "kpi-valor")
            )
          )
        ),
        
        fluidRow(
          column(
            8,
            div(
              class = "tarjeta-contenido",
              
              h4("Histograma"),
              
              plotly::plotlyOutput(
                "histograma_numericas",
                height = "390px"
              )
            )
          ),
          
          column(
            4,
            div(
              class = "tarjeta-contenido",
              
              h4("Dispersión"),
              
              plotly::plotlyOutput(
                "boxplot_numericas",
                height = "390px"
              )
            )
          )
        )
      ),
      
      
      
      # 4.5 CORRELACIONES
      
      
      tabPanel(
        title = "Correlaciones",
        value = "correlaciones",
        
        encabezado_eda(
          "EDA",
          "Correlaciones",
          c("Spearman"), url_libro = libro_url
        ),
        
        # KPI
        fluidRow(
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Calidad del sueño · BDI-II",
                class = "kpi-etiqueta"),
              h2(textOutput("rho_calidad", inline = TRUE),
                 class = "kpi-valor")
            )
          ),
          
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Horas de sueño · BDI-II",
                class = "kpi-etiqueta"),
              h2(textOutput("rho_horas", inline = TRUE),
                 class = "kpi-valor")
            )
          ),
          
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Cronotipo · BDI-II",
                class = "kpi-etiqueta"),
              h2(textOutput("rho_cronotipo", inline = TRUE),
                 class = "kpi-valor")
            )
          ),
          
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Tiempo de pantalla · BDI-II",
                class = "kpi-etiqueta"),
              h2(textOutput("rho_pantalla", inline = TRUE),
                 class = "kpi-valor")
            )
          )
        ),
        
        fluidRow(
          column(
            8,
            div(
              class = "tarjeta-contenido",
              
              h4("Matriz de correlación de Spearman"),
              p("Triángulo inferior sin diagonal."),
              
              plotly::plotlyOutput(
                "matriz_spearman",
                height = "520px"
              )
            )
          ),
          
          column(
            4,
            div(
              class = "tarjeta-contenido",
              
              h4("Asociación con bdi_total"),
              
              plotly::plotlyOutput(
                "asociacion_bdi",
                height = "520px"
              )
            )
          )
        ),
        
        fluidRow(
          column(
            6,
            div(
              class = "tarjeta-contenido",
              
              h4("Pantalla frente a dominios del sueño"),
              
              DT::DTOutput("tabla_pantalla_sueno")
            )
          ),
          
          column(
            6,
            div(
              class = "tarjeta-contenido",
              
              h4("Dominios del sueño frente a BDI-II"),
              
              DT::DTOutput("tabla_sueno_bdi")
            )
          )
        )
      ),
      
      
      
      # 4.6 OUTLIERS
      
      
      tabPanel(
        title = "Outliers",
        value = "outliers",
        
        encabezado_eda(
          "EDA",
          "Outliers",
          c("Regla IQR · 1,5", "7 variables"), url_libro = libro_url
        ),
        
        fluidRow(
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Filas con algún outlier",
                class = "kpi-etiqueta"),
              h2(textOutput("out_filas", inline = TRUE),
                 class = "kpi-valor")
            )
          ),
          
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Deprimidos en esas filas",
                class = "kpi-etiqueta"),
              h2(textOutput("out_depresion", inline = TRUE),
                 class = "kpi-valor kpi-terracota")
            )
          ),
          
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Outliers en BDI-II",
                class = "kpi-etiqueta"),
              h2(textOutput("out_bdi", inline = TRUE),
                 class = "kpi-valor")
            )
          ),
          
          column(
            3,
            div(
              class = "tarjeta-kpi",
              p("Variables sin outliers",
                class = "kpi-etiqueta"),
              h2(textOutput("out_sin", inline = TRUE),
                 class = "kpi-valor")
            )
          )
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "tarjeta-contenido",
              
              h4("Outliers por variable (IQR)"),
              
              p("Límites de Tukey: Q1 − 1,5 × IQR y Q3 + 1,5 × IQR."),
              
              DT::DTOutput("tabla_outliers")
            )
          )
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "tarjeta-contenido",
              
              h4("Proporción de outliers según estado depresivo"),
              
              p(
                "Predictores de pantalla y sueño. ",
                "Se excluye bdi_total."
              ),
              
              plotly::plotlyOutput(
                "grafico_outliers",
                height = "420px"
              )
            )
          )
        )
      ),
      
      # 4.7 SEGMENTACIÓN / CLÚSTERES
      tabPanel(
        title = "Segmentación",
        value = "cluster",
        encabezado_eda("EDA", "Perfiles de pantalla y sueño", c("K-means", "PCA")),
        fluidRow(
          column(12, div(class = "tarjeta-contenido",
                         h4("Configuración"),
                         sliderInput("numero_clusters", "Número de clústeres (K)",
                                     min = 2, max = 8, value = 3, step = 1)
          ))
        ),
        fluidRow(
          column(4, tarjeta_kpi("Adolescentes analizados", "cluster_n", "Observaciones completas")),
          column(4, tarjeta_kpi("Número de perfiles", "cluster_k", "Agrupamiento K-means")),
          column(4, tarjeta_kpi("Varianza PCA explicada", "cluster_pca", "Dos primeras componentes"))
        ),
        fluidRow(
          column(8, div(class = "tarjeta-contenido", h4("Proyección de los perfiles"),
                        p("Proyección PCA bidimensional de los grupos formados usando pantalla y sueño."),
                        plotly::plotlyOutput("grafico_clusters", height = "480px")
          )),
          column(4, div(class = "tarjeta-contenido", h4("Tamaño de los grupos"),
                        DT::DTOutput("tabla_clusters")
          ))
        ),
        fluidRow(
          column(8, div(class = "tarjeta-contenido",
                        h4("Síntomas depresivos por perfil"),
                        p("Prevalencia dentro de cada perfil de adolescentes con BDI-II ≥ 14."),
                        plotly::plotlyOutput("grafico_depresion_cluster", height = "450px")
          )),
          column(4, div(class = "tarjeta-contenido",
                        h4("Casos por perfil"),
                        DT::DTOutput("tabla_depresion_cluster")
          ))
        ),
        fluidRow(
          column(8, div(class = "tarjeta-contenido", h4("Características de los perfiles"),
                        p("Valores Z promedio de las seis variables utilizadas para agrupar."),
                        plotly::plotlyOutput("perfil_clusters", height = "450px")
          )),
          column(4, div(class = "tarjeta-contenido", h4("Asociación con depresión"),
                        DT::DTOutput("tabla_chi_clusters"),
                        p(class="texto-metodo", "Prueba exploratoria. La asociación no establece causalidad ni valida el número de grupos.")
          ))
        )
      )
    ),
    
    # 5. CIERRE
    navbarMenu(
      "Cierre",
      
      tabPanel(
        title = "Hallazgos",
        value = "hallazgos",
        
        encabezado_pagina("Cierre", "Hallazgos"),
        
        fluidRow(
          column(3, tarjeta_kpi("Prevalencia BDI-II ≥ 14", "hz_prevalencia", "De toda la muestra", "kpi-terracota")),
          column(3, tarjeta_kpi("Brecha por sexo", "hz_brecha", "Chicas vs. chicos deprimidos", "kpi-terracota")),
          column(3, tarjeta_kpi("Mayor asociación con BDI-II", "hz_top_var", "Variable con |ρ| más alto")),
          column(3, tarjeta_kpi("Valor de ρ", "hz_top_rho", "Correlación de Spearman"))
        ),
        
        fluidRow(
          column(
            12,
            div(
              class = "tarjeta-contenido",
              h4("Asociaciones con el puntaje BDI-II"),
              p("Correlación de Spearman entre cada predictor y bdi_total, calculada sobre los datos cargados."),
              DT::DTOutput("tabla_hallazgos")
            )
          )
        ),
        
       
      ),
      
      tabPanel(
        title = "Limitaciones",
        value = "limitaciones",
        
        encabezado_pagina("Cierre", "Limitaciones"),
        
        fluidRow(
          column(
            6,
            div(
              class = "tarjeta-contenido",
              h4("Diseño y medición"),
              tags$ul(
                tags$li("Las asociaciones del EDA son correlacionales: no permiten afirmar causalidad."),
                tags$li("Pantallas y sueño provienen de medidas de autorreporte y pueden tener sesgo de memoria."),
                tags$li("El BDI-II ≥ 14 es un punto de corte de tamizaje, no un diagnóstico clínico.")
              )
            )
          ),
          column(
            6,
            div(
              class = "tarjeta-contenido",
              h4("Alcance del análisis"),
              tags$ul(
                tags$li("La muestra proviene de centros escolares de Estocolmo; la generalización a otros contextos es limitada."),
                tags$li("Los outliers se identifican con el criterio IQR de Tukey y se conservan en el análisis."),
              )
            )
          )
        )
      )
    ),
    
    # 6. ESTUDIOS
    navbarMenu(
      "Estudios",
      tabPanel(
        title = "Estudios revisados", value = "estudios",
        encabezado_eda("Estudios", "Estudios revisados", c("5 publicaciones")),
        h4(class = "titulo-bloque", "Estudios revisados"),
        div(class = "estudios-grid",
            estudio_card("Hökby et al., 2023",
                         "Longitudinal Effects of Screen Time on Depressive Symptoms among Swedish Adolescents: The Moderating and Mediating Role of Coping Engagement Behavior",
                         "4.793 adolescentes suecos", "Longitudinal (0, 3 y 12 meses)",
                         "El tiempo de pantalla no predijo la depresión de forma directa (p = 0,469); sí de forma indirecta al interferir con el afrontamiento centrado en el problema, con un efecto máximo de 3,4 puntos en el BDI-II.",
                         url="https://www.mdpi.com/1660-4601/20/4/3771"),
            estudio_card("Lemke et al., 2023",
                         "Associations between sleep habits, quality, chronotype and depression in a large cross-sectional sample of Swedish adolescents",
                         "8.449 adolescentes suecos de 12–16 años", "Transversal",
                         "La duración del sueño entre semana (OR = 0,773), la calidad del sueño (OR = 0,327) y el cronotipo tardío (OR = 1,126) predijeron la depresión (BDI-II > 13); la duración en fin de semana no fue significativa.",
                         url="https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0293580"),
            estudio_card("Saleem et al., 2024",
                         "Exploring the relationship between social media use and symptoms of depression and anxiety among children and adolescents: A systematic narrative review",
                         "67 estudios (de 4.850 identificados), población de 5 a 18 años", "Revisión narrativa sistemática",
                         "Asociación positiva consistente entre el uso de redes sociales y los síntomas de depresión y ansiedad, con alta heterogeneidad metodológica."),
            estudio_card("Hökby et al., 2025",
                         "Adolescents’ screen time displaces multiple sleep pathways and elevates depressive symptoms over twelve months",
                         "4.810 adolescentes suecos de 12–16 años", "Longitudinal · SEM multigrupo",
                         "El tiempo de pantalla deterioró el sueño a los 3 meses. En chicas, el efecto sobre la depresión fue mediado por la calidad (57 %), la duración (38 %) y el cronotipo (45 %); en chicos fue directo.",
                         url="https://journals.plos.org/globalpublichealth/article?id=10.1371/journal.pgph.0004262", base = TRUE),
            estudio_card("Chen et al., 2026",
                         "Predicting adolescent depression: an interpretable machine learning model",
                         "1.226 adolescentes chinos de 13–25 años", "Aprendizaje automático (5 algoritmos)",
                         "XGBoost obtuvo el mejor desempeño (AUC = 0,927). Según SHAP, los predictores más relevantes fueron el uso de somníferos, la indiferencia parental y el nivel educativo.",
                         url="https://doi.org/10.1186/s12911-026-03482-5")
        ),
        p(class="texto-metodo", "Fichas reproducidas de las referencias compartidas; verifica los detalles bibliográficos y las fuentes primarias antes de citarlas en tu investigación.")
      )
    ),
    
    # 7. ACERCA DE / EQUIPO Y RECURSOS
    navbarMenu(
      "Acerca de",
      tabPanel(
        title = "Equipo y recursos", value = "equipo",
        encabezado_eda("Acerca de", "Equipo y recursos", c("2 integrantes", "Universidad del Norte")),
        h4(class="titulo-bloque", "Equipo"),
        div(class="equipo-grid",
            integrante_card("Luis Cabarcas", "8vo semestre", "Matemáticas · Ciencia de Datos", "L", "luis.jpg"),
            integrante_card("Santiago", "8vo semestre", "Ciencia de Datos", "S", "santiago.jpg")
        ),
        h4(class="titulo-bloque", "Recursos del proyecto"),
        div(class="recursos-grid",
            recurso_card(
              "Repositorio",
              "Código del dashboard.",
              url = "https://github.com/sjimenezurrea-sudo/Dashboard-shiny.git",
              "Abrir en GitHub"
            ),
            
            recurso_card(
              "R Book",
              "Documento del proyecto: marco teórico, metodología y EDA.",
              url = "https://sjimenezurrea-sudo.github.io/rbook_Dataviz2026/",
              "Abrir el libro"
            ),
            
            recurso_card(
              "Dataset",
              "Screen Time vs Mental Health (ML-ready) en Kaggle.",
              url = "https://www.kaggle.com/datasets/kylefengkfeng209/screen-time-vs-mental-health-ml-ready",
              "Abrir dataset"
            )
        )
      )
    )
  )
)

