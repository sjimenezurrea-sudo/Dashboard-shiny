# Funciones visuales que se cargan desde global.R y se invocan dentro de navbarPage().
# Requiere encabezado_pagina(seccion,pagina), definido en tu ui.R.
md_card <- function(titulo, ..., subtitulo=NULL) {
  shiny::div(class='tarjeta-contenido',shiny::h4(titulo),
    if(!is.null(subtitulo)) shiny::p(subtitulo,class='texto-metodo'), ...)
}
md_plot <- function(id, altura='380px') plotly::plotlyOutput(id,height=altura)

md_tabs_modelado <- function(encabezado_pagina) {
  shiny::navbarMenu('Modelado',
    shiny::tabPanel('Benchmarking',value='benchmarking',
      encabezado_pagina('Modelado','Benchmarking'),
      shiny::div(class='tarjeta-contenido md-sticky',
        shinyWidgets::radioGroupButtons('md_tarea','Tarea',
          choices=c('Clasificación · depressed'='clf','Regresión · bdi_total'='reg'),
          selected='clf',size='sm'),
        shiny::div(class='md-acciones',shiny::span('Modelos'),
          shiny::actionLink('md_solo','Solo el modelo final'),
          shiny::actionLink('md_todos','Mostrar todos')),
        shiny::uiOutput('md_selector'),
        shiny::actionButton('md_actualizar','Recargar resultados',icon=shiny::icon('refresh'),class='btn-sm')),
      shiny::uiOutput('md_bm_kpis'),
      shiny::conditionalPanel("input.md_tarea == 'clf'",
        shiny::fluidRow(shiny::column(12,md_card('Validación cruzada de los ganadores',
          DT::DTOutput('md_cv_clf')))),
        shiny::fluidRow(shiny::column(12,md_card('PR-AUC de CV por modelo',
          md_plot('md_puntos_clf','350px'),
          subtitulo='Puntos = media; barras = desviación estándar.'))),
        shiny::fluidRow(shiny::column(6,md_card('Curvas ROC · test',md_plot('md_roc'))),
                        shiny::column(6,md_card('Curvas Precision–Recall · test',md_plot('md_pr')))),
        shiny::fluidRow(shiny::column(12,md_card('Métricas de prueba',DT::DTOutput('md_test_clf'))))),
      shiny::conditionalPanel("input.md_tarea == 'reg'",
        shiny::fluidRow(shiny::column(12,md_card('Validación cruzada de los regresores',DT::DTOutput('md_cv_reg')))),
        shiny::fluidRow(shiny::column(12,md_card('RMSE de CV por modelo',md_plot('md_puntos_reg','350px')))),
        shiny::fluidRow(shiny::column(12,md_card('Métricas en prueba',DT::DTOutput('md_test_reg')))))
    ),
    shiny::tabPanel('Modelos finales',value='modelos_finales',
      encabezado_pagina('Modelado','Modelos finales'),
      shiny::uiOutput('md_final_titulo'),shiny::uiOutput('md_final_kpis_clf'),
      shiny::fluidRow(
        shiny::column(5,md_card('Matriz de confusión',md_plot('md_matriz','340px'))),
        shiny::column(7,md_card('Precisión, sensibilidad y F1 según el umbral',md_plot('md_umbral','340px')))),
      shiny::fluidRow(shiny::column(12,md_card('Importancia por permutación del clasificador',
         md_plot('md_imp_clf','350px'),subtitulo='Disminución de PR-AUC al permutar un predictor en el test; no es SHAP.'))),
      shiny::uiOutput('md_final_titulo_reg'),shiny::uiOutput('md_final_kpis_reg'),
      shiny::fluidRow(
        shiny::column(6,md_card('BDI-II predicho frente al real',md_plot('md_pred_real','390px'))),
        shiny::column(6,md_card('Coeficientes del modelo lineal, si aplica',DT::DTOutput('md_coef_reg')))),
      shiny::fluidRow(shiny::column(12,md_card('Importancia por permutación del regresor',md_plot('md_imp_reg','330px')))),
      shiny::fluidRow(shiny::column(12,md_card('BDI estimado ≥ 14 frente al clasificador',
        DT::DTOutput('md_experimento'),
        subtitulo='Un puntaje regresado y umbralizado no sustituye la clasificación probabilística.')))
    ),
    shiny::tabPanel('Modelos por género',value='genero_modelado',
      encabezado_pagina('Modelado','Modelos por género'),
      shiny::uiOutput('md_genero_kpis'),
      shiny::fluidRow(shiny::column(12,md_card('Modelo conjunto frente a modelos por sexo',DT::DTOutput('md_genero_tabla')))),
      shiny::fluidRow(shiny::column(12,md_card('Importancia por permutación · chicos y chicas',
        md_plot('md_genero_imp','380px')))),
      shiny::fluidRow(shiny::column(12,md_card('Sensibilidad y precisión · umbral conjunto vs. propio',
        md_plot('md_genero_umbral','350px'))))
    ),
    shiny::tabPanel('Clustering',value='clustering_modelado',
      encabezado_pagina('Modelado','Clustering'),
      shiny::uiOutput('md_cluster_kpis'),
      shiny::fluidRow(shiny::column(12,md_card('Perfiles de sueño y pantalla · medias estandarizadas',
        md_plot('md_perfiles','410px')))),
      shiny::fluidRow(
        shiny::column(7,md_card('Proyección PCA',md_plot('md_pca','440px'))),
        shiny::column(5,md_card('Prevalencia de depresión por perfil',md_plot('md_prevalencias','440px')))),
      shiny::fluidRow(
        shiny::column(6,md_card('BDI-II por perfil',md_plot('md_bdi_cluster','370px'))),
        shiny::column(6,md_card('Proporción de chicas por perfil',md_plot('md_sexo_cluster','370px')))),
      shiny::fluidRow(shiny::column(12,md_card('Selección de K (silueta aproximada)',
        DT::DTOutput('md_k_tabla'))))
    ),
    shiny::tabPanel('Predicción',value='prediccion_modelado',
      encabezado_pagina('Modelado','Predicción'),
      shiny::fluidRow(
        shiny::column(5,md_card('Perfil del adolescente',
          shiny::uiOutput('md_controles_pred'),
          shiny::actionButton('md_calcular','Calcular',class='btn-primary'))),
        shiny::column(7,
          md_card('1 · Clasificación: probabilidad de síntomas depresivos',
            shiny::h2(shiny::textOutput('md_probabilidad',inline=TRUE)),
            shiny::textOutput('md_estado_pred'),md_plot('md_prob_plot','160px')),
          md_card('2 · Regresión: puntaje BDI-II estimado',
            shiny::h2(shiny::textOutput('md_bdi_estimado',inline=TRUE)),
            shiny::textOutput('md_categoria_pred'),md_plot('md_bdi_plot','160px')))),
      shiny::p('Herramienta académica y exploratoria; no proporciona un diagnóstico clínico.',
               class='texto-metodo')
    )
  )
}
