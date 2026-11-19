# Funciones de server, se cargan en global.R; se invoca md_server(input,output,session)
# desde el server principal, sin sustituir otros observers ni outputs.
md_server <- function(input, output, session) {
  datos_md <- shiny::reactive({
    input$md_actualizar
    if (!file.exists(md_ruta)) return(NULL)
    tryCatch(readRDS(md_ruta), error=function(e) NULL)
  })
  d_md <- function() {
    d <- datos_md()
    shiny::validate(shiny::need(!is.null(d),
      paste('Faltan resultados. Ejecuta source("scripts/entrenar_modelado.R")',
            'desde la raíz del proyecto y vuelve a abrir la aplicación.')))
    d
  }
  md_kpis <- function(items) {
    shiny::fluidRow(lapply(items,function(item)
      shiny::column(width=12/length(items),
        shiny::div(class='tarjeta-kpi',shiny::p(item[[1]],class='kpi-etiqueta'),
          shiny::h2(item[[2]],class='kpi-valor'),
          shiny::p(item[[3]],class='kpi-descripcion')))))
  }
  get_meta <- function(d,tipo) if(tipo=='clf')d$clf$meta else d$reg$meta
  get_cv <- function(d,tipo) if(tipo=='clf')d$clf$cv else d$reg$cv
  get_test <- function(d,tipo) if(tipo=='clf')d$clf$test else d$reg$test
  get_final <- function(d,tipo) get_meta(d,tipo)$modelo
  md_ancha <- function(long, modelos=NULL) {
    if(!is.null(modelos)) long <- long[long$modelo %in% modelos,,drop=FALSE]
    if(!nrow(long)) return(data.frame())
    partes <- split(long,long$modelo)
    do.call(rbind,lapply(partes,function(g) {
      z <- data.frame(modelo=g$modelo[1],nombre=g$nombre[1])
      for(i in seq_len(nrow(g))) {
        nm <- g$metrica[i]; z[[paste0(nm,'_media')]] <- g$media[i]
        z[[paste0(nm,'_DE')]] <- g$desviacion[i]
      }
      z
    }))
  }
  # ------------------- SELECTOR BENCHMARK -------------------
  output$md_selector <- shiny::renderUI({
    d <- d_md(); tarea <- input$md_tarea %||% 'clf'
    cv <- get_cv(d,tarea); noms <- unique(cv$modelo)
    shinyWidgets::checkboxGroupButtons('md_modelos',label=NULL,
      choices=stats::setNames(noms,md_nombre_modelo(noms)),
      selected=intersect(get_final(d,tarea),noms),
      size='sm',justified=FALSE,individual=FALSE)
  })
  observeEvent(input$md_solo, {
    d <- datos_md(); req(!is.null(d))
    shinyWidgets::updateCheckboxGroupButtons(session,'md_modelos',
      selected=get_final(d,input$md_tarea %||% 'clf'))
  })
  observeEvent(input$md_todos, {
    d <- datos_md(); req(!is.null(d))
    shinyWidgets::updateCheckboxGroupButtons(session,'md_modelos',
      selected=unique(get_cv(d,input$md_tarea %||% 'clf')$modelo))
  })
  md_sel <- function(tipo) {
    d <- d_md(); noms <- intersect(unique(get_cv(d,tipo)$modelo),input$md_modelos)
    shiny::validate(shiny::need(length(noms)>0,'Selecciona al menos un modelo.'))
    noms
  }
  output$md_bm_kpis <- shiny::renderUI({
    d <- d_md(); tipo <- input$md_tarea %||% 'clf'
    cv <- md_ancha(get_cv(d,tipo)); final <- get_final(d,tipo)
    row <- cv[cv$modelo==final,,drop=FALSE]
    if(tipo=='clf') {
      puntajes <- cv[['PR-AUC_media']]
      md_kpis(list(
        c('Modelo final',md_nombre_modelo(final),'Elegido mediante PR-AUC de CV'),
        c('PR-AUC CV',paste(md_formato(row[['PR-AUC_media']]),'±',md_formato(row[['PR-AUC_DE']])), '5 folds'),
        c('Rango de PR-AUC',md_formato(diff(range(puntajes))), 'Máximo – mínimo entre candidatos'),
        c('Prevalencia (azar)',md_formato(d$clf$meta$prevalencia_train), 'Línea base en PR')))
    } else {
      md_kpis(list(
        c('Modelo final',md_nombre_modelo(final),'Elegido mediante RMSE de CV'),
        c('RMSE CV',paste(md_formato(row[['RMSE_media']]),'±',md_formato(row[['RMSE_DE']])), 'Puntos BDI-II'),
        c('Mejor RMSE',md_formato(min(cv[['RMSE_media']])), 'Entre los candidatos'),
        c('R² CV',md_formato(row[['R²_media']]), '5 folds')))
    }
  })
  md_cv_tabla <- function(tipo) {
    d <- d_md(); x <- md_ancha(get_cv(d,tipo), md_sel(tipo))
    key <- if(tipo=='clf')'PR-AUC_media' else 'RMSE_media'
    x <- x[order(x[[key]],decreasing=(tipo=='clf')),,drop=FALSE]
    met <- if(tipo=='clf')c('PR-AUC','ROC-AUC','Precision','Recall','F1','Accuracy') else c('RMSE','MAE','R²')
    tab <- data.frame(Modelo=md_nombre_modelo(x$modelo),check.names=FALSE)
    for(m in met) tab[[m]] <- paste0(
      vapply(x[[paste0(m,'_media')]],md_formato,character(1)), ' ± ',
      vapply(x[[paste0(m,'_DE')]],md_formato,character(1)))
    DT::datatable(tab,rownames=FALSE,class='compact tabla-eda',
      options=list(dom='t',scrollX=TRUE,ordering=FALSE))
  }
  output$md_cv_clf <- DT::renderDT(md_cv_tabla('clf'))
  output$md_cv_reg <- DT::renderDT(md_cv_tabla('reg'))
  md_puntos <- function(tipo) {
    d<-d_md(); x<-md_ancha(get_cv(d,tipo),md_sel(tipo))
    met<-if(tipo=='clf')'PR-AUC' else 'RMSE'
    media<-x[[paste0(met,'_media')]]; de<-x[[paste0(met,'_DE')]]
    x$Etiqueta<-factor(md_nombre_modelo(x$modelo),levels=md_nombre_modelo(x$modelo[order(media)]))
    plotly::plot_ly(data=x,x=media,y=~Etiqueta,type='scatter',mode='markers',
      marker=list(size=12,color=ifelse(x$modelo==get_final(d,tipo),'#9E8154','#A3C3E5'),
                  symbol=ifelse(x$modelo==get_final(d,tipo),'diamond','circle')),
      error_x=list(type='data',array=de,visible=TRUE,width=5,thickness=1.5),
      showlegend=FALSE) %>%
      plotly::layout(xaxis=list(title=paste(met,'de CV (media ± DE)')),
                     yaxis=list(title=''),paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)') %>%
      plotly::config(displayModeBar=FALSE,responsive=TRUE)
  }
  output$md_puntos_clf <- plotly::renderPlotly(md_puntos('clf'))
  output$md_puntos_reg <- plotly::renderPlotly(md_puntos('reg'))
  md_curvas <- function(tipo) {
    d <- d_md(); nombres <- md_sel('clf')
    p <- plotly::plot_ly()
    for(n in nombres) {
      curva <- md_curva(d$test_y,d$clf$predicciones[[n]]$prob)
      p <- if(tipo=='roc') plotly::add_lines(p,data=curva,x=~FPR,y=~Recall,
                                              name=md_nombre_modelo(n),line=list(shape='hv'))
        else plotly::add_lines(p,data=curva,x=~Recall,y=~Precision,
                               name=md_nombre_modelo(n),line=list(shape='hv'))
    }
    formas <- if(tipo=='roc')list(list(type='line',x0=0,y0=0,x1=1,y1=1,
                                          line=list(color='#bbbbbb',dash='dash')))
      else list(list(type='line',x0=0,x1=1,y0=d$clf$meta$prevalencia_test,
                     y1=d$clf$meta$prevalencia_test,line=list(color='#bbbbbb',dash='dash')))
    plotly::layout(p,xaxis=list(title=if(tipo=='roc')'Tasa de falsos positivos' else 'Recall',range=c(0,1)),
      yaxis=list(title=if(tipo=='roc')'Sensibilidad' else 'Precisión',range=c(0,1)),
      shapes=formas, paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)') %>%
      plotly::config(displayModeBar=FALSE,responsive=TRUE)
  }
  output$md_roc <- plotly::renderPlotly(md_curvas('roc'))
  output$md_pr <- plotly::renderPlotly(md_curvas('pr'))
  md_test_tabla <- function(tipo) {
    d <- d_md(); t <- get_test(d,tipo)
    t <- t[t$modelo %in% md_sel(tipo),,drop=FALSE]
    t$modelo <- md_nombre_modelo(t$modelo)
    wide <- stats::reshape(t,idvar='modelo',timevar='metrica',direction='wide')
    names(wide) <- sub('^valor\\.','',names(wide))
    wide[-1] <- lapply(wide[-1], function(z) vapply(z,md_formato,character(1)))
    DT::datatable(wide,rownames=FALSE,class='compact tabla-eda',
                  options=list(dom='t',scrollX=TRUE,ordering=FALSE))
  }
  output$md_test_clf <- DT::renderDT(md_test_tabla('clf'))
  output$md_test_reg <- DT::renderDT(md_test_tabla('reg'))
  # ------------------- MODELOS FINALES -------------------
  output$md_final_titulo <- shiny::renderUI({
    d<-d_md(); shiny::h3(paste('Clasificación ·',md_nombre_modelo(d$clf$meta$modelo)))
  })
  output$md_final_kpis_clf <- shiny::renderUI({
    d<-d_md();m<-d$clf$meta$metricas_test_umbral
    md_kpis(list(c('PR-AUC test',md_formato(m['PR-AUC']),'Probabilidades en prueba'),
      c('ROC-AUC test',md_formato(m['ROC-AUC']),'Discriminación'),
      c('Recall',md_formato(m['Recall']),paste('Umbral',md_formato(d$clf$meta$umbral))),
      c('Precisión',md_formato(m['Precision']),'Umbral fijado con train')))
  })
  output$md_matriz <- plotly::renderPlotly({
    d<-d_md();real<-factor(d$test_y,levels=c(0,1),labels=c('No','Sí'))
    pred<-factor(as.integer(d$clf$predicciones[[d$clf$meta$modelo]]$prob >= d$clf$meta$umbral),
                 levels=c(0,1),labels=c('No','Sí'))
    m<-table(real,pred)
    plotly::plot_ly(x=colnames(m),y=rownames(m),z=unclass(m),type='heatmap',
      colorscale=list(c(0,'#e6eff8'),c(1,'#8eabc7')),showscale=FALSE) %>%
      plotly::layout(xaxis=list(title='Predicho'),yaxis=list(title='Real'))
  })
  output$md_umbral <- plotly::renderPlotly({
    d<-d_md(); t<-d$clf$umbrales; p<-plotly::plot_ly()
    for (m in c('Precision','Recall','F1')) p<-plotly::add_lines(p,data=t,x=~umbral,y=t[[m]],name=m)
    plotly::layout(p,xaxis=list(title='Umbral',range=c(0,1)),yaxis=list(title='Valor',range=c(0,1)),
       shapes=list(list(type='line',x0=d$clf$meta$umbral,x1=d$clf$meta$umbral,y0=0,y1=1,
                        line=list(color='#9E8154',dash='dash'))))
  })
  md_bar_imp <- function(datos,col='importancia') {
    datos$variable <- factor(datos$variable,levels=datos$variable[order(datos[[col]])])
    plotly::plot_ly(datos,x=datos[[col]],y=~variable,type='bar',orientation='h',
                    marker=list(color='#A3C3E5')) %>%
      plotly::layout(xaxis=list(title='Cambio en métrica'),yaxis=list(title=''),
                     paper_bgcolor='rgba(0,0,0,0)',plot_bgcolor='rgba(0,0,0,0)')
  }
  output$md_imp_clf <- plotly::renderPlotly(md_bar_imp(d_md()$clf$importancia))
  output$md_final_titulo_reg <- shiny::renderUI({
    d<-d_md(); shiny::h3(paste('Regresión ·',md_nombre_modelo(d$reg$meta$modelo)))
  })
  output$md_final_kpis_reg <- shiny::renderUI({
    d<-d_md();m<-d$reg$meta$metricas_test
    md_kpis(list(c('RMSE test',md_formato(m['RMSE']),'Puntos BDI'),
                 c('MAE test',md_formato(m['MAE']),'Puntos BDI'),
                 c('R² test',md_formato(m['R²']),'Varianza explicada')))
  })
  output$md_pred_real <- plotly::renderPlotly({
    d<-d_md();p<-d$reg$predicciones[[d$reg$meta$modelo]]$pred
    plotly::plot_ly(x=d$test_bdi,y=p,type='scatter',mode='markers',
                    marker=list(color='#A3C3E5',opacity=.42,size=6),showlegend=FALSE) %>%
      plotly::layout(xaxis=list(title='BDI-II real',range=c(0,63)),
        yaxis=list(title='BDI-II predicho',range=c(0,63)),
        shapes=list(list(type='line',x0=0,y0=0,x1=63,y1=63,
                         line=list(color='#9E8154',dash='dash'))))
  })
  output$md_coef_reg <- DT::renderDT({
    d<-d_md();x<-d$reg$coeficientes
    if(is.null(x)) x<-data.frame(Mensaje='El modelo ganador no tiene coeficientes lineales.')
    DT::datatable(x,rownames=FALSE,options=list(dom='t',scrollX=TRUE))
  })
  output$md_imp_reg <- plotly::renderPlotly(md_bar_imp(d_md()$reg$importancia))
  output$md_experimento <- DT::renderDT({
    d<-d_md();x<-do.call(rbind,lapply(names(d$experimento),function(n)
      data.frame(Enfoque=n,t(d$experimento[[n]]),check.names=FALSE)))
    DT::datatable(x,rownames=FALSE,options=list(dom='t',scrollX=TRUE))
  })
  # ------------------- GENERO -------------------
  output$md_genero_kpis <- shiny::renderUI({
    d<-d_md();g<-d$genero
    if(length(g)<2) return(shiny::p('Se necesitan datos de ambos grupos.'))
    md_kpis(list(
      c('ROC-AUC · chicos',md_formato(g$Boy$especifico['ROC-AUC']),'Modelo específico'),
      c('ROC-AUC · chicas',md_formato(g$Girl$especifico['ROC-AUC']),'Modelo específico'),
      c('Recall · chicos',md_formato(g$Boy$especifico['Recall']),'Umbral propio'),
      c('Recall · chicas',md_formato(g$Girl$especifico['Recall']),'Umbral propio')))
  })
  output$md_genero_tabla <- DT::renderDT({
    g<-d_md()$genero
    x<-do.call(rbind,lapply(names(g),function(s)do.call(rbind,lapply(c('especifico','conjunto'),function(tipo)
      data.frame(Sexo=if(s=='Boy')'Chicos' else 'Chicas',Enfoque=tipo,
        N_test=g[[s]]$n_test,Umbral=if(tipo=='especifico')g[[s]]$umbral else d_md()$clf$meta$umbral,
        ROC_AUC=g[[s]][[tipo]]['ROC-AUC'],Precision=g[[s]][[tipo]]['Precision'],
        Recall=g[[s]][[tipo]]['Recall'])))))
    DT::datatable(x,rownames=FALSE,options=list(dom='t',scrollX=TRUE)) %>%
      DT::formatRound(columns=names(x)[-(1:3)],digits=3,dec.mark=',')
  })
  output$md_genero_imp <- plotly::renderPlotly({
    g<-d_md()$genero
    x<-do.call(rbind,lapply(names(g),function(s) data.frame(
      Sexo=if(s=='Boy')'Chicos' else 'Chicas',g[[s]]$importancia)))
    plotly::plot_ly(x,x=~variable,y=~importancia,color=~Sexo,type='bar') %>%
      plotly::layout(barmode='group',xaxis=list(title='',tickangle=-25),
                     yaxis=list(title='Pérdida de PR-AUC al permutar'))
  })
  output$md_genero_umbral <- plotly::renderPlotly({
    g<-d_md()$genero
    x<-do.call(rbind,lapply(names(g),function(s)do.call(rbind,lapply(c('especifico','conjunto'),function(tipo)
      data.frame(Sexo=if(s=='Boy')'Chicos' else 'Chicas',Modelo=tipo,
                 Recall=g[[s]][[tipo]]['Recall'],Precision=g[[s]][[tipo]]['Precision'])))))
    plotly::plot_ly(x,x=~Sexo,y=~Recall,color=~Modelo,type='bar') %>%
      plotly::layout(barmode='group',yaxis=list(range=c(0,1),title='Recall'),
        annotations=list(list(x=0,y=1.04,text='Comparar también Precision en la tabla',
                              showarrow=FALSE,xref='paper',yref='paper',xanchor='left')))
  })
  # ------------------- CLUSTERING -------------------
  output$md_cluster_kpis <- shiny::renderUI({
    d<-d_md(); c<-d$clustering;w<-c$perfiles[which.max(c$perfiles$prevalencia),]
    sil<-c$evaluacion$silueta[c$evaluacion$k==c$k]
    md_kpis(list(c('Perfiles',as.character(c$k),'K-means estandarizado'),
      c('Silueta',md_formato(sil),'Estimación en muestra de hasta 600'),
      c('Mayor prevalencia',paste0(md_formato(100*w$prevalencia,1),' %'),as.character(w$perfil))))
  })
  output$md_perfiles <- plotly::renderPlotly({
    d<-d_md()$clustering$centroides
    plotly::plot_ly(d,x=~variable,y=~media,color=~perfil,type='bar') %>%
      plotly::layout(barmode='group',xaxis=list(title='',tickangle=-30),
                     yaxis=list(title='Media estandarizada'))
  })
  output$md_pca <- plotly::renderPlotly({
    d<-d_md()$clustering$puntos
    plotly::plot_ly(d,x=~PC1,y=~PC2,color=~perfil,type='scatter',mode='markers',
                    marker=list(size=5,opacity=.5)) %>%
      plotly::layout(xaxis=list(title='PC1'),yaxis=list(title='PC2'))
  })
  output$md_prevalencias <- plotly::renderPlotly({
    d<-d_md()$clustering$perfiles
    plotly::plot_ly(d,x=~perfil,y=~(100*prevalencia),type='bar',
      marker=list(color='#EFAC91')) %>% plotly::layout(yaxis=list(title='Porcentaje'))
  })
  output$md_bdi_cluster <- plotly::renderPlotly({
    d<-d_md()$clustering$puntos
    plotly::plot_ly(d,x=~perfil,y=~bdi_total,type='box',color=~perfil,showlegend=FALSE) %>%
      plotly::layout(xaxis=list(title=''),yaxis=list(title='BDI-II'))
  })
  output$md_sexo_cluster <- plotly::renderPlotly({
    d<-d_md()$clustering$perfiles
    plotly::plot_ly(d,x=~perfil,y=~(100*chicas),type='bar',
      marker=list(color='#D6A5C3')) %>%
      plotly::layout(yaxis=list(title='% chicas',range=c(0,100)))
  })
  output$md_k_tabla <- DT::renderDT({
    DT::datatable(d_md()$clustering$evaluacion,rownames=FALSE,
      options=list(dom='t',paging=FALSE)) %>%
      DT::formatRound(columns=c('silueta','inercia'),digits=3,dec.mark=',')
  })
  # ------------------- PREDICCION -------------------
  output$md_controles_pred <- shiny::renderUI({
    d<-d_md()
    campos <- lapply(seq_len(nrow(d$train_stats)),function(i) {
      z<-d$train_stats[i,]
      shiny::sliderInput(paste0('md_val_',z$variable),label=z$variable,
        min=round(z$p01,2),max=round(z$p99,2),
        value=round(z$mediana,2),step=.05)
    })
    shiny::tagList(shiny::radioButtons('md_sexo','Sexo',
      choices=c('Chicos'='Boy','Chicas'='Girl'),selected='Boy',inline=TRUE),campos)
  })
  pred_actual <- shiny::reactiveVal(NULL)
  shiny::observeEvent(input$md_calcular,{
    d<-d_md(); nueva<-as.data.frame(as.list(stats::setNames(
      lapply(md_predictores_numericos,function(v)input[[paste0('md_val_',v)]]),
      md_predictores_numericos)))
    shiny::req(all(vapply(nueva,function(z)!is.null(z)&&is.finite(as.numeric(z)),logical(1))))
    nueva$sex<-input$md_sexo
    prob<-md_predecir(d$clf$modelos[[d$clf$meta$modelo]],nueva)
    bdi<-md_predecir(d$reg$modelos[[d$reg$meta$modelo]],nueva)
    pred_actual(list(prob=prob[1],bdi=bdi[1],umbral=d$clf$meta$umbral))
  })
  output$md_probabilidad <- shiny::renderText({
    p<-pred_actual(); if(is.null(p))return('—')
    paste0(md_formato(p$prob*100,1),' %')
  })
  output$md_estado_pred <- shiny::renderText({
    p<-pred_actual();if(is.null(p))return('Completa el formulario y pulsa Calcular.')
    paste(if(p$prob>=p$umbral)'Positivo' else 'Negativo','en tamizaje exploratorio; umbral',md_formato(p$umbral))
  })
  output$md_bdi_estimado <- shiny::renderText({
    p<-pred_actual();if(is.null(p))return('—');md_formato(p$bdi,1)
  })
  output$md_categoria_pred <- shiny::renderText({
    p<-pred_actual();if(is.null(p))return('')
    paste('Categoría de puntaje:',if(p$bdi<14)'Mínima' else if(p$bdi<20)'Leve' else if(p$bdi<29)'Moderada' else 'Severa')
  })
  output$md_prob_plot <- plotly::renderPlotly({
    p<-pred_actual();shiny::req(!is.null(p))
    plotly::plot_ly(x=c('Probabilidad'),y=c(p$prob),type='bar',marker=list(color='#EFAC91'),
                    showlegend=FALSE) %>% plotly::layout(yaxis=list(range=c(0,1),title='Probabilidad'))
  })
  output$md_bdi_plot <- plotly::renderPlotly({
    p<-pred_actual();shiny::req(!is.null(p))
    plotly::plot_ly(x=c('BDI-II estimado'),y=c(p$bdi),type='bar',marker=list(color='#A3C3E5'),
                    showlegend=FALSE) %>% plotly::layout(yaxis=list(range=c(0,63),title='Puntos BDI'))
  })
}

# Evita dependencia de paquetes de operadores en la selección por defecto.
`%||%` <- function(x,y) if(is.null(x)||length(x)==0) y else x
