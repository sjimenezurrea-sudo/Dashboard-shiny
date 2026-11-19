# Entrenamiento INICIAL R: clasificacion, regresion, genero, clustering y prediccion.
# Adaptacion metodologica, NO replica exacta de los 9+12 algoritmos Python.
# Ejecutar desde la raiz del proyecto Shiny, fuera del servidor:
# source('scripts/entrenar_modelado.R', encoding='UTF-8')

source('R/modelado_config.R', encoding='UTF-8')
if (!file.exists('screen_time_mental_health.csv'))
  stop('Falta screen_time_mental_health.csv en la raiz del proyecto.')
datos <- md_preparar(read.csv('screen_time_mental_health.csv', stringsAsFactors=FALSE))
set.seed(md_seed)

# La misma particion se utiliza para clasificacion, regresion y genero.
md_particion <- function(d, frac=.8) {
  estratos <- interaction(d$sex, d$depressed, drop=TRUE)
  ix <- unlist(lapply(split(seq_len(nrow(d)), estratos), function(z)
    sample(z, max(1L, min(length(z)-1L, floor(length(z)*frac))))), use.names=FALSE)
  list(train=sort(ix), test=setdiff(seq_len(nrow(d)),ix))
}
particion <- md_particion(datos)
train <- datos[particion$train,,drop=FALSE]
test <- datos[particion$test,,drop=FALSE]

metodos_clf <- c('logistica','logistica_pesos')
if (requireNamespace('glmnet', quietly=TRUE)) metodos_clf <- c(metodos_clf,'ridge','lasso')
if (requireNamespace('randomForest', quietly=TRUE)) metodos_clf <- c(metodos_clf,'random_forest')
if (requireNamespace('xgboost', quietly=TRUE)) metodos_clf <- c(metodos_clf,'xgboost')
metodos_reg <- 'lineal'
if (requireNamespace('glmnet', quietly=TRUE)) metodos_reg <- c(metodos_reg,'ridge_reg','lasso_reg','elasticnet')
if (requireNamespace('randomForest', quietly=TRUE)) metodos_reg <- c(metodos_reg,'random_forest_reg')
if (requireNamespace('xgboost', quietly=TRUE)) metodos_reg <- c(metodos_reg,'xgboost_reg')

md_ajustar <- function(nombre, d, tipo, variables=md_predictores) {
  pp <- md_matrix(d, variables)
  X <- pp$X
  y <- if(tipo=='clf') d$depressed else d$bdi_total
  if (nombre %in% c('logistica','logistica_pesos')) {
    w <- if(nombre=='logistica_pesos') ifelse(y==1, .5/mean(y), .5/(1-mean(y))) else rep(1,length(y))
    ajuste <- suppressWarnings(stats::glm(y ~ ., data=data.frame(y=y,X,check.names=TRUE),
                    weights=w,family=stats::binomial()))
  } else if (nombre %in% c('ridge','lasso','ridge_reg','lasso_reg','elasticnet')) {
    ajuste <- glmnet::glmnet(X,y, family=if(tipo=='clf')'binomial' else 'gaussian',
       alpha=switch(nombre, ridge=0,lasso=1,ridge_reg=0,lasso_reg=1,elasticnet=.5),
       lambda=if(tipo=='clf') .05 else .15,standardize=FALSE)
  } else if (nombre %in% c('random_forest','random_forest_reg')) {
    ajuste <- randomForest::randomForest(x=X, y=if(tipo=='clf')factor(y,levels=c(0,1)) else y,
                                         ntree=250, mtry=max(1,floor(sqrt(ncol(X)))))

  } else if (nombre %in% c("xgboost", "xgboost_reg")) {

    # Formato de datos compatible con XGBoost
    dtrain <- xgboost::xgb.DMatrix(
      data = X,
      label = as.numeric(y)
    )

    # Clasificacion o regresion
    objetivo <- if (tipo == "clf") {
      "binary:logistic"
    } else {
      "reg:squarederror"
    }

    ajuste <- xgboost::xgb.train(
      params = list(
        objective = objetivo,
        max_depth = 3,
        learning_rate = 0.05,
        subsample = 0.85,
        colsample_bytree = 0.9,
        nthread = 2,
        verbosity = 0
      ),
      data = dtrain,
      nrounds = 150,
      verbose = 0
    )

  } else if (nombre=='lineal') {
    ajuste <- stats::lm(y ~ ., data=data.frame(y=y,X,check.names=TRUE))
  } else stop('Modelo sin implementar: ',nombre)
  list(nombre=nombre, tipo=tipo, variables=variables, params=pp$params, ajuste=ajuste)
}
md_predecir <- function(modelo, d) {
  X <- md_matrix(d,params=modelo$params)$X
  nom <- modelo$nombre
  p <- if (nom %in% c('logistica','logistica_pesos')) {
    as.numeric(stats::predict(modelo$ajuste,newdata=as.data.frame(X,check.names=TRUE),type='response'))
  } else if (nom %in% c('ridge','lasso','ridge_reg','lasso_reg','elasticnet')) {
    as.numeric(stats::predict(modelo$ajuste,newx=X,type='response'))
  } else if (nom=='random_forest') {
    as.numeric(stats::predict(modelo$ajuste,newdata=X,type='prob')[,'1'])
  } else if (nom=='random_forest_reg') {
    as.numeric(stats::predict(modelo$ajuste,newdata=X))

  } else if (nom %in% c("xgboost", "xgboost_reg")) {

    nuevos_datos <- xgboost::xgb.DMatrix(
      data = X
    )

    as.numeric(
      stats::predict(
        modelo$ajuste,
        nuevos_datos
      )
    )

  } else if (nom=='lineal') {
    as.numeric(stats::predict(modelo$ajuste,newdata=as.data.frame(X,check.names=TRUE)))
  } else stop('Modelo sin predict: ',nom)
  p <- if(modelo$tipo=='clf') pmin(1,pmax(0,p)) else pmin(63,pmax(0,p))
  p
}

md_folds <- function(y,k=5,estratificar=TRUE) {
  folds <- integer(length(y))
  if (estratificar) {
    for (nivel in unique(y)) {
      ix <- which(y==nivel); ix <- sample(ix)
      folds[ix] <- rep(seq_len(k),length.out=length(ix))
    }
  } else {
    ix <- sample(seq_along(y))
    folds[ix] <- rep(seq_len(k),length.out=length(ix))
  }
  folds
}
md_cv <- function(nombre, d, tipo, variables=md_predictores,k=5L) {
  y <- if(tipo=='clf') d$depressed else d$bdi_total
  folds <- md_folds(y,k,estratificar=(tipo=='clf'))
  oof <- rep(NA_real_,nrow(d))
  for (f in seq_len(k)) {
    tr <- d[folds!=f,,drop=FALSE]; va <- d[folds==f,,drop=FALSE]
    modelo <- md_ajustar(nombre,tr,tipo,variables)
    oof[folds==f] <- md_predecir(modelo,va)
  }
  metricas <- do.call(rbind,lapply(seq_len(k),function(f) {
    ix <- which(folds==f)
    if(tipo=='clf') md_metricas_clf(y[ix],oof[ix]) else md_metricas_reg(y[ix],oof[ix])
  }))
  colnames(metricas) <- colnames(metricas) # conservar las etiquetas originales
  list(oof=oof, metricas=metricas,
       medias=colMeans(metricas,na.rm=TRUE),
       desviaciones=apply(metricas,2,sd,na.rm=TRUE))
}
md_umbral <- function(y,p,recall_minimo=.70) {
  candidatos <- unique(c(.5,as.numeric(quantile(p,probs=seq(0,1,length.out=501),names=FALSE))))
  m <- t(vapply(candidatos,function(u) md_metricas_clf(y,p,u)[c('Precision','Recall','F1')],numeric(3)))
  ix <- which(m[,'Recall']>=recall_minimo)
  if(length(ix)==0) return(.5)
  orden <- order(-m[ix,'Precision'],-candidatos[ix])
  candidatos[ix[orden[1]]]
}

# ----------------- CLASIFICACION -----------------
message('1/5 - Benchmark de clasificacion')
cv_clf <- vector('list',length(metodos_clf));names(cv_clf) <- metodos_clf
ajustes_clf <- list(); test_clf <- list()
for(nom in metodos_clf) {
  set.seed(md_seed)
  res <- md_cv(nom,train,'clf')
  cv_clf[[nom]] <- res
  ajustes_clf[[nom]] <- md_ajustar(nom,train,'clf')
  prob <- md_predecir(ajustes_clf[[nom]],test)
  test_clf[[nom]] <- list(prob=prob,metricas=md_metricas_clf(test$depressed,prob))
}
tabla_cv_clf <- do.call(rbind,lapply(names(cv_clf),function(n) {
  r <- cv_clf[[n]]
  data.frame(modelo=n,nombre=md_nombre_modelo(n),
    metrica=names(r$medias),media=as.numeric(r$medias),desviacion=as.numeric(r$desviaciones))
}))
tabla_test_clf <- do.call(rbind,lapply(names(test_clf),function(n) {
  data.frame(modelo=n,metrica=names(test_clf[[n]]$metricas),valor=unname(test_clf[[n]]$metricas))
}))
rank_clf <- vapply(cv_clf,function(x) x$medias['PR-AUC'],numeric(1))
final_clf <- names(which.max(rank_clf))
p_oof <- cv_clf[[final_clf]]$oof
umbral <- md_umbral(train$depressed,p_oof)
p_final <- test_clf[[final_clf]]$prob
meta_clf <- list(modelo=final_clf,umbral=umbral,recall_minimo=.70,
 n_folds=5L, n_test=nrow(test),prevalencia_train=mean(train$depressed),
 prevalencia_test=mean(test$depressed),
 metricas_test_05=md_metricas_clf(test$depressed,p_final,.5),
 metricas_test_umbral=md_metricas_clf(test$depressed,p_final,umbral))
umbrales <- seq(.01,.99,by=.01)
tabla_umbral <- do.call(rbind,lapply(umbrales,function(u)
  data.frame(umbral=u,t(md_metricas_clf(train$depressed,p_oof,u)[c('Precision','Recall','F1')]),
             check.names=FALSE)))

# ----------------- REGRESION -----------------
message('2/5 - Benchmark de regresion BDI-II')
cv_reg <- list(); ajustes_reg <- list(); test_reg <- list()
for(nom in metodos_reg) {
  set.seed(md_seed)
  res <- md_cv(nom,train,'reg')
  cv_reg[[nom]] <- res
  ajustes_reg[[nom]] <- md_ajustar(nom,train,'reg')
  pred <- md_predecir(ajustes_reg[[nom]],test)
  test_reg[[nom]] <- list(pred=pred,metricas=md_metricas_reg(test$bdi_total,pred))
}
tabla_cv_reg <- do.call(rbind,lapply(names(cv_reg),function(n) {
  r <- cv_reg[[n]]
  data.frame(modelo=n,nombre=md_nombre_modelo(n),
             metrica=names(r$medias),media=as.numeric(r$medias),desviacion=as.numeric(r$desviaciones))
}))
tabla_test_reg <- do.call(rbind,lapply(names(test_reg),function(n)
  data.frame(modelo=n,metrica=names(test_reg[[n]]$metricas),valor=unname(test_reg[[n]]$metricas))))
rank_reg <- vapply(cv_reg,function(x)x$medias['RMSE'],numeric(1))
final_reg <- names(which.min(rank_reg))
pred_bdi <- test_reg[[final_reg]]$pred
meta_reg <- list(modelo=final_reg,n_folds=5L,n_test=nrow(test),
                 metricas_test=md_metricas_reg(test$bdi_total,pred_bdi))

# ----------------- INTERPRETABILIDAD -----------------
message('3/5 - Importancia por permutacion en el test')
md_permutacion <- function(modelo,d,objetivo,metrica='PR-AUC',repeticiones=6L) {
  y <- d[[objetivo]]
  p_base <- md_predecir(modelo,d)
  valor_base <- if(modelo$tipo=='clf') md_metricas_clf(y,p_base)[metrica] else md_metricas_reg(y,p_base)['RMSE']
  out <- do.call(rbind,lapply(modelo$variables,function(variable) {
    cambios <- replicate(repeticiones, {
      d_perm <- d; d_perm[[variable]] <- sample(d_perm[[variable]])
      p <- md_predecir(modelo,d_perm)
      if(modelo$tipo=='clf') valor_base-md_metricas_clf(y,p)[metrica] else md_metricas_reg(y,p)['RMSE']-valor_base
    })
    data.frame(variable=variable,importancia=mean(cambios),desviacion=sd(cambios))
  }))
  out
}
set.seed(md_seed)
import_clf <- md_permutacion(ajustes_clf[[final_clf]],test,'depressed')
import_reg <- md_permutacion(ajustes_reg[[final_reg]],test,'bdi_total')
coef_reg <- NULL
ajuste_reg <- ajustes_reg[[final_reg]]
if(final_reg %in% c('lineal','ridge_reg','lasso_reg','elasticnet')) {
  beta <- if(final_reg=='lineal') stats::coef(ajuste_reg$ajuste) else as.matrix(stats::coef(ajuste_reg$ajuste))[,1]
  coef_reg <- data.frame(variable=names(beta),coeficiente=as.numeric(beta))
}
experimento <- list(
  'Clasificacion directa'=md_metricas_clf(test$depressed,p_final,umbral),
  'BDI estimado >= 14'=md_metricas_clf(test$depressed,as.numeric(pred_bdi>=14),.5)
)
# ADVERTENCIA: La PR-AUC de un predictor binario no es comparable directamente con probabilidades continuas.

# ----------------- GENERO -----------------
message('4/5 - Modelos por sexo (mismo split original)')
md_genero <- list()
for (sexo in c('Boy','Girl')) {
  tr <- train[train$sex==sexo,,drop=FALSE]
  te <- test[test$sex==sexo,,drop=FALSE]
  vars <- md_predictores_numericos
  if(nrow(tr)<60 || length(unique(tr$depressed))<2) next
  set.seed(md_seed)
  cv_g <- md_cv(final_clf,tr,'clf',vars)
  propio_umbral <- md_umbral(tr$depressed,cv_g$oof)
  fit_g <- md_ajustar(final_clf,tr,'clf',vars)
  p_g <- md_predecir(fit_g,te)
  p_conjunto <- md_predecir(ajustes_clf[[final_clf]],te)
  met_g <- md_metricas_clf(te$depressed,p_g,propio_umbral)
  met_conj <- md_metricas_clf(te$depressed,p_conjunto,umbral)
  md_genero[[sexo]] <- list(n_test=nrow(te),umbral=propio_umbral,
    especifico=met_g, conjunto=met_conj,
    modelo=fit_g,importancia=md_permutacion(fit_g,te,'depressed'))
}

# ----------------- CLUSTERING -----------------
message('5/5 - Clustering K-means')
vars_cluster <- md_predictores_numericos
Xc <- scale(as.matrix(datos[,vars_cluster,drop=FALSE]))
set.seed(md_seed)
idx_silueta <- sample.int(nrow(Xc),min(nrow(Xc),600L))
evaluacion_k <- do.call(rbind,lapply(2:8,function(k) {
  fit <- stats::kmeans(Xc,centers=k,nstart=30,iter.max=100)
  sil <- if(requireNamespace('cluster',quietly=TRUE))
    mean(cluster::silhouette(fit$cluster[idx_silueta],stats::dist(Xc[idx_silueta,,drop=FALSE]))[,3])
  else NA_real_
  data.frame(k=k,silueta=sil,inercia=fit$tot.withinss)
}))
k_final <- if(all(is.na(evaluacion_k$silueta))) 3L else evaluacion_k$k[which.max(evaluacion_k$silueta)]
set.seed(md_seed)
km <- stats::kmeans(Xc,centers=k_final,nstart=50,iter.max=100)
centros <- aggregate(Xc,by=list(cluster=km$cluster),FUN=mean)
ord <- order(centros$sleep_quality_index)
renumerar <- setNames(seq_len(k_final),centros$cluster[ord])
perfil <- factor(paste('Perfil',renumerar[as.character(km$cluster)]),
                 levels=paste('Perfil',seq_len(k_final)))
pca <- stats::prcomp(Xc,center=FALSE,scale.=FALSE)
puntos <- data.frame(subject_id=datos$subject_id,perfil=perfil,
                     PC1=pca$x[,1],PC2=pca$x[,2],
                     depressed=datos$depressed,bdi_total=datos$bdi_total,sex=datos$sex)
perfiles <- do.call(rbind,lapply(levels(perfil),function(n) {
  ix <- which(perfil==n)
  data.frame(perfil=n,n=length(ix),prevalencia=mean(datos$depressed[ix]),
    bdi_media=mean(datos$bdi_total[ix]),chicas=mean(datos$sex[ix]=='Girl'))
}))
centroides <- do.call(rbind,lapply(levels(perfil),function(n) {
  ix <- which(perfil==n)
  data.frame(perfil=n,variable=colnames(Xc),media=colMeans(Xc[ix,,drop=FALSE]))
}))
clustering <- list(k=k_final,evaluacion=evaluacion_k,puntos=puntos,
                   perfiles=perfiles,centroides=centroides,
                   varianza=summary(pca)$importance[2,1:2])

# ----------------- GUARDAR OBJETO DE RESULTADOS -----------------
resultado <- list(
  version='primera_base_R_2026_11', seed=md_seed,
  n_train=nrow(train), n_test=nrow(test),predictores=md_predictores,
  train_stats=data.frame(variable=md_predictores_numericos,
    p01=sapply(train[,md_predictores_numericos,drop=FALSE],quantile,probs=.01),
    p99=sapply(train[,md_predictores_numericos,drop=FALSE],quantile,probs=.99),
    mediana=sapply(train[,md_predictores_numericos,drop=FALSE],median)),
  clf=list(cv=tabla_cv_clf,test=tabla_test_clf,meta=meta_clf,
    modelos=ajustes_clf,predicciones=test_clf,oof_ganador=p_oof,
    umbrales=tabla_umbral,importancia=import_clf),
  reg=list(cv=tabla_cv_reg,test=tabla_test_reg,meta=meta_reg,
    modelos=ajustes_reg,predicciones=test_reg,importancia=import_reg,
    coeficientes=coef_reg),
  test_y=test$depressed,test_bdi=test$bdi_total,test_sex=test$sex,
  train_y=train$depressed,
  genero=md_genero,clustering=clustering,experimento=experimento
)
dir.create(dirname(md_ruta), recursive=TRUE,showWarnings=FALSE)
saveRDS(resultado,md_ruta)
message('Guardado: ',normalizePath(md_ruta))
message('Clasificador seleccionado por PR-AUC CV: ',final_clf,
        ' | Regresor por RMSE CV: ',final_reg)
message('NOTA: implementacion inicial; no incluye todos los algoritmos/tuning del proyecto Python.')
