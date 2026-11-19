# Configuracion y utilidades del modelado R Shiny.
# Se carga desde global.R; NO sustituye df ni las funciones del EDA.
md_predictores_numericos <- c(
  'sleep_quality_index', 'avg_sleep_hours',
  'midsleep_weekend_hours', 'social_jetlag_hours',
  'screen_time_index'
)
md_predictores <- c(md_predictores_numericos, 'sex')
md_ruta <- file.path('resultados', 'modelado_R', 'modelado_completo.rds')
md_seed <- 42L

md_y_binaria <- function(x) {
  z <- trimws(as.character(x))
  y <- ifelse(z %in% c('1', 'Deprimido'), 1L,
              ifelse(z %in% c('0', 'No deprimido'), 0L, NA_integer_))
  as.integer(y)
}

md_preparar <- function(datos) {
  columnas <- c(md_predictores, 'depressed', 'bdi_total', 'subject_id')
  faltan <- setdiff(columnas, names(datos))
  if (length(faltan)) stop('Columnas ausentes: ', paste(faltan, collapse=', '))
  d <- datos[, columnas, drop=FALSE]
  d$depressed <- md_y_binaria(d$depressed)
  d$bdi_total <- as.numeric(d$bdi_total)
  for (v in md_predictores_numericos) d[[v]] <- as.numeric(d[[v]])
  d$sex <- as.character(d$sex)
  validas <- !is.na(d$subject_id) & !is.na(d$sex) &
    d$sex %in% c('Boy','Girl') & !is.na(d$depressed) &
    is.finite(d$bdi_total) & d$bdi_total >= 0 & d$bdi_total <= 63 &
    complete.cases(d[, md_predictores_numericos, drop=FALSE])
  for (v in md_predictores_numericos) validas <- validas & is.finite(d[[v]])
  d <- d[validas, , drop=FALSE]
  if (nrow(d) < 100 || length(unique(d$depressed)) != 2) stop('No hay datos validos suficientes.')
  if (anyDuplicated(d$subject_id)) stop('subject_id duplicado: revisar antes de dividir los datos.')
  rownames(d) <- NULL
  d
}

md_matrix <- function(datos, variables=md_predictores, params=NULL) {
  nums <- intersect(variables, md_predictores_numericos)
  if (is.null(params)) {
    centro <- vapply(datos[,nums,drop=FALSE], mean, numeric(1), na.rm=TRUE)
    escala <- vapply(datos[,nums,drop=FALSE], sd, numeric(1), na.rm=TRUE)
    escala[!is.finite(escala) | escala == 0] <- 1
    params <- list(nums=nums, centro=centro, escala=escala,
                   sexo=('sex' %in% variables))
  }
  X <- sweep(sweep(as.matrix(datos[,params$nums,drop=FALSE]), 2, params$centro, '-'),
             2, params$escala, '/')
  if (isTRUE(params$sexo)) X <- cbind(X, sex_Girl=as.numeric(datos$sex == 'Girl'))
  storage.mode(X) <- 'double'
  list(X=X, params=params)
}

md_ap <- function(y, p) {
  if (length(unique(y)) < 2 || any(!is.finite(p))) return(NA_real_)
  o <- order(p, decreasing=TRUE)
  yy <- y[o]; pp <- p[o]
  ix <- which(!duplicated(pp, fromLast=TRUE))
  tp <- cumsum(yy == 1L)[ix]
  recall <- tp / sum(yy == 1L)
  precision <- tp / ix
  sum(diff(c(0, recall)) * precision)
}

md_curva <- function(y, p) {
  o <- order(p, decreasing=TRUE)
  yy <- y[o]; pp <- p[o]
  ix <- which(!duplicated(pp, fromLast=TRUE))
  tp <- cumsum(yy == 1L)[ix]; fp <- ix - tp
  data.frame(Recall=c(0, tp / sum(yy == 1L)),
             Precision=c(1, tp/ix),
             FPR=c(0, fp / sum(yy == 0L)))
}

md_metricas_clf <- function(y, p, umbral=.5) {
  pred <- as.integer(p >= umbral)
  tp <- sum(pred == 1L & y == 1L)
  fp <- sum(pred == 1L & y == 0L)
  fn <- sum(pred == 0L & y == 1L)
  prec <- if (tp+fp == 0) 0 else tp/(tp+fp)
  rec <- if (tp+fn == 0) 0 else tp/(tp+fn)
  curva <- md_curva(y,p)
  auc <- sum(diff(curva$FPR) * (head(curva$Recall,-1)+tail(curva$Recall,-1))/2)
  c('PR-AUC'=md_ap(y,p), 'ROC-AUC'=auc, Precision=prec, Recall=rec,
    F1=if(prec+rec==0)0 else 2*prec*rec/(prec+rec),
    Accuracy=mean(pred==y), Brier=mean((p-y)^2))
}
md_metricas_reg <- function(y, p) c(RMSE=sqrt(mean((y-p)^2)),
  MAE=mean(abs(y-p)), 'R²'=1-sum((y-p)^2)/sum((y-mean(y))^2))
md_nombre_modelo <- function(x) {
  nombres <- c(logistica='Logística', logistica_pesos='Logística benchmark (pesos)',
               ridge='Logística Ridge (L2)', lasso='Logística Lasso (L1)',
               random_forest='Random Forest', xgboost='XGBoost',
               lineal='Lineal (OLS)', elasticnet='ElasticNet',
               ridge_reg='Ridge', lasso_reg='Lasso', random_forest_reg='Random Forest',
               xgboost_reg='XGBoost')
  z <- unname(nombres[as.character(x)])
  z[is.na(z)] <- as.character(x)[is.na(z)]
  z
}
md_formato <- function(x, digits=3) {
  if(length(x)==0||is.na(x[1])||!is.finite(as.numeric(x[1]))) return('—')
  formatC(as.numeric(x[1]), digits=digits, format='f', decimal.mark=',')
}
