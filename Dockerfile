
FROM rocker/shiny-verse:latest

RUN R -e "install.packages(c('DT','plotly','bslib','caret','pROC','randomForest','shinyWidgets','ellmer','shinychat','promises','glmnet','xgboost'), repos='https://cloud.r-project.org')"

WORKDIR /app
COPY . /app

EXPOSE 10000

CMD ["Rscript", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '10000')))"]
