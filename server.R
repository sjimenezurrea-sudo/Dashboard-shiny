
library(shiny)


server <- function(input, output, session) {
  
  observeEvent(input$ir_inicio, {
    updateTabItems(
      session,
      inputId = "tabs",
      selected = "inicio"
    )
  })
  
  observeEvent(input$ir_indicadores, {
    updateTabItems(
      session,
      inputId = "tabs",
      selected = "indicadores"
    )
  })
  
  observeEvent(input$ir_resultados, {
    updateTabItems(
      session,
      inputId = "tabs",
      selected = "resultados"
    )
  })
  
}
