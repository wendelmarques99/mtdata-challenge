source("./Dados.R")

library(shiny)

ui <- fluidPage(
  tags$img(
    src = "http://mt2.cl/wp-content/uploads/2021/08/Mt2_Logo2021_s.png", 
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
