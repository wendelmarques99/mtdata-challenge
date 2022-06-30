library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
#data(mtcars)
source("./Dados.R")

variaveis_preditoras = colnames(dados_teste)[-c(1, 2)]

# Define UI for application
ui = fluidPage(
  includeCSS("css.css"),
  tags$head(
    tags$img(
      src = "http://mt2.cl/wp-content/uploads/2021/08/Mt2_Logo2021_s.png", 
      width = "110",
      height = "49"
    )
  ),
  sidebarLayout(
  sidebarPanel(
   selectizeInput(inputId = "indep",
                  label = "Variáveis Preditoras", 
                  multiple = TRUE, choices = as.list(variaveis_preditoras),
                  selected = variaveis_preditoras[1]),
  shinyWidgets::prettyRadioButtons(
    fill = TRUE,
    inputId = "Periodo",
    label = "Período dos dados de teste",
    icon = icon("check"),
    choices = c(
      "2003 - 2020",
      "2010 - 2020", 
      "2015 - 2020"),
    animation = "tada",
    status = "default", 
    selected = "17 Anos"
  ), 
  shiny::actionButton(
    inputId = "rodar",
    "Rodar!"
  )),
  mainPanel(
    plotly::plotlyOutput(outputId = "previsao")
    )
  )
)
# Define server logic 
server <- function(input, output) {
 
  dados_treino_reativo <- reactive({ 
    
    if (input$Periodo == "2003 - 2020"){
      dados_treino
    }else if(input$Periodo == "2010 - 2020"){
      dados_treino %>% 
        dplyr::filter(
          date >= as.Date("2010-01-01"),
          date <= as.Date("2020-12-01")
        )
    }else{
      dados_treino %>% 
        dplyr::filter(
          date >= as.Date("2015-01-01"),
          date <= as.Date("2020-12-01")
        )
    }
  })
  
  plot <- eventReactive(input$rodar, {
    
    recipe_formula <- reactive({
    
      dados_treino_reativo() %>%
        recipe() %>%
        update_role(trucks, new_role = "outcome") %>%
        update_role(!!!input$indep, new_role = "predictor") %>% 
        prep() %>% 
        formula()
    })
    
    lm_reg <- reactive({
      lm(recipe_formula(), data = dados_treino_reativo())
    })
    
    previsao <- reactive({
      tibble::tibble(
        Data = dados_teste$date, 
        Venda = predict(lm_reg(), dados_teste), 
        Classificacao = 'Previsão'
      )
    })
    
    p <- plotly::plot_ly()
    
    p <- add_trace(p, line = list(
      color = "rgba(0,0,0,1)", 
      fillcolor = "rgba(0,0,0,1)"
    ), hoveron = "points",
    x = dados_treino_reativo()$date, 
    y = dados_treino_reativo()$trucks, 
    type = "scatter",
    mode = "lines", 
    name = "Observado")
    
    p <- 
      add_trace(p, line = list(
        color = "rgba(0,0,255,1)", 
        fillcolor = "rgba(0,0,255,1)"),
        x = previsao()$Data, 
        y = previsao()$Venda,
        mode = "lines",
        name = "Previsão",
        type = "scatter"
      )

  })
  
  output$previsao <- renderPlotly({
    
    layout(plot(),
           margin = list(l=0,r=0,t=50,b=0,pad=0),
           title = "Previsão de Vendas - 12 Meses",
           xaxis = list("Período", c(0, 1)), 
           yaxis = list("Vendas"))
       })
}




# Run the application 
shinyApp(ui = ui, server = server)