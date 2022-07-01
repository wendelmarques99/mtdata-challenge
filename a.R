library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
#data(mtcars)
source("./Dados.R")


# Estilo da fonte ---------------------------------------------------------
t <- list(
  family = "inherit",
  size = 15,
  color = '#222222')


variaveis_preditoras = colnames(dados_teste)[-c(1, 2)]

# Define UI for application
ui = fluidPage(
  includeCSS("css.css"),
  tags$header(
    tags$img(
      src = "http://mt2.cl/wp-content/uploads/2021/08/Mt2_Logo2021_s.png", 
      width = "110",
      height = "49",
      style = "margin-top: 10px"
    )
  ),
  sidebarLayout(
  sidebarPanel(
   selectizeInput(inputId = "indep",
                  label = "Variáveis Preditoras", 
                  multiple = TRUE, 
                  choices = as.list(variaveis_preditoras),
                  selected = variaveis_preditoras[1]),
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
 
  
  plot <- eventReactive(input$rodar, {
    
    recipe_formula <- reactive({
    if (!is.null(input$indep)){
      dados_treino %>%
        recipe() %>%
        update_role(trucks, new_role = "outcome") %>%
        update_role(!!!input$indep, new_role = "predictor") %>% 
        prep() %>% 
        formula()
    }else{
      stop()
    }
  })
    
    lm_reg <- reactive({
      lm(recipe_formula(), data = dados_treino)
    })
    
    previsao <- reactive({
      tibble::tibble(
        Data = as.Date(dados_teste$date), 
        Venda = predict(lm_reg(), dados_teste), 
        Classificacao = 'Previsão',
        id = 1:12
      )
    })

# plot --------------------------------------------------------------------
    
    p <- plotly::plot_ly()
    
    p <- add_trace(p,
        line = list(
      color = "rgba(0,0,0,1)", 
      fillcolor = "rgba(0,0,0,1)"
    ), hoveron = "points",
    x = dados_treino$date, 
    y = dados_treino$trucks, 
    type = "scatter",
    mode = "lines", 
      name = "Observado")
    
    p <- 
      add_trace(p, line = list(
        color = "#4240ae", 
        fillcolor = "#4240ae",
        dash = "dot", width = 3),
        x = previsao()$Data, 
        y = previsao()$Venda,
        mode = "lines",
        name = "Previsão",
        type = "scatter"
        
      )
    
        p <- add_trace(p, 
                  line = list(color = "#4240ae", 
                               fillcolor = "#4240ae"), 
                  x = c(as.Date("2020-12-01"), as.Date("2021-01-01")), 
                  y = c(dados_treino %>% tail(1) %>% dplyr::pull(2), previsao()$Venda[1]), 
                  name = "",
                  type = "scatter",
                  mode = "lines", 
                  showlegend = FALSE,
                  hoverinfo = "text"
                  )
        p
    
})
  
observeEvent(input$rodar, {
  if (is.null(input$indep)){
    showModal(modalDialog(
      title = "Erro",
      "Nenhuma variável selecionada. Selecione!",
      easyClose = TRUE,
      footer = modalButton("Dispensar")
      
    ))
  }
})

output$previsao <- renderPlotly({
  # input$rodar
  #   Sys.sleep(1.2)
    layout(plot(),
           margin = list(l = 0, r = 0, t = 50, b = 0, pad = 0),
           title = "<b>Previsão de Vendas - 12 Meses</b>",
           font = t, 
           legend = list(x = .1, y = .9))
  })


}




# Run the application 
shinyApp(ui = ui, server = server)
