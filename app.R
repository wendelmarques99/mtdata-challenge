library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
library(shinycssloaders)
library(dplyr)
library(plotly)
library(tidyr)

# Estilo da fonte ---------------------------------------------------------
t <- list(
  family = "inherit",
  size = 15,
  color = '#222222')

# NA -> media(variable) ----------------------------------------------------
Dados <- readxl::read_excel(
  "Data_Wendel.xlsx"
) %>% 
  janitor::clean_names() %>% 
  dplyr::mutate(
    ibc_br = tidyr::replace_na(ibc_br, mean(ibc_br, na.rm = TRUE)),
    industrial_production =  tidyr::replace_na(industrial_production, mean(industrial_production, na.rm = TRUE)),
    retail_sales = tidyr::replace_na(retail_sales, mean(retail_sales, na.rm = TRUE)),
    usd_brl = tidyr::replace_na(usd_brl, mean(usd_brl, na.rm = TRUE))
  )

# Dados de Treino ---------------------------------------------------------
inicio <- as.Date("2003-01-01")
fim <- as.Date("2020-12-01")

dados_treino <- Dados %>% 
  dplyr::filter(
    date >=  inicio, 
    date <= fim
  ) 
# Dados de Teste ----------------------------------------------------------
dados_teste <- Dados %>% 
  dplyr::filter(
    date > fim
  )

variaveis_preditoras = colnames(dados_teste)[-c(1, 2)]

# Define UI for application
ui = fluidPage(
  shiny::includeCSS("css.css"),
  tags$header(
    tags$img(
      src = "http://mt2.cl/wp-content/uploads/2021/08/Mt2_Logo2021_s.png", 
      width = "110",
      height = "49",
      style = "margin-top: 10px"
    )
  ),
  shiny::sidebarLayout(
  shiny::sidebarPanel(
  shiny::uiOutput("input_selecionar"),
  shiny::uiOutput("btn")),
  shiny::mainPanel(
    plotly::plotlyOutput(outputId = "previsao") %>% 
      shinycssloaders::withSpinner(color.background = "#4240ae", 
                                   color = "#4240ae")
  )
 )
)
# Define server logic 
server <- function(input, output) {
 
output$input_selecionar <- renderUI({
shiny::selectizeInput(inputId = "indep",
        label = "Variáveis Preditoras", 
        multiple = TRUE, 
        choices = as.list(variaveis_preditoras),
        selected = variaveis_preditoras[1])
})
  
output$btn <- shiny::renderUI({
  shiny::actionButton(
    inputId = "rodar",
    "Rodar"
  )
})

  plot <- shiny::eventReactive(input$rodar, {
    
    req(input$indep)
    
    recipe_formula <- reactive({
      dados_treino %>%
        recipes::recipe() %>%
        recipes::update_role(trucks, new_role = "outcome") %>%
        recipes::update_role(!!!input$indep, new_role = "predictor") %>% 
        recipes::prep() %>% 
        formula()
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
    
    p <- plotly::add_trace(p,
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
      plotly::add_trace(p,
        line = list(
        color = "#4240ae", 
        fillcolor = "#4240ae",
        dash = "dot", width = 3),
        x = previsao()$Data, 
        y = previsao()$Venda,
        mode = "lines",
        name = "Previsão",
        type = "scatter"
        
      )
    
        p <- plotly::add_trace(p, 
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
  
  shiny::observeEvent(input$rodar, {
  if (is.null(input$indep)){
    shiny::showModal(shiny::modalDialog(
      title = "Erro",
      "Nenhuma variável selecionada. Selecione!",
      easyClose = TRUE,
      footer = shiny::modalButton("Dispensar")
    ))
  }
})

output$previsao <- plotly::renderPlotly({

   Sys.sleep(.5)
   plotly::layout(plot(),
           margin = list(l = 0, r = 0, t = 50, b = 0, pad = 0),
           title = "<b>Previsão de Vendas - 12 Meses</b>",
           font = t, 
           legend = list(x = .1, y = .9))
  })


}




# Run the application 
shinyApp(ui = ui, server = server)
