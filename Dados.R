
# Librarys ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(magrittr)
library(janitor)
library(plotly)

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
  
# 
# `colnames<-`(Dados, c("Date", "Trucks","Ibc BR", "Retail Sales", "Business Confidence Index", "USD BRL", "Base Interest Rate",
#                 "Industrial Production", "Business Credit Consessions", "Commodity Price Index", 
#                   "Index Of Employed Persons Industry", "Uncertainty Index"))
# Dados de Treino ---------------------------------------------------------

inicio <- as.Date("2003-01-01")
fim <- as.Date("2020-12-01")


dados_treino <- Dados %>% 
  dplyr::filter(
    date >=  inicio, 
    date <= fim
  ) 
# Modelo ------------------------------------------------------------------
Modelo_Linear <- lm(trucks ~ ibc_br, data = dados_treino)

# Dados de Teste ----------------------------------------------------------
dados_teste <- Dados %>% 
  dplyr::filter(
    date > fim
  )

previsao <- tibble::tibble(
  Data = dados_teste$date, 
  Venda = predict(Modelo_Linear, dados_teste), 
                  Classificacao = 'Previsão'
)

p <- plotly::plot_ly()

p <- add_trace(p, line = list(
  color = "rgba(0,0,0,1)", 
  fillcolor = "rgba(0,0,0,1)"
), hoveron = "points",
x = dados_treino$date, 
y = dados_treino$trucks, 
type = "scatter",
mode = "lines", 
name = "Observado")


p <- add_trace(p, line = list(
  color = "rgba(0,0,255,1)", 
  fillcolor = "rgba(0,0,255,1)"),
  x = previsao$Data, 
  y = previsao$Venda,
  mode = "lines",
  name = "Previsão",
  type = "scatter"
)


layout(p,
       margin = list(
  b = 40, 
  l = 60, 
  r = 10, 
  t = 25
),
title = "Previsão de Vendas - 12 Meses",
xaxis = list("Período", c(0, 1)), 
yaxis = list("Vendas"))

