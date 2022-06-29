
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
# DF contento dados de treino e as previsoes para 12 meses a frente -------
Dados_historicos_e_de_previsoes <- tibble::tibble(
Data = dados_treino$date,
Venda = dados_treino$trucks,
Classificacao = "Treino"
) %>% 
rbind(
tibble::tibble(
  Data = dados_teste$date, 
  Venda = predict(Modelo_Linear, dados_teste), 
                  Classificacao = 'Previsão'
))



p <- plotly::plot_ly()

add_trace(p, line = list(
  color = "rgba(0,0,0,1)", 
  fillcolor = "rgba(0,0,0,1)"
), x = Dados_historicos_e_de_previsoes$Data, y = Dados_historicos_e_de_previsoes$Venda, type = "scatter", mode = "lines")

