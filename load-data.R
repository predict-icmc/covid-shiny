#' @PREDICT-ICMC load-data.R

#'* carrega o ambiente de execução do shiny*
library(R.utils)
library(shiny)
library(leaflet)
library(RColorBrewer)
# trocar pelas funcoes
# library(tidyverse)
library(magrittr)
library(crul)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(memoise)
#library(feather)
library(plotly)
library(data.table)
# library(minpack.lm)
library(forecast)
# remotes::install_github("predict-icmc/gather-data")
#library(gather.covid)
library(geobr)
library(shinybusy)
#library(brazilmaps)
library(viridis)
library(sf)
library(lubridate)

# Variaveis a serem exibidas
vars <- c(
  "Variação da média móvel de casos" = "var_mm_confirmed",
  "Variação da média móvel de óbitos" = "var_mm_deaths",
  "Confirmados / 100 mil habitantes" = "totalCases_per_100k_inhabitants",
  "Total de Casos Confirmados" = "totalCases",
  "Total de Óbitos" = "deaths",
  #"Letalidade" = "last_available_death_rate",
  "Novos Casos Confirmados" = "newCases",
  "Novos Óbitos" = "newDeaths",
  "Vacinados / 100 mil (1a dose)" = "vaccinated_single_per_100_inhabitants",
  "Vacinados / 100 mil (2a dose)" = "vaccinated_second_per_100_inhabitants",
  "Testagem / 100 mil" = "tests_per_100k_inhabitants"
  #"Populacão Estimada 2019" = "estimated_population_2019"
)

vars_plot <- c(
  "Novos Casos Confirmados" = "newCases",
  "Novos Óbitos" = "newDeaths",
  "Total de Casos Confirmados" = "totalCases",
  "Total de Óbitos" = "deaths",
  #"Letalidade" = "last_available_death_rate",
  "Confirmados / 100 mil habitantes" = "totalCases_per_100k_inhabitants"
)


vars_plot_mm <- c(
  "Novos Casos Confirmados" = "newCases",
  "Novos Óbitos" = "newDeaths"
)

vars_plot_pred <- c(
  "Casos Confirmados" = "totalCases",
  "Óbitos" = "deaths"
)


baixar_seade <- function() {
  # Lendo a base de dados do SEADE com os casos em SP
  casos <- fread("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv", sep = ";")
  leitos <- fread("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes_serie_nova_variacao_semanal.csv", sep = ";")
  
  # incluindo os codigos das DRS na base de leitos
  leitos <- leitos %>% mutate(cod_drs = parse_number(nome_drs, na = c("", "Estado de São Paulo")))
  
  
  # ajustando manualmente os codigos das DRS
  leitos$cod_drs[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)] <- c(10, 7, 8, 13, 16, 12, 3, 4, 5, 11, 2, 9, 1, 14, 15, 6, 17)
  
  casos_drs <- casos %>%
    arrange(desc(datahora)) %>%
    # filter(nome_drs == "Grande São Paulo") %>%
    group_by(cod_drs, datahora) %>%
    summarise(
      total_novos_casos = sum(casos_novos),
      total_novos_obitos = sum(obitos_novos)
    ) %>%
    mutate(
      mm7d_casos = frollmean(total_novos_casos, 7),
      mm7d_obitos = frollmean(total_novos_obitos, 7)
    ) %>%
    left_join(leitos, by = c("datahora", "cod_drs"))
  
  casos_drs$datahora <- casos_drs$datahora %>% lubridate::as_date()
  
  # trocando as virgulas por pontos
  casos_drs$ocupacao_leitos <- as.numeric(gsub(",", ".", gsub("\\.", "", casos_drs$ocupacao_leitos)))
  
  casos_drs
  
}


calcula_casos_regiao <- function(dados){
  # separando por regioes e calculando os casos do brasil
  dados %>%
    mutate(
      regiao =
        ifelse(state %in% c("AM", "TO", "PA", "RO", "RR", "AP", "AC"), "NORTE",
               ifelse(state %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"), "NORDESTE",
                      ifelse(state %in% c("PR", "RS", "SC"), "SUL",
                             ifelse(state %in% c("MG", "RJ", "ES", "SP"), "SUDESTE",
                                    ifelse(state %in% c("DF", "MT", "MS", "GO"), "CENTRO-OESTE", "NA")
                             )
                      )
               )
        )
    ) %>%
    arrange(desc(date)) %>%
    group_by(regiao, date) %>%
    summarise(
      newCases = sum(newCases),
      newDeaths = sum(newDeaths)
    ) %>%
    mutate(
      mm7d_confirmed = frollmean(newCases, 7),
      mm7d_deaths = frollmean(newDeaths, 7)
    )

}

calcula_mm <- function(dados){
  # acrescentar a media_movel

  dados %>%
    arrange(desc(state)) %>%
    group_by(state,city) %>%
    mutate(
      mm7d_confirmed = frollmean(newCases, 7),
      mm7d_deaths = frollmean(newDeaths, 7)
    ) %>%
    mutate(
      var_mm_confirmed = replace_na((mm7d_confirmed / lag(mm7d_confirmed, 14) - 1) * 100, 0),
      var_mm_deaths = replace_na( (mm7d_deaths / lag(mm7d_deaths, 14) - 1) * 100, 0)) %>%
    ungroup()
}

calcula_mm_br <- function(dados){
  
  dados %>%
    group_by(date) %>%
    mutate(
      mm7d_confirmed = frollmean(newCases, 7),
      mm7d_deaths = frollmean(newDeaths, 7)
    ) %>%
    mutate(
      var_mm_confirmed = replace_na((mm7d_confirmed / lag(mm7d_confirmed, 14) - 1) * 100, 0),
      var_mm_deaths = replace_na( (mm7d_deaths / lag(mm7d_deaths, 14) - 1) * 100, 0)) %>%
    ungroup()

}


# leitura dos dados.
#dados <- read_feather(latlong)
#dados_regioes <- read_feather(caso_regioes)


drs_seade <- baixar_seade()

# novo banco de dados
#dados <- calcula_mm(fread("https://github.com/wcota/covid19br/raw/master/cases-brazil-cities-time.csv.gz"))
#dados$date <- as_date(dados$date)

dados_estados <- fread("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-states.csv") %>% calcula_mm()
dados_estados$date <- as_date(dados_estados$date)

dados_regioes <- calcula_casos_regiao(calcula_mm(dados_estados))
dados_regioes$date <- as_date(dados_regioes$date)

#dados_cidades <- fread("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-cities.csv")
#dados_cidades$date <- as_date(dados_cidades$date)

#dados_br <- fread("https://raw.githubusercontent.com/wcota/covid19br/master/cases-brazil-total.csv") %>% calcula_mm_br()
#dados_br$date <- as_date(dados_br$date)

#dt <- dados # %>% filter(estimated_population_2019 > 200000)
estados <- dados_estados$state %>%
  unique() %>% sort() %>% 
  as.character() 


regioes <- c("CENTRO-OESTE", "NORDESTE", "NORTE", "SUDESTE", "SUL")
#cart <- read_feather(obcartorio)
