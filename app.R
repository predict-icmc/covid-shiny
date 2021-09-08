#' @PREDICT-ICMC app.R

#'* enviar site (necessario ter token do shinyapps na maquina)*
# rsconnect::deployApp(account = "predict-icmc")

#'* carregando as dependencias no ambiente*
#' para informacoes sobre as dependências necessárias, consulte o script abaixo
source("load-data.R")

# interface do usuário - shiny
ui <- fluidPage(

  # barra de carregamento superior
  add_busy_bar(color = "#045a8d", height = "6px"), # barra de loading superior
  # tab do mapa interativo
  navbarPage("PREDICT - Covid-19",
    id = "nav",
    tabPanel(
      "Mapa Interativo",
      div(
        class = "outer",
        tags$head(
          # Include our custom CSS
          includeCSS("styles.css"),
          includeScript("gomap.js")
        ),
        # plot do mapa
        leafletOutput("map", width = "100%", height = "100%"),
        # painel flutuante
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
          width = 330, height = "auto",
          h2("Mapa Interativo do Covid-19"),
          radioButtons("radio", h3("Listar por"),
            choices = list("Estados" = 0), #"Cidades" = 1,  "Regiões" = 2),
            selected = 0
          ),
          selectInput("color", "Variável:", vars, selected = "var_mm_confirmed"),
          
          checkboxInput("log", "Escala log", value = FALSE),
          selectInput("palette", "Palheta de Cores:",
            list(
              "Sequencial:" = list("Reds", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
              "Divergente: " = list("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"),
              "Colorblind: " = list("viridis", "magma", "plasma")
            ),
            selected = "Spectral"
          ),
          numericInput("nclass", "Número de classes (legenda):", min = 3, max = 9, value = 5)


          # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          #                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),
        # plotlyOutput(outputId = "predict_cases")
        # plotOutput("graph", height = 200, click = "plot_click"),
        # plotOutput("histCentile", height = 200),
        # plotOutput("scatterCollegeIncome", height = 250)
      ),
      # logo inferior
      tags$div(id = "cite", tags$img(src = "logo-predict-small1.jpg"))

      #         'Desenvolvido pelo grupo', tags$em('PREDICT-Covid19'),
      # )
    ),
    # tab graficos por estado
    tabPanel(
      "Modelagem preditiva",
      # painel por cidades
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(
          position = "left",
          h4(textOutput("last_update"), align = "left"),
          radioButtons("radio1", h3("Tipo de previsão"),
            choices = list("Estadual" = 0, "Regional" = 2, "Federal" = 3), #"Municipal" = 1
            selected = 0
          ),
          # Input: estado e cidade ----
          uiOutput(outputId = "state"),
         # uiOutput(outputId = "choosecity"), # ,  selected ="São Paulo"),
          radioButtons("radio2",h3("Variável") ,
                       choices = list("Novos Casos" = 1, "Novos Óbitos" = 0),
                       selected = 1
          ),
          numericInput("pred_rng", "Intervalo de predição (semanas)", min=1, max=10, value=3),
          radioButtons("radio3",h3("Modelo") ,
                       choices = list("ARIMA" = 0, "NNAR" = 1),
                       selected = 0
          ),
          checkboxInput("usemm", "Modelar com a média móvel", value = FALSE),
          numericInput("minScore", "I.C. Mínimo (%)", min=60, max=99, value=80),
          numericInput("maxScore", "I.C. Máximo (%)", min=60, max=99.9, value=95),


          # actionButton(
          #   inputId = "submit_state",
          #   label ="Gerar previsão"),
          # # labels com informacoes

          # h3(textOutput("case_count"), align = "right"),
          # h3(textOutput("deaths_count"), align = "right"),
          # h3(textOutput("letality_count"), align = "right"),
          # h3(textOutput("new_cases_count"), align = "right"),
          # h3(textOutput("newDeaths_count"), align = "right"),
          span(tags$i(h5("Dados retirados do portal brasil.io")), style = "color:#045a8d"),
          span(tags$i(h6("A notificação dos casos está sujeita a uma variação significativa devido a política de testagem e capacidade das Secretarias Estaduais e Municipais de Saúde.")), style = "color:#045a8d"),
        ),
        mainPanel(

          # graficos do plotly por estado
          h2(textOutput('pred_hdr')),
          h4(textOutput("title")),
          # radioButtons("predType", "" , vars_plot_mm, selected = "totalCases"),
          plotlyOutput(outputId = "predict_cases"),

          # h2("Variacão da média móvel"),
          # radioButtons("plotTypeMM", "", vars_plot_mm, selected = "newCases"),
          # plotlyOutput(outputId = "mmPlot"),

          # h2("Comparacão 2019-2020 de óbitos notificados em cartório"),
          # h5("Pode haver atraso na consolidação dos dados"),
          # plotlyOutput(outputId = "cartMap"),
          # #h2("Acumulado no período"),
          #radioButtons("plotType", " ", vars_plot, selected = "totalCases"),
          #plotlyOutput(outputId = "distPlot")


          # plotlyOutput(outputId = "predict_deaths")
        )
      )
      # )
    ),
    tabPanel(
      "Previsão de internações por DRS (SP)",
      # painel por drs
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(
          position = "left",
          h4(textOutput("last_update_drs"), align = "left"),

          # radioButtons("radio1", h3("Tipo de previsão"),
          #             choices = list("Estadual" = 0),#"Municipal" = 1, "Regional" = 2),
          #              selected = 0),

          # Tipo de plot

          # Input: estado e cidade ----
          selectInput(
            inputId = "drs",
            label = "Escolha uma DRS:",
            choices =c("DRS 01 Grande São Paulo", "DRS 02 Araçatuba", "DRS 03 Araraquara",
                       "DRS 04 Baixada Santista", "DRS 05 Barretos", "DRS 06 Bauru", "DRS 07 Campinas",
                       "DRS 08 Franca", "DRS 09 Marília","DRS 10 Piracicaba", "DRS 11 Presidente Prudente",
                       "DRS 12 Registro", "DRS 13 Ribeirão Preto", "DRS 14 São João da Boa Vista",
                       "DRS 15 São José do Rio Preto", "DRS 16 Sorocaba", "DRS 17 Taubaté")

          ),
          # uiOutput(outputId = "choosecity_map",  selected ="São Paulo"),
          # radioButtons("radio2",h3("Variável") ,
          #              choices = list("Novos Casos" = 1, "Novos Óbitos" = 0),
          #              selected = 1
          # ),
          # numericInput("pred_rng", "Intervalo de predição (semanas)", min=1, max=10, value=3),
          # radioButtons("radio3",h3("Modelo") ,
          #              choices = list("ARIMA" = 0, "NNAR" = 1),
          #              selected = 0
          # # ),
          # numericInput("minScore", "I.C. Mínimo", min=60, max=99, value=80),
          # numericInput("maxScore", "I.C. Máximo", min=60, max=99, value=95),

          span(tags$i(h5("Dados retirados do SEADE")), style = "color:#045a8d"),
          span(tags$i(h6("A notificação dos casos está sujeita a uma variação significativa devido a política de testagem e capacidade das Secretarias Estaduais e Municipais de Saúde.")), style = "color:#045a8d") # ,
          # h3(textOutput("case_count"), align = "right"),
          # h3(textOutput("deaths_count"), align = "right"),
          # h3(textOutput("letality_count"), align = "right"),
          # h3(textOutput("new_cases_count"), align = "right"),
          # h3(textOutput("newDeaths_count"), align = "right"),
        ),
        mainPanel(
          h2("Previsão de casos para os próximos 21 dias"),
          h4("Ajuste ao modelo de Redes Dinâmicas"),
          # radioButtons("predType", "", vars_plot_mm, selected = "totalCases"),
          plotlyOutput(outputId = "predict_cases_drs"),

          # h2("Acumulado no período")
          # radioButtons("plotType", " ", names(drs_seade)),
          # plotlyOutput(outputId = "distPlot")
        )
      )
      # )
    ),
    tabPanel(
      "Sobre",
      titlePanel(h2("Sobre o Predict-Covid19")),
      mainPanel(
        column(
          2,
          tags$img(src = "logo-predict-1.jpg", width = "100%")
        ),
        column(
          10,
          paste("Este projeto visa a análise de dados de COVID-19 por meio de técnicas de visualização de dados e modelos preditivos, para o dimensionamento e prevenção dos impactos da epidemia de COVID-19 e outras síndromes respiratórias agudas graves, utilizando estatística e ciência de dados. Nossa proposta envolve a predição do número de casos e óbitos, a demanda de internações hospitalares de acordo com diferentes intervenções não farmacológicas, o que inclui medidas de distanciamento social, isolamento voluntário, isolamento de sintomáticos, uso de equipamentos de proteção individual (EPIs), monitoramento de contatos próximos ou domiciliares, triagem em serviços de saúde, entre outras."),
          h2("Equipe"),
          tags$ul(
            tags$li("Cibele Russo"),
            tags$li("Abner Leite"),
            tags$li("Flaviane Louzeiro"),
            tags$li("Francisco Rosa Dias de Miranda"),
            tags$li("Paulo Filho"),
            tags$li("Yuri Reis")
          )
        )
      )
    )
  ),
  conditionalPanel("false", icon("crosshair"))
)

# server side da aplicacao
server <- function(input, output, session) {


  # observador que lansa as cidades a serem escolhidas
  # output$choosecity <- renderUI({
  #   if (input$radio1 == 1) {
  #     req(input$state)
  #     cities <- dt %>%
  #       filter(state == input$state) %>%
  #       arrange(city) %>%
  #       select(city) %>%
  #       unique()
  #     selectInput("chooseCity", "Escolha a Cidade", cities)
  #   }
  # })
  output$state <- renderUI({
    if (input$radio1 %in% c(0,1)) {
      selectInput("state",label = "Escolha um Estado:",
                  choices = estados, selected = "SP")
    }
    else if (input$radio1 == 2) {
      selectInput("region",label = "Escolha uma Região:",
                  choices = regioes)
    }
    else if (input$radio1 == 3) return(NULL)
  })


  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(attribution = 'Dados extraídos do site <a href="http://brasil.io/">brasil.io</a>') %>%
      setView(lng = -47.9292, lat = -15.7801, zoom = 4)
  })


  # observer que mantém o mapa e a legenda de acordo com as variaveis escolhidas pelo usr
  observe({
    req(input$color)

    colorBy <- input$color

    if (input$radio == 1) {
      zipdata <- dados_cidades
      # pegando as geometrias das cidades
      shp <- get_brmap("City")

      shp$City <- as.character(shp$City)
      # definindo que o dataframe contém dados geométricos
      shp_sf <- st_as_sf(shp) %>%
        st_transform(4326)
      # unindo os dados de COVID-19 com as geometrias das cidades.
      shp_sf <- shp_sf %>% filter(City %in% dados_cidades$ibgeID)
      shp_sf$City <- shp_sf$City %>% as.integer()
      shp_sf <- left_join(shp_sf, zipdata, by = c("City" = "ibgeID"))
    }
    else if (input$radio == 0)
    {
      zipdata <- dados_estados %>% filter(date == max(dados_estados$date)-1)
      # pegando as geometrias dos estados
      shp <- geobr::read_state() #get_brmap("State")

      #shp$City <- as.character(shp$State)
      # definindo que o dataframe contém dados geométricos
      shp_sf <- st_as_sf(shp) %>%
        st_transform(4326)
      # unindo os dados de COVID-19 com as geometrias dos estados
      #shp_sf <- shp_sf %>% filter(State %in% dados$ibgeID)
      #shp_sf$City <- shp_sf$City %>% as.integer()
      shp_sf <- left_join(shp_sf, zipdata, by = c("abbrev_state" = "state"))
      shp_sf$nome <- shp_sf$name_state
    }
    else if (input$radio == 2){
      zipdata <- dados_regioes %>% filter(date == max(dados_regioes$date)-1)
      # pegando as geometrias dos estados
      shp <- geobr::read_region() #get_brmap("State")
      
      #shp$City <- as.character(shp$State)
      # definindo que o dataframe contém dados geométricos
      shp_sf <- st_as_sf(shp) %>%
        st_transform(4326)
      # unindo os dados de COVID-19 com as geometrias dos estados
      #shp_sf <- shp_sf %>% filter(State %in% dados$ibgeID)
      #shp_sf$City <- shp_sf$City %>% as.integer()
      shp_sf <- left_join(shp_sf, zipdata, by = c("name_region" = "regiao"))
      shp_sf$nome <- shp_sf$name_region
    }
    else return(NULL)
    # browser()
    colorData <- shp_sf[[colorBy]]

    # escala log se solicitado
    if (input$log) {
      colorData <- log10(colorData)
    }

    req(input$palette)
    req(input$nclass)

    if (input$palette %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
      pal <- colorBin(input$palette, colorData, req(input$nclass), pretty = T, reverse = T)
    } else {
      pal <- colorBin(input$palette, colorData, req(input$nclass), pretty = T, reverse = F)
    }

    leafletProxy("map", data = shp_sf) %>%
      clearShapes() %>%
      addPolygons(
        data = shp_sf,
        smoothFactor = 0.5,
        fillOpacity = 0.5,
        weight = 0.5,
        fillColor = ~ pal(colorData),
        opacity = 0.8,
        stroke = F,
        highlightOptions = highlightOptions(
          color = "black",
          weight = 2,
          bringToFront = TRUE
        ),
        popup = ~ paste0(
          sep = " ",
          "<b>", nome, "<b><br>",
          #"Variacao da média móvel de casos: ", round(var_mm_confirmed, 2), "%<b><br>",
          #"Variacao da média móvel de óbitos: ", round(var_mm_deaths, 2), "%<b><br>",
          "<b>Casos confirmados: </b>", totalCases, "<br>",
          #"<b>Casos por 100k habitantes: </b>", totalCases_per_100k_inhabitants, tags$br(),
          "Novos casos: ", newCases, tags$br(),
          "Novos óbitos: ", newDeaths, tags$br(),
          #"Populacão: ", estimated_population_2019, tags$br(),
          "Total de Casos Confirmados: ", totalCases, tags$br()
          #"Total de óbitos: ", deaths, tags$br()
          #"Taxa de letalidade: ", last_available_death_rate
        ),
        label = ~nome, layerId = ~nome
      ) %>%
      addLegend("bottomright",
        title = names(colorBy),
        pal = pal,
        values = colorBy,
        opacity = 0.8, layerId = "colorLegend"
      )
  })

  # # funcao que mostra a cidade clicada
  # showCityPopup <- function(ibgeID, lat, lng) {
    #     selectedZip <- dados[dados$ibgeID == ibgeID,]
  #     content <- as.character(tagList(
  #         tags$h4(HTML(sprintf("%s %s",
  #                              selectedZip$city, selectedZip$state
  #         ))),
  #         sprintf("Novos casos: %s", selectedZip$newCases), tags$br(),
  #         sprintf("Novos óbitos: %s", selectedZip$newDeaths),tags$br(),
  #         sprintf("Populacão: %s", as.integer(selectedZip$estimated_population_2019)),tags$br(),
  #         sprintf("Total de Casos Confirmados: %s", as.integer(selectedZip$totalCases)), tags$br(),
  #         sprintf("Total de óbitos: %s", as.integer(selectedZip$deaths)), tags$br(),
  #         sprintf("Taxa de letalidade: %s%%", selectedZip$last_available_death_rate)
  #     ))
  #     leafletProxy("map") %>% addPopups(lng, lat, content, layerId = ibgeID)
  #
  # }

  #'@Gráficos

  #' @Previsoes no plot_ly

  # utilizando cashing para reaproveitar as contas no server
  forecast_c <- memoise(forecast)

  # objeto reativo que armazena o modelo utilizado
  dfit <- reactiveValues(data = NULL, xreg = NULL, title = NULL, model = NULL)

  # substituir valores negativos por zero
  replace_with_zero <- function(x) {
    ifelse(x < 0, yes = 0, no = x)
  }
  
  # recebe um modelo e calcula a previsao com a confiança estipulada
  calcula_pred <- reactive({
    lwr <- input$maxScore
    upr <- input$minScore
    rng <- input$pred_rng
    fit <- dfit$data
    mdl <- dfit$model
    #xreg <- dfit$xreg

    f <- forecast_c(fit, 7 * rng, PI = T, level = c(lwr/100, upr/100), model = mdl, biasadj = T)#, xreg = xreg$mean)
    tmp <- autoplot(f)
    dfit$title <- tmp$labels$title
    
    f$lower <- replace_with_zero(f$lower)
    f$upper <- replace_with_zero(f$upper)
    f$mean <- replace_with_zero(f$mean)
    
    f
  })

  output$title <- renderText({
    dfit$title
  })

  output$pred_hdr <- renderText({
    if (input$radio2 == 1) lbl <- "casos"
    else lbl <- "óbitos"
    paste0("Previsão de novos ",lbl," para os próximos ",7*input$pred_rng, " dias")
  })



  # previsao por DRS
  output$predict_cases_drs <- renderPlotly({
    # tratamento do reactive
    req(input$drs)

    codi_drs <- drs_seade %>%
      filter(nome_drs == input$drs & datahora == "2021-02-14") %>%
      select(cod_drs) %>%
      pull()

    sp <- drs_seade %>% filter(cod_drs == codi_drs)

    # modelo de redes neurais
    dfit$data <- nnetar(sp$total_novos_casos, p = 7)
    dfit$model <- "nnetar"
    # fit2 <- nnetar(x = sp$datahora, y = sp$ocupacao_leitos,p=7)

    show_modal_spinner(text = "Calculando previsão...")


    req(input$maxScore)
    req(input$minScore)
    req(input$pred_rng)

    pred <- calcula_pred()
    # constante de previsao = % ocupaçao * new_cases / pred(new_cases)

    const <- sp %>%
      last() %>%
      select(ocupacao_leitos, mm7d_casos)
    const$ocupacao_leitos <- as.numeric(const$ocupacao_leitos)
    const$total_novos_casos <- as.numeric(const$mm7d_casos)

    # constante de predicao de leitos de acordo com media movel de casos
    pred2 <- const$ocupacao_leitos * const$total_novos_casos
    # pred2 <- calcula_pred_c(fit2)

    remove_modal_spinner()

    trace1 <- list(
      line = list(
        color = "rgba(0,0,0,1)",
        fillcolor = "rgba(0,0,0,1)"
      ),
      mode = "lines",
      name = "Média móvel de casos confirmados (7 dias)",
      type = "scatter",
      x = sp$datahora,
      y = round(sp$mm7d_casos),
      xaxis = "x",
      yaxis = "y"
    )
    trace2 <- list(
      fill = "toself",
      line = list(
        color = "rgba(242,242,242,1)",
        fillcolor = "rgba(242,242,242,1)"
      ),
      mode = "lines",
      name = "Previsão de novos casos diários (95% de confiança)",
      type = "scatter",
      x = c(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)), rev(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)))),
      y = round(c(pred$upper[, 1], rev(pred$lower[, 1]))),
      xaxis = "x",
      yaxis = "y",
      hoveron = "points"
    )
    trace3 <- list(
      fill = "toself",
      line = list(
        color = "rgba(204,204,204,1)",
        fillcolor = "rgba(204,204,204,1)"
      ),
      mode = "lines",
      name = "Previsão de novos casos diários (80% de confiança)",
      type = "scatter",
      x = c(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)), rev(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)))),
      y = round(c(pred$upper[, 2], rev(pred$lower[, 2]))),
      xaxis = "x",
      yaxis = "y",
      hoveron = "points"
    )
    trace4 <- list(
      line = list(
        dash = 3,
        color = "rgba(0,0,255,1)",
        fillcolor = "rgba(0,0,255,1)"
      ),
      mode = "lines",
      name = "previsão de novos casos diários",
      type = "scatter",
      x = min(sp$datahora) - ddays(1) + ddays(time(pred$mean)),
      y = round(pred$mean),
      xaxis = "x",
      yaxis = "y"
    )
    trace5 <- list(
      line = list(
        color = "rgba(0,155,155,1)",
        fillcolor = "rgba(0,155,155,1)"
      ),
      mode = "lines",
      name = "% leitos ocupados",
      type = "scatter",
      x = sp$datahora, # min(sp$datahora) - ddays(1) + ddays(time(pred$mean)),
      y = sp$ocupacao_leitos %>% as.numeric(),
      xaxis = "x",
      yaxis = "y2"
    )
    trace7 <- list(
      line = list(
        dash = 3,
        color = "rgba(0,100,55,1)",
        fillcolor = "rgba(0,100,55,1)"
      ),
      mode = "lines",
      name = "previsão de internações",
      type = "scatter",
      x = min(sp$datahora) - ddays(1) + ddays(time(pred$mean)),
      y = pred2 / frollmean(pred$mean, 7, fill = const$mm7d_casos),
      xaxis = "x",
      yaxis = "y2"
    )
    data <- list(trace1, trace2, trace3, trace4, trace5, trace7)
    layout <- list(
      title = paste0(input$drs),
      xaxis = list(
        # title = "Data",
        domain = range(sp$datahora)
      ),
      yaxis = list(
        title = "Novos Casos Confirmados",
        domain = c(0, 1)
      ),
      yaxis2 = list(
        title = "% Leitos ocupados",
        domain = c(0, 1),
        overlaying = "y",
        automargin = TRUE,
        side = "right"
      ),
      margin = list(
        b = 40,
        l = 60,
        r = 10,
        t = 25
      )
    )
    p <- plot_ly()
    p <- add_trace(p, line = trace1$line, mode = trace1$mode, name = trace1$name, type = trace1$type, x = trace1$x, y = trace1$y, xaxis = trace1$xaxis, yaxis = trace1$yaxis)
    p <- add_trace(p, fill = trace2$fill, line = trace2$line, mode = trace2$mode, name = trace2$name, type = trace2$type, x = trace2$x, y = trace2$y, xaxis = trace2$xaxis, yaxis = trace2$yaxis, hoveron = trace2$hoveron)
    p <- add_trace(p, fill = trace3$fill, line = trace3$line, mode = trace3$mode, name = trace3$name, type = trace3$type, x = trace3$x, y = trace3$y, xaxis = trace3$xaxis, yaxis = trace3$yaxis, hoveron = trace3$hoveron)
    p <- add_trace(p, line = trace4$line, mode = trace4$mode, name = trace4$name, type = trace4$type, x = trace4$x, y = trace4$y, xaxis = trace4$xaxis, yaxis = trace4$yaxis)
    p <- add_bars(p, y = sp$total_novos_casos, x = sp$datahora, name = "Novos casos diários")
    p <- add_trace(p, line = trace5$line, mode = trace5$mode, name = trace5$name, type = trace5$type, x = trace5$x, y = trace5$y, xaxis = trace5$xaxis, yaxis = trace5$yaxis)
    p <- add_trace(p, line = trace7$line, mode = trace7$mode, name = trace7$name, type = trace7$type, x = trace7$x, y = trace7$y, xaxis = trace7$xaxis, yaxis = trace7$yaxis)
    p <- add_segments(p, yaxis = "y2", name = "100% de ocupação", x = as_date("2020-12-31"), xend = as_date("2021-06-15"), y = 100, yend = 100, line = list(dash = 6))
    p <- layout(p, title = layout$title, xaxis = layout$xaxis, yaxis = layout$yaxis, yaxis2 = layout$yaxis2, margin = layout$margin, legend = list(orientation = "h"))
  })

  # previsao por estado e municipio
  output$predict_cases <- renderPlotly({
    show_modal_spinner(text = "Calculando previsão...") # loading bar

    # cidade estado regiao ou br
    if (input$radio1 == 1) {
      req(input$chooseCity)
      #selectedCity <- dt %>% filter(state == input$state &
      #  city == input$chooseCity)
      title_g <- paste0(input$chooseCity, " - ", input$state)
    }
    else if (input$radio1 == 0) { #estado
      req(input$state)
      selectedCity <- dados_estados %>% filter(state == input$state)
      title_g <- paste0("Estado de ", input$state)
    }
    else if (input$radio1 == 2) { #regiao
      req(input$region)
      selectedCity <- dados_regioes %>% filter(regiao == input$region)
      title_g <- paste0("Região ",input$region)
    }
    else if (input$radio1 == 3) { #brasil todo
      selectedCity <- dados_estados %>% filter(state == "TOTAL")
      title_g <- paste0("Brasil")
    }


      if (input$radio2 == 1){ #casos
      # media movel
        mm <- selectedCity$mm7d_confirmed
        lin <- selectedCity$newCases

        if (input$usemm == 1)
          mdlvar <- selectedCity$mm7d_confirmed

        else mdlvar <- selectedCity$newCases

        lbl <- "casos"
      }
      else { # ou obitos
        mm <- selectedCity$mm7d_deaths
        lin <- selectedCity$newDeaths

        if (input$usemm == 1)
          mdlvar <- selectedCity$mm7d_deaths
        else  mdlvar <- selectedCity$newDeaths

        lbl <- "óbitos"
      }
      # tipo de modelagem a ser adotado
      if(input$radio3 == 1){ # redes neurais
        rng <- input$pred_rng
        ts_mdl <- xts::xts(x = mdlvar, order.by = selectedCity$date, frequency = 7)
        #xreg = auto.arima(ts_mdl, approximation=FALSE)
        dfit$data <- nnetar(ts_mdl, p = 7)#, xreg = xreg$fitted)
        dfit$model <- "nnetar"
        #dfit$xreg <- forecast(xreg, 7 * rng)
      }
      else{ # arima
        rng <- input$pred_rng
        ts_mdl <- xts::xts(x = mdlvar, order.by = selectedCity$date, frequency = 7)
        #xreg <- nnetar(ts_mdl, p = 7)
        dfit$data <- auto.arima(ts_mdl, approximation=FALSE)#, xreg = xreg$fitted)
        dfit$model <- "Arima"
        #dfit$xreg <- forecast(xreg, 7 * rng)
      }


      # intervalos de confiança para a predição
      pred <- calcula_pred()

      remove_modal_spinner() # remove a barra de carregamento
      #browser()

      trace1 <- list(
        line = list(
          color = "rgba(0,0,0,1)",
          fillcolor = "rgba(0,0,0,1)"
        ),
        mode = "lines",
        name = paste0("Média móvel de ",lbl," (7 dias)"),
        type = "scatter",
        x = selectedCity$date,
        y = round(mm),
        xaxis = "x",
        yaxis = "y"
      )
      trace2 <- list(
        fill = "toself",
        line = list(
          color = "rgba(242,242,242,1)",
          fillcolor = "rgba(242,242,242,1)"
        ),
        mode = "lines",
        name = paste0("Previsão de novos ",lbl," diários (",input$minScore,"% de confiança)"),
        type = "scatter",
        x = c(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)), rev(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)))),
        y = round(c(pred$upper[, 1], rev(pred$lower[, 1]))),
        xaxis = "x",
        yaxis = "y",
        hoveron = "points"
      )
      trace3 <- list(
        fill = "toself",
        line = list(
          color = "rgba(204,204,204,1)",
          fillcolor = "rgba(204,204,204,1)"
        ),
        mode = "lines",
        name = paste0("Previsão de novos ", lbl," diários (",input$maxScore,"% de confiança)"),
        type = "scatter",
        x = c(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)), rev(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)))),
        y = round(c(pred$upper[, 2], rev(pred$lower[, 2]))),
        xaxis = "x",
        yaxis = "y",
        hoveron = "points"
      )
      trace4 <- list(
        line = list(
          dash = 3,
          color = "rgba(0,0,255,1)",
          fillcolor = "rgba(0,0,255,1)"
        ),
        mode = "lines",
        name = paste0("previsão de novos ", lbl," diários"),
        type = "scatter",
        x = min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)),
        y = round(pred$mean),
        xaxis = "x",
        yaxis = "y"
      )
      data <- list(trace1, trace2, trace3, trace4)
      layout <- list(
        title = title_g,
        xaxis = list(
          # title = "Data",
          domain = range(selectedCity$date)
        ),
        yaxis = list(
          title = paste0("Novos ", lbl," diários"),
          domain = c(0, 1)
        ),
        margin = list(
          b = 40,
          l = 60,
          r = 10,
          t = 25
        )
      )
      p <- plot_ly()
      p <- add_trace(p, line = trace1$line, mode = trace1$mode, name = trace1$name, type = trace1$type, x = trace1$x, y = trace1$y, xaxis = trace1$xaxis, yaxis = trace1$yaxis)
      p <- add_trace(p, fill = trace2$fill, line = trace2$line, mode = trace2$mode, name = trace2$name, type = trace2$type, x = trace2$x, y = trace2$y, xaxis = trace2$xaxis, yaxis = trace2$yaxis, hoveron = trace2$hoveron)
      p <- add_trace(p, fill = trace3$fill, line = trace3$line, mode = trace3$mode, name = trace3$name, type = trace3$type, x = trace3$x, y = trace3$y, xaxis = trace3$xaxis, yaxis = trace3$yaxis, hoveron = trace3$hoveron)
      p <- add_trace(p, line = trace4$line, mode = trace4$mode, name = trace4$name, type = trace4$type, x = trace4$x, y = trace4$y, xaxis = trace4$xaxis, yaxis = trace4$yaxis)
      p %>% add_bars(x = ~date, y = ~ mdlvar)
      p <- layout(p, title = layout$title, xaxis = layout$xaxis, yaxis = layout$yaxis, margin = layout$margin, legend = list(orientation = "h"))
      p %>% add_bars(y = lin, x = selectedCity$date, name = paste0("Novos ",lbl," diários"))


  })

  # plot dinamico com a media movel.
  output$distPlot <- renderPlotly({
    # expressão do reactive para tratamento do input
    # req(input$chooseCity)
    req(input$state)
    req(input$plotType)

    # seleciona cidade ou estado
    if (input$radio1 == 1) {
      #selectedCity <- dt %>% filter(state == input$state &
       # city == input$chooseCity)
    } else if (input$radio1 == 0) {
      selectedCity <- dados_estados %>% filter(state == input$state)
    }

    # input
    selectedvar <- selectedCity %>%
      select(input$plotType) %>%
      pull()

    if (input$plotType %in% c("newCases", "newDeaths")) {
      p <- selectedCity %>%
        plot_ly() %>%
        add_lines(x = ~date, y = ~ frollmean(selectedvar, 7)) %>%
        add_bars(x = ~date, y = ~selectedvar) %>%
        config(displayModeBar = F) %>%
        hide_legend()
    }
    else {
      p <- selectedCity %>%
        plot_ly() %>%
        add_lines(x = ~date, y = ~selectedvar) %>%
        config(displayModeBar = F) %>%
        hide_legend()
    }

  })

}
# browser()
shinyApp(ui = ui, server = server)

# enviar site
# rsconnect::deployApp(account = "predict-icmc")
