ui <- fluidPage(
  titlePanel("Graf starostnih struktur in HDI/BDP/populacije"),
  tabsetPanel(
    tabPanel("BDP",
      sidebarLayout(
        position = "left",
        sidebarPanel("Parametri za graf",
                    selectInput(inputId = "leto1",
                                label = "Leto",
                                choices = c("skupaj", leta2),
                                selected = "skupaj"),
                    selectInput(inputId = "podatek", 
                                label = "Parametri za graf",
                                choices = c("BDP PPP" = "gdp_ppp",
                                            "BDP pc" = "bdp_pc",
                                            "HDI" = "HDI",
                                            "Populacija" = "population")),
                    radioButtons(inputId = "skupina",
                                 label = "Starostna skupina",
                                 choices = c("0-14",
                                             "15-64",
                                             "65+")),
                    selectInput(inputId = "smooth",
                                label = "Prileganje",
                                choices = c("brez",
                                            "Linearno prileganje" = "lm",
                                            "GLM" = "glm",
                                            "LOESS" = "loess"), selected = "brez"),
                    checkboxGroupInput(inputId = "log1",
                                       label  = "Skala",
                                       choices = c("x logaritmirana" = "logx",
                                                   "y logaritmirana" = "logy"),
                                                   selected = c("logx","logy"))),
    mainPanel(plotOutput("grafBDP", height = "500px", width = "100%"))
    )
  ),
tabPanel("Izobrazba",
         sidebarLayout(
           position = "left",
           sidebarPanel("Parametri za graf",
                        selectInput(inputId = "leto2",
                                    label = "Leto",
                                    choices = c("skupaj", leta2),
                                    selected = "skupaj"),
                        selectInput(inputId = "izo",
                                    label = "Stopnja izobrazbe",
                                    choices = c("Primarna izobrazba" = "primary",
                                                "Sekundarna izobrazba" = "secondary",
                                                "Terciarna izobrazba" = "tertiary")),
                        radioButtons(inputId = "skupina2",
                                     label = "Starostna skupina",
                                     choices = c("0-14",
                                                 "15-64",
                                                 "65+")),
                        selectInput(inputId = "smooth2",
                                    label = "Prileganje",
                                    choices = c("brez",
                                                "Linearno prileganje" = "lm",
                                                "GLM" = "glm",
                                                "LOESS" = "loess"), selected = "brez"),
                        checkboxGroupInput(inputId = "log2",
                                           label  = "Skala",
                                           choices = c("x logaritmirana" = "logx2",
                                                       "y logaritmirana" = "logy2"),
                                           selected = c("logx2","logy2"))),
                        mainPanel(plotOutput("grafIZO", height = "500px", width = "100%"))
      )
    )
  )
)

server <- function(input, output) {
  x <- reactive({input$log1})
  y <- reactive({
    if (length(x()) == 2){
      if (input$leto1 != "skupaj"){
      za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina, year == input$leto1)
      ggplot(za_graf,
             aes(x = za_graf[ ,input$podatek], y = percentage)) + geom_point() + 
        scale_x_log10() + scale_y_log10() +  xlab(input$podatek) + geom_smooth(method = input$smooth, fullrange = FALSE)
      }
      else{
        za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina)
        ggplot(za_graf,
               aes(x = za_graf[ ,input$podatek], y = percentage, color = factor(year))) + geom_point() + 
          scale_x_log10() + scale_y_log10() + xlab(input$podatek) + geom_smooth(method = input$smooth, se = FALSE, fullrange = FALSE)
      }
    }
    else if(length(x()) == 1){
      if(x() == "logx"){
        if (input$leto1 != "skupaj"){
          za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina, year == input$leto1)
          ggplot(za_graf,aes(x = za_graf[ ,input$podatek], y = percentage)) + geom_point() + 
            scale_x_log10() +  xlab(input$podatek) + geom_smooth(method = input$smooth, fullrange = FALSE)
        }
        else{
          za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina)
          ggplot(za_graf,aes(x = za_graf[ ,input$podatek], y = percentage, color = factor(year))) + geom_point() + 
            scale_x_log10() + xlab(input$podatek) + geom_smooth(method = input$smooth, se = FALSE, fullrange = FALSE)
        }
      }
      else{
        if (input$leto1 != "skupaj"){
          za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina, year == input$leto1)
          ggplot(za_graf, aes(x = za_graf[ ,input$podatek], y = percentage)) + geom_point() + scale_y_log10() +  
            xlab(input$podatek) + geom_smooth(method = input$smooth, se = FALSE, fullrange = FALSE)
        }
        else{
          za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina)
          ggplot(za_graf, aes(x = za_graf[ ,input$podatek], y = percentage, color = factor(year))) + geom_point() + scale_y_log10() +
            xlab(input$podatek) + geom_smooth(method = input$smooth, fullrange = FALSE)
          
        }
      }
    }
  else{
    if (input$leto1 != "skupaj"){
      za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina, year == input$leto1)
      ggplot(za_graf,
             aes(x = za_graf[ ,input$podatek], y = percentage)) + geom_point() + 
        xlab(input$podatek) + geom_smooth(method = input$smooth, fullrange = FALSE)
    }
    else{
      za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina)
      ggplot(za_graf,
             aes(x = za_graf[ ,input$podatek], y = percentage, color = factor(year))) + geom_point() + 
         xlab(input$podatek) + geom_smooth(method = input$smooth, se = FALSE, fullrange = FALSE)
    }
  }
})
  w <- reactive({input$log2})
  z <- reactive({
    if(length(w()) == 2){  
      if(input$leto2 != "skupaj"){
        grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, year == input$leto2, series == input$izo)
        ggplot(grafIzo, aes(x = percentageIzo, y = percentage)) + geom_point() + geom_smooth(method = input$smooth2, fullrange = FALSE) + 
        scale_x_log10() + scale_y_log10()                                                                                      
      }
      else{
        grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, series == input$izo)
        ggplot(grafIzo, aes(x = percentageIzo, y = percentage, color = factor(year))) + geom_point() + 
        geom_smooth(method = input$smooth2, fullrange = FALSE, se = FALSE) + 
        scale_x_log10() + scale_y_log10()
      }
    }
    else if(length(w()) == 1){
      if(input$log2 == "logx2"){
        if(input$leto2 != "skupaj"){
          grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, year == input$leto2, series == input$izo)
          ggplot(grafIzo, aes(x = percentageIzo, y = percentage)) + geom_point() + 
          geom_smooth(method = input$smooth2, fullrange = FALSE) + scale_x_log10()
        }
        else{
          grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, series == input$izo)
          ggplot(grafIzo, aes(x = percentageIzo, y = percentage, color = factor(year))) + geom_point() + 
            geom_smooth(method = input$smooth2, fullrange = FALSE, se = FALSE) + scale_x_log10()
        }
      }
      else{
        if(input$leto2 != "skupaj"){
          grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, year == input$leto2, series == input$izo)
          ggplot(grafIzo, aes(x = percentageIzo, y = percentage)) + geom_point() + 
            geom_smooth(method = input$smooth2, fullrange = FALSE) + scale_y_log10()
        }
        else{
          grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, series == input$izo)
          ggplot(grafIzo, aes(x = percentageIzo, y = percentage, color = factor(year))) + geom_point() + 
            geom_smooth(method = input$smooth2, fullrange = FALSE, se = FALSE) + scale_y_log10()
        }
      }
    }
    else if(length(w()) == 0){
      if(input$leto2 != "skupaj"){
        grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, year == input$leto2, series == input$izo)
        ggplot(grafIzo, aes(x = percentageIzo, y = percentage)) + geom_point() + geom_smooth(method = input$smooth2, fullrange = FALSE)
      }
      else{
        grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, series == input$izo)
        ggplot(grafIzo, aes(x = percentageIzo, y = percentage, color = factor(year))) + geom_point() + 
          geom_smooth(method = input$smooth2, fullrange = FALSE, se = FALSE)
      }
    }
  })
  output$grafBDP <- renderPlot(y())
  output$grafIZO <- renderPlot(z())
}





shinyApp(ui = ui, server = server)
