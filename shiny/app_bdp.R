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
                                            "LOESS" = "loess"), selected = "brez")),
    mainPanel(plotOutput("grafBDP", height = "400px", width = "600px")),
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
                                                "LOESS" = "loess"), selected = "brez")),
                        mainPanel(plotOutput("grafIZO")),
      )
    )
  )
)

server <- function(input, output) {
  y <- reactive({
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
  })
  z <- reactive({
    if(input$leto2 != "skupaj"){
      grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, year == input$leto2, series == input$izo)
      ggplot(grafIzo, aes(x = percentageIzo, y = percentage)) + geom_point() + geom_smooth(method = input$smooth2, fullrange = FALSE)
    }
    else{
      grafIzo <- izo_starostne %>% filter(Age_group == input$skupina2, series == input$izo)
      ggplot(grafIzo, aes(x = percentageIzo, y = percentage, color = factor(year))) + geom_point() + 
      geom_smooth(method = input$smooth2, fullrange = FALSE, se = FALSE)
    }
  })
  output$grafBDP <- renderPlot(y())
  output$grafIZO <- renderPlot(z())
}





shinyApp(ui = ui, server = server)
