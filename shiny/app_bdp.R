ui <- fluidPage(
  titlePanel("Graf starostnih struktur in HDI/BDP/populacije"),
  sidebarLayout(
    position = "right",
    sidebarPanel("Parametri za graf",
                selectInput(inputId = "leto",
                            label = "Leto",
                            choices = c("skupaj", leta2),
                            selected = "skupaj"),
                selectInput(inputId = "podatek", 
                            label = "Parametri za graf",
                            choices = c("BDP PPP" = "gdp_ppp",
                                        "BDP pc" = "bdp_pc",
                                        "HDI" = "HDI",
                                        "Populacija" = "population"),
                            br()),
                radioButtons(inputId = "skupina",
                             label = "Starostna skupina",
                             choices = c("0-14",
                                         "15-64",
                                         "65+")),
  ),
  mainPanel(plotOutput("graf", height = "400px", width = "600px"))
  )
)



server <- function(input, output) {
  y <- reactive({
    if (input$leto != "skupaj"){
    za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina, year == input$leto)
    ggplot(za_graf,
           aes(x = za_graf[ ,input$podatek], y = percentage)) + geom_point() + 
      scale_x_log10() + scale_y_log10() +  xlab(input$podatek)
    
    }
    else{
      za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina)
      ggplot(za_graf,
             aes(x = za_graf[ ,input$podatek], y = percentage, color = factor(year))) + geom_point() + 
        scale_x_log10() + scale_y_log10() + xlab(input$podatek)
      
    }
  })
  output$graf <- renderPlot(y())
}






shinyApp(ui = ui, server = server)
