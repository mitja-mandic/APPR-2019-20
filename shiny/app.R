ui <- fluidPage(
  titlePanel("Igranje"),
  sidebarLayout(
    position = "right",
    sidebarPanel("kaj na graf",
                selectInput(inputId = "podatek", 
                            label = "Parametri za graf",
                            choices = c("BDP" = "gdp",
                                        "HDI" = "HDI"),
                            br()),
                radioButtons(inputId = "skupina",
                             label = "Starostna skupina",
                             choices = c("0-14",
                                         "15-64",
                                         "65+"))
  ),
  mainPanel(plotOutput("graf"))
  )
)


server <- function(input, output) {
  y <- reactive({
  za_graf <- bdp_starostneStrutkure %>% filter(Age_group == input$skupina)
    ggplot(za_graf,
           aes(x = za_graf[ ,input$podatek], y = percentage)) + geom_point() + 
      scale_x_log10()
  })
  output$graf <- renderPlot(y())
}






shinyApp(ui = ui, server = server)
