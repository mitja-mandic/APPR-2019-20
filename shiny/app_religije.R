ui <- fluidPage(
  titlePanel("Graf starostnih struktur in religij"),
  sidebarLayout(
    position = "right",
    sidebarPanel("Izbor religij",
                 checkboxGroupInput(inputId = "religija",
                                    label = "religija",
                                    choices = c("skupaj",
                                                "Kristjani" = "christians",
                                                "Muslimani" = "muslims",
                                                "Judje" = "jews",
                                                "Budisti" = "buddhists",
                                                "Hindujci" = "hindus"), selected = "skupaj"),
                 radioButtons(inputId = "skupina",
                              label = "Starostna skupina",
                              choices = c("0-14",
                                          "15-64",
                                          "65+"))
                 ),
  mainPanel(plotOutput("graf_religije", height = "400px", width = "600px"))
  )
)


server <- function(input, output){
  y <- reactive({
    if(input$religija != "skupaj"){
    graf <- prevladujocaVera_starostne %>% filter(religion == input$religija, Age_group == input$skupina)
    ggplot(graf, aes(x = country, y = percentage.y, color = religion)) + geom_point() + facet_wrap(year~.)
    }
    else{
      graf <- prevladujocaVera_starostne %>% filter(Age_group == input$skupina)
      ggplot(graf, aes(x = country, y = percentage.y, color = religion)) + geom_point() + facet_wrap(year~.)
    }
  })
  output$graf_religije <- renderPlot(y())
}


shinyApp(ui = ui, server = server)
