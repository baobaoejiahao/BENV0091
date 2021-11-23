library(shiny)

# Front-end (UI)
ui <- fluidPage(
  sliderInput(inputId = "num", 
              label = "Choose a number", 
              value = 2, min = 0.1, max = 10),
  plotOutput("hist")
)

# R Code (server)
server <- function(input, output) {
  output$hist <- renderPlot({
    data <- tibble(x = seq(0, 2*pi, 0.01),
                   y = sin(input$num * x))
    ggplot(data, aes(x = x, y = y)) + geom_line()
  })
}

# Run the application
shinyApp(ui = ui, server = server)









