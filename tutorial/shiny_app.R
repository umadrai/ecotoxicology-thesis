library(shiny)
library(plotly)

ui <- fluidPage(
  plotlyOutput('myPlot'),
  verbatimTextOutput("se")
)

server <- function(input, output, session){
  output$myPlot = renderPlotly({
    plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species) %>%
      layout(dragmode = "select")
  })
  
  output$se <- renderPrint({
    d <- event_data("plotly_selected")
    a <- subset(iris, (iris$Sepal.Length %in% d$x & iris$Petal.Length %in% d$y))
    a
  })
}

shinyApp(ui, server)