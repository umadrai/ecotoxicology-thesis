library(shiny)
library(plotly)
library(DT)


ui <- fluidPage(
  plotlyOutput('myPlot'),
  fluidRow(
        DT::dataTableOutput("res")
  ),
  fluidRow(
    verbatimTextOutput("es")
  )
  
)

server <- function(input, output, session){
  output$myPlot = renderPlotly({
    plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species) %>%
      layout(dragmode = "select")
  })
  
  output$res <- renderDataTable({
    d <- event_data("plotly_selected")
    a <- subset(iris, (iris$Sepal.Length %in% d$x & iris$Petal.Length %in% d$y))
    a
  })
  # output$es <- renderPrint({
  #   d <- event_data("plotly_selected")
  #   d
  # })
  
}

shinyApp(ui, server)