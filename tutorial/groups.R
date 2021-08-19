library(shiny)
library(plotly)
library(DT)


#ori <- datatable(orig)

ui <- fluidPage(
  plotlyOutput('myPlot'),
  fluidRow(
    DT::dataTableOutput("res")
  )
  
  
)

server <- function(input, output, session){
  output$myPlot = renderPlotly({
    plot_ly(data = orig, x = orig$DIM_1, y = orig$DIM_2, color = groups) %>%
      layout(dragmode = "select")
  })
  
  output$res <- renderDataTable({
    d <- event_data("plotly_selected")
    a <- subset(orig, (orig$DIM_1 %in% d$x & orig$DIM_2 %in% d$y))
    a
  })
}

shinyApp(ui, server)