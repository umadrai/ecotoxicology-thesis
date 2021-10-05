library(shiny)
library(plotly)
library(DT)


orig[,6:7] <- round(orig[,6:7], 4)

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
    plot_ly(data = orig, x = orig$`1`, y = orig$`2`, color = groups) %>%
      layout(dragmode = "select")
  })
  
  output$es <- renderPrint({
    d <- event_data("plotly_selected")
    d
  })
  
  output$res <- renderDataTable({
    d <- event_data("plotly_selected")
    #browser()
    a <- subset(orig, (orig$`1` %in% d$x & orig$`2` %in% d$y))
    a
  })
}

shinyApp(ui, server)