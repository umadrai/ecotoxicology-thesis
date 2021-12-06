#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    title = "Trying Shiny App",
    h1("TSNE plot"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("fps",
                        "Select Type of fingerprint:",
                        choices = list("a", "b", "c"),
                        selected = NULL,
                        multiple = FALSE,
                        selectize = TRUE,
                        width = 40,
                        size = NULL
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput('myPlot'),
            fluidRow(
                DT::dataTableOutput("res")
                ),
            fluidRow(
                verbatimTextOutput("es")
                )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
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

# Run the application 
shinyApp(ui = ui, server = server)
