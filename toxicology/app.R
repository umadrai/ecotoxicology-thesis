# LCEC50 Final sheet works
library(shiny)
library(plotly)
library(ggplot2)
library(crosstalk)
#library(openxlsx)
source("data_Shiny.R", local = TRUE)
source("clustering.R", local = TRUE)
source("final_regression.R", local = TRUE)

# Define UI for data upload app ----
ui <- fluidPage(
    # Importing js for message alerts
    
    # App title ----
    titlePanel("EcoToxicology Thesis"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            # Input: Select a file ----
            fileInput("file", "Choose Data File (xlsx)",
                      multiple = FALSE,
                      accept = c(".xlsx")),
            
            #####
            #actionButton("choice", "Select Columns"),
            #selectInput("columnss", "Select Columns2", choices = NULL, multiple = TRUE),
            
            
            # Horizontal line ----
            # tags$hr(),
            # 
            # Input: Checkbox if file has header ----
            #checkboxInput("header", "Header", TRUE),
            #actionButton("choice", "Select Columns"),
            helpText("Step 1: Upload File."),
            helpText("Step 2: Select Columns below."),
            helpText("Note: Please select CAS and Smiles column"),
            checkboxGroupInput("columns","Columns: No file Selected", choices = NULL),
            
            # Horizontal line ----
            #tags$hr()
            # # Input: Select separator ----
            # radioButtons("sep", "Separator",
            #              choices = c(Comma = ",",
            #                          Semicolon = ";",
            #                          Tab = "\t"),
            #              selected = ","),
            # 
            # # Input: Select quotes ----
            # radioButtons("quote", "Quote",
            #              choices = c(None = "",
            #                          "Double Quote" = '"',
            #                          "Single Quote" = "'"),
            #              selected = '"'),
            # 
            # # Horizontal line ----
            # tags$hr(),
            # 
            # Input: Select number of rows to display ----
            # Standard
            # MACCS
            # Extended
            # Graph
            # Circular"
            radioButtons("fp_types", "Fingerprint Types",
                         choices = c(Standard = "standard",
                                     MACCS = "maccs",
                                     Extended = "extended",
                                     Graph = "graph",
                                     Circular = "circular"),
                         selected = "maccs"),
            actionButton("choice", "Create TSNE"),
            helpText("Please click below to create Clusters"),
            actionButton("cluster", "Create Clusters"),
            helpText("Please click below to perform regression analysis"),
            actionButton("reg_button", "Regression")

        ),
        
        # Main panel for displaying outputs ----
          mainPanel(
            # Output: Data file ----
            fluidRow(
              div(
                id = "datadetails",
                tags$h3("Data Set"),
                column(
                  width = 12,
                  DT::dataTableOutput("table")
                  ),
                style = "height:200px;"
                
              )
            ),
            

              tags$h3("TSNE Plot"),
              plotlyOutput('myPlot'),
            
              uiOutput("fingerPrints"),
            
              #textOutput("txt"),
            
              tags$h3("Clusters"),
              #plotlyOutput('clusters_plot'),
            
              plotlyOutput('clusters_plot2'),
              DT::dataTableOutput("table2"),
              
            
              tags$h3("Visualising clusters on TSNE"),
              plotlyOutput('tsne_cluster'),
              DT::dataTableOutput("table3"),
            
              tags$h3("Regression Plot"),
              plotlyOutput('reg_plot'),
              DT::dataTableOutput("fingerprints"),
        )
        
    )
)


server <- function(input, output, session){
      
    data <- reactive({
                req(input$file)
                df <- openxlsx::read.xlsx(xlsxFile = input$file$datapath, 1)
                
                # updateCheckboxGroupInput(session, "columns", "Select Columns", choices = col_names)
            })
    observeEvent(data(),{
        updateCheckboxGroupInput(session, "columns", "Columns:", choices = colnames(data()))
    })
        
    filtereddata <- eventReactive(input$columns, {
        df <- data()
        s <- subset(df, select = input$columns)
        return(s)
    })
    
    # output$datahead <- renderTable({
    #   # Overview of data user uploaded
    #     example_data <- data()
    #     example_data$mol_L <- format(example_data$mol_L, scientific = TRUE)
    #     head(example_data)
    # })

    output$table <- DT::renderDataTable(DT::datatable({
      datahead <- data()
      datahead
    },
    extensions = 'Buttons',
    
    options = list(
      #paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = FALSE,
      ordering = TRUE,
      dom = 'Bftsp',
      buttons = c('copy', 'csv', 'excel')
    ),
    
    class = "display"
    ),
    server = FALSE    # To Allow whole table to be downloaded
    )
    
    ####### Printing cluster DT
    output$table2 <- DT::renderDataTable(DT::datatable({
      data <- dataa()
      data
    },
    extensions = 'Buttons',
    
    options = list(
      #paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = FALSE,
      ordering = TRUE,
      dom = 'Bftsp',
      buttons = c('copy', 'csv', 'excel')
    ),
    
    class = "display"
    ), server = FALSE)
    
    output$table3 <- DT::renderDataTable(DT::datatable({
      data <- tsne_clusterdata()
      data
    },
    extensions = 'Buttons',
    
    options = list(
      #paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = FALSE,
      ordering = TRUE,
      dom = 'Bftsp',
      buttons = c('copy', 'csv', 'excel')
    ),
    
    class = "display"
    ), server = FALSE)
    # Showing data with fingerprints
    # output$fingerPrints <- renderTable({
    #   fp_data <- tsne_data()
    #   orig_data <- data()
    #   orig_names <- names(orig_data)
    #   #by = c("CAS", "Smiles")
    #   full_data <- inner_join(orig_data, fp_data, by = c(orig_names[1], orig_names[2]), copy = FALSE)
    # 
    #   head(full_data)
    # 
    # })
    # For creating the data ready for TSNE plot
    tsne_data <- eventReactive(input$choice, {
      
        use_df <- filtereddata()
        fp_type <- input$fp_types
      
        dataa <- tsne_Data(use_df, fp_type)

        return(dataa)
     })
      
    # Using the data to create the plot
    tsne_plot <- eventReactive(input$choice, {
      
      dataa <- tsne_data()
      fp_type <- input$fp_type
      tsne_plot_data <- create_tsne(dataa, fp_type)
      
      return(tsne_plot_data)
    })
    
    output$myPlot = renderPlotly({
      tsne_plt <- tsne_plot()
      tsne_plt[[1]]
      
      #plot_ly(data = iris, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species) %>%
      # layout(dragmode = "select")
    })
    
    # output$txt <- renderText({
    #   data <- as.data.frame(tsne_data())
    #   # data2 <- clusters()
    #   # data2 <- as.data.frame(data2[[2]])
    #   paste("You chose",  dim(data))
    # })
    
    # Call the clustering function from clustering.R script
    clusters <- eventReactive(input$cluster, {

       data <- tsne_plot()
       dataa <- as.data.frame(data[[2]])
       details <- create_clusters(dataa[,3:4])

      return(details)
    })
    
    
    # Output plot for showing clusters
    output$clusters_plot = renderPlotly({

      details <- clusters()
      #clust_plot <- details[[1]]
      #data_details <- details[[2]]

      (details[[1]])

    })
    


    # Replaced with tsne_cluster function
    # output$test <- renderPlotly({
    #   #data <- tsne_plot()
    #   #head(data[[2]])
    #   
    #  test <- clusters()
    #  data <- as.data.frame(test[[2]])
    #  #head(data$cluster)
    #  groups <- as.factor(data$cluster)
    #  g <- ggplot((data),aes(x,y, col = groups)) +  geom_point(alpha = 0.8) + theme_bw()
    #  
    #  ggplotly(g)
    #  
    #  
    #  
    # })
    
    
    # Button input for regression
    reg_data <- eventReactive(input$reg_button, {
      #data from clusters
      temp <- clusters()
      data <- as.data.frame(temp[[2]])
      #data from tsne
      temp2 <- tsne_plot()[[2]]
      #original df
      orig_df <- data()
      
      # Joining original data with cluster and tsne points data
      joined <- inner_join(data, temp2, by = c("x","y"), copy = FALSE)
      # Now joining with original df....
      joined <- inner_join(joined, orig_df, by = c("CAS", "Smiles"), copy = False)
      col_order <- order(colnames(joined))
      joined <- joined[, col_order]
      
      # Writing output to EXCEL for Sylvia
      #openxlsx::write.xlsx(joined, "all_data_clusters.xlsx")
      
      #calling function from final_regression.R
      results <- regression(orig_df)
      return(results)
    })
    # Plot of regressions 
    output$reg_plot <- renderPlotly({

      plots <- reg_data()

      plots

    })

  
    #####CROSSTALK
    dataa <- reactive({
      data <- clusters()
      dataa <- data[[2]]
      shared <- SharedData$new(dataa)
      return(shared)
    })
    
    output$clusters_plot2 <- renderPlotly({
      plot_ly(
        dataa(),
        x = ~x, 
        y = ~y,
        color = ~cluster,
        type = "scatter",
        mode = "markers"
      ) %>% 
        highlight("plotly_selected")
    })
    
    # For visualising clusters on tsne.
    tsne_clusterdata <- reactive({
      data1 <- tsne_data()
      data1_final <- data1[,1:2]
      data2 <- clusters()
      data2_final <- data2[[2]]
      final_data <- cbind(data1[,1:2], data2_final)
      shared <- SharedData$new(final_data)
      return(shared)
    })

    
    output$tsne_cluster <- renderPlotly({
      plot_ly(
        tsne_clusterdata(),
        x = ~x, 
        y = ~y,
        color = ~cluster,
        type = "scatter",
        mode = "markers"
      ) %>% 
        highlight("plotly_selected")
    })
    
    
    
    
}


# Create Shiny app ----
shinyApp(ui, server)

