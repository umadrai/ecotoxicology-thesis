library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(plotly)
library(dashTable)
app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

df <- read.csv(
  file = "https://raw.githubusercontent.com/plotly/datasets/master/solar.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

trace1 <- list(
  #uid = "eb0a1b2b-600e-4b00-90ef-c1299cb6c72e", 
  #mode = "markers", 
  type = "scatter", 
  x = main$x,
  y = main$y,
  color = ~ as.factor(main$groups),
  marker = list(size=10),
  #group = main$Group,
  text = main$groups
)


generate_table <- function(main, nrows=10) {
  
  rows <- lapply(1: min(nrows, nrow(df)),
                 function(i) {
                   htmlTr(children = lapply(as.character(df[i,]), htmlTd))
                 }
  )
  header <- htmlTr(children = lapply(names(df), htmlTh))
  htmlTable(
    children = c(list(header), rows)
  )
}

p <- plot_ly()
p <- add_trace(p, mode=trace1$mode, type=trace1$type, x=trace1$x, y=trace1$y, marker=trace1$marker, text=trace1$text)
p <- layout(p, title=layout$title, hovermode=layout$hovermode)
p


app$layout(
  htmlDiv(
    list(
      htmlH1('Eco - Toxicology Testing'),
      htmlDiv(children = "A web application framework for tSNE in R. Sample Example"),
      dccGraph(id = "exam",
        figure=p
      ),
      htmlDiv(
        id = 'txt'
      )
      ,
      htmlDiv( id = "try1",
               dashDataTable(
         id = "table"
        # columns = lapply(colnames(main),
        #                  function(colName){
        #                    list(
        #                      id = colName,
        #                      name = colName
        #                    )
        #                  }),
        # data = df_to_list(main[1:100,])
      )
      )
    )
  )
  )

names(main)

app$callback(
  output = list(id = "txt", property = 'children'),
  params = list(input(id = "exam", property = "selectedData")),
  
  function(selectedData){
    sprintf("selected points are \"%s\"", selectedData['points'])
  }
)

app$callback(
  output = list(id = "table", property = 'figure'),
  params = list(input(id = "exam", property = "selectedData")),
  
  function(selectedData){
    res <- list(columns = lapply(colnames(selectedData[['points']]),
                       function(colName){
                         list(
                           id = colName,
                           name = colName
                         )
                       }),
      data = selectedData)
    return (res)
    
     
  }
)

app$run_server()
