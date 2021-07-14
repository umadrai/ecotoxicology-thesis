library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

trace1 <- list(
  #uid = "eb0a1b2b-600e-4b00-90ef-c1299cb6c72e", 
  #mode = "markers", 
  type = "scatter", 
  x = iris$Sepal.Length,
  y = iris$Sepal.Width,
  color = ~ as.factor(iris$Species),
  marker = list(size=10),
  #group = main$Group,
  text = iris$Species
)




app$layout(
  htmlDiv(
    list(
      htmlH1('Hello Dash'),
      htmlDiv(children = "Dash: A web application framework for R."),
      dccGraph(
        figure=fig
      )
    )
  )
)

app$run_server()