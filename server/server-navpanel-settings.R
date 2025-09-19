output$viscolors <- renderPlot({
  ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Width)) + geom_point(size=6) 
})