output$viscolors <- renderPlot({
  ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Width)) + geom_point(size=6) 
})


output$psog <- renderPrint({ps.og()})
output$pstext <- renderPrint({ps.norm()})
output$rnames_return <- renderPrint({rnames()})
output$metavar_return <- renderPrint({Meta_vars()})