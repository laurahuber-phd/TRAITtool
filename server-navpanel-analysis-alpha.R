#source('mod-plot2.R')
library(shinycssloaders)

output$alphametrics_var2 <- output$alphametrics_var1 <- renderUI({
  req(ps.sub())
  ps <- ps.sub()
  selectInput('alpha_xvar',
              'Select x axis variable',
              sample_variables(ps))
})
richplot <- eventReactive(input$DrawAlphaPlot, { #draw alpha div plot when 'plot' button is clicked
  if (input$normoptions != "none") {ps <- ps.norm()} else {ps <- ps.sub()}
  runif(input$DrawAlphaPlot)
  withProgress(message="Making richness plot",
               plot_richness(ps, x=input$alpha_xvar, measures=input$alphametrics) + geom_boxplot()
  )
})
output$richness <- renderPlot({ #need separate call to actually render the plot
  richplot()
})
observe({
  showModal(modalDialog(
    renderPlot(richplot()),
    footer = downloadButton('download_richplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$rich_click)
output$download_richplot_popup <-  downloadHandler(
  filename = "rich_plot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=richplot()
    )
  }) 
#  histoplot <- eventReactive(input$DrawTaxPlot, { #draw the bar plot when the 'plot' button is pushed
#    if (input$normoptions != "none") {ps <- ps.norm()} else {ps <- ps.sub()}
#    runif(input$DrawTaxPlot)
#    plot_bar(ps, x=input$alpha_xvar, fill = input$taxa) + #make a bar plot  
#      geom_bar(stat="identity", position="stack")
#  })
# output$histo <- renderPlot({ #need separate call to actually render the plot
#    histoplot()
#  })
histo_plot <- plotServer(id='histo',
                         df=(if (input$normoptions != "none") {ps <- ps.norm()} else {ps <- ps.sub()}),
                         type='bar',
                         ranks=rnames(),
                         samplevars=Meta_vars()
) %>% bindEvent(ps.norm(),
                ps.sub(),
                rnames(),
                Meta_vars())

output$histo <- renderPlot({req(histo_plot())})

observe({
  showModal(modalDialog(
    withSpinner(renderPlot(histo_plot())),
    footer = downloadButton('download_histoplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$histo_click)
output$download_histoplot_popup <-  downloadHandler(
  filename = "histo_plot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=histo_plot()
    )
  }) 
#output$histoinfo <- renderPrint({
#  req(input$histohover)
#  if (input$normoptions != "none") {ps <- ps.norm()} else {ps <- ps.sub()}
#  nearPoints(ps,input$histohover,xvar=input$alpha_xvar2)
#})