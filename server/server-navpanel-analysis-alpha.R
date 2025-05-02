source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

output$alphametrics_var2 <- output$alphametrics_var1 <- renderUI({
  req(ps.sub())
  ps <- ps.sub()
  selectInput('alpha_xvar',
              'Select the explanatory variable',
              sample_variables(ps))
})
richplot <- eventReactive(input$go_rich, { #draw alpha div plot when 'plot' button is clicked
  if (input$normoptions != "none") {ps <- ps.norm()} else {ps <- ps.sub()}
  runif(input$go_rich)
  withProgress(message="Making richness plot",
               plot_richness(ps, x=input$alpha_xvar, measures=input$alphametrics) + geom_boxplot()
  )
})
output$rich_plot <- renderPlot({ #need separate call to actually render the plot
  richplot()
})
observe({
  showModal(modalDialog(
    renderPlot(richplot()),
    footer = downloadButton('rich_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$rich_click)

output$rich_plotdwnld <- output$rich_plotdwnld_popup <-  downloadHandler(
  filename = "richness_plot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=richplot()
    )
  }) 

histoplot_pretty  <- plotPrettyServer(id="histo_pretty")
histoplot_size <- plotSizeServer(id="histo_size") 
histoplot_general <- plotControlsServer(id="histo_gen",
                   df=(if (input$normoptions != "none") {ps.norm()} else {ps.sub()}),
                   ranks=rnames(),
                   samplevars=Meta_vars()
                   )

histo_base <- reactive({
  req(ps.sub())
  req(histoplot_general$color())
  (if (input$normoptions != "none") {ps <- ps.norm()} else {ps <- ps.sub()})
  plot_bar(ps,
           x=histoplot_general$x()) + 
    geom_bar(stat='identity',position='stack',aes(color=.data[[histoplot_general$color()]],
                                                  fill=.data[[histoplot_general$color()]])) + 
    guides(color='none',fill=guide_legend(histoplot_general$color()))

}) %>% bindEvent(ps.norm(),
                 ps.sub(),
                 histoplot_general$color(),
                 histoplot_general$x()
)

histoplot <- eventReactive(input$go_histoplot, {
  histo_base() + histoplot_general$facet() + histoplot_pretty() + histoplot_size() 
}) 

output$histo_plot <- renderPlot({histoplot()})

observe({
  showModal(modalDialog(
    renderPlot(histoplot()),
    footer = downloadButton('histo_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$histo_click)


output$histo_plotdwnld <- output$histo_plotdwnld_popup <-  downloadHandler(
  filename = "histoplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=histoplot()
    )

  }) 
