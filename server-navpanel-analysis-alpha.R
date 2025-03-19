#source('mod-plot2.R')
source('module-plotting-general.R',local=TRUE)
source('module-plotting-pretty.R',local=TRUE)
source('module-plotting-size.R',local=TRUE)
library(shinycssloaders)

output$alphametrics_var2 <- output$alphametrics_var1 <- renderUI({
  req(ps.sub())
  ps <- ps.sub()
  selectInput('alpha_xvar',
              'Select the explanatory variable',
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

output$download_richplot <- output$download_richplot_popup <-  downloadHandler(
  filename = "richness_plot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=richplot()
    )
  }) 

#histo_plot <- plotServer(id='histo',
#                         df=(if (input$normoptions != "none") {ps <- ps.norm()} else {ps <- ps.sub()}),
#                         type='bar',
#                         ranks=rnames(),
#                         samplevars=Meta_vars()
#) %>% bindEvent(ps.norm(),
#                ps.sub(),
#                rnames(),
#                Meta_vars())

#output$histo <- renderPlot({req(histo_plot())})
#histoplot_general <- reactiveValues(x=NULL,y=NULL,color=NULL,facet=NULL)

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
  #histoplot_general$facet() + base_theme 
  #labs(title="Distribution of taxonomic assignments",
  #     subtitle="(across the entire dataset)",
  #     x="Percent",
  #     caption="Unassigned sequences were included in percentage calculation but not plotted")

}) %>% bindEvent(ps.norm(),
                 ps.sub(),
                 histoplot_general$color(),
                 histoplot_general$x()
)

histoplot <- eventReactive(input$plot_histo, {
  histo_base() + histoplot_general$facet() + histoplot_pretty() + histoplot_size() 
}) 

output$histo <- renderPlot({histoplot()})

observe({
  showModal(modalDialog(
    renderPlot(histoplot()),
    footer = downloadButton('download_histoplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$histo_click)


output$download_histoplot <- output$download_histoplot_popup <-  downloadHandler(
  filename = "histoplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=histoplot()
    )

  }) 
