source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

violinplot_pretty  <- plotPrettyServer(id="violin_pretty")
violinplot_size <- plotSizeServer(id="violin_size") 
violinplot_general <- plotControlsServer(id="violin_gen",
                                        df=(if (input$normoptions != "none") {ps.norm.melt()} else {ps.sub.melt()}),
                                        ranks=rnames(),
                                        samplevars=Meta_vars()
)

violin_base <- reactive({
  req(ps.sub.melt())
  req(violinplot_general$x())
  (if (input$normoptions != "none") {ps <- ps.norm.melt()} else {ps <- ps.sub.melt()})
  
  ggplot(ps, aes(x=violinplot_general$x, y=violinplot_general$y, 
                 fill=violinplot_general$color)) + 
    geom_violin() + geom_boxplot(width=0.1)

}) %>% bindEvent(ps.norm.melt(),
                 ps.sub.melt(),
                 violinplot_general$color(),
                 violinplot_general$x(),
                 violinplot_general$y()
)

violinplot <- eventReactive(input$go_violin, {
  violin_base() + violinplot_general$facet() + violinplot_pretty() + violinplot_size() 
}) 

output$violin_plot <- renderPlot({violinplot()})

observe({
  showModal(modalDialog(
    renderPlot(violinplot()),
    footer = downloadButton('download_violinplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$violin_click)


output$violin_plotdwnld <- output$violin_plotdwnld_popup <-  downloadHandler(
  filename = "violinplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=violinplot()
    )
    
  }) 
