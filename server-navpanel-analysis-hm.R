source('module-plotting-general.R',local=TRUE)
source('module-plotting-pretty.R',local=TRUE)
source('module-plotting-size.R',local=TRUE)
library(shinycssloaders)

hmplot_pretty  <- plotPrettyServer(id="hm_pretty")
hmplot_size <- plotSizeServer(id="hm_size") 
hmplot_general <- plotControlsServer(id="hm_gen",
                                        df=(if (input$normoptions != "none") {ps.norm()} else {ps.sub()}),
                                        ranks=rnames(),
                                        samplevars=Meta_vars()
)

hm_base <- reactive({
  req(ps.sub())
  req(hmplot_general$x())
  (if (input$normoptions != "none") {ps <- ps.norm()} else {ps <- ps.sub()})
  plot_heatmap(ps,
               taxa.label=hmplot_general$color(),
               taxa.order=hmplot_general$color(),
               sample.label=hmplot_general$x(),
               sample.order=hmplot_general$x()
               )
}) %>% bindEvent(ps.norm(),
                 ps.sub(),
                 hmplot_general$color(),
                 hmplot_general$x()
)

hmplot <- eventReactive(input$plot_hm, {
  hm_base() + hmplot_general$facet() + hmplot_pretty() + hmplot_size() 
}) 

output$hm <- renderPlot({hmplot()})

observe({
  showModal(modalDialog(
    renderPlot(hmplot()),
    footer = downloadButton('download_hmplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$hm_click)


output$download_hmplot <- output$download_hmplot_popup <-  downloadHandler(
  filename = "hmplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=hmplot()
    )
    
  }) 
