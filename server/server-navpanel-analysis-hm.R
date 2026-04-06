source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

hmplot_pretty  <- plotPrettyServer(id="hm_pretty")
hmplot_size <- plotSizeServer(id="hm_size") 
hmplot_general <- plotControlsServer(id="hm_gen",
                                        df=ps.norm,
                                     var1=rnames, #color
                                     var2=Meta_vars, #x
                                     var3=Meta_vars, #y
                                     var4=Meta_vars #facet
)

hm_base <- reactive({
  req(ps.norm())
  req(hmplot_general$x())
  print("Heatmap: making base plot")
  ps <- ps.norm()
  plot_heatmap(ps,
               taxa.label=hmplot_general$color(),
               taxa.order=hmplot_general$color(),
               sample.label=hmplot_general$x(),
               sample.order=hmplot_general$x()
               )
}) %>% bindEvent(ps.norm(),
                 hmplot_general$color(),
                 hmplot_general$x()
)

hmplot <- eventReactive(input$go_hm, {
  print("Heatmap: assembling plot")
  hm_base() + hmplot_general$facet() + hmplot_pretty() + hmplot_size() 
}) 

observeEvent(input$go_hm, {
  print("Heatmap: rendering plot")
  output$hm_plot <- renderPlot({hmplot()})
})

observe({
  showModal(modalDialog(
    renderPlot(hmplot()),
    footer = downloadButton('download_hmplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$hm_click)


output$hm_plotdwnld <- output$hm_plotdwnld_popup <-  downloadHandler(
  filename = "hmplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=hmplot()
    )
    
  }) 
