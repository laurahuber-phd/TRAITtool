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
  hm_base() + hmplot_general$facet() + hmplot_pretty() + hmplot_size$aspectRatio() 
}) 

observeEvent(input$go_hm, {
  print("Heatmap: rendering plot")
  output$hm_plot <- renderPlot({hmplot()})
  
  tryCatch(
  ggsave(filename="hm.png", 
         plot=hmplot(),
         path=tempdir(),
         height=hmplot_size$height(),width=hmplot_size$width(),
         scale=2
         ),
  error=function(e){return(NA)}
  )
})

observe({
  showModal(modalDialog(
    renderPlot(hmplot()),
    footer = downloadButton('download_hmplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Heatmap: popup")
}) %>% bindEvent(input$hm_click)


output$hm_plotdwnld <- output$hm_plotdwnld_popup <-  downloadHandler(
  filename = "hmplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=hmplot(), 
           height=hmplot_size$height(),width=hmplot_size$width(),
           scale=2
    )
    
  }) 
