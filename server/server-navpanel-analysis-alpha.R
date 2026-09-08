source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

histoplot_pretty  <- plotPrettyServer(id="histo_pretty")
histoplot_size <- plotSizeServer(id="histo_size") 
histoplot_general <- plotControlsServer(id="histo_gen",
                   df=ps.norm,
                   var1=rnames, #color
                   var2=Meta_vars, #x
                   var3=Meta_vars, #y
                   var4=Meta_vars #facet
                   )


histo_base <- reactive({
  req(ps.norm())
  req(histoplot_general$color())
  ps <- ps.norm()
  print("Histogram: making base plot")
  plot_bar(ps,
           x=histoplot_general$x()) + 
    geom_bar(stat='identity',position='stack',aes(color=.data[[histoplot_general$color()]],
                                                  fill=.data[[histoplot_general$color()]])) + 
    guides(color=guide_legend(histoplot_general$color()),fill=guide_legend(histoplot_general$color()))

}) %>% bindEvent(ps.norm(),
                 histoplot_general$color(),
                 histoplot_general$x()
)

histoplot <- eventReactive(input$go_histoplot, {
  
  print("Histogram: assembling plot")
  
  histo_base() + histoplot_general$facet() + histoplot_pretty() + histoplot_size$aspectRatio() 
}) 

observeEvent(input$go_histoplot,{
  print("Histogram: rendering plot")
  output$histo_plot <- renderPlot({histoplot()})
  
  tryCatch(
  ggsave(filename="histoplot.png", 
         plot=histoplot(),
         path=tempdir(),
         height=histoplot_size$height(),width=histoplot_size$width(),
         scale=2
         ),
  error=function(e){return(NA)}
  )
})

observe({
  showModal(modalDialog(
    renderPlot(histoplot()),
    footer = downloadButton('histo_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Histogram: popup")
}) %>% bindEvent(input$histo_click)


output$histo_plotdwnld <- output$histo_plotdwnld_popup <-  downloadHandler(
  filename = "histoplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=histoplot(),
           height=histoplot_size$height(),width=histoplot_size$width(),
           scale=2
    )

  }) 
