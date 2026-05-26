source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)


#ggplot(ps.edit) + 
#  geom_line(aes(x=Troph,y=Abundance,
#                color=Rookery,linetype=Site
#                )) + 
#  facet_grid(Importance~Trip,scales="free_y")



tlineplot_pretty  <- plotPrettyServer(id="tline_pretty")
tlineplot_size <- plotSizeServer(id="tline_size") 
tlineplot_general <- plotControlsServer(id="tline_gen",
                                         df=ps.norm.melt,
                                         var1=rnames, #color
                                         var2=Meta_vars, #x
                                         #var3=Meta_vars(), #y
                                         var4=Meta_vars, #facet
                                         #var1=Meta_vars(), #color
                                         #var2=Meta_vars(), #x
                                         var3=reactive(ps.norm.melt() %>% select(contains("roph")) %>% colnames() )#, #y
                                         #var4=Meta_vars() #facet
) 

tline_base <- reactive({
  print("Line diagram: building base plot")
  req(ps.norm())
  #req(tlineplot_general$x())
  ps <- ps.norm.melt()
  
  ps.edit <- ps %>% mutate(Trophic.Level = (as.numeric(.data[[tlineplot_general$y()]]))) %>% filter(Abundance >0)
  
  ggplot(ps.edit, #x=.data[[tlineplot_general$x()]], #x axis
         #          y=.data[[tlineplot_general$y()]], #must be numeric
         #          fill=.data[[tlineplot_general$x()]]
         aes(x=Trophic.Level, #must be numeric
             y=Abundance, 
             
             color=.data[[tlineplot_general$x()]], #x axis
             linetype=.data[[tlineplot_general$y()]]
         )
  ) + 
    geom_line(na.rm=TRUE) 
  
}) %>% bindEvent(ps.norm(),
                 tlineplot_general$x(),
                 tlineplot_general$y(),
                 tlineplot_general$facet()
)

tlineplot <- eventReactive(input$go_tline, {
  print("Line diagram: assembling plot")
  tline_base() + tlineplot_general$facet() + tlineplot_pretty() + tlineplot_size() 
}) 

observeEvent(input$go_tline, {
  print("Line diagram: plotting")
  output$tline_plot <- renderPlot({tlineplot()})
})

observe({
  showModal(modalDialog(
    renderPlot(tlineplot()),
    footer = downloadButton('download_tlineplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Line diagram: popup")
}) %>% bindEvent(input$tline_click)


output$tline_plotdwnld <- output$tline_plotdwnld_popup <-  downloadHandler(
  filename = "tlineplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=tlineplot()
    )
    
  }) 
