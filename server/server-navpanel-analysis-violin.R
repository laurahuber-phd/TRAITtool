source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

violinplot_pretty  <- plotPrettyServer(id="violin_pretty")
violinplot_size <- plotSizeServer(id="violin_size") 
violinplot_general <- plotControlsServer(id="violin_gen",
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

violin_base <- reactive({
  print("Violin: building base plot")
  req(ps.norm())
  #req(violinplot_general$x())
  ps <- ps.norm.melt()
  
  ps.edit <- ps %>% mutate(Trophic.Level = (as.numeric(.data[[violinplot_general$y()]]))) %>% filter(Abundance >0)
  
  get_box_stats <- function(y, upper_limit = max(ps.edit$Trophic.Level) * 1.15) {
    return(data.frame(
      y = 0.95 * upper_limit,
      label = paste(
        "Count =", length(y), "\n",
        "Mean =", round(mean(y), 2), "\n",
        "Median =", round(median(y), 2), "\n"
      )
    ))
  }
  
  ggplot(ps.edit, #x=.data[[violinplot_general$x()]], #x axis
            #          y=.data[[violinplot_general$y()]], #must be numeric
            #          fill=.data[[violinplot_general$x()]]
                       aes(x=.data[[violinplot_general$x()]], #x axis
                 y=Trophic.Level, #must be numeric
                 fill=.data[[violinplot_general$x()]]
                 )
         ) + 
    geom_violin(na.rm=TRUE,alpha=0.2) + 
    geom_boxplot(width=0.25,alpha=0.8) + 
    stat_summary(fun="median",
                 #fun=get_box_stats,
                 geom="label",
                 #geom="text",
                 aes(label = after_stat(round(y, 2)),
                     y=max(Trophic.Level,na.rm=TRUE)
                 ),
                 #hjust = 0.5, 
                 #vjust = -1,
                 fill="white"
                 )

}) %>% bindEvent(ps.norm(),
                 violinplot_general$x(),
                 violinplot_general$y(),
                 violinplot_general$facet()
)

violinplot <- eventReactive(input$go_violin, {
  print("Violin: assembling plot")
  violin_base() + violinplot_general$facet() + violinplot_pretty() + violinplot_size$aspectRatio() 
}) 

observeEvent(input$go_violin, {
  print("Violin: plotting")
  output$violin_plot <- renderPlot({violinplot()})
  
  tryCatch(
  ggsave(filename="violinplot.png", 
         plot=violinplot(),
         path=tempdir(),
         height=violinplot_size$height(),width=violinplot_size$width(),
         scale=2
         ),
  error=function(e){return(NA)}
  )
})

observe({
  showModal(modalDialog(
    renderPlot(violinplot()),
    footer = downloadButton('download_violinplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Violin: popup")
}) %>% bindEvent(input$violin_click)


output$violin_plotdwnld <- output$violin_plotdwnld_popup <-  downloadHandler(
  filename = "violinplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=violinplot(),
           height=violinplot_size$height(),width=violinplot_size$width(),
           scale=2
    )
    
  }) 
