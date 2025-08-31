source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

violinplot_pretty  <- plotPrettyServer(id="violin_pretty")
violinplot_size <- plotSizeServer(id="violin_size") 
violinplot_general <- plotControlsServer(id="violin_gen",
                                        df=ps.norm.melt(),
                                        var1=rnames(), #color
                                        var2=Meta_vars(), #x
                                        #var3=Meta_vars(), #y
                                        var4=Meta_vars(), #facet
                                        #var1=Meta_vars(), #color
                                        #var2=Meta_vars(), #x
                                        var3=(ps.norm.melt() %>% select(contains("roph")) %>% colnames() )#, #y
                                        #var4=Meta_vars() #facet
) 

violin_base <- reactive({
  req(ps.norm())
  #req(violinplot_general$x())
  ps <- ps.norm.melt()
  
  ps.edit <- ps %>% mutate(Trophic.Level = (as.numeric(.data[[violinplot_general$y()]]))) %>% filter(Abundance >0)
  
  ggplot(ps.edit, #x=.data[[violinplot_general$x()]], #x axis
            #          y=.data[[violinplot_general$y()]], #must be numeric
            #          fill=.data[[violinplot_general$x()]]
                       aes(x=.data[[violinplot_general$x()]], #x axis
                 y=Trophic.Level, #must be numeric
                 fill=.data[[violinplot_general$x()]]
                 )
         ) + 
    geom_violin(na.rm=TRUE) + geom_boxplot(width=0.1)

}) %>% bindEvent(ps.norm(),
                 #ps.norm.melt(),
                 #ps.sub.melt(),
                 #violinplot_general$color(),
                 violinplot_general$x(),
                 violinplot_general$y(),
                 violinplot_general$facet()
)

violinplot <- eventReactive(input$go_violin, {
  violin_base() + violinplot_general$facet() + violinplot_pretty() + violinplot_size() 
}) 

observeEvent(input$go_violin, {
  output$violin_plot <- renderPlot({violinplot()})
})

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
