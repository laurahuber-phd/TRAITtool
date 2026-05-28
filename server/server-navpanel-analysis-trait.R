source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

############## Bar Plot Trophic Level ############## 

trophbarplot_pretty  <- plotPrettyServer(id="trophbar_pretty")
trophbarplot_size <- plotSizeServer(id="trophbar_size") 
trophbarplot_general <- plotControlsServer(id="trophbar_general",
                                        df=ps.norm.melt,
                                        #var1=rnames, #color
                                        var2=Meta_vars, #x
                                        #var3=Meta_vars, #y
                                        var4=Meta_vars, #facet
                                        #var3=reactive(ps.norm.melt() %>% 
                                                      #select(contains("roph")) %>% 
                                                      #colnames() )#, #y
                                        #var4=Meta_vars() #facet
) 

trophbar_base <- reactive({
  print("Trophic bar plot: building base plot")
  req(ps.norm())
  #req(trophbarplotplot_general$x())
  ps <- ps.norm.melt() %>% filter(Abundance>0) %>% filter(!is.na(Trophic.Val))
  
  if(!is.null(ps$Trophic.Val)){
    
    #my_vars <- function() {
   #   c(any_of(c("Trophic.Val", 
    #    ps[[trophbarplot_general$x()]],
    #    #ps[[trophbarplot_general$y()]],
    #    #ps[[trophbarplot_general$color()]],
    #   ps[[trophbarplot_general$facet()]]
    #  )))
   # }
    
    #ps.edit <- ps %>% group_by(across(my_vars()) ) %>%
    #  summarize(RelAbund=Abundance/sum(Abundance)) 
    
  #ps.edit <- ps %>% group_by(Trophic.Val,
    #across(any_of(c(Trophic.Val,
  #         .data[[trophbarplot_general$x()]],
   #        .data[[trophbarplot_general$facet()]]
   #        #)))
   # ) %>%
   # summarize(RelAbund=Abundance/sum(Abundance)) #%>% 
  #  #filter(Abundance > 0)
  
  ggplot(ps#, x=.data[[trophbarplot_general$x()]], #x axis
         #          y=.data[[trophbarplot_general$y()]], #y axis
         #          fill=Trophic.Val
  ) + 
    geom_bar(aes(x=.data[[trophbarplot_general$x()]], #x axis
                 fill=Trophic.Val)
             ) + 
    coord_flip()
  }
  
}) %>% bindEvent(ps.norm(),
                 trophbarplot_general$x(),
                 trophbarplot_general$y(),
                 trophbarplot_general$facet()
)


trophbarplot <- eventReactive(input$go_trophbar, {
  print("Trophic bar plot: assembling plot")
  trophbar_base() + trophbarplot_general$facet() + trophbarplot_pretty() + trophbarplot_size() 
}) 

observeEvent(input$go_trophbar, {
  print("Trophic bar plot: plotting")
  output$trophbar_plot <- renderPlot({trophbarplot()})
})

observe({
  showModal(modalDialog(
    renderPlot(trophbarplot()),
    footer = downloadButton('download_trophbarplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Trophic bar plot: popup")
}) %>% bindEvent(input$trophbar_click)


output$trophbar_plotdwnld <- output$trophbar_plotdwnld_popup <-  downloadHandler(
  filename = "trophbarplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=trophbarplot()
    )
    
  }) 


############## Tline ############## 

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
