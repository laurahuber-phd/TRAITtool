source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

############## Bar Plot Trophic Level ############## 
observe({ 
  updatePickerInput(session,
                    "choose_trophbar_axis", 
                    choices=c(Meta_vars()),
                    selected=Meta_vars()[[1]]
  )
  updatePickerInput(session,
                    "choose_trophbar_facet",
                    choices=c(Meta_vars()),
                    selected=""
  )
}) %>% bindEvent(ps.norm())

trophbarplot_pretty  <- plotPrettyServer(id="trophbar_pretty")
trophbarplot_size <- plotSizeServer(id="trophbar_size") 


trophbarplot <- eventReactive(input$go_trophbar, { #assign a variable that runs when plot button is pushed
  runif(input$go_trophbar)
  req(ps.norm())
  print("Trophic bar plot: building base plot")
  
  ps <- ps.norm.melt() 
  if(!is.null(ps$Trophic.Val)){
    
  ps <- ps.norm.melt() %>% filter(Abundance>0) %>% filter(!is.na(Trophic.Val))
  
  my_vars <- function() {
       c(any_of(c("Trophic.Val", 
                  input$choose_trophbar_axis, 
                  input$choose_trophbar_facet
                  )
                ))
     }
  
  ps.edit <- ps %>% group_by(across(my_vars()) ) #%>% summarize(RelAbund=Abundance/sum(Abundance)) 
  
  baseplot <- ggplot(ps.edit) + 
    geom_bar(aes(x=.data[[input$choose_trophbar_axis]], #x axis
                 y=Abundance, #y=RelAbund,
                 fill=Trophic.Val), stat="identity", position = "fill"
    ) +  
    scale_y_continuous(labels=function(x) paste0(x * 100, "%")) + 
    labs(ylab="Relative Abundance",title="Relative abundance of each trophic group")
    coord_flip() 
  
  if(!is.null(input$choose_trophbar_facet) & length(input$choose_trophbar_facet) < 2) {
    finalplot <- baseplot + facet_wrap(~input$choose_trophbar_facet,scales="free") 
  } else if(!is.null(input$choose_trophbar_facet) & length(input$choose_trophbar_facet) == 2) {
    finalplot <- baseplot + facet_grid(rows=vars(.data[[input$choose_trophbar_facet[1]]]),
                                       cols= vars(.data[[input$choose_trophbar_facet[2]]]),
                                       scales="free") 
  } else {finalplot <- baseplot}
  
  #baseplot <- plot_ordination(physeq=ps.norm(),
  #                            ordination=distreact(),
  #                            type=input$ord_vis,
  #                            color=input$choose_ord_color,
  #                            shape=input$choose_ord_shape ) + 
  #  geom_point(size=input$ord_plotpoint) 
  
  #if(input$choose_ord_ellipse=="Color") {
  #  finalplot <- baseplot +  stat_ellipse(aes(group=.data[[input$choose_ord_color]],
  #                                            #color=.data[[input$choose_ord_ellipse]],
  #                                            #fill=.data[[input$choose_ord_ellipse]],
  #                                            #linetype=.data[[input$choose_ord_ellipse]]
  #  ), 
  #  show.legend=FALSE, #since the ellipse grouping is the same as color, we don't need a legend
  #  #geom='polygon', #requires a fill and alpha aes
  #  type='norm', #assumes a multi-variate normal distribution
  #  level=0.95, #confidence level
  #  ) + labs(caption="Ellipses drawn with a normal distribution at 95% confidence interval")
  #} else if (input$choose_ord_ellipse=="Shape") {
  #  finalplot <- baseplot +  stat_ellipse(aes(group=.data[[input$choose_ord_shape]]), 
  #                                        show.legend=FALSE, #since the ellipse grouping is the same as color, we don't need a legend
  #                                        level=0.95, #confidence level
  #  ) + labs(caption="Ellipses drawn with a normal distribution at 95% confidence interval")
  #} else {
  #  finalplot <- baseplot 
  #}
  
  finalplot + trophbarplot_pretty() + trophbarplot_size() 
    #theme(text = element_text(size=input$ord_plottext),
    #      aspect.ratio=(input$ord_plotheight/input$ord_plotwidth)) + 
    #labs(subtitle=paste0("Using ",input$ord_dist," distance matrix"),
    #     title=paste0(input$ord_meth," Ordination Plot")
    #)
  }
  
})




















#trophbar_base <- reactive({
#  print("Trophic bar plot: building base plot")
#  req(ps.norm())
  #req(trophbarplotplot_general$x())
#  ps <- ps.norm.melt() %>% filter(Abundance>0) %>% filter(!is.na(Trophic.Val))
  
#  if(!is.null(ps$Trophic.Val)){
    
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
  
#  ggplot(ps#, x=.data[[trophbarplot_general$x()]], #x axis
#         #          y=.data[[trophbarplot_general$y()]], #y axis
#         #          fill=Trophic.Val
#  ) + 
#    geom_bar(aes(x=.data[[trophbarplot_general$x()]], #x axis
#                 fill=Trophic.Val)
#             ) + 
#    coord_flip()
#  }
#  
#}) %>% bindEvent(ps.norm(),
#                 trophbarplot_general$x(),
#                 trophbarplot_general$y(),
#                 trophbarplot_general$facet()
#)

#trophbarplot <- eventReactive(input$go_trophbar, {
#  print("Trophic bar plot: assembling plot")
#  trophbar_base() + #trophbarplot_general$facet() + 
#    trophbarplot_pretty() + trophbarplot_size() 
#}) 

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
