source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

############## Violin plot ##############  

tviolinplot_pretty  <- plotPrettyServer(id="tviolin_pretty")
tviolinplot_size <- plotSizeServer(id="tviolin_size") 
tviolinplot_general <- plotControlsServer(id="tviolin_general",
                                         df=ps.norm.melt,
                                         #var1=rnames, #color
                                         var2=Meta_vars, #x
                                         #var3=Meta_vars(), #y
                                         var4=Meta_vars, #facet
                                         #var1=Meta_vars(), #color
                                         #var2=Meta_vars(), #x
                                         var3=reactive(ps.norm.melt() %>% select(contains("emp")) %>% colnames() )#, #y
                                         #var4=Meta_vars() #facet
) 

tviolinplot_base <- reactive({
  print("Violin: building base plot")
  req(ps.norm())
  #req(tviolinplot_general$x())
  ps <- ps.norm.melt()

  my_vars <- function() {
    any_of(c(tviolinplot_general$x(),
               tviolinplot_general$facet()
               )
             )
      
  }
  
  ps.edit <- ps #%>% group_by(across(unlist(my_vars())) ) %>% 
    #summarize(Tempprefmean=mean(Tempprefmean),
    #          Tempprefmin=min(Tempprefmin),
    #          Tempprefmax=max(Tempprefmax)) %>%
    #arrange()
  
  baseplot <- ggplot(ps.edit,
                     aes(x=.data[[tviolinplot_general$x()]], #x axis
                         y=.data[[tviolinplot_general$y()]], #y axis #Tempprefmean,
                         fill=.data[[tviolinplot_general$x()]]
                     )
  ) + 
    geom_violin(na.rm=TRUE) + geom_boxplot(width=0.1) + 
    stat_summary(fun="median",
                 #fun=get_box_stats,
                 geom="label",
                 #geom="text",
                 aes(label = after_stat(round(y, 2))
                 ),
                 #hjust = 0.5, 
                 #vjust = -1,
                 fill="white"
    )
  
}) %>% bindEvent(ps.norm(),
                 tviolinplot_general$x(),
                 tviolinplot_general$y(),
                 tviolinplot_general$facet()
)

tviolinplot <- eventReactive(input$go_tviolin, {
  print("Violin: assembling plot")
  tviolinplot_base() + tviolinplot_general$facet() + tviolinplot_pretty() + tviolinplot_size$aspectRatio() 
}) 

observeEvent(input$go_tviolin, {
  print("Violin: plotting")
  output$tviolin_plot <- renderPlot({tviolinplot()})
  
  tryCatch(
    ggsave(filename="tviolinplot.png", 
           plot=tviolinplot(),
           path=tempdir(),
           height=tviolin_size$height(),width=tviolin_size$width(),
           scale=2
    ),
    error=function(e){return(NA)}
  )
})

observe({
  showModal(modalDialog(
    renderPlot(tviolinplot()),
    footer = downloadButton('download_tviolinplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Violin: popup")
}) %>% bindEvent(input$tviolin_click)


output$tviolin_plotdwnld <- output$tviolin_plotdwnld_popup <-  downloadHandler(
  filename = "tviolinplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=tviolinplot(),
           height=tviolinplot_size$height(),width=tviolinplot_size$width(),
           scale=2
    )
    
  }) 

############## Lollipop plot ############## 
observe({ 
  updatePickerInput(session,
                    "choose_tlolli_yaxis", 
                    choices=c(rnames()),
                    selected=""
  )
  #updatePickerInput(session,
  #                  "choose_tlolli_xaxis", 
  #                  choices=c(as.list(ps.norm.melt() %>% select(where(is.numeric)) %>% colnames() %>% sort())),
  #                  selected=""
 # )
  updatePickerInput(session,
                    "choose_tlolli_facet",
                    choices=c(Meta_vars()),
                    selected=""
  )
}) %>% bindEvent(ps.norm())

observe({
  req(ps.norm())
  req(input$choose_tlolli_yaxis)
  
  ps <- ps.norm.melt()
  lolvar <- ps[[{input$choose_tlolli_yaxis}]] %>% unique() %>% sort()
  updateCheckboxGroupInput(session,
                           "tlolli_samples",
                           choices=as.list(lolvar))
  print("Temperature lolli plot: List of group members updated")
  
})

tlolliplot_pretty  <- plotPrettyServer(id="tlolli_pretty")
tlolliplot_size <- plotSizeServer(id="tlolli_size") 
tlolliplot <- eventReactive(input$go_tlolli, {
  req(ps.norm())
  print("Temperature lolli plot: building base plot")
  ps <- ps.norm.melt() #filter(Abundance>0) %>% filter(!is.na(Trophic.val))
  
  if(!is.null(ps$Trophic.val)){
    
    my_vars <- function() {
      c(any_of(c(#"Trophic.val",
        input$choose_tlolli_facet,
        input$choose_tlolli_yaxis
      )
      ))
    }
    
    ps.edit <- ps %>% group_by(across(my_vars()) ) %>% 
      summarize(Tempprefmean=mean(Tempprefmean),
                Tempprefmin=min(Tempprefmin),
                Tempprefmax=max(Tempprefmax)) %>%
      arrange()
    
    baseplot <- ggplot(ps.edit,
                       aes(x=.data[[input$choose_tlolli_yaxis]],
                           y=Tempprefmean)
                       )
    
  if(!is.na(input$temprange_max) | !is.na(input$temprange_min)) {
    baseplot <- baseplot + 
      annotate("rect", 
               ymin = input$temprange_min, ymax = input$temprange_max, 
               xmin = -Inf, xmax = Inf,
               fill = "lightblue", alpha = 0.4) #+ 
      #labs(caption="()")
  }
    
     baseplot <- baseplot +
      geom_segment(
        aes(x=ps.edit[[input$choose_tlolli_yaxis]], xend=ps.edit[[input$choose_tlolli_yaxis]], 
            y=Tempprefmin, yend=Tempprefmax,
            color=ps.edit[[input$choose_tlolli_yaxis]] %in% input$tlolli_samples#,
            #size=ps.edit[[input$choose_tlolli_yaxis]] %in% input$tlolli_samples
        )) + 
      geom_point(
        aes(x=ps.edit[[input$choose_tlolli_yaxis]], y=Tempprefmean,
            color=ps.edit[[input$choose_tlolli_yaxis]] %in% input$tlolli_samples,
            size=ps.edit[[input$choose_tlolli_yaxis]] %in% input$tlolli_samples
        )) + 
      scale_color_manual(values=c("TRUE"="orange","FALSE"="grey"),name=NULL,labels=NULL) + 
      scale_size_manual(values=c("TRUE"=3,"FALSE"=1),name=NULL,labels=NULL) +
      coord_flip()
    
    if(!is.null(input$choose_tlolli_facet) & length(input$choose_tlolli_facet) < 2) {
      finalplot <- baseplot + facet_wrap(~.data[[input$choose_tlolli_facet]],scales="free") 
    } else if(!is.null(input$choose_tlolli_facet) & length(input$choose_tlolli_facet) == 2) {
      finalplot <- baseplot + facet_grid(rows=vars(.data[[input$choose_tlolli_facet[1]]]),
                                         cols= vars(.data[[input$choose_tlolli_facet[2]]]),
                                         scales="free") 
    } else {finalplot <- baseplot} 
    
    finalplot + tlolliplot_pretty() + tlolliplot_size$aspectRatio()
  }
})

observeEvent(input$go_tlolli, {
  print("Temperature lolli plot: plotting")
  output$tlolli_plot <- renderPlot({tlolliplot()})
  
  tryCatch(
    ggsave(filename="tlolliplot.png", 
           path=tempdir(),
           plot=tlolliplot(),
           height=tlolliplot_size$height(),width=tlolliplot_size$width(),
           scale=2
    ),
    error=function(e){return(NA)}
  )
})

observe({
  showModal(modalDialog(
    renderPlot(tlolliplot()),
    footer = downloadButton('download_tlolliplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Trophic bar plot: popup")
}) %>% bindEvent(input$tlolli_click)

output$tlolli_plotdwnld <- output$tlolli_plotdwnld_popup <-  downloadHandler(
  filename = "tlolliplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=tlolliplot(),
           height=tlolliplot_size$height(),width=tlolliplot_size$width(),
           scale=2
    )
    
  }) 