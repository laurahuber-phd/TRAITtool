source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)


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