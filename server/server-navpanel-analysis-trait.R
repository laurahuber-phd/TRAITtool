source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

############## Violin Plot Trophic Level ############## 
violinplot_pretty  <- plotPrettyServer(id="violin_pretty")
violinplot_size <- plotSizeServer(id="violin_size") 
violinplot_general <- plotControlsServer(id="violin_gen",
                                         df=ps.norm.melt,
                                         #var1=rnames, #color
                                         var2=Meta_vars, #x
                                         #var3=Meta_vars(), #y
                                         var4=Meta_vars, #facet
                                         #var1=Meta_vars(), #color
                                         #var2=Meta_vars(), #x
                                         var3=reactive(ps.norm.melt() %>% select(contains("roph")) %>% colnames() )#, #y
                                         #var4=Meta_vars() #facet
) 

violin_base <- reactive({
  req(ps.norm())
  req(violinplot_general$x())
  req(violinplot_general$y())
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
  
  ggplot(ps.edit, 
         aes(x=.data[[violinplot_general$x()]], #x axis
             y=Trophic.Level, #must be numeric
             fill=.data[[violinplot_general$x()]]
         )
  ) + 
    geom_violin(na.rm=TRUE,alpha=0.2) + 
    geom_boxplot(width=0.25,alpha=0.6) + 
    stat_summary(fun="median",
                 geom="label",
                 aes(label = after_stat(round(y, 2))
                 ),
                 fill="white",
                 alpha=0.7,
                 position=position_nudge(y=0.3)
    )
  
}) 

violinplot <- reactive({
  print("Violin: assembling plot")
  req(input$go_violin|trigger_plots())
  
  violin_base() + 
    violinplot_general$facet() + 
    violinplot_pretty() + 
    violinplot_size$aspectRatio() 
}) 

output$violin_click_plot <- renderPlot({
  #req(violinplot())
  violinplot()
  })
output$violin_s_plot <- renderPlot({
  #req(violinplot())
  violinplot()
})
output$violin_a_plot <- renderPlot({
  #req(violinplot())
  violinplot()
})
  
observe({
  req(violinplot())
  print("Saving violinplot()")
  
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

observeEvent(input$violin_click,{
  req(violinplot())
  print("MTI violin: popup")
  
  showModal(modalDialog(
    withSpinner(plotOutput('violin_click_plot')),
    footer = downloadButton('violin_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) 

output$violin_plotdwnld <- downloadHandler(
  filename = "violinplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=violinplot(),
           height=violinplot_size$height(),width=violinplot_size$width(),
           scale=2
    )
  }) 
output$violin_plotdwnld_popup <- downloadHandler(
  filename = "violinplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=violinplot(),
           height=violinplot_size$height(),width=violinplot_size$width(),
           scale=2
    )
  }) 

############## Bar Plot Trophic Level ############## 
observe({ 
  updatePickerInput(session,
                    "choose_trophbar_facet",
                    choices=c(Meta_vars())
  )
}) %>% bindEvent(ps.norm(),Meta_vars())

trophbarplot_pretty  <- plotPrettyServer(id="trophbar_pretty")
trophbarplot_size <- plotSizeServer(id="trophbar_size") 
trophbarplot_base <- reactive({
  req(ps.norm())
  print("Trophic bar plot: building base plot")
  
  ps <- ps.norm.melt() 
  
  if(!is.null(ps$Trophic.val)){
    
    ps <- ps.norm.melt() %>% 
      filter(Abundance>0) %>% #filter(!is.na(Trophic.val))
      mutate(Trophic.val=case_when(is.na(Trophic.val) ~ "Not assigned",
                                   .default=Trophic.val)) %>% 
      mutate(Trophic.val=factor(Trophic.val,levels=unique(Trophic.val))) %>% 
      mutate(Trophic.val=fct_relevel(Trophic.val,"Not assigned",after = Inf))
    
    my_vars <- c("Trophic.val", 
                 #input$choose_trophbar_axis, 
                 input$choose_trophbar_facet
    )
    print(c("arrange dataset by",my_vars))
    if(!is.null(input$choose_trophbar_facet)) {
    ps.edit <- ps %>% 
      group_by(across(any_of(my_vars) )) %>% 
      summarize(Abundance=sum(Abundance),.groups="keep") %>% 
      arrange(input$choose_trophbar_facet[[1]]) %>% 
      ungroup() %>% 
      group_by(across(any_of(input$choose_trophbar_facet))) %>% 
      mutate(Perc=round(Abundance/sum(Abundance),
                        digits=4)*100) %>% 
      mutate(csum=rev(cumsum(rev(Perc))),
             ypos=csum-0.5*Perc,
             repos=ifelse(Perc<=10,ypos,NA),
             ypos=ifelse(Perc>10,ypos,NA)
      )
    } else {
      ps.edit <- ps %>% 
        group_by(across(any_of(my_vars) )) %>% 
        summarize(Abundance=sum(Abundance),.groups="keep") %>% 
        ungroup() %>% 
        mutate(Perc=round(Abundance/sum(Abundance),
                          digits=4)*100) %>% 
        mutate(csum=rev(cumsum(rev(Perc))),
               ypos=csum-0.5*Perc,
               repos=ifelse(Perc<=10,ypos,NA),
               ypos=ifelse(Perc>10,ypos,NA)
        )
    }
    
    print(group_keys(ps.edit))
    baseplot <- ggplot(ps.edit, 
                       aes(x="",#x=.data[[input$choose_trophbar_axis]], #x axis
                           y=Perc,#y=Abundance, #y=RelAbund,
                           fill=Trophic.val,
                           color=Trophic.val
                       )
    ) + 
      geom_bar(stat="identity"#, position = "fill"
                ) +  
      #scale_y_continuous(labels=function(x) paste0(x * 100, "%")) + 
      labs(y="Relative Abundance",
           title="Relative abundance of each trophic group") +
      #coord_flip() 
      geom_text(aes(y=ypos,label=Perc),color="black") +
      ggrepel::geom_label_repel(aes(y=repos,label=paste0(Perc,"%")),
                                size = 4.5, nudge_x = 1, show.legend = FALSE,color="black") +
      coord_polar("y",start=0) + 
      theme_void() #+ # remove background, grid, numeric labels
    #geom_text(aes(label = Trophic.val #round(value,2)
    #              ),
    #          color="black",
    #          position = position_stack(vjust = 0.5))
    
    if(!is.null(input$choose_trophbar_facet) & length(input$choose_trophbar_facet) < 2) {
      print("one faceting group selected")
      finalplot <- baseplot + facet_wrap(~.data[[input$choose_trophbar_facet]],scales="free") 
    } else if(!is.null(input$choose_trophbar_facet) & length(input$choose_trophbar_facet) == 2) {
      print("two faceting groups selected")
      finalplot <- baseplot + facet_grid(rows=vars(.data[[input$choose_trophbar_facet[1]]]),
                                         cols= vars(.data[[input$choose_trophbar_facet[2]]]),
                                         scales="free") 
    } else {
      print("no faceting groups selected")
      finalplot <- baseplot
    }
    finalplot
}
}) 

trophbarplot <- reactive({
  print("Trophic bar plot: plotting")
  req(input$go_trophbar | trigger_plots())
  
  base <- trophbarplot_base()
  pretty <- trophbarplot_pretty()
  size <- trophbarplot_size$aspectRatio()
  
  base + pretty + theme_void() + size
  }) 

output$trophbar_click_plot <- renderPlot({
  #req(trophbarplot())
  trophbarplot() 
  })
output$trophbar_s_plot <- renderPlot({
  #req(trophbarplot())
  trophbarplot() 
  })
output$trophbar_plot <- renderPlot({
  #req(trophbarplot())
  trophbarplot() 
  })

observe({
  req(trophbarplot())
  print("Saving trophbarplot()")
  
  tryCatch(
  ggsave(filename="trophbarplot.png", 
         path=tempdir(),
         plot=trophbarplot(),
         height=trophbarplot_size$height(),width=trophbarplot_size$width(),
         scale=2
         ),
  error=function(e){return(NA)}
)
})

observeEvent(input$trophbar_click,{
  req(trophbarplot())
  print("Feeding guild pie chart: popup")
  
  showModal(modalDialog(
    withSpinner(plotOutput('trophbar_click_plot')),
    footer = downloadButton('trophbar_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) 

output$trophbar_plotdwnld <- downloadHandler(
  filename = "trophbarplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=trophbarplot(),
           height=trophbarplot_size$height(),width=trophbarplot_size$width(),
           scale=2
    )
  }) 
output$trophbar_plotdwnld_popup <-  downloadHandler(
  filename = "trophbarplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=trophbarplot(),
           height=trophbarplot_size$height(),width=trophbarplot_size$width(),
           scale=2
    )
  }) 
############## Temperature violin plot ##############  
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
  req(ps.norm())
  print("Violin: building base plot")
  ps <- ps.norm.melt()
  
  my_vars <- function() {
    any_of(c(tviolinplot_general$x(),
             tviolinplot_general$facet()
    )
    )
  }
  
  ps.edit <- ps %>% 
    group_by(.data[[tviolinplot_general$x()]]) %>% 
    mutate(TempRange=max(Tempprefmax,na.rm=TRUE)-min(Tempprefmin,na.rm=TRUE)) %>%
    rowwise() %>% 
    mutate(TempIndex=Tempprefmean/TempRange) 
  
  baseplot <- ggplot(ps.edit,
                     aes(x=.data[[tviolinplot_general$x()]], #x axis
                         y=TempIndex, #y axis #Tempprefmean,
                         fill=.data[[tviolinplot_general$x()]]
                     )
  ) + 
    geom_violin(na.rm=TRUE,alpha=0.2) + 
    geom_boxplot(width=0.25,alpha=0.6) + 
    stat_summary(fun="median",
                 geom="label",
                 aes(label = after_stat(round(y, 2))
                 ),
                 fill="white",
                 alpha=0.7,
                 position=position_nudge(y=0.3)
    )
}) 

tviolinplot <- reactive({ #reactive updates after the observer
  req(input$go_tviolin | trigger_plots())
  print("Temperature index violin: assembling plot")
  
  tviolinplot_base() + 
    tviolinplot_general$facet() + 
    tviolinplot_pretty() +
    tviolinplot_size$aspectRatio()
}) 

output$tviolin_click_plot <- renderPlot({ #plot is rendered after observer
  req(tviolinplot())
  tviolinplot() 
}) 
output$tviolin_s_plot <- renderPlot({
  req(tviolinplot())
  tviolinplot() 
})
output$tviolin_plot <- renderPlot({
  req(tviolinplot())
  tviolinplot() 
})

observe({
  req(tviolinplot())
  print("Saving tviolinplot()")
  
  tryCatch(
    ggsave(filename="tviolinplot.png", 
           plot=tviolinplot(),
           path=tempdir(),
           height=tviolinplot_size$height(),width=tviolinplot_size$width(),
           scale=2
    ),
    error=function(e){return(NA)}
  )
})

observeEvent(input$tviolin_click, {
  req(tviolinplot())
  print("Temperature index violin: popup")
  
  showModal(modalDialog(
    withSpinner(plotOutput('tviolin_click_plot')),
    footer = downloadButton('tviolin_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) 
output$tviolin_plotdwnld <- downloadHandler(
  filename = "tviolinplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=tviolinplot(),
           height=tviolinplot_size$height(),width=tviolinplot_size$width(),
           scale=2
    )
  })
output$tviolin_plotdwnld_popup <- downloadHandler(
  filename = "tviolinplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=tviolinplot(),
           height=tviolinplot_size$height(),width=tviolinplot_size$width(),
           scale=2
    )
  })

# plot to choose what temperature value   
tviolinmeanplot_pretty  <- plotPrettyServer(id="tviolinmean_pretty")
tviolinmeanplot_size <- plotSizeServer(id="tviolinmean_size") 
tviolinmeanplot_general <- plotControlsServer(id="tviolinmean_general",
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

tviolinmeanplot_base <- reactive({
  req(ps.norm())
  req(tviolinmeanplot_general$x())
  req(tviolinmeanplot_general$y())
  print("Violin: building base plot")
  ps <- ps.norm.melt()
  
  my_vars <- function() {
    any_of(c(tviolinmeanplot_general$x(),
             tviolinmeanplot_general$facet()
    )
    )
    
  }
  
  ps.edit <- ps 
  
  baseplot <- ggplot(ps.edit,
                     aes(x=.data[[tviolinmeanplot_general$x()]], #x axis
                         y=.data[[tviolinmeanplot_general$y()]], #y axis #Tempprefmean,
                         fill=.data[[tviolinmeanplot_general$x()]]
                     )
  ) + 
    geom_violin(na.rm=TRUE,alpha=0.2) + 
    geom_boxplot(width=0.25,alpha=0.6) + 
    stat_summary(fun="median",
                 geom="label",
                 aes(label = after_stat(round(y, 2))
                 ),
                 fill="white",
                 alpha=0.7,
                 position=position_nudge(y=0.3)
    )
  
}) 

tviolinmeanplot <- reactive({
  req(input$go_tviolinmean|trigger_plots())
  print("Temperature violin: assembling plot")
  
  tviolinmeanplot_base() + 
    tviolinmeanplot_general$facet() + 
    tviolinmeanplot_pretty() + 
    tviolinmeanplot_size$aspectRatio() 
}) 

output$tviolinmean_click_plot <- renderPlot({
  req(tviolinmeanplot())
  tviolinmeanplot() 
}) 
output$tviolinmean_s_plot <- renderPlot({
  req(tviolinmeanplot())
  tviolinmeanplot() 
})
output$tviolinmean_plot <- renderPlot({
  req(tviolinmeanplot())
  tviolinmeanplot() 
})

observe({
  req(tviolinmeanplot())
  print("Saving tviolinmeanplot()")
  
  tryCatch(
    ggsave(filename="tviolinmeanplot.png", 
           plot=tviolinmeanplot(),
           path=tempdir(),
           height=tviolinmeanplot_size$height(),width=tviolinmeanplot_size$width(),
           scale=2
    ),
    error=function(e){return(NA)}
  )
})

observeEvent(input$tviolinmean_click, {
  req(tviolinmeanplot())
  print("Temperature violin: popup")
  
  showModal(modalDialog(
    withSpinner(plotOutput('tviolinmean_click_plot')),
    footer = downloadButton('tviolinmean_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) 

output$tviolinmean_plotdwnld <- downloadHandler(
  filename = "tviolinmeanplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=tviolinmeanplot(),
           height=tviolinmeanplot_size$height(),width=tviolinmeanplot_size$width(),
           scale=2
    )
  }) 
output$tviolinmean_plotdwnld_popup <- downloadHandler(
  filename = "tviolinmeanplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=tviolinmeanplot(),
           height=tviolinmeanplot_size$height(),width=tviolinmeanplot_size$width(),
           scale=2
    )
  }) 

############## Temperature lollipop plot ############## 
observe({ 
  updatePickerInput(session,
                    "choose_tlolli_yaxis", 
                    choices=c(rnames())#,
                    #selected=""
  )
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

tlolliplot_base <- reactive({
  req(ps.norm())
  print("Temperature lolli plot: building base plot")
  ps <- ps.norm.melt() 
  
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
        aes(x=.data[[input$choose_tlolli_yaxis]], xend=.data[[input$choose_tlolli_yaxis]], 
            y=Tempprefmin, yend=Tempprefmax,
            color=.data[[input$choose_tlolli_yaxis]] %in% input$tlolli_samples
        )) + 
      geom_point(
        aes(x=.data[[input$choose_tlolli_yaxis]], y=Tempprefmean,
            color=.data[[input$choose_tlolli_yaxis]] %in% input$tlolli_samples,
            size=.data[[input$choose_tlolli_yaxis]] %in% input$tlolli_samples
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
    
    finalplot 
  }
})

tlolliplot <- reactive({
  req(input$go_tlolli | trigger_plots())
  
  tlolliplot_base() + tlolliplot_pretty() + tlolliplot_size$aspectRatio()
})

output$tlolli_click_plot <- renderPlot({
  req(tlolliplot())
  tlolliplot()
})
output$tlolli_s_plot <- renderPlot({
  req(tlolliplot())
  tlolliplot()
})
output$tlolli_plot <-renderPlot({
  req(tlolliplot())
  tlolliplot()
})

observe({
  req(tlolliplot())
  print("Saving tlolliplot()")
  
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

observeEvent(input$tlolli_click, {
  req(tlolliplot())
  print("Temperature lollipop plot: popup")
  
  showModal(modalDialog(
    withSpinner(plotOutput('tlolli_click_plot')),
    footer = downloadButton('tlolli_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) 

output$tlolli_plotdwnld <- downloadHandler(
  filename = "tlolliplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=tlolliplot(),
           height=tlolliplot_size$height(),width=tlolliplot_size$width(),
           scale=2
    )
  }) 
output$tlolli_plotdwnld_popup <- downloadHandler(
  filename = "tlolliplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=tlolliplot(),
           height=tlolliplot_size$height(),width=tlolliplot_size$width(),
           scale=2
    )
  }) 

############## Bar Plot Body Size ############## 
observe({ 
  updatePickerInput(session,
                    "choose_sizebar_axis", 
                    choices=c(ps.norm.melt() %>% select(contains("ength")) %>% colnames())#,
                    #selected=""
  )
  updatePickerInput(session,
                    "choose_sizebar_facet",
                    choices=c(Meta_vars())#,
                    #selected=""
  )
}) %>% bindEvent(ps.norm(),Meta_vars())

sizebarplot_pretty  <- plotPrettyServer(id="sizebar_pretty")
sizebarplot_size <- plotSizeServer(id="sizebar_size") 

sizebarplot_base <- reactive({
  req(ps.norm())
  req(input$choose_sizebar_axis)
  print("Size bar plot: building base plot")

  ps <- ps.norm.melt() %>% 
    filter(Abundance>0) %>%
    mutate(BodySize=case_when(.data[[input$choose_sizebar_axis]] > 100 ~ "Giant (>100 cm)",
                              .data[[input$choose_sizebar_axis]] >= 20 ~ "Large (>20 cm)",
                              .data[[input$choose_sizebar_axis]] < 20 & .data[[input$choose_sizebar_axis]] >= 10 ~ " Medium (10-19 cm)",
                              .data[[input$choose_sizebar_axis]] < 10 & .data[[input$choose_sizebar_axis]] >= 5 ~ " Small (5-10 cm)",
                              .data[[input$choose_sizebar_axis]] <5 ~ "Very small (<5 cm)",
                              .default="Not assigned")) %>% 
    mutate(BodySize=factor(BodySize,levels=unique(BodySize))) %>% 
    mutate(BodySize=fct_relevel(BodySize,"Not assigned",after=Inf))
  
  my_vars <- c("BodySize",
               #input$choose_sizebar_axis, 
               input$choose_sizebar_facet
               )
  print(c("arrange dataset by",my_vars))

  if(!is.null(input$choose_sizebar_facet)) {
    ps.edit <- ps %>% 
      group_by(across(any_of(my_vars) )) %>% 
      summarize(Abundance=sum(Abundance),.groups="keep") %>% 
      arrange(input$choose_sizebar_facet[[1]]) %>% 
      ungroup() %>% 
      group_by(across(any_of(input$choose_sizebar_facet))) %>% 
      mutate(Perc=round(Abundance/sum(Abundance),
                        digits=4)*100) %>% 
      mutate(csum=rev(cumsum(rev(Perc))),
             ypos=csum-0.5*Perc,
             repos=ifelse(Perc<=10,ypos,NA),
             ypos=ifelse(Perc>10,ypos,NA)
      )
  } else {
    ps.edit <- ps %>% 
      group_by(across(any_of(my_vars) )) %>% 
      summarize(Abundance=sum(Abundance),.groups="keep") %>% 
      ungroup() %>% 
      mutate(Perc=round(Abundance/sum(Abundance),
                        digits=4)*100) %>% 
      mutate(csum=rev(cumsum(rev(Perc))),
             ypos=csum-0.5*Perc,
             repos=ifelse(Perc<=10,ypos,NA),
             ypos=ifelse(Perc>10,ypos,NA)
      )
  }

  baseplot <- ggplot(ps.edit, 
                     aes(x="",#x=.data[[input$choose_sizebar_axis]], #x axis
                         y=Perc,#y=Abundance, #y=RelAbund,
                         fill=BodySize,
                         color=BodySize
                     )
  ) + 
    geom_bar( stat="identity"#, position = "fill"
              ) +  
    #scale_y_continuous(labels=function(x) paste0(x * 100, "%")) + 
    #labs(y="Relative Abundance",
    #     title="Relative abundance of each trophic group") +
    #coord_flip() 
    geom_text(aes(y=ypos,label=Perc),color="black") +
    ggrepel::geom_label_repel(aes(y=repos,label=paste0(Perc,"%")),
                              size = 4.5, nudge_x = 1, show.legend = FALSE,color="black") +
    coord_polar("y",start=0) + 
    theme_void() #+ # remove background, grid, numeric labels
  #geom_text(aes(label = Trophic.val #round(value,2)
  #              ),
  #          color="black",
  #          position = position_stack(vjust = 0.5))
  
  if(!is.null(input$choose_sizebar_facet) & length(input$choose_sizebar_facet) < 2) {
    print("one faceting group selected")
    finalplot <- baseplot + facet_wrap(~.data[[input$choose_sizebar_facet]],scales="free") 
  } else if(!is.null(input$choose_sizebar_facet) & length(input$choose_sizebar_facet) == 2) {
    print("two faceting groups selected")
    finalplot <- baseplot + facet_grid(rows=vars(.data[[input$choose_sizebar_facet[1]]]),
                                       cols= vars(.data[[input$choose_sizebar_facet[2]]]),
                                       scales="free") 
  } else {
    print("no faceting groups selected")
    finalplot <- baseplot
    }
  finalplot
}) 

sizebarplot <- reactive({
  print("Body size: assembling plot")
  req(input$go_sizebar|trigger_plots())
  #trigger_plots()
  
  sizebarplot_base() + 
    sizebarplot_pretty() + theme_void() +
    sizebarplot_size$aspectRatio()
  }) #%>% bindEvent(input$go_sizebar,trigger_plots())

output$sizebar_click_plot <- renderPlot({
  req(sizebarplot())
  sizebarplot()
  })
outputOptions(output, "sizebar_click_plot", suspendWhenHidden = FALSE)
output$sizebar_s_plot <- renderPlot({
  req(sizebarplot())
  sizebarplot()
})
output$sizebar_plot <- renderPlot({
  req(sizebarplot())
  sizebarplot()
})

observe({
  req(sizebarplot())
  print("Saving sizebarplot()")
  
  tryCatch(
    ggsave(filename="sizebarplot.png", 
           path=tempdir(),
           plot=sizebarplot(),
           height=sizebarplot_size$height(),width=sizebarplot_size$width(),
           scale=2
    ),
    error=function(e){return(NA)}
  )
}) 

observeEvent(input$sizebar_click,{
  req(sizebarplot())
  print("Body size pie chart: popup")
  
  showModal(modalDialog(
    withSpinner(plotOutput('sizebar_click_plot')),
    footer = downloadButton('sizebar_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) 

output$sizebar_plotdwnld <- downloadHandler(
  filename = "sizebarplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=sizebarplot(),
           height=sizebarplot_size$height(),width=sizebarplot_size$width(),
           scale=2
    )
  }) 
output$sizebar_plotdwnld_popup <-  downloadHandler(
  filename = "sizebarplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=sizebarplot(),
           height=sizebarplot_size$height(),width=sizebarplot_size$width(),
           scale=2
    )
  }) 

############## Bar Plot IUCN status ############## 
observe({ 
  updatePickerInput(session,
                    "choose_iucnbar_axis", 
                    choices=c(ps.norm.melt() %>% select(contains("IUCN", ignore.case = TRUE)) %>% colnames())#,
                    #selected=""
  )
  updatePickerInput(session,
                    "choose_iucnbar_facet",
                    choices=c(Meta_vars())#,
                    #selected=""
  )
}) %>% bindEvent(ps.norm(),Meta_vars())

iucnbarplot_pretty  <- plotPrettyServer(id="iucnbar_pretty")
iucnbarplot_size <- plotSizeServer(id="iucnbar_size") 

iucnbarplot_base <- reactive({
  req(ps.norm())
  req(input$choose_iucnbar_axis)
  print("IUCN bar plot: building base plot")
  
  names <- ps.norm.melt() %>%
    select(contains(regex("IUCN_category",ignore_case = TRUE))) %>%
    colnames()
  
  req(length(names)>0)
  
  ps <- ps.norm.melt() %>% 
    rename(IUCN_Category_Status = input$choose_iucnbar_axis) %>% 
    mutate(IUCN_Category_Status = str_to_upper(IUCN_Category_Status)) %>%
    mutate(IUCN_Status = case_when(is.na(IUCN_Category_Status) ~ "Not Found in Red List",
                                   .default=IUCN_Category_Status)) %>% 
    mutate(IUCN_Status=factor(IUCN_Status,levels=unique(IUCN_Status))) %>% 
    mutate(IUCN_Status=fct_relevel(IUCN_Status,"Not Found in Red List",after = Inf))
  print(colnames(ps))
  
  my_vars <- c("IUCN_Status",
               "IUCN_Category_Status",
               #input$choose_iucnbar_axis, 
               input$choose_iucnbar_facet
  )
  print(c("arrange dataset by",my_vars))
  
  if(!is.null(input$choose_iucnbar_facet)) {
    ps.edit <- ps %>% 
      group_by(across(any_of(my_vars) )) %>% 
      summarize(Abundance=sum(Abundance),.groups="keep") %>% 
      arrange(input$choose_iucnbar_facet[[1]]) %>% 
      ungroup() %>% 
      group_by(across(any_of(input$choose_iucnbar_facet))) %>% 
      mutate(Perc=round(Abundance/sum(Abundance),
                        digits=4)*100) %>% 
      mutate(csum=rev(cumsum(rev(Perc))),
             ypos=csum-0.5*Perc,
             repos=ifelse(Perc<=10,ypos,NA),
             ypos=ifelse(Perc>10,ypos,NA)
      )
  } else {
    ps.edit <- ps %>% 
      group_by(across(any_of(my_vars) )) %>% 
      summarize(Abundance=sum(Abundance),.groups="keep") %>% 
      ungroup() %>% 
      mutate(Perc=round(Abundance/sum(Abundance),
                        digits=4)*100) %>% 
      mutate(csum=rev(cumsum(rev(Perc))),
             ypos=csum-0.5*Perc,
             repos=ifelse(Perc<=10,ypos,NA),
             ypos=ifelse(Perc>10,ypos,NA)
      )
  }
  

  baseplot <- ggplot(ps.edit, 
                     aes(x="",#x=.data[[input$choose_iucnbar_axis]], #x axis
                         y=Perc,#y=Abundance,
                         fill=IUCN_Status,#Iucn_category,
                         color=IUCN_Status#Iucn_category
                     )
  ) + 
    geom_bar( stat="identity"#, position = "fill"
              ) +  
    #scale_y_continuous(labels=function(x) paste0(x * 100, "%")) + 
    #labs(y="Relative Abundance",
    #     title="Relative abundance of each trophic group") +
    #coord_flip() 
    geom_text(aes(y=ypos,label=Perc),color="black") +
    ggrepel::geom_label_repel(aes(y=repos,label=paste0(Perc,"%")),
                              size = 4.5, nudge_x = 1, show.legend = FALSE,color="black") +
    coord_polar("y",start=0) + 
    theme_void() #+ # remove background, grid, numeric labels
  #geom_text(aes(label = Trophic.val #round(value,2)
  #              ),
  #          color="black",
  #          position = position_stack(vjust = 0.5))
  
  if(!is.null(input$choose_iucnbar_facet) & length(input$choose_iucnbar_facet) < 2) {
    print("one faceting group selected")
    finalplot <- baseplot + facet_wrap(~.data[[input$choose_iucnbar_facet]],scales="free") 
  } else if(!is.null(input$choose_iucnbar_facet) & length(input$choose_iucnbar_facet) == 2) {
    print("two faceting groups selected")
    finalplot <- baseplot + facet_grid(rows=vars(.data[[input$choose_iucnbar_facet[1]]]),
                                       cols= vars(.data[[input$choose_iucnbar_facet[2]]]),
                                       scales="free") 
  } else {
    print("no faceting groups selected")
    finalplot <- baseplot
  }
  
  finalplot
  
}) %>% bindEvent(ps.norm(),#Meta_vars(),
  #input$choose_iucnbar_axis,
  input$choose_iucnbar_facet
)

iucnbarplot <- reactive({
  req(input$go_iucnbar|trigger_plots())
  
  iucnbarplot_base() + 
    iucnbarplot_size$aspectRatio()
  }) 

observe({
  req(iucnbarplot())
  print("IUCN bar plot: saving")
  
  tryCatch(
    ggsave(filename="iucnbarplot.png", 
           path=tempdir(),
           plot=iucnbarplot(),
           height=iucnbarplot_size$height(),width=iucnbarplot_size$width(),
           scale=2
    ),
    error=function(e){return(NA)}
  )
})

output$iucnbar_plot <- renderPlot({
  req(iucnbarplot())
  iucnbarplot()
  })
output$iucnbar_s_plot <- renderPlot({
  req(iucnbarplot())
  iucnbarplot()
})

observeEvent(input$iucnbar_click,{
  req(iucnbarplot())
  print("IUCN status pie chart: popup")
  
  showModal(modalDialog(
    withSpinner(plotOutput('iucnbar_click_plot')),
    footer = downloadButton('iucnbar_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) 

output$iucnbar_plotdwnld <- downloadHandler(
  filename = "iucnbarplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=iucnbarplot(),
           height=iucnbarplot_size$height(),width=iucnbarplot_size$width(),
           scale=2
    )
  }) 
output$iucnbar_plotdwnld_popup <-  downloadHandler(
  filename = "iucnbarplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,
           plot=iucnbarplot(),
           height=iucnbarplot_size$height(),width=iucnbarplot_size$width(),
           scale=2
    )
  }) 

############## Lollipop plot ############## 
observe({ 
  updatePickerInput(session,
                    "choose_lolli_yaxis", 
                    choices=c(rnames()),
                    selected=""
  )
  updatePickerInput(session,
                    "choose_lolli_xaxis", 
                    choices=c(as.list(ps.norm.melt() %>% select(where(is.numeric)) %>% colnames() %>% sort())),
                    selected=""
  )
  updatePickerInput(session,
                    "choose_lolli_facet",
                    choices=c(Meta_vars()),
                    selected=""
  )
}) %>% bindEvent(ps.norm())

observe({
  req(ps.norm())
  req(input$choose_lolli_yaxis)
  
  ps <- ps.norm.melt()
  lolvar <- ps[[{input$choose_lolli_yaxis}]] %>% unique() %>% sort()
  updateCheckboxGroupInput(session,
                           "lolli_samples",
                           choices=as.list(lolvar))
  print("Trophic lolli plot: List of group members updated")
  
})

lolliplot_pretty  <- plotPrettyServer(id="lolli_pretty")
lolliplot_size <- plotSizeServer(id="lolli_size") 
lolliplot_base <- reactive({
  req(ps.norm())
  req(input$choose_lolli_yaxis)
  req(input$choose_lolli_xaxis)
  
  print("Trophic lolli plot: building base plot")
  ps <- ps.norm.melt() #filter(Abundance>0) %>% filter(!is.na(Trophic.val))
  
  if(!is.null(ps$Trophic.val)){
    
    my_vars <- function() {
      c(any_of(c(#"Trophic.val",
        input$choose_lolli_facet,
        input$choose_lolli_yaxis
      )
      ))
    }
    
    ps.edit <- ps %>% 
      group_by(across(my_vars()) ) %>% 
      arrange() 
    
    baseplot <- ggplot(ps.edit, aes(x=.data[[input$choose_lolli_yaxis]],
                                    y=.data[[input$choose_lolli_xaxis]])
    ) +
      geom_segment(
        aes(x=ps.edit[[input$choose_lolli_yaxis]], xend=ps.edit[[input$choose_lolli_yaxis]], 
            y=0, yend=.data[[input$choose_lolli_xaxis]],
            color=ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples
        )) + 
      geom_point(
        aes(color=ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples,
            size=ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples
        )) + 
      scale_color_manual(values=c("TRUE"="orange","FALSE"="grey"),name=NULL,labels=NULL) + 
      scale_size_manual(values=c("TRUE"=3,"FALSE"=1),name=NULL,labels=NULL) +
      coord_flip()
    
    if(!is.null(input$choose_lolli_facet) & length(input$choose_lolli_facet) < 2) {
      finalplot <- baseplot + facet_wrap(~.data[[input$choose_lolli_facet]],scales="free") 
    } else if(!is.null(input$choose_lolli_facet) & length(input$choose_lolli_facet) == 2) {
      finalplot <- baseplot + facet_grid(rows=vars(.data[[input$choose_lolli_facet[1]]]),
                                         cols= vars(.data[[input$choose_lolli_facet[2]]]),
                                         scales="free") 
    } else {finalplot <- baseplot} 
    
    finalplot
  }
})

lolliplot <- reactive({
  req(input$go_lolli|trigger_plots())
  
  lolliplot_base() + 
    lolliplot_pretty() + 
    lolliplot_size$aspectRatio()
}) 
output$lolli_click_plot <- renderPlot({
  req(lolliplot())
  lolliplot()
})
output$lolli_plot <- renderPlot({
  req(lolliplot())
  lolliplot()
})

observe({
  req(lolliplot())
  print("Saving lolliplot()")
  
  tryCatch(
    ggsave(filename="lolliplot.png", 
           path=tempdir(),
           plot=lolliplot(),
           height=lolliplot_size$height(),width=lolliplot_size$width(),
           scale=2
    ),
    error=function(e){return(NA)}
  )
})

observeEvent(input$lolli_click, {
  req(lolliplot())
  print("MTI lollipop plot: popup")
  
  showModal(modalDialog(
    withSpinner(plotOutput('lolli_click_plot')),
    footer = downloadButton('lolli_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) 

output$lolli_plotdwnld <- downloadHandler(
  filename = "lolliplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=lolliplot(),
           height=lolliplot_size$height(),width=lolliplot_size$width(),
           scale=2
    )
  }) 
output$lolli_plotdwnld_popup <-  downloadHandler(
  filename = "lolliplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=lolliplot(),
           height=lolliplot_size$height(),width=lolliplot_size$width(),
           scale=2
    )
  }) 

############## Richness plot ############## 
observe({
  updateSelectInput(session,'alpha_xvar',
                    'Select the explanatory variable',
                    Meta_vars())
  updateSelectInput(session,"alpha_facet",
                    'Select the faceting variable - backspace to erase',
                    c("",Meta_vars()))
}) %>% bindEvent(Meta_vars()) 

richplot <- reactive({
  req(input$go_rich | trigger_plots())
  print("Alpha diversity: making plot")
  
  baseplot <- plot_richness(ps.norm(), 
                            x=input$alpha_xvar, 
                            measures=input$alphametrics) + 
    stat_summary(fun="median",
                 geom="label",
                 aes(label = after_stat(round(y, 2))
                 ),
                 fill="grey",
                 alpha=0.8,
                 position=position_nudge(y=1)
    ) + 
    geom_jitter(width=0.15)
  
  if(isTruthy(input$alpha_box)) {
    baseplot <- baseplot+geom_boxplot(alpha=0.75)
  } 
  
  if( input$alpha_facet=="") {
    finalplot <- baseplot
  } else if(!is.null(input$alpha_facet) & length(input$alpha_facet) == 1) {
    finalplot <- baseplot + facet_wrap(~.data[[input$alpha_facet]],scales="free") 
  } else if(!is.null(input$alpha_facet) & length(input$alpha_facet) == 2) {
    finalplot <- baseplot + facet_grid(rows=vars(.data[[input$alpha_facet[1]]]),
                                       cols= vars(.data[[input$alpha_facet[2]]]),
                                       scales="free") 
  } else {finalplot <- baseplot}
  
  finalplot
})

output$rich_s_plot <- renderPlot({
  richplot()
})
output$rich_a_plot <- renderPlot({
  richplot()
})
output$rich_click_plot <- renderPlot({
  richplot()
})
observe({
  print("Alpha diversity: saving plot")
  req(richplot())
  
  tryCatch(
    ggsave(filename="richplot.png", 
           plot=richplot(),
           path=tempdir(),
           scale=2
    ),
    error=function(e){return(NA)}
  )
}) 

observe({
  showModal(modalDialog(
    renderPlot(richplot()),
    footer = downloadButton('rich_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$rich_click)

output$rich_plotdwnld <- output$rich_plotdwnld_popup <-  downloadHandler(
  filename = "richness_plot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=richplot(),
           scale=2
    )
  }) 
