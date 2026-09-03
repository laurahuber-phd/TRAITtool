source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

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
  
  ps.edit <- ps %>% 
    group_by(.data[[tviolinplot_general$x()]]) %>% 
    #group_by(across(unlist(my_vars())) ) %>% 
    mutate(TempRange=max(Tempprefmax,na.rm=TRUE)-min(Tempprefmin,na.rm=TRUE)) %>%
    rowwise() %>% 
    mutate(TempIndex=Tempprefmean/TempRange) 
  #summarize(Tempprefmean=mean(Tempprefmean),
  #          Tempprefmin=min(Tempprefmin),
  #          Tempprefmax=max(Tempprefmax)) %>%
  #arrange()
  
  baseplot <- ggplot(ps.edit,
                     aes(x=.data[[tviolinplot_general$x()]], #x axis
                         y=TempIndex, #y axis #Tempprefmean,
                         fill=.data[[tviolinplot_general$x()]]
                     )
  ) + 
    geom_violin(na.rm=TRUE,alpha=0.2) + 
    geom_boxplot(width=0.25,alpha=0.8) + 
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

tviolinplot <- reactive({
  print("Violin: assembling plot")
  tviolinplot_base() + tviolinplot_general$facet() + tviolinplot_pretty() + tviolinplot_size$aspectRatio() 
}) 

observeEvent(input$go_tviolin, {
  print("Violin: plotting")
  output$tviolin_s_plot <- output$tviolin_plot <- renderPlot({tviolinplot()})
  
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
  print("Violin: building base plot")
  req(ps.norm())
  #req(tviolinmeanplot_general$x())
  ps <- ps.norm.melt()
  
  my_vars <- function() {
    any_of(c(tviolinmeanplot_general$x(),
             tviolinmeanplot_general$facet()
    )
    )
    
  }
  
  ps.edit <- ps #%>% 
  #group_by(across(unlist(my_vars())) ) %>% 
  #summarize(Tempprefmean=mean(Tempprefmean),
  #          Tempprefmin=min(Tempprefmin),
  #          Tempprefmax=max(Tempprefmax)) %>%
  #arrange()
  
  baseplot <- ggplot(ps.edit,
                     aes(x=.data[[tviolinmeanplot_general$x()]], #x axis
                         y=.data[[tviolinmeanplot_general$y()]], #y axis #Tempprefmean,
                         fill=.data[[tviolinmeanplot_general$x()]]
                     )
  ) + 
    geom_violin(na.rm=TRUE,alpha=0.2) + 
    geom_boxplot(width=0.25,alpha=0.8) + 
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
                 tviolinmeanplot_general$x(),
                 tviolinmeanplot_general$y(),
                 tviolinmeanplot_general$facet()
)

tviolinmeanplot <- reactive({
  print("Violin: assembling plot")
  tviolinmeanplot_base() + tviolinmeanplot_general$facet() + tviolinmeanplot_pretty() + tviolinmeanplot_size$aspectRatio() 
}) 

observeEvent(input$go_tviolinmean, {
  print("Violin: plotting")
  output$tviolinmean_s_plot <- output$tviolinmean_plot <- renderPlot({tviolinmeanplot()})
  
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

observe({
  showModal(modalDialog(
    renderPlot(tviolinmeanplot()),
    footer = downloadButton('download_tviolinmeanplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Violin: popup")
}) %>% bindEvent(input$tviolinmean_click)


output$tviolinmean_plotdwnld <- output$tviolinmean_plotdwnld_popup <-  downloadHandler(
  filename = "tviolinmeanplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=tviolinmeanplot(),
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

tlolliplot <- reactive({
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
        aes(x=.data[[input$choose_tlolli_yaxis]], xend=.data[[input$choose_tlolli_yaxis]], 
            y=Tempprefmin, yend=Tempprefmax,
            color=.data[[input$choose_tlolli_yaxis]] %in% input$tlolli_samples#,
            #size=.data[[input$choose_tlolli_yaxis]] %in% input$tlolli_samples
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
    
    finalplot + tlolliplot_pretty() + tlolliplot_size$aspectRatio()
  }
}) %>% bindEvent(input$choose_iucnbar_facet)

observeEvent(input$go_tlolli, {
  print("Temperature lolli plot: plotting")
  output$tlolli_s_plot <- output$tlolli_plot <- renderPlot({tlolliplot()})
  
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
  #print("Violin: building base plot")
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
                 aes(label = after_stat(round(y, 2))
                 ),
                 #hjust = 0.5, 
                 #vjust = -1,
                 fill="white"
    )
  
}) %>% bindEvent(violinplot_general$x(),
                 violinplot_general$y(),
                 violinplot_general$facet(),
                 ps.norm(),Meta_vars()
)

violinplot <- reactive({
  print("Violin: assembling plot")
  violin_base() + violinplot_general$facet() + violinplot_pretty() + violinplot_size$aspectRatio() 
})

observeEvent(input$go_violin, {
  print("Violin: plotting")
  output$violin_s_plot <- output$violin_plot <- renderPlot({violinplot()})
  
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

############## Bar Plot Trophic Level ############## 
observe({ 
  #updatePickerInput(session,
  #                  "choose_trophbar_axis", 
  #                  choices=c(Meta_vars()),
  #                  selected=Meta_vars()[[1]]
  #)
  updatePickerInput(session,
                    "choose_trophbar_facet",
                    choices=c(Meta_vars())#,
                    #selected=""
  )
}) %>% bindEvent(ps.norm(),Meta_vars())

trophbarplot_pretty  <- plotPrettyServer(id="trophbar_pretty")
trophbarplot_size <- plotSizeServer(id="trophbar_size") 


trophbarplot_base <- reactive({
  req(ps.norm())
  print("Trophic bar plot: building base plot")
  
  ps <- ps.norm.melt() 
  
  if(!is.null(ps$Trophic.val)){
    
    ps <- ps.norm.melt() %>% filter(Abundance>0) %>% filter(!is.na(Trophic.val))
    
    my_vars <- c("Trophic.val", 
                 #input$choose_trophbar_axis, 
                 input$choose_trophbar_facet
    )
    print(c("arrange dataset by",my_vars))
    
    ps.edit <- ps %>% 
      group_by(across(any_of(my_vars) )) %>% 
      summarize(Abundance=sum(Abundance),.groups="keep") %>% 
      arrange(input$choose_trophbar_facet)
    #summarize(RelAbund=Abundance/sum(Abundance)) 
    print(group_keys(ps.edit))
    baseplot <- ggplot(ps.edit, 
                       aes(x="",#x=.data[[input$choose_trophbar_axis]], #x axis
                           y=Abundance, #y=RelAbund,
                           fill=Trophic.val,
                           color=Trophic.val
                       )
    ) + 
      geom_bar( stat="identity", position = "fill") +  
      #scale_y_continuous(labels=function(x) paste0(x * 100, "%")) + 
      labs(y="Relative Abundance",
           title="Relative abundance of each trophic group") +
      #coord_flip() 
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
    
}) %>% bindEvent(input$choose_trophbar_facet#,
                 #ps.norm(),Meta_vars()
                 )

trophbarplot <- reactive({trophbarplot_base() +
    trophbarplot_size$aspectRatio() })

observeEvent(input$go_trophbar, {
  print("Trophic bar plot: plotting")
  output$trophbar_s_plot <- output$trophbar_plot <- renderPlot({trophbarplot() })
  
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

observe({
  showModal(modalDialog(
    renderPlot(trophbarplot_base()),
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
    ggsave(file,plot=trophbarplot(),
           height=trophbarplot_size$height(),width=trophbarplot_size$width(),
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
  print("Size bar plot: building base plot")

  ps <- ps.norm.melt() %>% 
    filter(Abundance>0) %>%
    mutate(BodySize=case_when(.data[[input$choose_sizebar_axis]] > 100 ~ "Giant (>100 cm)",
                              .data[[input$choose_sizebar_axis]] >= 20 ~ "Large (>20 cm)",
                              .data[[input$choose_sizebar_axis]] < 20 & .data[[input$choose_sizebar_axis]] >= 10 ~ " Medium (10-19 cm)",
                              .data[[input$choose_sizebar_axis]] < 10 & .data[[input$choose_sizebar_axis]] >= 5 ~ " Small (5-10 cm)",
                              .data[[input$choose_sizebar_axis]] <5 ~ "Very small (<5 cm)",
                              .default="Not assigned"))
  
  my_vars <- c("BodySize",
               input$choose_sizebar_axis, 
               input$choose_sizebar_facet
               )
  print(c("arrange dataset by",my_vars))
  
  ps.edit <- ps %>% 
    group_by(across(any_of(my_vars)) ) %>% 
    summarize(Abundance=sum(Abundance),.groups="keep") %>% 
    arrange(input$choose_sizebar_axis)
  #summarize(RelAbund=Abundance/sum(Abundance)) 
  
  baseplot <- ggplot(ps.edit, 
                     aes(x="",#x=.data[[input$choose_sizebar_axis]], #x axis
                         y=Abundance, #y=RelAbund,
                         fill=BodySize,
                         color=BodySize
                     )
  ) + 
    geom_bar( stat="identity", position = "fill") +  
    #scale_y_continuous(labels=function(x) paste0(x * 100, "%")) + 
    #labs(y="Relative Abundance",
    #     title="Relative abundance of each trophic group") +
    #coord_flip() 
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
  
}) %>% bindEvent(#ps.norm(),Meta_vars(),
                 input$choose_sizebar_axis,input$choose_sizebar_facet
                 )

sizebarplot <- reactive({sizebarplot_base()+sizebarplot_size$aspectRatio()})

observe({
  print("Size bar plot: plotting")
  output$sizebar_s_plot <- output$sizebar_plot <- renderPlot({sizebarplot()})
  
  tryCatch(
    ggsave(filename="sizebarplot.png", 
           path=tempdir(),
           plot=sizebarplot(),
           height=sizebarplot_size$height(),width=sizebarplot_size$width(),
           scale=2
    ),
    error=function(e){return(NA)}
  )
}) %>% bindEvent(input$go_sizebar)


observe({
  showModal(modalDialog(
    renderPlot(sizebarplot_base()),
    footer = downloadButton('download_sizebarplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Trophic bar plot: popup")
}) %>% bindEvent(input$sizebar_click)

output$sizebar_plotdwnld <- output$sizebar_plotdwnld_popup <-  downloadHandler(
  filename = "sizebarplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=sizebarplot(),
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
  print("IUCN bar plot: building base plot")
  
  names <- ps.norm.melt() %>%
    select(contains(regex("IUCN_category",ignore_case = TRUE))) %>%
    colnames()
  
  ps <- ps.norm.melt() %>% 
    rename(IUCN_Category_Status = input$choose_iucnbar_axis) %>% 
    mutate(IUCN_Category_Status = str_to_upper(IUCN_Category_Status)) %>%
    mutate(IUCN_Status = case_when(is.na(IUCN_Category_Status) ~ "Not Found in Red List",
                                   .default=IUCN_Category_Status)) 
  print(colnames(ps))
  
  my_vars <- c("IUCN_Status",
               "IUCN_Category_Status",
               #input$choose_iucnbar_axis, 
               input$choose_iucnbar_facet
  )
  print(c("arrange dataset by",my_vars))
  
  ps.edit <- ps %>% 
    group_by(across(any_of(my_vars)) ) %>% 
    summarize(Abundance=sum(Abundance),.groups="keep") %>% 
    arrange(input$choose_iucnbar_axis)
  
  baseplot <- ggplot(ps.edit, 
                     aes(x="",#x=.data[[input$choose_iucnbar_axis]], #x axis
                         y=Abundance,
                         fill=IUCN_Status,#Iucn_category,
                         color=IUCN_Status#Iucn_category
                     )
  ) + 
    geom_bar( stat="identity", position = "fill") +  
    #scale_y_continuous(labels=function(x) paste0(x * 100, "%")) + 
    #labs(y="Relative Abundance",
    #     title="Relative abundance of each trophic group") +
    #coord_flip() 
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
  
}) %>% bindEvent(#ps.norm(),Meta_vars(),
  #input$choose_iucnbar_axis,
  input$choose_iucnbar_facet
)

iucnbarplot <- reactive({iucnbarplot_base()+iucnbarplot_size$aspectRatio()}) 

observe({
  print("IUCN bar plot: plotting")
  output$iucnbar_s_plot <- output$iucnbar_plot <- renderPlot({iucnbarplot()})
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

observe({
  showModal(modalDialog(
    renderPlot(iucnbarplot_base()),
    footer = downloadButton('download_iucnbarplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Trophic bar plot: popup")
}) %>% bindEvent(input$iucnbar_click)

output$iucnbar_plotdwnld <- output$iucnbar_plotdwnld_popup <-  downloadHandler(
  filename = "iucnbarplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=iucnbarplot(),
           height=iucnbarplot_size$height(),width=iucnbarplot_size$width(),
           scale=2
    )
    
  }) 

############## Line plot Trophic Level ############## 

tlineplot_pretty  <- plotPrettyServer(id="tline_pretty")
tlineplot_size <- plotSizeServer(id="tline_size") 
tlineplot_general <- plotControlsServer(id="tline_gen",
                                        df=ps.norm.melt,
                                        #var1=rnames, #color
                                        var2=Meta_vars, #x
                                        var3=Meta_vars, #y
                                        var4=Meta_vars, #facet
                                        #var1=Meta_vars(), #color
                                        #var2=Meta_vars(), #x
                                        var1=reactive(ps.norm.melt() %>% select(contains("roph")) %>% colnames() )#, 
                                        #var4=Meta_vars() #facet
) 


tlineplot <- eventReactive(input$go_tline, { #assign a variable that runs when plot button is pushed
  runif(input$go_tline)
  req(ps.norm())
  print("Trophic line plot: building base plot")
  
  ps <- ps.norm.melt() 
  if(!is.null(ps$Trophic.val)){
    
    ps <- ps.norm.melt() %>% mutate(TrophicScore = (as.numeric(.data[[tlineplot_general$color()]]))) %>% filter(Abundance>0) 
    
    baseplot <- ggplot(ps, 
                       aes(x=TrophicScore, #must be numeric
                           y=Abundance, 
                           color=.data[[tlineplot_general$x()]], 
                           linetype=.data[[tlineplot_general$y()]]
                       )
    ) + 
      geom_line(na.rm=TRUE)  
    
    #if (is.numeric(ps[[tlineplot_general$y()]])) {
    #  baseplot <- baseplot + scale_linetype_binned()
    #} 
   # if (is.numeric(ps[[tlineplot_general$x()]])) {
   #   baseplot <- baseplot + scale_color_binned()
   # } 
    
      baseplot + 
      tlineplot_general$facet() + tlineplot_pretty() + tlineplot_size$aspectRatio() 
  }
  
})

observeEvent(input$go_tline, {
  print("Line diagram: plotting")
  output$tline_plot <- renderPlot({tlineplot()})
  
  tryCatch(
  ggsave(filename="tlineplot.png", 
         path=tempdir(),
         plot=tlineplot(),
         height=tlineplot_size$height(),width=tlineplot_size$width(),
         scale=2
  ),
  error=function(e){return(NA)}
  )
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
    ggsave(file,plot=tlineplot(),
           height=tlineplot_size$height(),width=tlineplot_size$width(),
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
lolliplot <- eventReactive(input$go_lolli, {
  req(ps.norm())
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
    
    ps.edit <- ps %>% group_by(across(my_vars()) ) %>% 
      #summarize(input$choose_lolli_xaxis=sum(input$choose_lolli_xaxis)) %>% 
      arrange() #%>% #arrange(Trophic.val) %>% 
    #mutate(x=factor(x,x)) %>% 
    #summarize(RelAbund=Abundance/sum(Abundance)) 
    
    baseplot <- ggplot(ps.edit, aes(x=.data[[input$choose_lolli_yaxis]],
                                    y=.data[[input$choose_lolli_xaxis]])
                       ) +
      geom_segment(
        aes(x=ps.edit[[input$choose_lolli_yaxis]], xend=ps.edit[[input$choose_lolli_yaxis]], 
            y=0, yend=.data[[input$choose_lolli_xaxis]],
            color=ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples#,
            #size=ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples
            )) + 
      geom_point(
        aes(color=ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples,
            size=ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples
            )) + 
      scale_color_manual(values=c("TRUE"="orange","FALSE"="grey"),name=NULL,labels=NULL) + 
      scale_size_manual(values=c("TRUE"=3,"FALSE"=1),name=NULL,labels=NULL) +
      coord_flip()
    
    # Add annotation
    #p + annotate("text", x=grep("D", data$x), y=data$y[which(data$x=="D")]*1.2, 
    #             label="Group D is very impressive", 
    #             color="orange", size=4 , angle=0, fontface="bold", hjust=0) + 
    #  
    #  annotate("text", x = grep("A", data$x), y = data$y[which(data$x=="A")]*1.2, 
    #           label = paste("Group A is not too bad\n (val=",data$y[which(data$x=="A")] %>% round(2),")",sep="" ) , 
    #           color="orange", size=4 , angle=0, fontface="bold", hjust=0) 
    
    if(!is.null(input$choose_lolli_facet) & length(input$choose_lolli_facet) < 2) {
      finalplot <- baseplot + facet_wrap(~.data[[input$choose_lolli_facet]],scales="free") 
    } else if(!is.null(input$choose_lolli_facet) & length(input$choose_lolli_facet) == 2) {
      finalplot <- baseplot + facet_grid(rows=vars(.data[[input$choose_lolli_facet[1]]]),
                                         cols= vars(.data[[input$choose_lolli_facet[2]]]),
                                         scales="free") 
    } else {finalplot <- baseplot} 
  
  finalplot + lolliplot_pretty() + lolliplot_size$aspectRatio()
  }
})

observeEvent(input$go_lolli, {
  print("Trophic lolli plot: plotting")
  output$lolli_plot <- renderPlot({lolliplot()})
  
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

observe({
  showModal(modalDialog(
    renderPlot(lolliplot()),
    footer = downloadButton('download_lolliplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Trophic bar plot: popup")
}) %>% bindEvent(input$lolli_click)

output$lolli_plotdwnld <- output$lolli_plotdwnld_popup <-  downloadHandler(
  filename = "lolliplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=lolliplot(),
           height=lolliplot_size$height(),width=lolliplot_size$width(),
           scale=2
    )
    
  }) 
