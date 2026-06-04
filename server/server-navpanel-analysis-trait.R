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
    labs(y="Relative Abundance",
         title="Relative abundance of each trophic group") +
    coord_flip() 
  
  if(!is.null(input$choose_trophbar_facet) & length(input$choose_trophbar_facet) < 2) {
    finalplot <- baseplot + facet_wrap(~input$choose_trophbar_facet,scales="free") 
  } else if(!is.null(input$choose_trophbar_facet) & length(input$choose_trophbar_facet) == 2) {
    finalplot <- baseplot + facet_grid(rows=vars(.data[[input$choose_trophbar_facet[1]]]),
                                       cols= vars(.data[[input$choose_trophbar_facet[2]]]),
                                       scales="free") 
  } else {finalplot <- baseplot}
  
  finalplot + trophbarplot_pretty() + trophbarplot_size() 
  }
  
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

############## Line plot ############## 

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
  if(!is.null(ps$Trophic.Val)){
    
    ps <- ps.norm.melt() %>% mutate(TrophicScore = (as.numeric(.data[[tlineplot_general$color()]]))) %>% filter(Abundance>0) 
    
    baseplot <- ggplot(ps, 
                       aes(x=TrophicScore, #must be numeric
                           y=Abundance, 
                           color=.data[[tlineplot_general$x()]], 
                           linetype=.data[[tlineplot_general$y()]]
                       )
    ) + 
      geom_line(na.rm=TRUE)  
    
      baseplot + 
      tlineplot_general$facet() + tlineplot_pretty() + tlineplot_size() 
  }
  
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

############## Lollipop plot ############## 
observe({ 
  updatePickerInput(session,
                    "choose_lolli_yaxis", 
                    choices=c(rnames()),
                    selected=""
  )
  updatePickerInput(session,
                    "choose_lolli_xaxis", 
                    choices=c(as.list(ps.norm.melt() %>% select(where(is.numeric)) %>% colnames())),
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
  lolvar <- ps[[{input$choose_lolli_yaxis}]] %>% unique()
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
  ps <- ps.norm.melt() #filter(Abundance>0) %>% filter(!is.na(Trophic.Val))
  
  if(!is.null(ps$Trophic.Val)){
    
    my_vars <- function() {
      c(any_of(c("Trophic.Val", 
                 #input$choose_lolli_yaxis, 
                 input$choose_lolli_facet
      )
      ))
    }
    
    ps.edit <- ps %>% group_by(across(my_vars()) ) %>% arrange() #%>% arrange(Trophic.Val) %>% mutate(x=factor(x,x))  #%>% summarize(RelAbund=Abundance/sum(Abundance)) 
    
    baseplot <- ggplot(ps.edit, aes(x=.data[[input$choose_lolli_yaxis]], y=.data[[input$choose_lolli_xaxis]])) +
      geom_segment(
        aes(x=ps.edit[[input$choose_lolli_yaxis]], xend=ps.edit[[input$choose_lolli_yaxis]], y=0, yend=.data[[input$choose_lolli_xaxis]]), 
        color=ifelse(ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples, "orange", "grey"), 
        size=ifelse(ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples, 1.3, 0.7)
      ) +
      geom_point(
        color=ifelse(ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples, "orange", "grey"), 
        size=ifelse(ps.edit[[input$choose_lolli_yaxis]] %in% input$lolli_samples, 5, 2)
      ) +
      coord_flip() 
    
    # Add annotation
    #p + annotate("text", x=grep("D", data$x), y=data$y[which(data$x=="D")]*1.2, 
    #             label="Group D is very impressive", 
    #             color="orange", size=4 , angle=0, fontface="bold", hjust=0) + 
    #  
    #  annotate("text", x = grep("A", data$x), y = data$y[which(data$x=="A")]*1.2, 
    #           label = paste("Group A is not too bad\n (val=",data$y[which(data$x=="A")] %>% round(2),")",sep="" ) , 
    #           color="orange", size=4 , angle=0, fontface="bold", hjust=0) 
    
    
    if(!is.null(input$choose_trophbar_facet) & length(input$choose_trophbar_facet) < 2) {
      finalplot <- baseplot + facet_wrap(~input$choose_trophbar_facet,scales="free") 
    } else if(!is.null(input$choose_trophbar_facet) & length(input$choose_trophbar_facet) == 2) {
      finalplot <- baseplot + facet_grid(rows=vars(.data[[input$choose_trophbar_facet[1]]]),
                                         cols= vars(.data[[input$choose_trophbar_facet[2]]]),
                                         scales="free") 
    } else {finalplot <- baseplot} 
  
  finalplot + lolliplot_pretty() + lolliplot_size()
  }
})

observeEvent(input$go_lolli, {
  print("Trophic lolli plot: plotting")
  output$lolli_plot <- renderPlot({lolliplot()})
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
    ggsave(file,plot=lolliplot()
    )
    
  }) 
