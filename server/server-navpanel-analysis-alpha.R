source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

#output$alphametrics_var2 <- output$alphametrics_var1 <- renderUI({
#  req(ps.norm())
#  ps <- ps.norm()
#  selectInput('alpha_xvar',
#              'Select the explanatory variable',
#              Meta_vars())
#})

observe({
  updateSelectInput(session,'alpha_xvar',
                    'Select the explanatory variable',
                    Meta_vars())
  updateSelectInput(session,"alpha_facet",
                    'Select the faceting variable - backspace to erase',
                    c("",Meta_vars()))
}) %>% 
  bindEvent(Meta_vars()) #respond to any changes in the Metadata file that is uploaded

richplot <- reactive({ #draw alpha div plot when 'plot' button is clicked
  print("Alpha diversity: making plot")
  
  baseplot <- plot_richness(ps.norm(), 
                            x=input$alpha_xvar, 
                            measures=input$alphametrics) + geom_jitter(width=0.15)
  
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
})%>% bindEvent(input$go_rich)

observe({
  print("Alpha diversity: rendering plot")
  output$rich_s_plot <- output$rich_plot <- renderPlot({  richplot() })

tryCatch(
ggsave(filename="richplot.png", 
       plot=richplot(),
       path=tempdir(),
       scale=2
       ),
error=function(e){return(NA)}
)

}) %>% bindEvent(input$go_rich)

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

#histoplot_pretty <- reactiveVal(NULL)
#histoplot_size <- reactiveVal(NULL)
#histoplot_general <- reactiveValues(color=NULL,x=NULL,y=NULL,facet=NULL)

#observeEvent(ps.norm(), {
histoplot_pretty  <- plotPrettyServer(id="histo_pretty")
histoplot_size <- plotSizeServer(id="histo_size") 
histoplot_general <- plotControlsServer(id="histo_gen",
                   df=ps.norm,
                   var1=rnames, #color
                   var2=Meta_vars, #x
                   var3=Meta_vars, #y
                   var4=Meta_vars #facet
                   )
#})

histo_base <- reactive({
  req(ps.norm())
  req(histoplot_general$color())
  ps <- ps.norm()
  print("Histogram: making base plot")
  plot_bar(ps,
           x=histoplot_general$x()) + 
    geom_bar(stat='identity',position='stack',aes(color=.data[[histoplot_general$color()]],
                                                  fill=.data[[histoplot_general$color()]])) + 
    guides(color=guide_legend(histoplot_general$color()),fill=guide_legend(histoplot_general$color()))

}) %>% bindEvent(ps.norm(),
                 histoplot_general$color(),
                 histoplot_general$x()
)

histoplot <- eventReactive(input$go_histoplot, {
  
  print("Histogram: assembling plot")
  
  histo_base() + histoplot_general$facet() + histoplot_pretty() + histoplot_size$aspectRatio() 
}) 

observeEvent(input$go_histoplot,{
  print("Histogram: rendering plot")
  output$histo_plot <- renderPlot({histoplot()})
  
  tryCatch(
  ggsave(filename="histoplot.png", 
         plot=histoplot(),
         path=tempdir(),
         height=histoplot_size$height(),width=histoplot_size$width(),
         scale=2
         ),
  error=function(e){return(NA)}
  )
})

observe({
  showModal(modalDialog(
    renderPlot(histoplot()),
    footer = downloadButton('histo_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
  print("Histogram: popup")
}) %>% bindEvent(input$histo_click)


output$histo_plotdwnld <- output$histo_plotdwnld_popup <-  downloadHandler(
  filename = "histoplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=histoplot(),
           height=histoplot_size$height(),width=histoplot_size$width(),
           scale=2
    )

  }) 
