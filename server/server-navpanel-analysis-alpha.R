source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
library(shinycssloaders)

output$alphametrics_var2 <- output$alphametrics_var1 <- renderUI({
  req(ps.norm())
  ps <- ps.norm()
  selectInput('alpha_xvar',
              'Select the explanatory variable',
              sample_variables(ps))
})
richplot <- eventReactive(input$go_rich, { #draw alpha div plot when 'plot' button is clicked
  withProgress(message="Making richness plot",
               plot_richness(ps.norm(), x=input$alpha_xvar, measures=input$alphametrics) + geom_boxplot()
  )
})

observeEvent(input$go_rich, {
output$rich_plot <- renderPlot({ #need separate call to actually render the plot
  richplot()
  })
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
    ggsave(file,plot=richplot()
    )
  }) 

#histoplot_pretty <- reactiveVal(NULL)
#histoplot_size <- reactiveVal(NULL)
#histoplot_general <- reactiveValues(color=NULL,x=NULL,y=NULL,facet=NULL)

#observeEvent(ps.norm(), {
histoplot_pretty  <- plotPrettyServer(id="histo_pretty")
histoplot_size <- plotSizeServer(id="histo_size") 
histoplot_general <- plotControlsServer(id="histo_gen",
                   df=ps.norm(),
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
  plot_bar(ps,
           x=histoplot_general$x()) + 
    geom_bar(stat='identity',position='stack',aes(color=.data[[histoplot_general$color()]],
                                                  fill=.data[[histoplot_general$color()]])) + 
    guides(color='none',fill=guide_legend(histoplot_general$color()))

}) %>% bindEvent(ps.norm(),
                 histoplot_general$color(),
                 histoplot_general$x()
)

histoplot <- eventReactive(input$go_histoplot, {
  histo_base() + histoplot_general$facet() + histoplot_pretty() + histoplot_size() 
}) 

observeEvent(input$go_histoplot,{
  output$histo_plot <- renderPlot({histoplot()})
})

observe({
  showModal(modalDialog(
    renderPlot(histoplot()),
    footer = downloadButton('histo_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$histo_click)


output$histo_plotdwnld <- output$histo_plotdwnld_popup <-  downloadHandler(
  filename = "histoplot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=histoplot()
    )

  }) 
