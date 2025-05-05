observe({ 
  req(ps.sub())
  updatePickerInput(session,
                    "choose_ord_color", 
                    choices=c(Meta_vars()),
                    selected=""
                    )
  updatePickerInput(session,
                    "choose_ord_shape",
                    choices=c(Meta_vars()),
                    selected=""
                    )
}) 

distreact <- eventReactive(input$calc_ord, {
  runif(input$calc_ord)
  
  req(input$ord_dist)
  req(input$ord_meth)
  
  ps <- ps.sub()
  withProgress(
    tryCatch(ordinate(
      physeq=ps,
      k=3,
      trymax=150,
      maxit=500,
      method=input$ord_meth, #ordination method #c("DCA", "CCA", "RDA", "CAP", "DPCoA", "NMDS", "MDS", "PCoA")
      distance=input$ord_dist, #distance matrix
    ),error=function(e){return("Calculation failed")} 
    )
    )
})

output$ord_complete <- renderText({
  if(distreact()=="Calculation failed") { return(paste0(distreact()))} else
  return( paste0("Stress: ",distreact()$stress) )
})

## Hide download button until report is generated
#output$ord_complete <- reactive({
#  return(!is.null(distreact()))
#})
#outputOptions(output, 'ord_complete', suspendWhenHidden= FALSE)

returnconverged <- eventReactive(input$ReturnStress, {
  req(distreact())
  runif(input$ReturnStress)
  distan <- distreact()
  cat("Stress", distan$stress, fill=TRUE)
})
output$converged <- renderPrint({ #print the stress
  returnconverged()
})
plotstress <- eventReactive(input$PlotStress,{
  req(distreact())
  runif(input$PlotStress)
  distan <- distreact()
  stressplot(distan)
})
output$ordstress <- renderPlot({ #plot the stress
  plotstress()
})
plotscree <- eventReactive(input$PlotScree,{
  req(distreact())
  runif(input$PlotScree)
  distan <- distreact()
  screeplot(distan)
})
output$ordscree <- renderPlot({ #plot scree plot
  plotscree()
})

observe({
  req(distreact())
  if (!is.null(input$choose_ord_color)) {
    updateRadioGroupButtons(session,
                            "choose_ord_ellipse",
                            disabled=FALSE)
  }
  if (!is.null(input$choose_ord_shape)) {
    updateRadioGroupButtons(session,
                            "choose_ord_ellipse",
                            disabled=FALSE)
  }
})

ordplot <- eventReactive(input$go_ord, { #assign a variable that runs when plot button is pushed
  req(distreact())
  runif(input$go_ord)
  
  # ells <- input$choose_metavars4
  
  baseplot <- plot_ordination(physeq=ps.sub(),
                  ordination=distreact(),
                  type=input$ord_vis,
                  color=input$choose_ord_color,
                  shape=input$choose_ord_shape ) + 
    geom_point(size=input$ord_plotpoint) 
  
  if(input$choose_ord_ellipse=="Color") {
      finalplot <- baseplot +  stat_ellipse(aes(group=.data[[input$choose_ord_color]],
                                                #color=.data[[input$choose_ord_ellipse]],
                                                #fill=.data[[input$choose_ord_ellipse]],
                                                #linetype=.data[[input$choose_ord_ellipse]]
                                                ), 
                                            show.legend=FALSE, #since the ellipse grouping is the same as color, we don't need a legend
                                            #geom='polygon', #requires a fill and alpha aes
                                            type='norm', #assumes a multi-variate normal distribution
                                            level=0.95, #confidence level
                                            ) + labs(caption="Ellipses drawn with a normal distribution at 95% confidence interval")
  } else if (input$choose_ord_ellipse=="Shape") {
    finalplot <- baseplot +  stat_ellipse(aes(group=.data[[input$choose_ord_shape]]), 
                                          show.legend=FALSE, #since the ellipse grouping is the same as color, we don't need a legend
                                          level=0.95, #confidence level
    ) + labs(caption="Ellipses drawn with a normal distribution at 95% confidence interval")
  } else {
    finalplot <- baseplot 
  }

  
  finalplot + theme(text = element_text(size=input$ord_plottext),aspect.ratio=(input$ord_plotheight/input$ord_plotwidth)) + 
    labs(subtitle=paste0("Using ",input$ord_dist," distance matrix"),
         title=paste0(input$ord_meth," Ordination Plot")
    )
})
output$ord_plot <- renderPlot({
  ordplot()
})
observe({
  showModal(modalDialog(
    renderPlot(ordplot()),
    footer = downloadButton('download_ordplot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$ord_click)
output$download_ordplot_popup <-  downloadHandler(
  filename = "ord_plot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=ordplot()
    )
  }) 
output$psog <- renderPrint({ps.all()})
output$pstext <- renderPrint({ps.sub()})