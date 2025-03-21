observe({ 
  req(ps.sub())
  updateSelectInput(session,
                    "choose_ord_color", 
                    choices=c(NA,Meta_vars())
                    )
  updateSelectInput(session,
                    "choose_ord_shape",
                    choices=c(NA,Meta_vars())
                    )
  #updateSelectInput(session,
  #                  "choose_ord_fill", 
  #                  choices=c(NA,Meta_vars())
  #                  )
  updateSelectInput(session,
                    "choose_ord_ellipse",
                    choices=c(NA,Meta_vars())
                    )
}) 

distreact <- eventReactive(input$calc_ord, {
  runif(input$calc_ord)
  
  req(input$orddist)
  req(input$ordmeth)
  
  ps <- ps.sub()
  withProgress(
    
    ordinate(
      physeq=ps,
      k=3,
      trymax=150,
      maxit=500,
      method=input$ordmeth, #ordination method #c("DCA", "CCA", "RDA", "CAP", "DPCoA", "NMDS", "MDS", "PCoA")
      distance=input$orddist, #distance matrix
  )
  
    )
})

output$report_ord <- renderText({
  return( paste0("Stress: ",distreact()$stress) )
})

# Hide download button until report is generated
output$ord_complete <- reactive({
  return(!is.null(distreact()))
})
outputOptions(output, 'ord_complete', suspendWhenHidden= FALSE)

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

ordplot <- eventReactive(input$DrawPlot, { #assign a variable that runs when plot button is pushed
  req(distreact())
  runif(input$DrawPlot)
  
  # ells <- input$choose_metavars4
  
  baseplot <- plot_ordination(physeq=ps.sub(),
                  ordination=distreact(),
                  type=input$ordvis,
                  color=input$choose_ord_color,
                  shape=input$choose_ord_shape ) + 
    geom_point(size=input$disp_point) 
  
  if(is.na(input$choose_ord_ellipse)) {
    finalplot <- baseplot
  } else {
    finalplot <- baseplot + stat_ellipse(aes(group=input$choose_ord_ellipse,fill=input$choose_ord_ellipse), 
                                         geom='polygon',
                                         type='norm',
                                         alpha=0)
  }
  
    #stat_ellipse(aes(group=ells(),
    #    fill=ells()
    #    ),
    #geom="polygon",type="norm",
    #alpha=input$disp_transp)
  
  finalplot + theme(text = element_text(size=input$disp_text),aspect.ratio=(input$ordplot_height/input$ordplot_width)) + 
    labs(subtitle=paste0("Using ",input$orddist," distance matrix"),
         title=paste0(input$ordmeth," Ordination Plot")
    )
})
output$ord <- renderPlot({
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