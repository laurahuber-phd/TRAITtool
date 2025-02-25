distreact <- reactive({
  req(input$orddist)
  req(input$ordmeth)
  ps <- ps.sub()
  ordination_function(ps=ps,
                      dstnc=input$orddist, #distance matrix
                      mthd=input$ordmeth, #ordination method
                      kf=3, #k
                      tmf=150, #trymax
                      maxf=500 #max iterations
  )
})
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
  plot_ordination(physeq=ps.sub(),
                  ordination=distreact(),
                  type=input$ordvis,
                  color=input$choose_color,
                  shape=input$choose_shape
  ) + 
    geom_point(size=input$disp_point) + 
    #stat_ellipse(aes(group=ells(),
    #    fill=ells()
    #    ),
    #geom="polygon",type="norm",
    #alpha=input$disp_transp) +
    theme(text = element_text(size=input$disp_text)) + 
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