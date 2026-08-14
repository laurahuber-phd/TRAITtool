source("server/server-navpanel-analysis-temp.R",local=TRUE)
source("server/server-navpanel-analysis-trait.R",local=TRUE)
source("server/server-navpanel-analysis-violin.R",local=TRUE)

observe({
  req(ps.all())
  req(input$pages == "analysistab")
  req(input$aggregate_var)
  
  print("Default: Default analysis selected")
  #aggvar <- input$aggregate_var
  # ---------------------  trophic level plots --------------------- #
  ## violin plot 
  ### needs trophic level to plot (violinplot_general$y) 
  #### (currently selects the first value in this list: ps.norm.melt() %>% select(contains("roph")) %>% colnames())
  
  ### needs x-axis variable (violinplot_general$x)
  #### (currently selects the first value in this list: Meta_vars())
  
  ### update title  
  updateTextInput(session, "violin_pretty-plottitle", value = "Mean Trophic Index")
  print("MTI plot title updated")
  
  ## pie chart
  ### update the aggregate variable
  updatePickerInput(session,
                    "choose_trophbar_facet",
                    #choices=c(Meta_vars()),
                    selected=isolate(input$aggregate_var)
  )
  print(c("trophbar facet picker variable updated to",isolate(input$aggregate_var)))
  
  # ---------------------  temperature plots --------------------- #  
  ## index violin plot  
  ### needs grouping/x-axis variable (tviolinplot_general$x)
  #### (currently selects the first value in this list: Meta_vars())
  #
  ### needs temperature/y-axis variable (tviolinplot_general$y)
  #### currently uses an index generated within the plotting reactive 
  #
  ### update title
  updateTextInput(session, "tviolin_pretty-plottitle", 
                  value = "Temperature Index")
  updateTextInput(session, "tviolin_pretty-plotcaption", 
                  value = "Index was calculated by dividing the median preferred temperature for each species by the difference in temperature range of the community group, identified by the grouping variable.")
  
  ## raw temp violin plot  
  ### needs grouping/x-axis variable (tviolinplot_general$x)
  #### (currently selects the first value in this list: Meta_vars())
  #
  ### needs temperature/y-axis variable (tviolinplot_general$y)
  #### currently selects the first value in this list: ps.norm.melt() %>% select(contains("emp")) %>% colnames()
  #
  ### update title  
  updateTextInput(session, "tviolinmean_pretty-plottitle", 
                  value = "Temperature Preference")
  #tvio_sub <- switch(tviolinmeanplot_general$y(), 
  #                   contains("min") = "Minimum",
  #                   contains("max") = "Maximum",
  #                   contains("median") = "Median",
  #                   contains("mean") = "Mean")
  #updateTextInput(session, "tviolinmean_pretty-plotsubtitle",value = tvio_sub )
  
  ## lollipop plot  
  ### needs y-axis variable (choose_tlolli_yaxis)
  #updatePickerInput(session,"choose_tlolli_yaxis",
  #                   choices=isolate(input$aggregate_var)) #make aggregate variable the y-axis for temperature lollipop plot
  updatePickerInput(session,
                    "choose_tlolli_facet",
                    selected=isolate(input$aggregate_var)
  )
  print(c("temperature lolli plot facet picker variable updated to",isolate(input$aggregate_var)))
  
  # ---------------------  body size plots --------------------- #  
  ## pie chart 
  ### needs size selection to plot (input$choose_sizebar_axis)
  #### currently selects the first value in this list: ps.norm.melt() %>% select(contains("ength")) %>% colnames()
  #
  ### needs facet variable (choose_sizebar_facet)
  updatePickerInput(session,
                    "choose_sizebar_facet",
                    choices=c(Meta_vars()),
                    selected=isolate(input$aggregate_var))
  
  print(c("sizebar facet picker variable updated to",isolate(input$aggregate_var)))
  #
  ### update title  
  updateTextInput(session, "sizebar_pretty-plottitle", 
                  value = input$choose_sizebar_axis)
  
  # ---------------------  IUCN status plots --------------------- #   
  # update iucn status plot  
  updatePickerInput(session,
                    "choose_iucnbar_facet",
                    choices=c(Meta_vars()),
                    selected=isolate(input$aggregate_var))
  
  print(c("iucnbar facet picker variable updated to",isolate(input$aggregate_var)))
  #
  ### update title  
  updateTextInput(session, "iucnbar_pretty-plottitle", 
                  value = "IUCN Red List Status")
  
  # ---------------------  Richness plots --------------------- #   
  ## needs x-axis variable (alpha_xvar)
  #updateSelectInput(session,"alpha_xvar",
  #                  choices=isolate(input$aggregate_var)) #make aggregate variable the x-axis for alpha diversity plot
  
#updateNumericInput(session,"prune_samples",value=100)
#updateNumericInput(session,"prune_tax",value=1)
#updateRadioButtons(session,"normoptions",selected="tax_rel")
  
shinyjs::click("go_violin") #simulate clicking 'go' for trophic plots
shinyjs::click("go_trophbar")

shinyjs::click("go_tviolin") #simulate clicking 'go' for temperature plots
shinyjs::click("go_tviolinmean")
shinyjs::click("go_tlolli")

shinyjs::click("go_sizebar") #simulate clicking 'go' for size plot
shinyjs::click("go_rich") #simulate clicking 'go' for richness plot
shinyjs::click("go_iucnbar")

#if (isTruthy(ps.norm.melt() %>% select(contains("Troph")) %>% colnames())) {
#
#}

},priority=10) %>% bindEvent(input$aggregate_var,input$pages,input$go_aggregate)
