#source("server/server-navpanel-analysis-temp.R",local=TRUE)
source("server/server-navpanel-analysis-trait.R",local=TRUE)
#source("server/server-navpanel-analysis-violin.R",local=TRUE)

update_reqs <- reactiveVal(NULL)
trigger_plots <- reactiveVal(0)

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
    updateTextInput(session, 
                    "violin_pretty-plottitle", 
                    value = "Mean Trophic Index")
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
    print("Temperature index violin plot title updated")
    print("Temperature index violin plot caption updated")
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
    print("Temperature violin plot title updated")
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
    print("Body size pie chart title updated")
    
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
    print("IUCN pie chart title updated")
    
    # ---------------------  Richness plots --------------------- #   
    ## needs x-axis variable (alpha_xvar)
    #updateSelectInput(session,"alpha_xvar",
    #                  choices=isolate(input$aggregate_var)) #make aggregate variable the x-axis for alpha diversity plot
    
    #updateNumericInput(session,"prune_samples",value=100)
    #updateNumericInput(session,"prune_tax",value=1)
    #updateRadioButtons(session,"normoptions",selected="tax_rel")
    updateCheckboxInput(session,"alpha_box",value=TRUE)

  
},priority=10) #priority 10 runs before priority 1

observe({
  
  req(ps.all())
  req(input$pages == "analysistab")
  req(input$aggregate_var)
  
  # Capture values of the aggregate variable and the page
  current_aggvar <- input$aggregate_var
  current_page <- input$pages # Replace with your navbar/tab ID
  
  # Ensure both requirements actually contain data before proceeding
  req(!is.null(current_aggvar)#, !is.null(current_page)
  )
  
  p <- update_reqs()
  
  # if this is the first time both are filled
  is_first_time <- is.null(p) #is_first_time==TRUE when p is null 
  
  # if p is not null  
  if (!is_first_time) { 
    aggvar_changed <- !identical(current_aggvar, p$var) #aggvar_changed == TRUE when current_aggvar != p$var
    
    #page_changed <- !identical(current_page, p$page)
    #both_changed <- aggvar_changed && page_changed
  } #else { #if is.null(p)==TRUE
  #both_changed <- FALSE
  #}
  
  
  # Trigger action on the target page if it's the first time OR if both changed
  if (current_page == "analysistab" && (is_first_time || aggvar_changed )) {
    print("Triggering plot generation after aggvar change")
    trigger_plots(trigger_plots() + 1)
    #shinyjs::click("go_violin") #simulate clicking 'go' for trophic plots
    #shinyjs::click("go_trophbar")
    
    #shinyjs::click("go_tviolin") #simulate clicking 'go' for temperature plots
    #shinyjs::click("go_tviolinmean")
    #shinyjs::click("go_tlolli")
    
    #shinyjs::click("go_sizebar") #simulate clicking 'go' for size plot
    #shinyjs::click("go_rich") #simulate clicking 'go' for richness plot
    #shinyjs::click("go_iucnbar")
    
    #if (isTruthy(ps.norm.melt() %>% select(contains("Troph")) %>% colnames())) {
    #
    #}
    
  }  
  
  update_reqs(list(var = current_aggvar, page = current_page))
  
},priority=1) #%>% bindEvent(input$aggregate_var,
               #              input$pages,
                #             input$go_aggregate)
