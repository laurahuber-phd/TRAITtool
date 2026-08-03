source("server/server-navpanel-analysis-temp.R",local=TRUE)
source("server/server-navpanel-analysis-trait.R",local=TRUE)
source("server/server-navpanel-analysis-violin.R",local=TRUE)

observe({
  req(ps.all())
  req(input$pages == "analysistab")
  req(input$aggregate_var)
  
  print("Default: Default analysis selected")
  #aggvar <- input$aggregate_var
  # update trophic level plots  
  ## violin plot 
  ### needs trophic level to plot (violinplot_general$y) 
  #### (currently selects the first value in this list: ps.norm.melt() %>% select(contains("roph")) %>% colnames())
  #
  ### needs x-axis variable (violinplot_general$x)
  #### (currently selects the first value in this list: Meta_vars())
  #
  
  ## feeding guild  
  ### needs size selection to plot (input$choose_sizebar_axis)
  #### currently selects the first value in this list: ps.norm.melt() %>% select(contains("ength")) %>% colnames()
  #
  ## to update the aggregate variable
  #updateSelectInput(session,"choose_sizebar_facet",
  #                  choices=isolate(input$aggregate_var))
  updatePickerInput(session,
                    "choose_trophbar_facet",
                    #choices=c(Meta_vars()),
                    selected=isolate(input$aggregate_var)
  )
  print(c("trophbar facet picker variable updated to",isolate(input$aggregate_var)))
  
  # update temperature plots  
  ## violin plot  
  ### needs x-axis variable (tviolinplot_general$x)
  #
  ### needs y-axis variable (tviolinplot_general$y)
  #### currently selects the first value in this list: ps.norm.melt() %>% select(contains("emp")) %>% colnames()
  # 
 # tviolinplot_general$x <- reactive(aggvar)

  ## lollipop plot  
  ### needs y-axis variable (choose_tlolli_yaxis)
  #updatePickerInput(session,"choose_tlolli_yaxis",
 #                   choices=isolate(input$aggregate_var)) #make aggregate variable the y-axis for temperature lollipop plot
  
  # update body size plots 
  ## pie chart 
  ### needs facet variable (choose_sizebar_facet)
  updatePickerInput(session,
                    "choose_sizebar_facet",
                    choices=c(Meta_vars()),
                    selected=isolate(input$aggregate_var))
  
  print(c("sizebar facet picker variable updated to",isolate(input$aggregate_var)))
  
  # update richness plots  
  ## needs x-axis variable (alpha_xvar)
  #updateSelectInput(session,"alpha_xvar",
  #                  choices=isolate(input$aggregate_var)) #make aggregate variable the x-axis for alpha diversity plot
  
  
  
#"violin_s_plot"
#"violin_s_click"
#"trophbar_s_plot"
#"trophbar_s_click"

#"tviolin_s_plot"
#"tviolin_s_click"
#"tlolli_s_plot"
#"tlolli_s_click"

#"sizebar_s_plot"
#"sizebar_s_click"
#"rich_s_plot"
#"rich_s_click"

#updateNumericInput(session,"prune_samples",value=100)
#updateNumericInput(session,"prune_tax",value=1)
#updateRadioButtons(session,"normoptions",selected="tax_rel")

shinyjs::click("go_violin") #simulate clicking 'go' for trophic plots
shinyjs::click("go_trophbar")

shinyjs::click("go_tviolin") #simulate clicking 'go' for temperature plots
shinyjs::click("go_tlolli")

shinyjs::click("go_sizebar") #simulate clicking 'go' for size plot
shinyjs::click("go_rich") #simulate clicking 'go' for richness plot

#if (isTruthy(ps.norm.melt() %>% select(contains("Troph")) %>% colnames())) {
#
#}

},priority=10) %>% bindEvent(input$aggregate_var,input$pages,input$go_aggregate)
