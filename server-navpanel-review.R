source("module-readtable.R")

tableServer(id='meta_table',
            editable=TRUE,
            #data=MetadataTable(x) #Error in MetadataTable(x) : could not find function "MetadataTable"
            data=MetadataTable #doesnt crash the app but won't upload
            #data=MetadataTable$x
            #data=reactive(MetadataTable$x)
) 
tableServer(id='otu_table',
            data=OTUTable #doesnt crash the app but won't upload
) 
tableServer(id='tax_table',
            data=TaxTable #doesnt crash the app but won't upload
) 

#observeEvent() will respond to events caused by reactive inputs
#observe({...}) %>% bindEvent() is equivalent and is the preferred way to do it because it can be used with render functions as well 
observe({ 
  updateSelectInput(session,
                    "cols",#reactive input=metadata upload; response=return column names as choice options
                    choices=Meta_vars()) 
  updateSelectInput(session,
                    "choose_color", #response=return column names as color choice options
                    choices=c("None selected", Meta_vars()))
  updateSelectInput(session,
                    "choose_shape", #use column names as shape options
                    choices=c("None selected",Meta_vars()))
  updateSelectInput(session,
                    "choose_fill", #use column names as fill options
                    choices=c("None selected",Meta_vars()))
  updateSelectInput(session,
                    "choose_ellipse", #use column names as ellipse grouping options
                    choices=c('None selected',Meta_vars()))
}) %>% 
  bindEvent(Meta_vars()) #respond to any changes in the Metadata file that is uploaded
observe({ 
  blavar <- MetadataTable$x[[input$cols]]
  updateCheckboxGroupInput(session,
                           "blank_samples",
                           choices=as.list(blavar))
}) %>% bindEvent(input$cols, #update blank selection if a new column is selected
                 MetadataTable$x, #update blank selection if a new metadata table is loaded
                 input$load_ex #update blank selection if 'load' button is pressed
) 
observeEvent(ps.sub(),{ #reactive input=the ps object we're using
  updatePickerInput(inputId = 'ranks',
                    label = 'Taxonomic Rank', #reaction=update picker selection using the rank names of the ps object
                    choices = rank_names(ps.sub()))
})  
observe({
  #req(input$Metadata)
  updateSelectInput(session,
                    "cols2",
                    choices=c(Meta_vars())
  )
}) %>% bindEvent(Meta_vars(),
                 input$select_switch
)

observe({
  #req(input$Metadata)
  if(input$select_switch) {
    accordion_panel_open(session,
                         id='select_accord',
                         values="Filters")
    selvar <- MetadataTable$x[[input$cols2]]
    updateCheckboxGroupInput(session,
                             "select_samples",
                             choices=as.list(selvar))
  } else if (input$select_switch==FALSE) {
    accordion_panel_close(session,
                          id='select_accord',
                          values="Filters")
    updateCheckboxGroupInput(session,
                             'select_samples',
                             choices=c(""))
  }
}) %>% bindEvent(input$select_switch,
                 input$cols2,
                 MetadataTable$x
) 