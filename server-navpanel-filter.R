output$psog_filter <- renderPrint({ps.all()})
output$pstext_filter <- renderPrint({ps.sub()})

trackNums <- reactiveValues(remblank_bef=NULL,
                            remblank_aft=NULL,
                            prune_bef=NULL,
                            prune_aft=NULL,
                            selsamp_bef=NULL,
                            selsamp_aft=NULL,
                            seltax_bef=NULL,
                            seltax_aft=NULL)
observeEvent(ps.all(),{
  trackNums$remblank_bef=NULL
  trackNums$remblank_aft=NULL
  trackNums$prune_bef=NULL
  trackNums$prune_aft=NULL
  trackNums$selsamp_bef=NULL
  trackNums$selsamp_aft=NULL
  trackNums$seltax_bef=NULL
  trackNums$seltax_aft=NULL
})

output$prune_bef <- renderText({paste("Number of samples before pruning: ",trackNums$prune_bef)}) #samples before pruning
output$prune_aft <- renderText({paste("Number of samples after pruning: ",trackNums$prune_aft)}) #samples after pruning

output$remblank_bef <- renderText({paste("Number of samples before: ",trackNums$remblank_bef)}) #samples before blank removal
output$remblank_aft <- renderText({paste("Number of samples after: ",trackNums$remblank_aft)}) #samples after blank removal

output$selsamp_bef <- renderText({paste("Number of samples before: ",trackNums$selsamp_bef)}) #samples before selection
output$selsamp_aft <- renderText({paste("Number of samples after: ",trackNums$selsamp_aft)}) #samples after selection

output$seltax_bef <- renderText({paste("Number of ASVs before: ",trackNums$seltax_bef)}) #samples before selection
output$seltax_aft <- renderText({paste("Number of ASVs after: ",trackNums$seltax_aft)}) #samples after selection

# ---- # Update picker variables # ---- 
observe({ 
  req(Meta_vars())
  updateSelectInput(session,
                    "blank_cols",#for blank selection
                    choices=c(Meta_vars())) 
  updateSelectInput(session,
                    "sample_cols",#for sample selection
                    choices=c(Meta_vars())) 
}) %>% 
  bindEvent(Meta_vars()) #respond to any changes in the Metadata file that is uploaded

observeEvent(input$taxa_switch, { #whenever switch to subset taxa is triggered
  req(ps.sub())
  updateSelectInput(inputId = 'select_rank', 
                    label = 'Taxonomic Rank', 
                    choices = c(as.list(rank_names(ps.sub())))
  ) #update picker selection using the rank names of the ps object
}) 

Meta_vars <- reactive({ #read the metadata file and identify the variable names 
  #req(input$Metadata)
  return(names(MetadataTable$x))
}) %>% bindEvent(input$load_ex, #update meta_vars if the load button is pressed (new dataset)
                 input$Metadata, #update meta_vars if a new metadata table is uploaded manual
                 MetadataTable$x
                 )

rnames <- reactive({ #read the metadata file and identify the variable names 
  #req(input$Metadata)
  return(rank_names(ps.sub()))
}) %>% bindEvent(ps.sub() )

# ---- # Filter by blanks # ---- 
observe({ 
  req(ps.sub())
  req(input$blank_cols)
  
  blavar <- as.list(get_variable(ps.sub(),{input$blank_cols})) %>% unique() #unique list of values 
  updateCheckboxGroupInput(session,
                           "blank_samples",
                           choices=as.list(blavar))
}) %>% bindEvent(input$blank_cols, #update blank selection if a new column is selected
                 MetadataTable$x#, #update blank selection if a new metadata table is loaded 
                 #input$load_ex #update blank selection if 'load' button is pressed
) 

#  ---- # Filter by samples # ---- 
observe({
  if(isTruthy(input$sample_switch)) { #if sample_switch==TRUE
    
    accordion_panel_open(session, #open the accordion panel to allow selecting samples  
                         id='sample_accord',
                         values=TRUE)
    req(input$sample_cols)
    selvar <- as.list(get_variable(ps.sub(),{input$sample_cols})) %>% unique() #unique list of values 
    #selvar <- as.list(MetadataTable$x[[input$sample_cols]]) %>% unique() #unique list of values 
    updateCheckboxGroupInput(session,
                             "select_samples",
                             choices=as.list(selvar)) #update the available filtering options
    
  } else if (!isTruthy(input$sample_switch)) {
    
    accordion_panel_close(session, #close the accordion panel to allow selecting samples  
                          id='sample_accord',
                          values=TRUE)
    
    updateCheckboxGroupInput(session,
                             'select_samples',
                             choices=c(""))
    
    shinyjs::hide("befaft_sample")
  }
}) %>% bindEvent(input$sample_switch,
                 input$sample_cols,
                 MetadataTable$x
                 ) 

observe({
  req(ps.sub())
  if(isTruthy(input$sample_switch) && input$go_samples && !is.null(input$select_samples)) {
    shinyjs::show("befaft_sample")
  } 
}) %>% bindEvent(input$sample_switch,
                 input$go_samples,
                 ignoreInit=TRUE)

# ---- # Filter by taxa # ----  
observe({
  if(isTruthy(input$taxa_switch)) { #if tax subset panel is open
    
    accordion_panel_open(session, #open the accordion panel 
                         id='select_taxaccord',
                         values=TRUE)
    req(input$select_rank)
    
    taxvar <- as.list(get_taxa_unique(ps.sub(),input$select_rank)) #make list of unique values 
    updateCheckboxGroupInput(session,
                             "select_taxa",
                             choices=as.list(taxvar)) #update the available filtering options
    
  } else if (!isTruthy(input$taxa_switch)) { #if tax subset panel is closed 
    
    accordion_panel_close(session, #close the accordion panel 
                          id='select_taxaccord',
                          values=TRUE)
    
    updateCheckboxGroupInput(session, #clear selectable taxa 
                             'select_taxa',
                             choices=c(""))
    
    shinyjs::hide("befaft_tax") #hide counts
  }
}) %>% bindEvent(input$taxa_switch,
                 input$select_rank,
                 input$adjust,
                 MetadataTable$x
                 ) 

observe({
  req(ps.sub())
  if(isTruthy(input$taxa_switch && input$go_taxa && !is.null(input$select_taxa))) {
    shinyjs::show("befaft_tax")
  }
}) %>% bindEvent(input$taxa_switch,
                 input$go_taxa,
                 ignoreInit=TRUE)

subfunc <- function(ps,x) {
  tryCatch(x,error=function(e){return(FALSE)})
}

# ---- # Do the filtering # -----
ps.sub <- reactive({
  req(ps.all())
  trackNums$prune_aft <- trackNums$prune_bef <- NULL #reset counts
  trackNums$prune_bef <- as.character(nsamples(ps.all()))
    
  ps <- subfunc(ps.all(),
                ps.all() %>% 
                  prune_taxa(taxa_sums(.)>=input$prune_tax,.) %>% 
                  prune_samples(sample_sums(.)>=input$prune_samples,.)
  )
  
  if (!isTruthy(ps)) {
    showModal(
      modalDialog(title="ERROR",
                  "One or more of your subsetting criteria is too strict",
                  size='l',
                  easyClose = FALSE
      )
    ) 
  }
  
  req(ps)
  tnames <- rank_names(ps)
  trackNums$prune_aft <- as.character(nsamples(ps))
  
  ############################### BLANKS ############################################
  if(isTruthy(input$rem_blanks)) { #if remove blanks is selected
    trackNums$remblank_bef <- trackNums$remblank_aft <- NULL #reset counts
    trackNums$remblank_bef <- as.character(nsamples(ps))
    blakeep = NULL #make empty list of samples
    
    if(!is.null(input$blank_samples)) { #if there are blanks selected
      if(input$blank_cols == "Sample") { #if the filtering parameter is sample id
        blasam = sample_names(ps)
        blarem = input$blank_samples #list of blank samples 
        blakeep = blasam[!blasam %in% blarem] #remove blanks from sample list 
      } else { #with any other filtering parameter
        blacolvals = as.data.frame(as.matrix(sample_data(ps))) %>% select(input$blank_cols) #get sample_data of selected variable 
        blakeep = row.names(blacolvals %>% filter(!blacolvals[[1]] %in% input$blank_samples)) #list of sample names EXCEPT for those selected
      }
      ps <- prune_samples(blakeep,ps)
      trackNums$remblank_aft <- as.character(nsamples(ps))
    }
    else {
      ps <- ps
    }
  }
  ############################### SAMPLES ###########################################   
  if(isTruthy(input$sample_switch) && input$go_samples) { #if user selects specific samples to vis
    runif(input$go_samples)
    trackNums$selsamp_aft <- trackNums$selsamp_bef <- NULL #reset counts
    trackNums$selsamp_bef <- as.character(nsamples(ps))
    if(!is.null(input$select_samples)) {
      selkeep = NULL #make empty list of samples 
      
      if(input$sample_cols == "Sample") { #if the filtering parameter is sample id
        selkeep = input$select_samples #list of selected samples 
      } else {
        selcolvals = as.data.frame(as.matrix(sample_data(ps))) %>% select(input$sample_cols) #get sample_data of selected variable 
        selkeep = row.names(selcolvals %>% filter(selcolvals[[1]] %in% input$select_samples)) #list of sample names selected
      }
      ps <- prune_samples(selkeep,ps)
    } else {
      ps <- ps
    }
    trackNums$selsamp_aft <- as.character(nsamples(ps))
  }
  ############################### TAXA ############################################
  if(isTruthy(input$adjust) && input$go_taxa) { #if remove selected taxa is selected
    runif(input$go_taxa)
  
    trackNums$seltax_aft <- trackNums$seltax_bef <- NULL #reset counts
    trackNums$seltax_bef <- as.character(ntaxa(ps))
    
    taxkeep = NULL #make blank list
    taxsam = as.data.frame(as.matrix(tax_table(ps)))
    
    taxsamsel = select(taxsam,input$select_rank)
    taxkeep = rownames(filter(taxsamsel, taxsamsel[[1]] %in% input$select_taxa))

    ps <- prune_taxa(taxkeep,ps)
    trackNums$seltax_aft <- as.character(ntaxa(ps))
  } 
  
  if(!isTruthy(input$adjust) && input$go_taxa) { #if keep selected taxa is selected
    runif(input$go_taxa)
    
    trackNums$seltax_aft <- trackNums$seltax_bef <- NULL #reset counts
    trackNums$seltax_bef <- as.character(ntaxa(ps))
    
    taxkeep = NULL #make blank list
    taxsam = as.data.frame(as.matrix(tax_table(ps)))
    
    taxsamsel = select(taxsam,input$select_rank)
    taxkeep = rownames(filter(taxsamsel, !taxsamsel[[1]] %in% input$select_taxa))
    
    ps <- prune_taxa(taxkeep,ps)
    trackNums$seltax_aft <- as.character(ntaxa(ps))
  }
 
  return(ps)
}) %>% bindEvent(input$prune_tax,input$prune_samples, #update ps when pruning numbers change
                 
                 input$rem_blanks,#update ps when remove blanks is selected
                 #input$blank_samples, #update ps when selected blank samples change
                 
                 input$sample_switch, #update ps when switch to remove samples is toggled
                 #input$adjust, #update ps whenever switch from keep/remove taxa
                 input$taxa_switch, #update ps when switch to remove taxa is toggled 
                 
                 #input$select_rank, #update when rank is selected (this is updated whenever taxa_switch is updated too)
                 #input$select_taxa,
                 input$go_samples, #update ps when 'go' is clicked to subset samples
                 input$go_taxa, #update ps when 'go' is clicked to subset taxa 
                 
                 input$assemble_ps, #update ps when assemble is clicked
                 input$load_ex #update ps when example data is loaded 
                 )

ps.norm <- reactive({
  req(ps.all())
  req(ps.sub())
  ps <- ps.sub()
  if(input$normoptions != "none") {TRANSFORM(ps,input$normoptions)}
}) %>% bindEvent(input$normoptions,
                 ps.sub()
)

ps.sub.melt <- reactive({
  req(ps.sub())
  psmelt(ps.sub())
})
