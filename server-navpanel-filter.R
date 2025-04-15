output$psog_filter <- renderPrint({ps.all()})
output$pstext_filter <- renderPrint({ps.sub()})

trackNums <- reactiveValues(remblank_bef=NULL,
                            remblank_after=NULL,
                            prune_before=NULL,
                            prune_after=NULL,
                            selsamp_bef=NULL,
                            selsam_aft=NULL)
observeEvent(ps.all(),{
  trackNums$remblank_bef=NULL
  trackNums$remblank_after=NULL
  trackNums$prune_before=NULL
  trackNums$prune_after=NULL
  trackNums$selsamp_bef=NULL
  trackNums$selsam_aft=NULL
})

output$prune_bef <- renderText({paste("Number of samples before pruning: ",trackNums$prune_bef)}) #samples before blank removal
output$prune_aft <- renderText({paste("Number of samples after pruning: ",trackNums$prune_aft)}) #samples after blank removal

output$remblank_bef <- renderText({paste("Number of samples before: ",trackNums$remblank_bef)}) #samples before blank removal
output$remblank_aft <- renderText({paste("Number of samples after: ",trackNums$remblank_aft)}) #samples after blank removal

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

observe({ #whenever ps.sub() changes 
  updateSelectInput(inputId = 'select_rank', 
                    label = 'Taxonomic Rank', 
                    choices = c(rnames())
  ) #update picker selection using the rank names of the ps object
}) %>% bindEvent(ps.sub())   

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
  blavar <- as.list(MetadataTable$x[[input$blank_cols]]) %>% unique() #unique(get_variable(ps.sub(),input$blank_cols))
  updateCheckboxGroupInput(session,
                           "blank_samples",
                           choices=as.list(blavar))
}) %>% bindEvent(input$blank_cols, #update blank selection if a new column is selected
                 MetadataTable$x#, #update blank selection if a new metadata table is loaded 
                 #input$load_ex #update blank selection if 'load' button is pressed
) 

#  ---- # Filter by samples # ---- 
observe({
  if(input$sample_switch) { #if sample_switch==TRUE
    
    accordion_panel_open(session, #open the accordion panel to allow selecting samples  
                         id='sample_accord',
                         values=TRUE)
    
    selvar <- as.list(MetadataTable$x[[input$sample_cols]]) %>% unique() #unique list of values 
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
    
  }
}) %>% bindEvent(input$sample_switch,
                 input$sample_cols,
                 MetadataTable$x
                 ) 

# ---- # Filter by taxa # ----  
observe({
  if(input$taxa_switch) { #if taxa_switch==TRUE
    
    accordion_panel_open(session, #open the accordion panel to allow selecting samples  
                         id='select_taxaccord',
                         values=TRUE)
    
    taxvar <- as.list(get_taxa_unique(ps.sub(),input$select_rank)) #unique list of values 
    updateCheckboxGroupInput(session,
                             "select_taxa",
                             choices=as.list(taxvar)) #update the available filtering options
    
  } else if (!isTruthy(input$taxa_switch)) {
    
    accordion_panel_close(session, #close the accordion panel to allow selecting samples  
                          id='select_taxaccord',
                          values=TRUE)
    
    updateCheckboxGroupInput(session,
                             'select_taxa',
                             choices=c(""))
    
  }
}) %>% bindEvent(input$taxa_switch,
                 input$select_rank,
                 MetadataTable$x
                 ) 

subfunc <- function(ps,x) {
  tryCatch(x,error=function(e){return(FALSE)})
}

# ---- # Do the filtering # -----
ps.sub <- reactive({
  req(ps.all())
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
  
  #####################################################################################
  if(isTruthy(input$rem_blanks)) { #if remove blanks is selected
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
    }
    else {
      ps <- ps
    }
  }
  ####################################################################################   
  if(isTruthy(input$sample_switch) && input$go_samples) { #if user selects specific samples to vis
    runif(input$go_samples)
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
  }
  ###################################################################################
  if(isTruthy(input$taxa_switch) && input$go_taxa) { #if user selects specific taxa to vis 
    taxkeep = NULL #make blank list
    taxsam = as.data.frame(as.matrix(tax_table(ps)))
    
    if(isTruthy(input$adjust)) { #if keep selected taxa is selected
      taxsamsel = select(taxsam,input$select_rank)
      taxkeep = rownames(filter(taxsamsel, taxsamsel[[1]] %in% input$select_taxa))
    } else { #if remove selected taxa is selected
      taxsamsel = select(taxsam,input$select_rank)
      taxkeep = rownames(filter(taxsamsel, !taxsamsel[[1]] %in% input$select_taxa))
    }
    ps <- prune_taxa(taxkeep,ps)
  }
  return(ps)
}) %>% bindEvent(input$prune_tax,input$prune_samples,
                 input$adjust,
                 input$rem_blanks,input$blank_samples,
                 input$assemble_ps,
                 input$sample_switch,input$go_samples,
                 input$go_taxa,
                 input$load_ex)

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
