output$psog_filter <- renderPrint({ps.all()})
output$pstext_filter <- renderPrint({ps.sub()})

trackNums <- reactiveValues(remblank_bef=NULL,
                            remblank_aft=NULL,
                            remblanktax_bef=NULL,
                            remblanktax_aft=NULL,
                            prune_bef=NULL,
                            prune_aft=NULL,
                            prunetax_bef=NULL,
                            prunetax_aft=NULL,
                            norm_bef=NULL,
                            norm_aft=NULL,
                            selsamp_bef=NULL,
                            selsamp_aft=NULL,
                            seltax_bef=NULL,
                            seltax_aft=NULL)
observeEvent(ps.all(),{
  trackNums$remblank_bef=NULL
  trackNums$remblank_aft=NULL
  trackNums$remblanktax_bef=NULL
  trackNums$remblanktax_aft=NULL
  trackNums$prune_bef=NULL
  trackNums$prune_aft=NULL
  trackNums$prunetax_bef=NULL
  trackNums$prunetax_aft=NULL
  trackNums$norm_bef=NULL
  trackNums$norm_aft=NULL
  trackNums$selsamp_bef=NULL
  trackNums$selsamp_aft=NULL
  trackNums$seltax_bef=NULL
  trackNums$seltax_aft=NULL
  print("Filter: ps.all changed; trackNums reactiveValues reset to NULL")
  
  N_sample_df  <- N_taxa_tab <- Summ_seqs_sel <- Num_seq_subset <- Num_otu_subset <- Summ_seqs <- Num_seq_all <- Num_otu_all <- NULL #reset
  print("Filter: Counting reactiveValues reset to NULL")
  rankperc_plot <- ordplot <- histoplot <- violinplot <- hmplot <- NULL #reset plots  
  print("Filter: Reset plots because dataset was remade")
}, suspended=TRUE)

output$prune_bef <- renderText({paste("Number of samples before pruning: ",trackNums$prune_bef)}) #samples before pruning
output$prune_aft <- renderText({paste("Number of samples after pruning: ",trackNums$prune_aft)}) #samples after pruning
output$prunetax_bef <- renderText({paste("Number of taxa before pruning: ",trackNums$prunetax_bef)}) #taxa before pruning
output$prunetax_aft <- renderText({paste("Number of taxa after pruning: ",trackNums$prunetax_aft)}) #taxa after pruning

output$remblank_bef <- renderText({paste("Number of samples before: ",trackNums$remblank_bef)}) #samples before blank removal
output$remblank_aft <- renderText({paste("Number of samples after: ",trackNums$remblank_aft)}) #samples after blank removal
output$remblanktax_bef <- renderText({paste("Number of taxa before: ",trackNums$remblanktax_bef)}) #taxa before blank removal
output$remblanktax_aft <- renderText({paste("Number of taxa after: ",trackNums$remblanktax_aft)}) #taxa after blank removal

output$norm_bef <- renderText({paste("Number of taxa before normalization: ",trackNums$norm_bef)}) 
output$norm_aft <- renderText({paste("Number of taxa after normalization: ",trackNums$norm_aft)}) 

output$selsamp_bef <- renderText({paste("Number of samples before: ",trackNums$selsamp_bef)}) #samples before selection
output$selsamp_aft <- renderText({paste("Number of samples after: ",trackNums$selsamp_aft)}) #samples after selection

output$seltax_bef <- renderText({paste("Number of ASVs before: ",trackNums$seltax_bef)}) #samples before selection
output$seltax_aft <- renderText({paste("Number of ASVs after: ",trackNums$seltax_aft)}) #samples after selection

output$selrank <- renderText({paste0("Ranks chosen: ",names(taxfilter_list))})
output$seltax <- renderText({paste0("Taxa chosen: ",reactiveValuesToList(taxfilter_list))})

subfunc <- function(ps,x) {
  tryCatch(x,error=function(e){return(FALSE)})
}

# ---- # Build initial ps.sub object # -----
observeEvent(c(input$assemble_ps,input$load_ex,input$go_addtroph), {
  req(ps.all())
  ps <- ps.all()
  
  trackNums$prune_aft <- trackNums$prune_bef <- NULL #reset counts
  trackNums$prunetax_aft <- trackNums$prunetax_bef <- NULL #reset counts
  trackNums$prune_bef <- as.character(nsamples(ps.all()))
  trackNums$prunetax_bef <- as.character(ntaxa(ps.all()))
  print(c("nsamples:",nsamples(ps.all())))
  print(c("ntaxa:",ntaxa(ps.all())))
}) 

ps.sub <- reactiveVal()

observeEvent(ps.all(),{
  ps.sub(ps.all())
})

observeEvent(c(input$prune_tax,input$prune_samples),{
  req(ps.sub())
  
  ps <- subfunc(ps.sub(),
                ps.sub() %>% 
                  prune_taxa(taxa_sums(.)>=input$prune_tax,.) %>% 
                  prune_samples(sample_sums(.)>=input$prune_samples,.)
  )
  print("Filtered data by minimum sample sequence number and tax occurrence")
  if (!isTruthy(ps)) {
    showModal(
      modalDialog(title="ERROR",
                  "One or more of your subsetting criteria is too strict", 
                  size='l',
                  easyClose = FALSE
      )
    ) 
    shinyjs::reset("prune_tax")
    shinyjs::reset("prune_samples")
  }
  
  req(ps)
  print("Filter: dataset pruned by min taxa sum and sample sum")
  print(c("min taxa sum:",input$prune_tax))
  print(c("min sample sum:",input$prune_samples))
  tnames <- rank_names(ps)
  
  trackNums$prune_aft <- as.character(nsamples(ps))
  trackNums$prunetax_aft <- as.character(ntaxa(ps))
  print(c("nsamples in new dataset:",as.character(nsamples(ps))))
  print(c("ntaxa in new dataset:",as.character(ntaxa(ps))))
  
  ps.sub(ps)
  
})

# ---- # Update picker variable selection # ---- 
observe({ 
  req(Meta_vars())
  updateSelectInput(session,
                    "blank_cols",#for blank selection
                    choices=c(Meta_vars())) 
  print("Filter: updated blank selection options")
  
  updateSelectInput(session,
                    "sample_filterby",#for sample selection
                    choices=c(Meta_vars()))
  print("Filter: updated sample selection options")
  
}) %>% 
  bindEvent(Meta_vars()) #respond to any changes in the Metadata file that is uploaded

#observeEvent(input$tax_switch, { #whenever switch to subset taxa is triggered
#  print("Filter: switch to subset taxa was triggered")
#  req(ps.sub())
#  updateSelectInput(inputId = 'tax_filterby', 
#                    label = 'Taxonomic Rank', 
#                    choices = c(as.list(rank_names(ps.sub())))
#  ) #update picker selection using the rank names of the ps object
#  print("Filter: updated taxonomic filter options")
#}, suspended=TRUE) 

# ---- # Update blank variable options # ---- 
observe({ 
  req(ps.sub())
  req(input$blank_cols)
  
  print("Filter: Getting unique values of selected negative control variable")
  blavar <- as.list(get_variable(ps.sub(),{input$blank_cols})) %>% unique() #unique list of values 
  print("Filter: Unique values of selected negative control variable identified")
  
  updateCheckboxGroupInput(session,
                           "blank_samples",
                           choices=as.list(blavar))
  print("Filter: List of available values to filter by neg controls updated")
  
}) %>% bindEvent(input$blank_cols, #update blank selection if a new column is selected
                 MetadataTable$x#, #update blank selection if a new metadata table is loaded 
                 #input$load_ex #update blank selection if 'load' button is pressed
) 

# ---- # Update sample variable filter and accordion # ---- 
observe({
                 if(isTruthy(input$sample_switch)) { #if sample_switch==TRUE
                   print("Toggle to select specific samples to view triggered...")
                   accordion_panel_open(session, #open the accordion panel to allow selecting samples  
                                        id='sample_accord',
                                        values=TRUE)
                   print("Filter: sample selection accordion panel opened")
                   
                 } else if (!isTruthy(input$sample_switch)) {
                   print("Filter: sample_switch==FALSE")
                   accordion_panel_close(session, #close the accordion panel to allow selecting samples  
                                         id='sample_accord',
                                         values=TRUE)
                   print("Filter: sample selection accordion panel closed")
                   updateCheckboxGroupInput(session,
                                            'sample_select',
                                            choices=c(""))
                   print("Filter: available sample filtering options removed")
                   shinyjs::hide("befaft_sample")
                   shinyjs::reset("sample_filterby")
                   print("Filter: before/after sample selection count hidden")
                 }
               }) %>% bindEvent(input$sample_switch)#,
#input$sample_filterby,
#MetadataTable$x
#) 

observeEvent(input$sample_filterby, {
  req(ps.sub())
  
  selvar <- get_variable(ps.sub(), {input$sample_filterby}) %>% unique() #unique list of values 
  
  print("Filter: sample variables identified")
  print(selvar)
  #selvar <- as.list(MetadataTable$x[[input$sample_filterby]]) %>% unique() #unique list of values 
  updateCheckboxGroupInput(session,
                           "sample_select",
                           choices=as.list(selvar)) #update the available filtering options
  print("Filter: available sample filtering options updated")
  
})

# ---- # Show sample numbers # ----  
observeEvent(input$go_sample, {
  req(ps.sub())
  
  if(isTruthy(input$sample_switch) && !is.null(input$sample_select)) {
    print("Filter: Subsetting by selected samples")
    
    shinyjs::show("befaft_sample")
    print("Filter: before/after sample selection count shown")
    
  } #else if(!isTruthy(input$sample_switch) && !is.null(input$sample_select)) { } #impossible because you can't click go_sample without the input$sample_switch being TRUE
  #else if(!isTruthy(input$sample_switch) && is.null(input$sample_select)) { } #impossible because you can't click go_sample without the input$sample_switch being TRUE
  else if(isTruthy(input$sample_switch) && is.null(input$sample_select)) {
    print("Filter: No samples selected but button to subset samples clicked")
  }
}) #%>% bindEvent(input$sample_switch,
#               input$go_sample,
#               ignoreInit=TRUE)


# ---- #  Update taxa filter and accordion # ----  
taxfilter_list <- reactiveValues() #make list to record what taxa are used to filter and what rank they belong to

observe({
                 if(isTruthy(input$tax_switch)) { #if tax subset panel is open
                   print("Filter: switch to subset taxa was triggered")
                   
                   accordion_panel_open(session, #open the accordion panel 
                                        id='tax_accord',
                                        values=TRUE)
                   print("Filter: taxa selection accordion panel opened")
                   
                   req(ps.sub())
                   updateSelectInput(inputId = 'tax_filterby', 
                                     label = 'Taxonomic Rank', 
                                     choices = c(as.list(rank_names(ps.sub())))
                   ) #update picker selection using the rank names of the ps object
                   print("Filter: updated taxonomic filter options")
                  
                 } else if (!isTruthy(input$tax_switch)) { #if tax subset panel is closed 
                   print("Filter: tax_switch==FALSE")
                   
                   accordion_panel_close(session, #close the accordion panel 
                                         id='tax_accord',
                                         values=TRUE)
                   print("Filter: taxa selection accordion panel closed")
                   
                   updateCheckboxGroupInput(session, #clear selectable taxa 
                                            'tax_select',
                                            choices=c(""),
                                            selected=NULL)
                   updateSelectInput(inputId = 'tax_filterby',
                                     label = 'Taxonomic Rank',
                                     choices = c(""))
                   print("Filter: available tax filtering options removed")
                   
                   shinyjs::hide("befaft_tax") #hide counts
                   print("Filter: before/after tax selection count hidden")
                   shinyjs::reset("tax_filterby")
                   taxfilter_list <- NULL #reset the list of taxa used to filter
                   print("Filter: list of taxa used to filter cleared")
                 }
               }) %>% bindEvent(input$tax_switch)

observeEvent(input$tax_filterby, {
  req(ps.sub())
  
  taxvar <- as.list(get_taxa_unique(ps.sub(),input$tax_filterby)) #make list of unique values 
  print(c("Filter: unique taxa for", input$tax_filterby,"identified"))
  print(taxvar)
  
  updateCheckboxGroupInput(session,
                           "tax_select",
                           choices=as.list(taxvar),
                           selected=NULL) #update the available filtering options
  
  print("Filter: available taxa filtering options updated")
})

# ---- # Show taxa numbers # ----  
observe({
  req(ps.sub())
  if(isTruthy(input$tax_switch && input$go_tax && !is.null(input$tax_select))) {
    print("Subsetting by selected taxa")
    shinyjs::show("befaft_tax")
    print("Filter: before/after tax selection count shown")
  }
}) %>% bindEvent(input$tax_switch,
                 input$go_tax,
                 ignoreInit=TRUE)
##############################################
ps.norm <- reactive({
  req(ps.all())
  return(ps.all())
})