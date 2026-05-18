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

##############################################
ps.norm <- reactive({
  req(ps.all())
  return(ps.all())
})