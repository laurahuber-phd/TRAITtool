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
  
  N_sample_df  <- N_taxa_tab <- Summ_seqs_sel <- Num_seq_subset <- Num_otu_subset <- Summ_seqs <- Num_seq_all <- Num_otu_all <- NULL #reset
  rankperc_plot <- ordplot <- histoplot <- violinplot <- hmplot <- NULL #reset plots  
})

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

# ---- # Update picker variables # ---- 
observe({ 
  req(Meta_vars())
  updateSelectInput(session,
                    "blank_cols",#for blank selection
                    choices=c(Meta_vars())) 
  updateSelectInput(session,
                    "sample_filterby",#for sample selection
                    choices=c(Meta_vars())) 
}) %>% 
  bindEvent(Meta_vars()) #respond to any changes in the Metadata file that is uploaded

observeEvent(input$tax_switch, { #whenever switch to subset taxa is triggered
  req(ps.sub())
  updateSelectInput(inputId = 'tax_filterby', 
                    label = 'Taxonomic Rank', 
                    choices = c(as.list(rank_names(ps.sub())))
  ) #update picker selection using the rank names of the ps object
}) 

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
    req(input$sample_filterby)
    selvar <- as.list(get_variable(ps.sub(),{input$sample_filterby})) %>% unique() #unique list of values 
    #selvar <- as.list(MetadataTable$x[[input$sample_filterby]]) %>% unique() #unique list of values 
    updateCheckboxGroupInput(session,
                             "sample_select",
                             choices=as.list(selvar)) #update the available filtering options
    
  } else if (!isTruthy(input$sample_switch)) {
    
    accordion_panel_close(session, #close the accordion panel to allow selecting samples  
                          id='sample_accord',
                          values=TRUE)
    
    updateCheckboxGroupInput(session,
                             'sample_select',
                             choices=c(""))
    
    shinyjs::hide("befaft_sample")
  }
}) %>% bindEvent(input$sample_switch,
                 input$sample_filterby,
                 MetadataTable$x
                 ) 

observe({
  req(ps.sub())
  if(isTruthy(input$sample_switch) && input$go_sample && !is.null(input$sample_select)) {
    shinyjs::show("befaft_sample")
  } 
}) %>% bindEvent(input$sample_switch,
                 input$go_sample,
                 ignoreInit=TRUE)

# ---- # Filter by taxa # ----  
taxfilter_list <- reactiveValues() #make list to record what taxa are used to filter and what rank they belong to

observe({
  if(isTruthy(input$tax_switch)) { #if tax subset panel is open
    
    accordion_panel_open(session, #open the accordion panel 
                         id='tax_accord',
                         values=TRUE)
    req(input$tax_filterby)
    
    taxvar <- as.list(get_taxa_unique(ps.sub(),input$tax_filterby)) #make list of unique values 
    updateCheckboxGroupInput(session,
                             "tax_select",
                             choices=as.list(taxvar),
                             selected=NULL) #update the available filtering options
    
  } else if (!isTruthy(input$tax_switch)) { #if tax subset panel is closed 
    
    accordion_panel_close(session, #close the accordion panel 
                          id='tax_accord',
                          values=TRUE)
    
    updateCheckboxGroupInput(session, #clear selectable taxa 
                             'tax_select',
                             choices=c(""),
                             selected=NULL)
    
    shinyjs::hide("befaft_tax") #hide counts
    taxfilter_list <- NULL #reset the list of taxa used to filter
  }
}) %>% bindEvent(input$tax_switch,
                 input$tax_filterby,
                 ps.sub(),
                 #input$tax_keepremove,
                 MetadataTable$x
                 ) 

observe({
  req(ps.sub())
  if(isTruthy(input$tax_switch && input$go_tax && !is.null(input$tax_select))) {
    shinyjs::show("befaft_tax")
  }
}) %>% bindEvent(input$tax_switch,
                 input$go_tax,
                 ignoreInit=TRUE)

subfunc <- function(ps,x) {
  tryCatch(x,error=function(e){return(FALSE)})
}

# ---- # Do the filtering # -----
ps.sub <- reactive({
  req(ps.all())
  trackNums$prune_aft <- trackNums$prune_bef <- NULL #reset counts
  trackNums$prunetax_aft <- trackNums$prunetax_bef <- NULL #reset counts
  trackNums$prune_bef <- as.character(nsamples(ps.all()))
  trackNums$prunetax_bef <- as.character(ntaxa(ps.all()))
    
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
    shinyjs::reset("prune_tax")
    shinyjs::reset("prune_samples")
  }
  
  req(ps)
  tnames <- rank_names(ps)
  trackNums$prune_aft <- as.character(nsamples(ps))
  trackNums$prunetax_aft <- as.character(ntaxa(ps))
  
  ############################### BLANKS ############################################
  if(isTruthy(input$rem_blanks)) { #if remove blanks is selected
    trackNums$remblank_bef <- trackNums$remblank_aft <- NULL #reset counts
    trackNums$remblanktax_bef <- trackNums$remblanktax_aft <- NULL  #reset counts
    trackNums$remblank_bef <- as.character(nsamples(ps))
    trackNums$remblanktax_bef <- as.character(ntaxa(ps))
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
      trackNums$remblanktax_aft <- as.character(ntaxa(ps))
    }
    else {
      ps <- ps
    }
  }
  ############################### SAMPLES ###########################################   
  if(isTruthy(input$sample_switch) && input$go_sample) { #if user selects specific samples to vis
    runif(input$go_sample)
    trackNums$selsamp_aft <- trackNums$selsamp_bef <- NULL #reset counts
    trackNums$selsamp_bef <- as.character(nsamples(ps))
    if(!is.null(input$sample_select)) {
      selkeep = NULL #make empty list of samples 
      
      if(input$sample_filterby == "Sample") { #if the filtering parameter is sample id
        selkeep = input$sample_select #list of selected samples 
      } else {
        selcolvals = as.data.frame(as.matrix(sample_data(ps))) %>% select(input$sample_filterby) #get sample_data of selected variable 
        selkeep = row.names(selcolvals %>% filter(selcolvals[[1]] %in% input$sample_select)) #list of sample names selected
      }
      ps <- prune_samples(selkeep,ps)
    } else {
      ps <- ps
    }
    trackNums$selsamp_aft <- as.character(nsamples(ps))
  }
  ############################### TAXA ############################################
  if(isTruthy(input$tax_switch) && input$go_tax) { #if remove selected taxa is selected
    runif(input$go_tax)
  
    trackNums$seltax_aft <- trackNums$seltax_bef <- NULL #reset counts
    trackNums$seltax_bef <- as.character(ntaxa(ps))
    
    taxkeep = NULL #make blank list
    taxsam = as.data.frame(as.matrix(tax_table(ps))) #extract taxonomy table
    taxsamsel = select(taxsam,input$tax_filterby) #subset taxonomy table by the rank selected by the user
    
    # make a new sublist each time go_tax is clicked 
    selected_taxalist <- input$tax_select 
    selected_taxalist[selected_taxalist=="NA"] <- NA
    selected_taxalist[selected_taxalist==""] <- NA
      
    taxfilter_list[[input$tax_filterby]] <- list(selected_taxalist) #list(input$tax_select)

      # prune taxa 
    if(isTruthy(input$tax_keepremove)) {  #if user selects keep taxa
      if (any(is.na(selected_taxalist))) { #if any of the selected taxa are NA
        
        taxkeep = tryCatch(rownames(filter(taxsamsel, 
                                           taxsamsel[[1]] %in% selected_taxalist | is.na(taxsamsel[[1]])
                                           )),
                           error=function(e){return(FALSE)})
      }
      else { #if NA is not selected
      taxkeep = tryCatch(rownames(filter(taxsamsel, taxsamsel[[1]] %in% selected_taxalist)), #keep taxa from user list
                         error=function(e){return(FALSE)}) #return FALSE if filtering cannot happen
      }
    }
    if(!isTruthy(input$tax_keepremove)) { #if user selected remove taxa
      if (any(is.na(selected_taxalist))) { #if any of the selected taxa are NA
        
        taxkeep = tryCatch(rownames(filter(taxsamsel, 
                                           !taxsamsel[[1]] %in% selected_taxalist | !is.na(taxsamsel[[1]])
        )),
        error=function(e){return(FALSE)})
      }
      else { 
      taxkeep =  tryCatch(rownames(filter(taxsamsel, 
                                          !taxsamsel[[1]] %in% selected_taxalist)),
                          error=function(e){return(FALSE)})
      }
    }
    if (isTruthy(taxkeep)) {
      ps <- prune_taxa(taxkeep,ps)
    } else if (is.null(input$tax_select)) {
      ps <- ps
    } else { 
      ps <- ps 
      showModal(
        modalDialog(title="ERROR",
                    "Selected taxa cannot be removed. Reset and try again.",
                    size='l',
                    easyClose = FALSE )
        ) 
      }
    
    trackNums$seltax_aft <- as.character(ntaxa(ps))
  } 
 ps <- ps %>% prune_samples(sample_sums(.)>0,.) %>% prune_taxa(taxa_sums(.)>0,.)
  return(ps)
}) %>% bindEvent(input$prune_tax,input$prune_samples, #update ps when pruning numbers change
                 
                 input$rem_blanks,#update ps when remove blanks is selected
                 #input$blank_samples, #update ps when selected blank samples change
                 
                 input$sample_switch, #update ps when switch to remove samples is toggled
                 #input$tax_keepremove, #update ps whenever switch from keep/remove taxa
                 input$tax_switch, #update ps when switch to remove taxa is toggled 
                 
                 #input$tax_filterby, #update when rank is selected (this is updated whenever tax_switch is updated too)
                 #input$tax_select,
                 input$go_sample, #update ps when 'go' is clicked to subset samples
                 input$go_tax, #update ps when 'go' is clicked to subset taxa 
                 
                 input$assemble_ps, #update ps when assemble is clicked
                 input$load_ex, #update ps when example data is loaded 
                 input$go_addtroph #update ps when adding trait data
                 )

ps.norm <- reactive({
  req(ps.all())
  req(ps.sub())
  ps <- ps.sub()

  #if(input$normoptions != "none") {TRANSFORM(ps,input$normoptions)}
  if(input$normoptions=="tax_count") {psnorm <- subfunc(ps,TRANSFORM(ps,input$normoptions,abund_cutoff=input$abund_cutoff))}
  if(input$normoptions=="tax_rel") {psnorm <- subfunc(ps,TRANSFORM(ps,input$normoptions,mean_cutoff=input$mean_cutoff))}
  if(input$normoptions!="tax_count" && input$normoptions!="tax_rel") {psnorm <- subfunc(ps,TRANSFORM(ps,input$normoptions))}
  if(input$normoptions=="none") {psnorm <- ps}
  
  if (!isTruthy(psnorm) && input$normoptions!="none") {
    showModal(
      modalDialog(title="ERROR",
                  "One or more of your subsetting criteria is too strict",
                  size='l',
                  easyClose = FALSE
      )
    ) 
    shinyjs::reset("abund_cutoff")
    shinyjs::reset("mean_cutoff")
  }
  
  req(psnorm)
  psnorm
  
}) %>% bindEvent(input$normoptions,
                 input$abund_cutoff,
                 input$mean_cutoff,
                 ps.sub(),
                 ps.all()
)

observe({
  req(ps.norm())
  
  if(input$normoptions=="none") {shinyjs::hide('befaft_norm')}
  else {    
    trackNums$norm_bef <- isolate(as.character(ntaxa(ps.sub()) ))
    trackNums$norm_aft <- isolate(as.character(ntaxa(ps.norm()) ))
    shinyjs::show("befaft_norm")
    }
})
