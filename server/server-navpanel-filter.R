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

subfunc <- function(x,ps) {
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

ps.prune <- reactiveValues(b=NULL,a=NULL)
ps.blank <- reactiveValues(b=NULL,a=NULL)
ps.sample <- reactiveValues(b=NULL,a=NULL)
ps.tax <- reactiveValues(b=NULL,a=NULL)

observeEvent(ps.all(),{
  ps.sub(ps.all())
  
  ps.prune <- reactiveValues(b=ps.all(),a=NULL)
  ps.blank <- reactiveValues(b=ps.all(),a=NULL)
  ps.sample <- reactiveValues(b=ps.all(),a=NULL)
  ps.tax <- reactiveValues(b=ps.all(),a=NULL)
})

observeEvent(c(input$prune_tax,input$prune_samples),{
  req(ps.sub())
  
  if(is.null(ps.prune$a)) { #if ps.prune$a is null (if pruning has not happened)
    ps.prune$b <- ps.sub()
  } #else { #if ps.prune$a is NOT null (if pruning has already happened)
    #ps.prune$b <- 
  #}
  
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
  ps.prune$a <- ps
  
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
                   shinyjs::hide("befaft_sample")
                   shinyjs::reset("sample_filterby")
                   shinyjs::reset("sample_select")
                   print("Filter: available sample filtering options removed")
                   print("Filter: before/after sample selection count hidden")
                   
                   ps.sub(ps.sample$b)
                   print(ps.sub())
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
                   
                   taxfilter_list <- NULL #reset the list of taxa used to filter
                   shinyjs::hide("befaft_tax") #hide counts
                   shinyjs::reset("tax_filterby")
                   shinyjs::reset("tax_select")
                   print("Filter: available taxa filtering options removed")
                   print("Filter: before/after tax selection count hidden")
                   
                   ps.sub(ps.tax$b)
                   print(ps.sub())
                   
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

############################### BLANKS ############################################
observe({
  req(ps.sub())
  
  if(is.null(ps.blank$a)) { #if ps.blank$a is null (if pruning has not happened)
    ps.blank$b <- ps.sub()
  } #else { #if ps.blank$a is NOT null (if pruning has already happened)
  #ps.blank$b <- 
  #}
  
  ps <- isolate(ps.sub())
  print("dataset prior to removing blanks")
  print(ps)
  
  if(isTruthy(input$rem_blanks)) { #if remove blanks is selected
    print("Filter: Remove blanks selected")
    
    #reset counts for sample number and taxa number 
    trackNums$remblank_bef <- trackNums$remblank_aft <- NULL 
    trackNums$remblanktax_bef <- trackNums$remblanktax_aft <- NULL
    trackNums$remblank_bef <- as.character(nsamples(ps))
    trackNums$remblanktax_bef <- as.character(ntaxa(ps))
    
    #make empty list of samples to keep 
    blakeep = NULL 
    
    ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
    # make list of samples to keep by removing samples identified by user #
    ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####
    if(!is.null(input$blank_samples)) { #if there are blanks selected
      print("Filter: Negative control samples identified")
      
      if(input$blank_cols == "Sample") { #if the filtering parameter is sample id
        print("Filter: negative control sample type is sample id")
        blasam = sample_names(ps)
        blarem = input$blank_samples #list of blank samples 
        blakeep = blasam[!blasam %in% blarem] #remove blanks from sample list 
        print(blarem)
        
      } else { #with any other filtering parameter
        print("Filter: negative control sample type is a sample variable")
        blacolvals = as.data.frame(as.matrix(sample_data(ps))) %>% select(input$blank_cols) #get sample_data of selected variable 
        print(c("Negative control sample variable name selected:",input$blank_cols))
        
        blakeep = row.names(blacolvals %>% filter(!blacolvals[[1]] %in% input$blank_samples)) #list of sample names EXCEPT for those selected
        print("List of samples to keep:")
        print(blakeep)
      }
      
      # use function to return FALSE if the subsetting criteria do not return a valid ps object 
      # if function returns FALSE, remove all selected blanks and uncheck 'remove blanks' button
      #shinyjs::delay(3000, #delay so it doesn't recalculate immediately when clicking samples
      ps.bla <- subfunc(prune_samples(blakeep,ps) %>% 
                          prune_samples(sample_sums(.)>0,.) %>%
                          prune_taxa(taxa_sums(.)>0,.)
      )
      #)
      print(ps.bla)
      
      if (!isTruthy(ps.bla)) { #if pruning does not work 
        showModal(
          modalDialog(title="ERROR",
                      "One or more of your subsetting criteria is too strict",
                      size='l',
                      easyClose = FALSE
          )
        ) 
        shinyjs::reset("blank_samples") #reset selected blanks
        shinyjs::reset("rem_blanks") #reset button to initiate blank removal 
      }
      
      # only move forward if subsetting is completed successfully
      req(ps.bla)
      
      trackNums$remblank_aft <- as.character(nsamples(ps.bla))
      trackNums$remblanktax_aft <- as.character(ntaxa(ps.bla))
      
      ps.sub(ps.bla)
      ps.blank$a <- ps.bla
      
      print("Filter: blank samples are pruned")
    }
    #else { #if no blanks are selected  
      #shinyjs::reset("blank_samples") 
      #shinyjs::reset("blank_cols")
      #ps.sub(ps)
      #trackNums$remblank_aft <- NULL
      #trackNums$remblanktax_aft <- NULL
    #}
  } #end of if(isTruthy(input$rem_blanks))
  else { #if !is.Truthy(input$rem_blanks)
    shinyjs::reset("blank_samples") 
    shinyjs::reset("blank_cols")
    
    print("Filter: reset negative control sample selection")
    ps.sub(ps.blank$b)
    print(ps.sub())
  }
}) %>% bindEvent(input$rem_blanks)

############################### SAMPLES ###########################################   
observe({
  if(is.null(ps.sample$a)) { #if ps.sample$a is null (if pruning has not happened)
    ps.sample$b <- ps.sub()
  } #else { #if ps.sample$a is NOT null (if pruning has already happened)
  #ps.sample$b <- 
  #}
  
  ps <- isolate(ps.sub())
  print("dataset prior to sample selection")
  print(ps)
  
  if(isTruthy(input$sample_switch) #&& input$go_sample
     ) { #if user selects specific samples to vis
    print("Filter: Samples to filter by selected")

    trackNums$selsamp_aft <- trackNums$selsamp_bef <- NULL #reset counts
    trackNums$selsamp_bef <- as.character(nsamples(ps))
    print(c("Number of samples prior to filtering",trackNums$selsamp_bef))
    
    if(!is.null(input$sample_select)) { #if there are samples selected
      selkeep = NULL #make empty list of samples 
      
      if(input$sample_filterby == "Sample") { #if the filtering parameter is sample id
        print("Filter: filtering parameter is sample id")
        selkeep = input$sample_select #list of selected samples 
        print(c("Samples to keep: ",selkeep))
      } else {
        print("Filter: filtering parameter is a sample variable")
        print(c("Filter: Sample variable to filter by is",input$sample_filterby))
        selcolvals = as.data.frame(as.matrix(sample_data(ps))) %>% select(input$sample_filterby) #get sample_data of selected variable 
        selkeep = row.names(selcolvals %>% filter(selcolvals[[1]] %in% input$sample_select)) #list of sample names selected
      }
      
      # add a check here to make sure that we can prune samples based on selkeep 
      # need to take these rownames and check them against the feature table to make sure it doesn't return a blank otu table
      # then can use to prune the ps object 
      
      ps.pr <- tryCatch(prune_samples(selkeep,ps), 
                        error=function(e){return(NULL)} #if it returns an error ps.pr will be NULL
      )
      if(isTruthy(ps.pr)) { #if ps.pr is NULL, a modal dialog box will show up that tells you that the subsetting criteria you chose will not work and will reset the filtering selections
        ps <- ps.pr
      } else {
        
        showModal(
          modalDialog(title="ERROR",
                      "One or more of your subsetting criteria is too strict",
                      size='l',
                      easyClose = FALSE
          )
        ) 
        shinyjs::reset("sample_filterby")
        shinyjs::reset("sample_select")
        shinyjs::reset("go_sample")
      }
      req(ps)
      
    } #else { #if no samples are selected
     # ps <- ps
    #}
    
    #report number of samples after filtering 
    trackNums$selsamp_aft <- as.character(nsamples(ps)) 
    print(c("Number of samples after filtering",trackNums$selsamp_aft))
    
    #reassign ps.sub 
    ps.sub(ps)
  }
  
  if(!isTruthy(input$sample_switch)) { #if sample switch is closed 
  shinyjs::reset("blank_samples") 
  shinyjs::reset("blank_cols")
  
  print("Filter: reset negative control sample selection")
  ps.sub(ps.sample$b)
  print(ps.sub())
  }
  
}) %>% bindEvent(input$go_sample)

############################### TAXA ############################################
observe({
  if(is.null(ps.tax$a)) { #if ps.tax$a is null (if pruning has not happened)
    ps.tax$b <- ps.sub()
  } #else { #if ps.tax$a is NOT null (if pruning has already happened)
  #ps.tax$b <- 
  #}
  
  ps <- isolate(ps.sub())
  print("dataset prior to tax selection")
  print(ps)
  
  if(#input$go_tax && 
     isTruthy(input$tax_switch)) { #if remove selected taxa is selected
    print("Filter: Samples to filter by selected")
    
    trackNums$seltax_aft <- trackNums$seltax_bef <- NULL #reset counts
    trackNums$seltax_bef <- as.character(ntaxa(ps))
    print(c("Number of taxa prior to filtering",trackNums$seltax_bef))
    
    taxkeep = NULL #make blank list
    taxsam = as.data.frame(as.matrix(tax_table(ps))) #extract taxonomy table
    taxsamsel = select(taxsam,input$tax_filterby) #subset taxonomy table by the rank selected by the user
    print(c("Tax ranks selected to filter by: ", input$tax_filterby))
    
    # make a new sublist each time go_tax is clicked 
    selected_taxalist <- input$tax_select 
    selected_taxalist[selected_taxalist=="NA"] <- NA
    selected_taxalist[selected_taxalist==""] <- NA
    
    taxfilter_list[[input$tax_filterby]] <- list(selected_taxalist) #list(input$tax_select)
    
    # prune taxa 
    if(isTruthy(input$tax_keepremove)) {  #if user selects keep taxa
      print("Filter: keep selected taxa")
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
      print("Filter: remove selected taxa")
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
      ps <- prune_taxa(taxkeep,ps) %>% prune_taxa(taxa_sums(.)>0,.)
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
  print("Filter: tax pruning parameters implemented")
  ps.tax$a <- ps
  ps.sub(ps)
  print(ps.sub())
}) %>% bindEvent(input$go_tax)


# ps <- ps %>% prune_samples(sample_sums(.)>0,.) %>% prune_taxa(taxa_sums(.)>0,.)
#  return(ps)
#}) %>% bindEvent(input$prune_tax,input$prune_samples, #update ps when pruning numbers change
#                 
#                 input$rem_blanks,#update ps when remove blanks is selected
#                 #input$blank_samples, #update ps when selected blank samples change
##                 
##                 input$sample_switch, #update ps when switch to remove samples is toggled
#                 #input$tax_keepremove, #update ps whenever switch from keep/remove taxa
#                 input$tax_switch, #update ps when switch to remove taxa is toggled 
#                 
#                 #input$tax_filterby, #update when rank is selected (this is updated whenever tax_switch is updated too)
#                 #input$tax_select,
#                 input$go_sample, #update ps when 'go' is clicked to subset samples
#                 input$go_tax, #update ps when 'go' is clicked to subset taxa 
#                 
#                 input$assemble_ps, #update ps when assemble is clicked
#                 input$load_ex, #update ps when example data is loaded 
#                 input$go_addtroph #update ps when adding trait data
#                 )

ps.norm <- reactive({
  req(ps.all())
  req(ps.sub())
  ps <- ps.sub()
  
  #if(input$normoptions != "none") {TRANSFORM(ps,input$normoptions)}
  if(input$normoptions=="tax_count") {
    print("Norm: abundance cutoff across samples")
    #psnorm <- subfunc(ps,TRANSFORM(ps,input$normoptions,abund_cutoff=input$abund_cutoff))
    psnorm <- tryCatch(TRANSFORM(ps,input$normoptions,abund_cutoff=input$abund_cutoff),
                       error=function(e){ps})
  }
  if(input$normoptions=="tax_rel") {
    print("Norm: relative abundance cutoff")
    #psnorm <- subfunc(ps,TRANSFORM(ps,input$normoptions,mean_cutoff=input$mean_cutoff))
    psnorm <- tryCatch(TRANSFORM(ps,input$normoptions,mean_cutoff=input$mean_cutoff),
                       error=function(e){ps})
  }
  if(input$normoptions!="tax_count" && input$normoptions!="tax_rel") {
    #psnorm <- subfunc(ps,TRANSFORM(ps,input$normoptions))
    print("Norm: transformation")
    psnorm <- tryCatch(TRANSFORM(ps,input$normoptions),
                       error=function(e){ps})
  }
  if(input$normoptions=="none") {
    print("Norm: no normalization")
    psnorm <- ps
  }
  
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
  print("Filter: ps.norm made")
  if(input$normoptions=="none") {shinyjs::hide('befaft_norm')}
  else {    
    trackNums$norm_bef <- isolate(as.character(ntaxa(ps.sub()) ))
    trackNums$norm_aft <- isolate(as.character(ntaxa(ps.norm()) ))
    shinyjs::show("befaft_norm")
  }
})

##############################################
ps.norm <- reactive({
  req(ps.all())
  return(ps.all())
})