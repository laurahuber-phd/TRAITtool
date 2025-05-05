source('module-plotting-pretty.R',local=TRUE)
source('module-plotting-size.R',local=TRUE)
source('module-plotting-general.R',local=TRUE)

source('server-aes-vis.R',local=TRUE) #base theme for ggplot2


observe({ #whenever ps.sub() changes 
  updatePickerInput(inputId = 'basics_ranks', 
                    label = 'Taxonomic Rank', 
                    choices = c(rnames())
  ) #update picker selection using the rank names of the ps object
}) %>% bindEvent(ps.sub())   

# ---- # Tab 1 (Sequences per sample) # ----- 
N_sample_df <- reactive({
  req(ps.sub())
  data.frame(Sample=sample_names(ps.sub()),
             "Sequences"=(sample_sums(ps.sub())),
             row.names=NULL
  ) %>% arrange(.,Sample)
}) %>%  bindEvent(ps.sub())

# ---- # Tab 2 (Num of each taxa) # ---- 
N_taxa_tab <- reactive({
  req(ps.sub())
  tibble(Rank = rank_names(ps.sub())) %>% 
    rowwise %>%
    mutate(Unique = length(get_taxa_unique((ps.sub()),Rank)))
}) %>% bindEvent(ps.sub())

# ---- # Tab 3 (Sequence distribution/qualities) # ---- 
## ---- ## Summary (min/max/avg) ## ---- 
Summ_seqs_sel <- reactive({
  req(ps.sub())
  ps <- ps.sub()
  df <- data.frame(MinNoReads=min(sample_sums(ps)) %>% round(digits=0),
                   MaxNoReads=max(sample_sums(ps)) %>% round(digits=0),
                   AvgNoReads=mean(sample_sums(ps)) %>% round(digits=0),
                   MedNoReads=median(sample_sums(ps)) %>% round(digits=0)
  )
  df_pretty <- rename(df,  
                      "Minimum number of reads"=MinNoReads,
                      "Maximum number of reads"=MaxNoReads,
                      "Average number of reads"=AvgNoReads,
                      "Median number of reads"=MedNoReads)
  t(df_pretty)
}) %>% bindEvent(ps.sub())

## ---- ## Num sequences (in subset) ## ----
Num_seq_subset <- reactiveValues(
  N_seq_num = NULL, #total number of sequences
  N_seq_assigned = NULL,  #number of assigned sequences
  N_singleton = NULL, #number of singletons
  N_seq_blanks = NULL #number of seqs in blanks
) 

observe({
  req(ps.sub())
  ps <- ps.sub()
  
  Num_seq_subset$N_seq_num = sum(taxa_sums(ps)) 
  
  Num_seq_subset$N_seq_assigned = tryCatch(sum(taxa_sums(subset_taxa(ps, rank_names(ps)[1]!="Not assigned" )
                                                         )),
                                           error=function(e){return(0)})
  
  #Num_seq_subset$N_singleton = length(taxa_names(prune_taxa(taxa_sums(ps)==1,ps)
  #                                               )) %>% round(digits=0)
  
 # Num_seq_subset$N_seq_blanks = if(is.null(input$blank_samples)) {return(0)} else 
 #   sum(taxa_sums(subset_blanks_rem(ps,input$blank_cols,input$blank_samples)))
  
 # isolate({
#    Num_seq_subset_list <- reactiveValuesToList(Num_seq_subset)
#    Num_seq_subset_names <- c("Total number of sequences:","Number of assigned sequences:")#,"Number of sequences in blanks:")
#    Num_seq_subset_df <- data.frame("Value"=Num_seq_subset_names,"Number"=Num_seq_subset_list)
#  })
  
}) %>% 
  bindEvent(input$blank_cols,
            input$blank_samples,
            ps.sub(), 
            ignoreNULL=FALSE)

## ---- ## Num ASVs ## ---- 
Num_otu_subset <- reactiveValues(
  N_otu_num = NULL, #number of OTUs
  N_otu_assigned = NULL, #number of assigned OTUs 
  N_otu_blanks = NULL #number of OTUs in blanks
) 

observe({
  req(ps.sub())
  ps <- ps.sub()
  
  Num_otu_subset$N_otu_num = ntaxa(ps)
  
  Num_otu_subset$N_otu_assigned = tryCatch(ntaxa(subset_taxa(ps, 
                                              rank_names(ps)[1]!="Not assigned"
                                              )),
                                           error=function(e){return(0)})
  
  #Num_otu_subset$N_otu_blanks = if(is.null(input$blank_samples)) {return(0)} else 
    #ntaxa(subset_blanks_rem(ps,input$blank_cols,input$blank_samples))
  
  #isolate({
  #  Num_otu_subset_list <- reactiveValuesToList(Num_otu_subset)
  #  Num_otu_subset_names <- c("Total number of ASVs:","Number of assigned ASVs:")#,"Number of ASVs in blanks:")
  #  Num_otu_subset_df <- data.frame("Value"=Num_otu_subset_names,"Number"=Num_otu_subset_list)
  #})
  
}) %>% 
  bindEvent(input$blank_cols,
            input$blank_samples,
            ps.sub(), 
            ignoreNULL=FALSE)

# ---- # Sidebar (Original sequence distribution/qualities) # ----
## ---- ## Summary ## ----
Summ_seqs <- reactive({
  req(ps.all())
  ps <- ps.all()
  df <- data.frame(MinNoReads=min(sample_sums(ps)) %>% round(digits=0),
                   MaxNoReads=max(sample_sums(ps)) %>% round(digits=0),
                   AvgNoReads=mean(sample_sums(ps)) %>% round(digits=0),
                   MedNoReads=median(sample_sums(ps)) %>% round(digits=0))
  df_pretty <- df %>% rename(  
    "Minimum"=MinNoReads,
    "Maximum"=MaxNoReads,
    "Average"=AvgNoReads,
    "Median"=MedNoReads)
  t(df_pretty)
}) %>% bindEvent(ps.all())

## ---- ## Num seq ## ----
Num_seq_all <- reactiveValues(
  N_seq_num = NULL, #total number of sequences
  N_seq_assigned = NULL, #number of assigned sequences
  N_singleton = NULL, #number of singletons
  N_seq_blanks = NULL #number of seqs in blanks
) 

observe({
  req(ps.all())
  ps <- ps.all()
  
  Num_seq_all$N_seq_num = sum(taxa_sums(ps)) 
  
  Num_seq_all$N_seq_assigned = tryCatch(sum(taxa_sums(subset_taxa(ps, 
                                                      rank_names(ps)[1]!="Not assigned"
                                                      ))), 
                                        error=function(e){return(0)}) 
  
  #Num_seq_all$N_singleton = length(taxa_names(prune_taxa(taxa_sums(ps)==1, 
  #                                           ps
  #                                           ))) %>% round(digits=0)
  
  #Num_seq_all$N_seq_blanks = if(is.null(input$blank_samples)) {return(0)} else 
  #  sum(taxa_sums(subset_blanks_rem(ps,input$blank_cols,input$blank_samples)))
}) %>% 
  bindEvent(input$blank_cols,
            input$blank_samples,
            ps.all(), 
            ignoreNULL = FALSE) 

## ---- ## Num ASV ## ---- 
Num_otu_all <- reactiveValues(
  N_otu_num = NULL, #number of OTUs
  N_otu_assigned = NULL,  #number of assigned OTUs
  N_otu_blanks = NULL #number of OTUs in blanks
) 

observe({
  req(ps.all())
  ps <- ps.all()
  
  Num_otu_all$N_otu_num = ntaxa(ps) 
  
  Num_otu_all$N_otu_assigned = tryCatch(ntaxa(subset_taxa(ps, 
                                              rank_names(ps)[1]!="Not assigned")),
                                        error=function(e){return(0)}) 
  
  #Num_otu_all$N_otu_blanks = if(is.null(input$blank_samples)) {return(0)} else 
  #  ntaxa(subset_blanks_rem(ps,input$blank_cols,input$blank_samples))
}) %>% 
  bindEvent(input$blank_cols,
            input$blank_samples,
            ps.all(), 
            ignoreNULL=FALSE)

# ---- Tax Info # ---- 
rankperc_df <- reactive({
  req(ps.sub.melt())
  ps <- ps.sub.melt()
  psplot.df <- ps %>% group_by(.data[[input$basics_ranks]]) %>%
    reframe(Abund=sum(Abundance),
            Total=sum(ps$Abundance),
            Perc=Abund/Total) %>%
    filter(
      !is.na( .data[[input$basics_ranks]] ) 
      ) %>% 
    arrange(.,desc(Perc),.by_group = TRUE)
  psplot.df[[input$basics_ranks]] <- factor( psplot.df[[input$basics_ranks]], 
                                      levels= psplot.df[[input$basics_ranks]])
  psplot.df
})

rankpercplot_pretty  <- plotPrettyServer(id="basics_pretty")
rankpercplot_size <- plotSizeServer(id="basics_size") 

rankperc_base <- reactive({
  req(rankperc_df())
  ggplot(rankperc_df(),
         aes(y=.data[[input$basics_ranks]],
             x=Perc,
             fill=.data[[input$basics_ranks]]
         )) + geom_col() + 
    labs(title="Distribution of taxonomic assignments",
         subtitle="(across the entire dataset)",
         x="Percent")
}) %>% bindEvent(ps.sub.melt(),
                 input$basics_ranks)

rankperc_plot <- eventReactive(input$go_rankperc, {
  rankperc_base() + rankpercplot_pretty() + rankpercplot_size() 
}) 


## --- # Summary # --------  
output$N_sample_placeholder <- renderDT({
  data.frame(Sample=c("Sample1",
                  "Sample2",
                  "Sample3"),Sequences=c("","Placeholder",""))
})
output$N_taxa_placeholder <- renderDT({
  data.frame(Rank=c("Kingdom","Phylum","Class","Order","Family","Genus","Species"),
             Unique=c("","","","Placeholder","","",""))
})
output$listtaxa_placeholder <- renderDT({
  data.frame(Kingdom=c("","Placeholder",""),Phylum=c("","Placeholder",""),
             Class=c("","Placeholder",""),Order=c("","...",""),
             Family=c("","...",""),Genus=c("","...",""),Species=c("","...",""))
})

observeEvent(checking$table, {
  if(checking$table==TRUE)
  {hide('N_sample_placeholder')
    hide('N_taxa_placeholder')
    hide('listtaxa_placeholder')} else {show('N_sample_placeholder')
      show('N_taxa_placeholder')
      show('listtaxa_placeholder')}
})


# --- # Returns # --------  


## ---- ## Tab 1 (Sequences per sample) ## ---- 
output$N_sample <- renderDataTable({
  N_sample_df()
})

## ---- ## Tab 2 (Num of each taxa) ## ----
output$N_taxa <- renderDataTable({ N_taxa_tab() })

## ---- ## Tab 3 (Sequence distribution/qualities) ## ----
#### ---- #### Summary (min/max/avg) #### ----
output$summ_seqs_sel <- renderTable({ Summ_seqs_sel() },
                                    digits=0,rownames=TRUE,colnames=FALSE,striped=TRUE)
#### ---- #### Num sequences (in subset) #### ----
output$N_sel_seq_total <- renderText({ paste("Total number of sequences:",Num_seq_subset$N_seq_num) })
output$N_sel_seq_assigned <- renderText({ paste("Number of assigned sequences:", Num_seq_subset$N_seq_assigned) })
output$N_sel_seq_blanks <- renderText({ paste("Number of sequences in blanks:",Num_seq_subset$N_seq_blanks) })
#### ---- #### Num ASVs (in subset) #### ----
output$N_sel_otu_total  <- renderText({ paste("Total number of OTUs:",Num_otu_subset$N_otu_num) })
output$N_sel_otu_assigned <- renderText({ paste("Number of assigned OTUs:",Num_otu_subset$N_otu_assigned) })
output$N_sel_otu_blanks <- renderText({ paste("Number of OTUs in blanks:",Num_otu_subset$N_otu_blanks) })

## ---- ## Sidebar ## ----
#### ---- #### Summary (min/max/avg) #### ----
output$summ_seqs <- renderTable({ Summ_seqs() },
                                digits=0,rownames=TRUE,colnames=FALSE,striped=TRUE)
#### ---- #### Num sequences (in subset) #### ----
output$N_seq_total <- renderText({ paste("Total number of sequences:",Num_seq_all$N_seq_num) })
output$N_seq_assigned <- renderText({ paste("Number of assigned sequences:",Num_seq_all$N_seq_assigned) })
output$N_seq_blanks <- renderText({ paste("Number of sequences in blanks:",Num_seq_all$N_seq_blanks) })
#### ---- #### Num ASVs (in subset) #### ----
output$N_otu_total  <- renderText({ paste("Total number of OTUs:",Num_otu_all$N_otu_num) })
output$N_otu_assigned <- renderText({ paste("Number of assigned OTUs:",Num_otu_all$N_otu_assigned) })
output$N_otu_blanks <- renderText({ paste("Number of OTUs in blanks:",Num_otu_all$N_otu_blanks) })


## ---- ## Taxa Info ## ---- 

### ---- ### List taxa ### ----
output$basics_listtaxa <- renderDataTable({
  req(ps.sub())
  tax_glom(ps.sub(),taxrank = input$basics_ranks, NArm=TRUE) %>% 
    tax_table(.) %>% 
    as.data.frame(.,row.names=FALSE) #throws error because tax_glom %>% tax_table doesn't work and I dont remember the workaround
  #cbind(tax_table(tax_glom(ps.sub(),taxrank = input$basics_ranks, NArm=TRUE))) #does not fix error
}) %>% bindEvent(input$rem_blanks,
                 input$adjust,
                 input$basics_ranks,
                 ps.sub())

### ---- ### Plot taxa ### ----
output$rankperc_plot <- renderPlot({rankperc_plot()})


observe({
  showModal(modalDialog(
    renderPlot(rankperc_plot()),
    footer = downloadButton('rankperc_plotdwnld_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$rankperc_click)

output$rankperc_plotdwnld_popup <- output$rankperc_plotdwnld <- downloadHandler(
  filename = "Pct_comp_plot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=rankperc_plot(),
           width=input$rankper_width,height=input$rankper_height
    )
  }) 
