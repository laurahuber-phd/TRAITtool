source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
source('functions/module-plotting-general.R',local=TRUE)

source('server/server-aes-vis.R',local=TRUE) #base theme for ggplot2


observe({ #whenever ps.norm() changes 
  req(ps.all())
  updatePickerInput(inputId = 'basics_ranks', 
                    label = 'Taxonomic Rank', 
                    choices = c(rnames())
  ) #update picker selection using the rank names of the ps object
}) #%>% bindEvent(ps.norm())   

# ---- # Tab 1 (Sequences per sample) # ----- 
N_sample_df <- reactive({
  req(ps.norm())
  data.frame(Sample=sample_names(ps.norm()),
             "Sequences"=(sample_sums(ps.norm())),
             row.names=NULL
  ) %>% arrange(.,Sample)
}) %>%  bindEvent(ps.norm())

# ---- # Tab 2 (Num of each taxa) # ---- 
N_taxa_tab <- reactive({
  req(ps.norm())
  tibble(Rank = rank_names(ps.norm())) %>% 
    rowwise %>%
    mutate(Unique = length(get_taxa_unique((ps.norm()),Rank)))
}) %>% bindEvent(ps.norm())

# ---- # Tab 3 (Sequence distribution/qualities) # ---- 

## ---- ## Subset summary ## ---- 
Summ_seqs_sel <- reactive({
  req(ps.norm())
  ps <- ps.norm()
  df <- data.frame(MinNoReads=min(sample_sums(ps)) %>% round(digits=0), #what is the minimum number of sequences in a sample
                   MaxNoReads=max(sample_sums(ps)) %>% round(digits=0), #what is the maximum 
                   AvgNoReads=mean(sample_sums(ps)) %>% round(digits=0), #what is the average  
                   MedNoReads=median(sample_sums(ps)) %>% round(digits=0)
  )
  df_pretty <- rename(df,  #then rename the dataframe 
                      "Minimum number of reads"=MinNoReads,
                      "Maximum number of reads"=MaxNoReads,
                      "Average number of reads"=AvgNoReads,
                      "Median number of reads"=MedNoReads)
  t(df_pretty)
}) %>% bindEvent(ps.norm())

# make empty reactive values variables to hold information about the subsetted dataset
Num_seq_subset <- reactiveValues(
  N_seq_num = NULL, #total number of sequences
  N_seq_assigned = NULL,  #number of assigned sequences
  N_singleton = NULL, #number of singletons
  N_seq_blanks = NULL #number of seqs in blanks
) 
Num_otu_subset <- reactiveValues(
  N_otu_num = NULL, #number of ASVs
  N_otu_assigned = NULL, #number of assigned ASVs 
  N_otu_blanks = NULL #number of ASVs in blanks
) 

observe({
  req(ps.norm())
  #runif(input$SeqSumNavTab=="Summary")
  #runif(input$pages=="basicstab")
  #runif(input$defaultgo)
  #runif(input$pages=="reporttab" && is.null(Num_seq_subset$N_seq_num))
  ps <- ps.norm()
  
  Num_seq_subset$N_seq_num = sum(taxa_sums(ps)) #calculate total number of sequences  
  Num_otu_subset$N_otu_num = ntaxa(ps) #calculate total number of ASVs
  print("Desc: sequence summary calculated")
  if(isTruthy(taxa_sums(ps)==1)) {
    Num_seq_subset$N_singleton = ntaxa(prune_taxa(taxa_sums(ps)==1,ps) ) 
    print("Desc: singletons calculated")
  } else {
    Num_seq_subset$N_singleton = 0
    print("Desc: no singletons")
  }

  }, suspended=TRUE) %>% bindEvent(ps.norm())

observe({
  req(ps.norm())
  #runif(input$pages=="basicstab")
  #runif(input$defaultgo)
  #runif(input$pages=="reporttab" && is.null(Num_seq_subset$N_seq_blanks))
  ps <- ps.og()
  
  if(isTruthy(input$rem_blanks)) { #if the user has selected remove blanks, return 'NA' 
    Num_seq_subset$N_seq_blanks <- NA 
    Num_otu_subset$N_otu_blanks <- NA
    print("Desc: Blanks were removed; cannot calculate numbers")
    } else { 
    if(!is.null(input$blank_cols) & !is.null(input$blank_samples)) { 
      #shinyjs::delay(3000)
      if(input$blank_cols == "Sample") { #if the filtering parameter is sample id
        blasam = sample_names(ps)
        blarem = input$blank_samples #list of blank samples 
        blakeep = blasam[blasam %in% blarem] #keep only blank samples
      } else { #with any other filtering parameter
        blacolvals = as.data.frame(as.matrix(sample_data(ps))) %>% select(input$blank_cols) #get sample_data of selected variable 
        blakeep = row.names(blacolvals %>% filter(blacolvals[[1]] %in% input$blank_samples)) #list of sample names selected
      }
      print("Desc: blank nums calculated")
      if(!is.null(blakeep)) {psbla <- prune_samples(blakeep,ps) %>% prune_taxa(taxa_sums(.)>0,.)} 
    }
    Num_seq_subset$N_seq_blanks <- if(!is.null(input$blank_cols) & !is.null(input$blank_samples)) {sum(taxa_sums(psbla))} else {"No blanks selected"}
    Num_otu_subset$N_otu_blanks <- if(!is.null(input$blank_cols) & !is.null(input$blank_samples)) {ntaxa(psbla)} else {"No blanks selected"}
    
    }
},suspended=TRUE) %>% bindEvent(ps.norm())

## ---- ## Sidebar (Original dataset summary) ## ---- ##
Summ_seqs <- reactive({
  req(ps.og())
  ps <- ps.og()
  df <- data.frame(MinNoReads=min(sample_sums(ps)) %>% round(digits=0), #what is the minimum number of sequences in a sample
                   MaxNoReads=max(sample_sums(ps)) %>% round(digits=0), #what is the maximum 
                   AvgNoReads=mean(sample_sums(ps)) %>% round(digits=0), #what is the average  
                   MedNoReads=median(sample_sums(ps)) %>% round(digits=0))
  df_pretty <- df %>% rename(  #then rename the dataframe 
    "Minimum"=MinNoReads,
    "Maximum"=MaxNoReads,
    "Average"=AvgNoReads,
    "Median"=MedNoReads)
  t(df_pretty)
}) %>% bindEvent(ps.og())

# make empty reactive values variables to hold information about the subsetted dataset
Num_seq_all <- reactiveValues(
  N_seq_num = NULL, #total number of sequences
  N_seq_assigned = NULL, #number of assigned sequences
  N_singleton = NULL, #number of singletons
  N_seq_blanks = NULL #number of seqs in blanks
) 
Num_otu_all <- reactiveValues(
  N_otu_num = NULL, #number of ASVs
  N_otu_assigned = NULL,  #number of assigned ASVs
  N_otu_blanks = NULL #number of ASVs in blanks
) 

observe({
  req(ps.og())
  #runif(input$pages=="basicstab")
  #runif(input$defaultgo)
  #runif(input$pages=="reporttab" && is.null(Num_seq_all$N_seq_num))
  ps <- ps.og()
  
  Num_seq_all$N_seq_num = sum(taxa_sums(ps)) #calculate total number of sequences  
  Num_otu_all$N_otu_num = ntaxa(ps) #calculate total number of ASVs
print("Desc: original dataframe numbers calculated")

    if(isTruthy(taxa_sums(ps)==1)) {
    Num_seq_all$N_singleton = ntaxa(prune_taxa(taxa_sums(ps)==1,ps) ) 
    print("Desc: singletons in original dataframe calculated")
  } else {
    Num_seq_all$N_singleton = 0
    print("Desc: no singletons in original dataframe")
  }

  }, suspended=TRUE) %>% 
  bindEvent(ps.og(), 
            ignoreNULL = FALSE) 

observe({
  req(ps.og())
  #runif(input$pages=="basicstab")
  #runif(input$defaultgo)
  #runif(input$pages=="reporttab" && is.null(Num_seq_all$N_seq_blanks))
  ps <- ps.og()
  
    if(!is.null(input$blank_cols) & !is.null(input$blank_samples)) { 
      if(input$blank_cols == "Sample") { #if the filtering parameter is sample id
        blasam = sample_names(ps)
        blarem = input$blank_samples #list of blank samples 
        blakeep = blasam[blasam %in% blarem] #keep only blank samples
      } else { #with any other filtering parameter
        blacolvals = as.data.frame(as.matrix(sample_data(ps))) %>% select(input$blank_cols) #get sample_data of selected variable 
        blakeep = row.names(blacolvals %>% filter(blacolvals[[1]] %in% input$blank_samples)) #list of sample names selected
      }
      if(!is.null(blakeep)) {psbla <- prune_samples(blakeep,ps) %>% prune_taxa(taxa_sums(.)>0,.)} 
    }
  print("Desc: blank nums in original dataset calculated")
    Num_seq_all$N_seq_blanks <- if(!is.null(input$blank_cols) & !is.null(input$blank_samples)) {sum(taxa_sums(psbla))} else {"No blanks selected"}
    Num_otu_all$N_otu_blanks <- if(!is.null(input$blank_cols) & !is.null(input$blank_samples)) {ntaxa(psbla)} else {"No blanks selected"}

}, suspended=TRUE) %>% bindEvent(ps.og(),
                 input$blank_samples)

# ---- Tax Info # ---- 
rankperc_df <- reactive({
  req(ps.norm.melt())
  print("Desc: making tax rank df")
  
  ps <- ps.norm.melt()
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
  print("Desc: making tax rank base plot")
  ggplot(rankperc_df(),
         aes(y=.data[[input$basics_ranks]],
             x=Perc,
             fill=.data[[input$basics_ranks]]
         )) + geom_col() + 
    labs(title="Distribution of taxonomic assignments",
         subtitle="(across the entire dataset)",
         x="Percent")
}) %>% bindEvent(ps.norm.melt(),
                 input$basics_ranks)

rankperc_plot <- eventReactive(input$go_rankperc, {
  print("Desc: assembling tax rank plot")
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
    hide('listtaxa_placeholder')
    print("Desc: placeholder table hidden")
    } else {show('N_sample_placeholder')
      show('N_taxa_placeholder')
      show('listtaxa_placeholder')
      print("Desc: placeholder table shown")
      }
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
output$N_sel_seq_assigned <- renderText({ paste("Number of singleton sequences:", Num_seq_subset$N_singleton) })
output$N_sel_seq_blanks <- renderText({ paste("Number of sequences in blanks:",Num_seq_subset$N_seq_blanks) })
#### ---- #### Num ASVs (in subset) #### ----
output$N_sel_otu_total  <- renderText({ paste("Total number of ASVs:",Num_otu_subset$N_otu_num) })
output$N_sel_otu_blanks <- renderText({ paste("Number of ASVs in blanks:",Num_otu_subset$N_otu_blanks) })

## ---- ## Sidebar ## ----
#### ---- #### Summary (min/max/avg) #### ----
output$summ_seqs <- renderTable({ Summ_seqs() },
                                digits=0,rownames=TRUE,colnames=FALSE,striped=TRUE)
#### ---- #### Num sequences (in subset) #### ----
output$N_seq_total <- renderText({ paste("Total number of sequences:",Num_seq_all$N_seq_num) })
output$N_seq_assigned <- renderText({ paste("Number of singleton sequences:",Num_seq_all$N_singleton) })
output$N_seq_blanks <- renderText({ paste("Number of sequences in blanks:",Num_seq_all$N_seq_blanks) })
#### ---- #### Num ASVs (in subset) #### ----
output$N_otu_total  <- renderText({ paste("Total number of ASVs:",Num_otu_all$N_otu_num) })
output$N_otu_blanks <- renderText({ paste("Number of ASVs in blanks:",Num_otu_all$N_otu_blanks) })

## ---- ## Taxa Info ## ---- 

### ---- ### List taxa ### ----
output$basics_listtaxa <- renderDataTable({
  req(ps.norm())
  tax_glom(ps.norm(),taxrank = input$basics_ranks, NArm=TRUE) %>% 
    tax_table(.) %>% 
    as.data.frame(.,row.names=FALSE) #throws error because tax_glom %>% tax_table doesn't work and I dont remember the workaround
  #cbind(tax_table(tax_glom(ps.norm(),taxrank = input$basics_ranks, NArm=TRUE))) #does not fix error
}) %>% bindEvent(input$rem_blanks,
                 input$adjust,
                 input$basics_ranks,
                 ps.norm())

### ---- ### Plot taxa ### ----
observeEvent(input$go_rankperc, {
output$rankperc_plot <- renderPlot({rankperc_plot()})
print("Desc: taxonomic rank relative percentage plot rendered")
})

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
