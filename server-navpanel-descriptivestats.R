# --- # Analysis Page variables # --------  

# reactives based on ps.sub()  
N_sample_df <- reactive({
  req(ps.sub())
  data.frame(Sample=sample_names(ps.sub()),
             "Sequences"=(sample_sums(ps.sub())),
             row.names=NULL
  ) %>% arrange(.,Sample)
}) %>%  bindEvent(ps.sub())

N_seq_num <- reactive({
  #req(ps.sub())
  sum(taxa_sums(ps.sub()))
})   %>% 
  bindEvent(ps.sub())
N_otu_num <- reactive({
  #req(ps.sub())
  ntaxa(ps.sub())
}) %>% 
  bindEvent(ps.sub())
N_taxa_tab <- reactive({
  #req(ps.sub())
  tibble(Rank = rank_names(ps.sub())) %>% 
    rowwise %>%
    mutate(Unique = length(get_taxa_unique((ps.sub()),Rank)))
}) %>% bindEvent(ps.sub())

Summ_seqs_sel <- reactive({
  #req(ps.sub())
  ps <- ps.sub()
  df <- data.frame(MinNoReads=min(sample_sums(ps)) %>% round(digits=0),
                   MaxNoReads=max(sample_sums(ps)) %>% round(digits=0),
                   AvgNoReads=mean(sample_sums(ps)) %>% round(digits=0),
                   MedNoReads=median(sample_sums(ps)) %>% round(digits=0)
  )
  df_pretty <- df %>% rename(  
    "Minimum number of reads"=MinNoReads,
    "Maximum number of reads"=MaxNoReads,
    "Average number of reads"=AvgNoReads,
    "Median number of reads"=MedNoReads)
  t(df_pretty)
}) %>% bindEvent(ps.sub())
N_sel_seq_single <- reactive({
  req(ps.sub())
  ps <- ps.sub()
  length(
    taxa_names(
      prune_taxa(taxa_sums(ps)==1,ps)
    )) %>% round(digits=0)
}) %>% bindEvent(ps.sub())
N_seq_total_num_sel <- reactive({
  req(ps.sub())
  sum(taxa_sums(ps.sub()))
}) %>% bindEvent(ps.sub())
N_seq_assigned_num_sel <- reactive({
  req(ps.sub())
  ps <- ps.sub()
  sum(taxa_sums(subset_taxa(ps,Kingdom != "Unassigned")))
}) %>% bindEvent(ps.sub())
N_otu_total_num_sel <- reactive({
  req(ps.sub())
  ntaxa(ps.sub())
}) %>% bindEvent(ps.sub())
N_otu_assigned_num_sel <- reactive({
  req(ps.sub())
  ps <- ps.sub()
  ntaxa(subset_taxa(ps,Kingdom != "Unassigned"))
}) %>% bindEvent(ps.sub())

N_seq_blanks_num_sel <- reactive({
  req(ps.sub())
  ps <- ps.sub()
  if(is.null(input$blank_samples)) {return(0)} else
    sum(taxa_sums(subset_blanks_rem(ps,input$cols,input$blank_samples)))
}) %>% bindEvent(input$cols,
                 input$blank_samples,
                 ps.sub())

N_otu_blanks_num_sel <- reactive({
  req(ps.sub())
  ps <- ps.sub()
  if(is.null(input$blank_samples)) {return(0)} else
    ntaxa(subset_blanks_rem(ps,input$cols,input$blank_samples))
}) %>% bindEvent(input$cols,
                 input$blank_samples,
                 ps.sub())
# reactives based on ps.all()
Summ_seqs <- reactive({
  #req(ps.all())
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
N_seq_single <- reactive({
  req(ps.all())
  ps <- ps.all()
  length(
    taxa_names(
      prune_taxa(taxa_sums(ps)==1,ps)
    )) %>% round(digits=0)
}) %>% bindEvent(ps.all())
N_seq_total_num <- reactive({
  req(ps.all())
  sum(taxa_sums(ps.all()))
}) %>% bindEvent(ps.all())
N_seq_assigned_num <- reactive({
  req(ps.all())
  ps <- ps.all()
  sum(taxa_sums(subset_taxa(ps,Kingdom != "Unassigned")))
}) %>% bindEvent(ps.all())
N_otu_total_num <- reactive({
  req(ps.all())
  ntaxa(ps.all())
}) %>% bindEvent(ps.all())
N_otu_assigned_num <- reactive({
  req(ps.all())
  ps <- ps.all()
  ntaxa(subset_taxa(ps,Kingdom != "Unassigned"))
}) %>% bindEvent(ps.all())

N_seq_blanks_num <- reactive({
  req(ps.all())
  ps <- ps.all()
  if(is.null(input$blank_samples)) {return(0)} else
    sum(taxa_sums(subset_blanks_rem(ps,input$cols,input$blank_samples)))
}) %>% bindEvent(input$cols,input$blank_samples)

N_otu_blanks_num <- reactive({
  req(ps.all())
  ps <- ps.all()
  if(is.null(input$blank_samples)) {return(0)} else
    ntaxa(subset_blanks_rem(ps,input$cols,input$blank_samples))
}) %>% bindEvent(input$cols,input$blank_samples)

N_rankperc_plot <- reactive({
  req(ps.all.melt())
  ps <- ps.all.melt()
  textsize <- (input$rankper_text * 2.5)
  psplot.df <- ps %>% group_by(.data[[input$ranks]]) %>%
    reframe(Abund=sum(Abundance),
            Total=sum(ps$Abundance),
            Perc=Abund/Total) %>%
    filter(.data[[input$ranks]] != "Unassigned") %>% 
    arrange(.,desc(Perc),.by_group = TRUE)
  psplot.df[[input$ranks]] <- factor( psplot.df[[input$ranks]], 
                                      levels= psplot.df[[input$ranks]])
  #  ggplot(.,aes(y=.data[[input$cols]],x=Perc,fill=.data[[input$ranks]])) + geom_col()
  ggplot(psplot.df,
         aes(y=.data[[input$ranks]],
             x=Perc,
             fill=.data[[input$ranks]]
         )) + geom_col() + 
    theme(legend.position = "bottom",
          legend.box.margin = margin(5,5,5,5),
          legend.spacing = unit(0,"pt"),
          aspect.ratio=(input$rankper_height/input$rankper_width),
          #plot.margin = unit(c(input$rankper_width,input$rankper_height,input$rankper_width,input$rankper_height),"cm"),
          text = element_text(size=(textsize+5)),
          #legend.key.size = unit(textsize,"pt"),
          legend.key.height = unit(textsize,"pt"),
          legend.key.width = unit(textsize,"pt")) + 
    guides(fill=guide_legend(ncol=input$rankper_legend)) +
    labs(title="Distribution of taxonomic assignments",
         subtitle="(across the entire dataset)",
         x="Percent",
         caption="Unassigned sequences were included in percentage calculation but not plotted")
}) %>% bindEvent(ps.all.melt(),input$rankper_legend,input$rankper_height,input$rankper_width,input$rankper_text,input$ranks)

output$N_otu_byrank_plot <- renderPlot({
  N_rankperc_plot()
})
observe({
  showModal(modalDialog(
    renderPlot(N_rankperc_plot()),
    footer = downloadButton('download_rankperc_Plot_popup', 'Download'),
    size="xl",
    easyClose = TRUE
  ))
}) %>% bindEvent(input$notu_click)

## --- # Summary # --------  

### For main tab (selected data)
output$N_sel_seq_total <- renderText({
  paste("Total number of sequences:",N_seq_total_num_sel())
})
output$N_sel_seq_assigned <- renderText({
  paste("Number of assigned sequences:",N_seq_assigned_num_sel())
})
output$N_sel_seq_blanks <- renderText({
  paste("Number of sequences in blanks:",N_seq_blanks_num_sel())
})
output$N_sel_otu_total  <- renderText({
  paste("Total number of OTUs:",N_otu_total_num_sel())
})
output$N_sel_otu_assigned <- renderText({
  paste("Number of assigned OTUs:",N_otu_assigned_num_sel())
})
output$N_sel_otu_blanks <- renderText({
  paste("Number of OTUs in blanks:",N_otu_blanks_num_sel())
})
output$summ_seqs_sel <- renderTable({
  Summ_seqs_sel()
},digits=0,rownames=TRUE,colnames=FALSE,striped=TRUE)

### For sidebar (Original/non-subset dataset)
output$N_sample <- renderDataTable({
  N_sample_df()
})
# output$N_seq <- renderText({
#    paste("Number of sequences in analysis: ", N_seq_num())
#    })
#  output$N_otu <- renderText({
#    paste("Number of OTUs in analysis: ",N_otu_num())
#    })
output$N_seq_total <- renderText({
  paste("Total number of sequences:",N_seq_total_num())
})
output$N_seq_assigned <- renderText({
  paste("Number of assigned sequences:",N_seq_assigned_num())
})
output$N_seq_blanks <- renderText({
  paste("Number of sequences in blanks:",N_seq_blanks_num())
})
output$N_otu_total  <- renderText({
  paste("Total number of OTUs:",N_otu_total_num())
})
output$N_otu_assigned <- renderText({
  paste("Number of assigned OTUs:",N_otu_assigned_num())
})
output$N_otu_blanks <- renderText({
  paste("Number of OTUs in blanks:",N_otu_blanks_num())
})
output$N_taxa <- renderDataTable({
  N_taxa_tab()
})

output$listtaxa <- renderDataTable({
  req(ps.sub())
  tax_glom(ps.sub(),taxrank = input$ranks, NArm=TRUE) %>% 
    tax_table(.) %>% 
    as.data.frame(.,row.names=FALSE) #throws error because tax_glom %>% tax_table doesn't work and I dont remember the workaround
  #cbind(tax_table(tax_glom(ps.sub(),taxrank = input$ranks, NArm=TRUE))) #does not fix error
}) %>% bindEvent(input$rem_blanks,
                 input$adjust,
                 input$ranks,
                 ps.sub())
output$summ_seqs <- renderTable({
  Summ_seqs()
},digits=0,rownames=TRUE,colnames=FALSE,striped=TRUE)
output$download_rankperc_Plot_popup <- output$download_rankperc_Plot <- downloadHandler(
  filename = "Pct_comp_plot.png",
  contentType = 'image/png',
  content = function(file) {
    ggsave(file,plot=N_rankperc_plot(),
           width=input$rankper_width,height=input$rankper_height
    )
  }) 
