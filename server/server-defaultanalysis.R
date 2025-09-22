observeEvent(input$defaultgo, {
  req(ps.all())
  print("Default analysis selected")
  updateNumericInput(session,"prune_samples",value=100)
  updateNumericInput(session,"prune_tax",value=1)
  updateRadioButtons(session,"normoptions",selected="tax_rel")
  
  req(ps.norm())
  shinyjs::show("def_prune")
  
  output$N_sample <- renderDataTable({ N_sample_df() })
  output$N_taxa <- renderDataTable({ N_taxa_tab() })
  output$summ_seqs_sel <- renderTable({ Summ_seqs_sel() },
                                      digits=0,rownames=TRUE,colnames=FALSE,striped=TRUE)
  output$N_sel_seq_total <- renderText({ paste("Total number of sequences:",Num_seq_subset$N_seq_num) })
  output$N_sel_seq_assigned <- renderText({ paste("Number of assigned sequences:", Num_seq_subset$N_seq_assigned) })
  output$N_sel_seq_blanks <- renderText({ paste("Number of sequences in blanks:",Num_seq_subset$N_seq_blanks) })
  output$N_sel_otu_total  <- renderText({ paste("Total number of OTUs:",Num_otu_subset$N_otu_num) })
  output$N_sel_otu_assigned <- renderText({ paste("Number of assigned OTUs:",Num_otu_subset$N_otu_assigned) })
  output$N_sel_otu_blanks <- renderText({ paste("Number of OTUs in blanks:",Num_otu_subset$N_otu_blanks) })
  
  output$summ_seqs <- renderTable({ Summ_seqs() },
                                  digits=0,rownames=TRUE,colnames=FALSE,striped=TRUE)
  output$N_seq_total <- renderText({ paste("Total number of sequences:",Num_seq_all$N_seq_num) })
  output$N_seq_assigned <- renderText({ paste("Number of assigned sequences:",Num_seq_all$N_seq_assigned) })
  output$N_seq_blanks <- renderText({ paste("Number of sequences in blanks:",Num_seq_all$N_seq_blanks) })
  output$N_otu_total  <- renderText({ paste("Total number of OTUs:",Num_otu_all$N_otu_num) })
  output$N_otu_assigned <- renderText({ paste("Number of assigned OTUs:",Num_otu_all$N_otu_assigned) })
  output$N_otu_blanks <- renderText({ paste("Number of OTUs in blanks:",Num_otu_all$N_otu_blanks) })
  
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

  shinyjs::click("go_rankperc") #simulate clicking 'go' for percent ranking plot 
  updatePickerInput(session, 
                    inputId = 'basics_ranks', 
                    selected=rnames()[[2]])
  shinyjs::show("def_plotrank")
  
  shinyjs::click("go_rich") #simulate clicking 'go' for richness plot
  shinyjs::show("def_plotrich")
  
  shinyjs::click("go_histoplot") #simulate clicking 'go' for basic histogram
  shinyjs::show("def_plothisto")
  
  shinyjs::click("calc_ord") #calculate distance matrix 
  shinyjs::click("go_ord") #ordination plot
  shinyjs::show("def_plotord")
  
  shinyjs::click("go_hm") #simulate clicking 'go' for heatmap  
  shinyjs::show("def_plothm")
  
  if (isTruthy(ps.norm.melt() %>% select(contains("Troph")) %>% colnames())) {
    shinyjs::click("go_violin")
    shinyjs::show("def_plotvio")
    
  }
  
  shinyjs::delay(4000,switch_page("reporttab"))
  
  
})