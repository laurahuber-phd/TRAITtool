library(dplyr)

Meta_vars <- reactive({ #read the metadata file and identify the variable names 
  #req(input$Metadata)
  req(!is.null(MetadataTable$x))
  print("Globalvars: Meta_vars() sample variables calculated")
  return(names(MetadataTable$x) %>% sort())
}) %>% bindEvent(input$load_ex, #update meta_vars if the load button is pressed (new dataset)
                 input$assemble_ps
                 #input$Metadata, #update meta_vars if a new metadata table is uploaded manual
                 #MetadataTable$x
)

rnames <- reactive({ 
  req(ps.all())
  
  if(exists("ps.norm()")==TRUE) {
    return(rank_names(ps.norm()))
    print("Globalvars: Rank names calculated from normalized dataset")
  }
  else {
    return(rank_names(ps.all()))
    print("Globalvars: Rank names calculated from original dataset")
  }
  
}) %>% bindEvent(ps.norm(),ps.all() )

ps.norm.melt <- reactive({
  req(ps.norm())
   
  ps <- ps.norm()
  print("Globalvars: Melted normalized dataframe")
  #psmelt(ps.norm()) %>% filter(Abundance>0)
  
  
  tax_vars <- function() {
    c(any_of(c(tax.numcols() )))
  }
  
  met_vars <- function() {
    c(any_of(c(met.numcols() )))
  }
  
  otu_df <- otu_table(ps) %>% as.matrix() %>% as.data.frame() 
  otu_df <- cbind(OTU=rownames(otu_df),otu_df)
  
  tax_df <- tax_table(ps)  %>% as.matrix() %>% as.data.frame() %>% mutate(across(tax_vars(), ~ as.numeric(.x)))
  tax_df <- cbind(OTU=rownames(tax_df),tax_df)
  
  sample_df <- sample_data(ps) %>% as.matrix() %>% as.data.frame() %>% mutate(across(met_vars(), ~ as.numeric(.x)))
  sample_df <- cbind(Sample=rownames(sample_df),sample_df)
   
  # Melt and merge with joins
  melted_physeq_tidy <- otu_df %>%
    pivot_longer(
      cols = -OTU,
      names_to = "Sample",
      values_to = "Abundance"
    ) %>%
    left_join(tax_df, by = "OTU") %>%
    left_join(sample_df, by = "Sample") %>% 
    filter(Abundance > 0)
  
}) %>% bindEvent(ps.norm(),ps.all())
