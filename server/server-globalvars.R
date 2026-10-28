library(dplyr)

Meta_vars <- reactiveVal()

observe({
  req(!is.null(MetadataTable$x))
  print("Globalvars: Meta_vars() sample variables calculated")
  
  varlist <- names(MetadataTable$x) %>% sort()
  
  Meta_vars(varlist)
}) %>% bindEvent(input$load_ex, #update meta_vars if the load button is pressed (new dataset)
                 input$assemble_ps
                 #input$Metadata, #update meta_vars if a new metadata table is uploaded manual
                 #MetadataTable$x
)

observe({
  req(ps.all())
  req(!is.null(Meta_vars))
  print("Globalvars: Meta_vars() sample variables re-calculated")
  
  oldlist <- names(MetadataTable$x) %>% sort()
  newfront <- isolate(input$aggregate_var)
  
  varlist <- c(oldlist[oldlist%in%newfront],
               oldlist[!oldlist%in%newfront]
               )
    
    Meta_vars(varlist)
},priority=1) %>% bindEvent(input$aggregate_var)


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
  
  tax_df <- tax_table(ps)  %>% as.matrix() %>% as.data.frame() %>% mutate(across(any_of(tax_vars()), ~ as.numeric(.x)))
  tax_df <- cbind(OTU=rownames(tax_df),tax_df)
  
  sample_df <- sample_data(ps) %>% as.matrix() %>% as.data.frame() %>% mutate(across(any_of(met_vars()), ~ as.numeric(.x)))
  sample_df <- cbind(TRAITsample=rownames(sample_df),sample_df)
   
  # Melt and merge with joins
  melted_physeq_tidy <- otu_df %>%
    pivot_longer(
      cols = -OTU,
      names_to = "TRAITsample",
      values_to = "Abundance"
    ) %>%
    left_join(tax_df, by = "OTU") %>%
    left_join(sample_df, by = "TRAITsample") %>% 
    filter(Abundance > 0)
  
}) %>% bindEvent(ps.norm(),ps.all())
