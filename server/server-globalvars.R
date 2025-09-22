library(dplyr)

Meta_vars <- reactive({ #read the metadata file and identify the variable names 
  #req(input$Metadata)
  req(!is.null(MetadataTable$x))
  print("Meta_vars() sample variables calculated")
  return(names(MetadataTable$x))
}) %>% bindEvent(input$load_ex, #update meta_vars if the load button is pressed (new dataset)
                 input$assemble_ps
                 #input$Metadata, #update meta_vars if a new metadata table is uploaded manual
                 #MetadataTable$x
)

rnames <- reactive({ 
  req(ps.norm())
  print("Rank names calculated")
  return(rank_names(ps.norm()))
}) %>% bindEvent(ps.norm(),ps.all() )

ps.norm.melt <- reactive({
  req(ps.norm())
  print("Melted normalized dataframe")
  psmelt(ps.norm()) %>% filter(Abundance>0)
}) %>% bindEvent(ps.norm(),ps.all())
