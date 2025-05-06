library(dplyr)

#Num_vars <- reactive({ #what metadata variables are numeric
#  req(MetadataTable$x)  
#  a <- as.data.frame(lapply(MetadataTable$x,class)) 
#  b <- t(a) %>% data.frame(Names=rownames(.)) %>% rename(.,"Vals"=".")
#  c <- filter(b,Vals=="integer" | Vals == "double") %>% .$Names
#  return(c)
#}) %>% bindEvent(input$load_ex,
#                 input$assemble_ps)


Meta_vars <- reactive({ #read the metadata file and identify the variable names 
  #req(input$Metadata)
  return(names(MetadataTable$x))
}) %>% bindEvent(input$load_ex, #update meta_vars if the load button is pressed (new dataset)
                 input$assemble_ps
                 #input$Metadata, #update meta_vars if a new metadata table is uploaded manual
                 #MetadataTable$x
)

rnames <- reactive({ #read the metadata file and identify the variable names 
  #req(input$Metadata)
  return(rank_names(ps.norm()))
}) %>% bindEvent(ps.norm() )

ps.norm.melt <- reactive({
  req(ps.norm())
  psmelt(ps.norm())
}) %>% bindEvent(ps.norm())
