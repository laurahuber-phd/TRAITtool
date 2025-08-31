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

#rnames <- reactiveVal(NULL)
#observeEvent(ps.all(),{
#  req(ps.all())
#  rnames <- rank_names(ps.norm())
#})

rnames <- reactive({ 
  return(rank_names(ps.norm()))
}) %>% bindEvent(ps.norm(),ps.all() )

ps.norm.melt <- reactive({
  req(ps.norm())
  psmelt(ps.norm())
}) %>% bindEvent(ps.norm(),ps.all())




output$psog <- renderPrint({ps.all()})
output$pstext <- renderPrint({ps.norm()})
output$rnames_return <- renderPrint({rnames()})
output$metavar_return <- renderPrint({Meta_vars()})