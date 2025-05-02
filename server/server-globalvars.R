
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
  return(rank_names(ps.sub()))
}) %>% bindEvent(ps.sub() )


ps.norm <- reactive({
  req(ps.all())
  req(ps.sub())
  ps <- ps.sub()
  if(input$normoptions != "none") {TRANSFORM(ps,input$normoptions)}
}) %>% bindEvent(input$normoptions,
                 ps.sub()
)

ps.sub.melt <- reactive({
  req(ps.sub())
  psmelt(ps.sub())
}) %>% bindEvent(ps.sub())

ps.norm.melt <- reactive({
  req(ps.norm())
  psmelt(ps.norm.melt())
}) %>% bindEvent(ps.norm())
