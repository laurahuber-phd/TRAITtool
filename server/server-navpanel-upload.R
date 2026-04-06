output$Metadata <- renderText({ # report what file is selected
  paste('Selected file: ', basename(file.path(input$Metadata)))
})
output$otutab <- renderText({ # report what file is selected
  paste('Selected file: ', basename(file.path(input$otutab)))
})
output$taxtab <- renderText({ # report what file is selected
  paste('Selected file: ', basename(file.path(input$taxtab)))
})

observe({
  shinyjs::show("load_success")
  print("ps.all object made")
}) %>% bindEvent(ps.all())

observeEvent(input$view_infodemux, {
             switch_page("infodemux")
  print("Button clicked: switch to info demultiplex page; from upload page raw analysis")
             }) 
observeEvent(input$view_infoseq,{
             switch_page("infoassignseq")
  print("Button clicked: switch to info feature table assembly page; from upload page raw analysis")
             }) 
observeEvent(input$view_infotax, {
             switch_page("infoassigntax")
  print("Button clicked: switch to info assign taxonomy page; from upload page partial analysis")
             }) 

observeEvent(input$view_infotax2, {
  switch_page("infoassigntax")
  print("Button clicked: switch to info assign taxonomy page; from upload page raw analysis")
}) 