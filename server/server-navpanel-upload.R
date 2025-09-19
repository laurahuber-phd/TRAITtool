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
}) %>% bindEvent(ps.all())

observeEvent(input$view_infodemux, {
             switch_page("infodemux")
             }) 
observeEvent(input$view_infoseq,{
             switch_page("infoassignseq")
             }) 
observeEvent(input$view_infotax, {
             switch_page("infoassigntax")
             }) 

observeEvent(input$view_infotax2, {
  switch_page("infoassigntax")
}) 