output$Metadata <- renderText({ # report what file is selected
  paste('Selected file: ', basename(file.path(input$Metadata)))
})
output$otutab <- renderText({ # report what file is selected
  paste('Selected file: ', basename(file.path(input$otutab)))
})
output$taxtab <- renderText({ # report what file is selected
  paste('Selected file: ', basename(file.path(input$taxtab)))
})