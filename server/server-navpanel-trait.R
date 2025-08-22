source("functions/module-readtable.R",local=TRUE)

tableServer(id='tax_table2',
            data=TaxTable 
) 

observeEvent(input$trait_assign, {
  
  output$trait_review <- renderDT(TaxTable$x)
  
}) 

