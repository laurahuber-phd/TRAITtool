source("function-buildps.R")

MetadataTable <- reactiveValues(x=NULL,og=NULL)
observeEvent(input$Metadata, {
  MetadataTable$x <- NULL
  MetadataTable$og <- NULL
})
observe({
  req(input$Metadata) 
  hideFeedback('Metadata')
  metfile <- uploadFunction(file=input$Metadata,
                            id="Metadata",
                            type="meta",
                            sep=input$meta_sep,
                            header=input$meta_header)
  MetadataTable$x <- metfile
  MetadataTable$og <- metfile
}) %>% bindEvent(input$Metadata,input$meta_sep,input$meta_header)

OTUTable <- reactiveValues(x=NULL,og=NULL)
observeEvent(input$otutab, {
  OTUTable$x <- NULL
  OTUTable$og <- NULL
})
observe({ #upload the OTU file
  req(input$otutab)
  hideFeedback("otutab")
  otufile <- uploadFunction(file=input$otutab,
                            id="otutab",
                            type="otu",
                            sep=input$otu_sep,
                            header=input$otu_header)
  OTUTable$x <- otufile
  OTUTable$og <- otufile
}) %>% bindEvent(input$otutab,input$otu_sep,input$otu_header)

output$otucomplete <- renderText({
  req(input$otutab)
  if(is.null(OTUTable$x)) {
    paste("uploaded file is not an otu table")
  } else {
    paste("table uploaded")
  }
})
#
TaxTable <- reactiveValues(x=NULL,og=NULL)  
observeEvent(input$taxtab, {
  TaxTable$x <- NULL
  TaxTable$og <- NULL
})
observe({ #upload the taxonomy file
  req(input$taxtab)
  hideFeedback("taxtab")
  taxfile <- uploadFunction(file=input$taxtab,
                            id='taxtab',
                            type="tax",
                            sep=input$tax_sep,
                            header=input$tax_header)
  #taxfile <- taxfile %>% replace(is.na(.),"Not assigned")
  TaxTable$x <- taxfile
  TaxTable$og <- taxfile
}) %>% bindEvent(input$taxtab,input$tax_sep,input$tax_header)
output$taxcomplete <- renderText({
  req(input$taxtab)
  if(is.null(TaxTable$x)) {
    paste("uploaded file is not an taxonomy table")
  } else {
    paste("table uploaded")
  }
})
observeEvent(input$load_ex, {
  MetadataTable$x <- NULL #reset to null every time input$load_ex is clicked
  MetadataTable$og <- NULL
  OTUTable$x <- NULL
  OTUTable$og <- NULL
  TaxTable$x <- NULL
  TaxTable$og <- NULL
  
  metfile <- vroom::vroom('data/Mock_metadata.csv',delim=',',
                          show_col_types = FALSE,
                          trim_ws=TRUE,
                          na = c("",".","NA","NULL","Unassigned","unassigned")
  )
 #metfile <- metfile  %>% replace(is.na(.),"Not assigned")
  MetadataTable$og <- MetadataTable$x <- metfile
  
  otufile <- vroom::vroom('data/otu_table.csv',delim=',',
                          show_col_types = FALSE,
                          trim_ws=TRUE,
                          na = c("",".","NA","NULL","Unassigned","unassigned")
  )
  #otufile <- otufile  %>% replace(is.na(.),"Not assigned")
  OTUTable$og <- OTUTable$x <- otufile
  
  taxfile <- vroom::vroom('data/tax_table.csv',delim=',',
                          show_col_types = FALSE,
                          trim_ws=TRUE,
                          na = c("",".","NA","NULL","Unassigned","unassigned")
  )
  #taxfile <- taxfile  %>% replace(is.na(.),"Not assigned")
  TaxTable$og <- TaxTable$x <- taxfile
})


## check that all the files are right to make a phyloseq object 
isgood <- reactive({
  if (is.null(MetadataTable$x)) { 
    "Metadata are not in correct format or the wrong files are uploaded"
  }  else if (is.null(OTUTable$x)) {
    "OTUs are not in correct format or the wrong files are uploaded"
  } else if (is.null(TaxTable$x)) {
    "Taxonomy are not in correct format or the wrong files are uploaded"
  } else {"Everything is great"}
}) %>% bindEvent(input$Metadata,input$taxtab,input$otutab,ignoreNULL=FALSE)  

ps.all <- eventReactive(c(input$assemble_ps,input$load_ex), {
  runif(input$assemble_ps)
  runif(input$load_ex)
  req(MetadataTable$x)
  req(OTUTable$x)
  req(TaxTable$x)
  assemble_ps(otu=OTUTable$x, # build the phyloseq object
              tax=TaxTable$x,
              meta=MetadataTable$x
  )
})
