source("functions/function-buildps.R")

###################### UPLOAD METADATA ######################
MetadataTable <- reactiveValues(x=NULL,og=NULL) #start with an empty reactiveValues 
observeEvent(input$Metadata, { #when a file is uploaded
  MetadataTable$x <- NULL #reset the values to null 
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

###################### UPLOAD FEATURE TABLE ######################
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
  if(is.null(OTUTable$x)) { #error message if OTU table isn't uploaded
    paste("uploaded file is not an otu table")
  } else {
    paste("table uploaded")
  }
})

###################### UPLOAD TAXONOMY ######################
countSpaces <- function(s) { sapply(gregexpr(" ", s), function(p) { sum(p>=0) } ) }

TaxTable <- reactiveValues(x=NULL,og=NULL)  
observeEvent(input$taxtab, {
  TaxTable$x <- NULL
  TaxTable$og <- NULL
})

observe({ #upload the taxonomy file
  req(input$taxtab)
  hideFeedback("taxtab")
  taxfile_upld <- uploadFunction(file=input$taxtab,
                            id='taxtab',
                            type="tax",
                            sep=input$tax_sep,
                            header=input$tax_header)
  
  taxfile <- taxfile_upld %>% rename_with(str_to_title) #make column names title case
  
  check <- taxfile %>% 
    filter(!is.na(Species)) %>% .[1:10,] %>% .$Species %>% #just look at a subset of the species assignments
    countSpaces(.) #will return a vector of the number of spaces in each 
  
  
  if(is.na(range(check)[2]) ) { #we then look at the range of values; if the highest number is 0 
    taxfile <- taxfile
  } else 
  if(range(check)[2] < 1) { #we then look at the range of values; if the highest number is 0 
    taxfile <- taxfile %>% 
      mutate(Species=case_when(!is.na(Species) ~ paste0(Genus," ",Species), #remake the species column to genus-species
                                                    .default=NA))
  } else {
    taxfile <- taxfile
  }
  
  TaxTable$x <- taxfile
  TaxTable$og <- taxfile_upld
}) %>% bindEvent(input$taxtab,input$tax_sep,input$tax_header)

output$taxcomplete <- renderText({
  req(input$taxtab)
  if(is.null(TaxTable$x)) {
    paste("uploaded file is not an taxonomy table")
  } else {
    paste("table uploaded")
  }
})

###################### USING EXAMPLE DATA ######################
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
  MetadataTable$og <- MetadataTable$x <- metfile
  print("Load: loaded metadata")
  
  otufile <- vroom::vroom('data/otu_table.csv',delim=',',
                          show_col_types = FALSE,
                          trim_ws=TRUE,
                          na = c("",".","NA","NULL","Unassigned","unassigned")
  )
  OTUTable$og <- OTUTable$x <- otufile
  print("Load: loaded asv table")
  
  taxfile_upld <- vroom::vroom('data/tax_table.csv',delim=',',
                          show_col_types = FALSE,
                          trim_ws=TRUE,
                          na = c("",".","NA","NULL","Unassigned","unassigned")
  )
  taxfile <- taxfile_upld %>% rename_with(str_to_title) %>% #change column names to sentence case 
      mutate(Species=case_when(!is.na(Species) ~ paste0(Genus," ",Species), #remake the species column to genus-species
                               .default=NA))
  TaxTable$x <- taxfile
  TaxTable$og <- taxfile_upld
  print("Load: loaded tax file")
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

ps.og <- eventReactive(c(input$assemble_ps,input$load_ex),{
  
  runif(input$assemble_ps)
  runif(input$load_ex)
  runif(input$go_addtroph)
  
  req(!is.null(MetadataTable$x))
  req(!is.null(OTUTable$x))
  req(!is.null(TaxTable$x))
  
  all <- tryCatch(assemble_ps(otu=OTUTable$x, # build the phyloseq object
                              tax=TaxTable$x,
                              #tax=new.tax,
                              meta=MetadataTable$x
  ), error=function(e){return(NULL)})
  if(isTruthy(all)) {
    all
  } else {
    return("Dataset not assembled")
  }
})

ps.all <- eventReactive(c(input$assemble_ps,input$load_ex,input$go_addtroph), {
  
  ps.all <- ps.sub <- ps.norm <- NULL 
  Meta_vars <- rnames <- NULL 
  
  runif(input$assemble_ps)
  runif(input$load_ex)
  runif(input$go_addtroph)
  
  req(MetadataTable$x)
  req(OTUTable$x)
  req(TaxTable$x)
  
  if(input$go_addtroph && !is.null(traits_tab$x)) {
    new.tax <- traits_tab$x
  } else {
    new.tax <- TaxTable$x
  }
  
  all <- tryCatch(assemble_ps(otu=OTUTable$x, # build the phyloseq object
                       #tax=TaxTable$x,
                       tax=new.tax,
                       meta=MetadataTable$x
  ), 
           error=function(e){return(NULL)})
  
  if(isTruthy(all)) {
    all
  } else {
      showModal(modalDialog("Components do not match. Check sample names and try again."))
    all
    }
})

observeEvent(ps.all(),{
  req(ps.all())
  updateActionButton(session,"defaultgo",disabled=FALSE)
  
  if(input$go_addtroph && !is.null(traits_tab$x)) {shinyjs::show("trait_ps_success")}
})

checking <- reactiveValues()
checking$table <- checking$empty <- NULL

observeEvent(MetadataTable$x, {
  checking$table <- checking$empty <- NULL #resets values
  if(is.null(MetadataTable$x)) { #if there is no input data to make a table out of
    checking$empty <- TRUE 
    checking$table <- FALSE
  } else if (!is.null(MetadataTable$x)) {
    checking$empty <- FALSE
    checking$table <- TRUE
  }
}, ignoreNULL = FALSE)

observeEvent(c(input$assemble_ps,input$load_ex,input$go_addtroph),{
  #ps.all() <- NULL
  #ps.sub() <- NULL
  #ps.norm() <- NULL
  #Meta_vars() <- rnames() <- NULL
  shinyjs::hide("def_prune")
  shinyjs::hide("def_plotrank")
  shinyjs::hide("def_plotrich")
  shinyjs::hide("def_plothisto")
  shinyjs::hide("def_plotord")
  shinyjs::hide("def_plothm")
  shinyjs::hide("def_plotvio")
  
  output$rankperc_plot <- renderPlot({NULL})
  output$rich_plot <- renderPlot({NULL})
  output$histo_plot <- renderPlot({NULL})
  output$hm_plot <- renderPlot({NULL})
  output$ord_plot <- renderPlot({NULL})
  output$violin_plot <- renderPlot({NULL})
  
  output$rep_richplot <- renderPlot({NULL})
  output$rep_percplot <- renderPlot({NULL})
  output$rep_ordplot <- renderPlot({NULL})
  output$rep_histplot <- renderPlot({NULL})
  output$rep_hmplot <- renderPlot({NULL})
  output$rep_vioplot <- renderPlot({NULL})
  
  shinyjs::hide("viewfinalreport")
  shinyjs::hide("dwnldviewreport")
})
