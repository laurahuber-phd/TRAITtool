source("functions/function-buildps.R")

tax.numcols <- reactiveVal()
met.numcols <- reactiveVal()

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
  
  y <- make.names(colnames(metfile), unique=TRUE)
  colnames(metfile) <- y
  
  z <- make.names(metfile[[1]],unique=TRUE)
  metfile[[1]] <- z
  rownames(metfile) <- z
  
  metfile <- metfile %>% rename_with(str_to_title) #make column names title case
  
  MetadataTable$x <- metfile
  MetadataTable$og <- metfile
  
  numeric.columns <- metfile %>% select(where(is.double) | where(is.numeric)) %>% colnames()
  met.numcols(numeric.columns)
  
  print("Load: loaded metadata")
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
  
  y <- make.names(colnames(otufile), unique=TRUE)
  colnames(otufile) <- y
  
  #z <- make.names(otufile[[1]],unique=TRUE)
  #otufile[[1]] <- z
  #otufile <- otufile %>% 
  #  mutate(across(where(is.character), str_to_sentence))
  
  OTUTable$x <- otufile
  OTUTable$og <- otufile
  print("Load: loaded asv table")
  
}) %>% bindEvent(input$otutab,input$otu_sep,input$otu_header)

output$otucomplete <- renderText({
  req(input$otutab)
  if(is.null(OTUTable$x)) { #error message if OTU table isn't uploaded
    paste("Load: uploaded file is not an otu table")
  } else {
    paste("Load: otu table uploaded")
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
  
  y <- make.names(colnames(taxfile_upld), unique=TRUE)
  colnames(taxfile_upld) <- y
  
  z <- make.names(taxfile_upld[[1]],unique=TRUE)
 # taxfile_upld[[1]] <- z
  
  taxfile <- taxfile_upld %>% 
    rename_with(str_to_title) %>% #make column names title case
    mutate(across(where(is.character), str_to_sentence)) #make everything in the data frame sentence case  
  taxfile[[1]] <- z
  
  if(any(str_detect(colnames(taxfile),"Species")==TRUE)) {
    # Do the following if one of the column names is "Species" (title case will be true because of above code)
    
    check <- taxfile %>% 
      filter(!is.na(Species)) %>% .[1:10,] %>% .$Species %>% #just look at a subset of the species assignments
      countSpaces(.) #will return a vector of the number of spaces in each 
    
    if(is.na(range(check)[2]) ) { #we then look at the range of values; if there are NA values in the range 
      taxfile <- taxfile
    } else 
      if(range(check)[2] < 1) { #we then look at the range of values; if the highest number is 0 
        
        if(any(str_detect(colnames(taxfile),"Genus")==TRUE)) {
        taxfile <- taxfile %>% 
          mutate(Species=case_when(!is.na(Species) ~ paste0(Genus," ",Species), #remake the species column to genus-species
                                   .default=NA))
        }
      } else { #if there is only one space, that's already in genus/species format; or if there are more than 1 space, then we can't change it here anyway
        taxfile <- taxfile
      }
  } else { #if no detected species 
    taxfile <- taxfile
  }

  TaxTable$x <- taxfile
  TaxTable$og <- taxfile_upld
  
  numeric.columns <- taxfile %>% select(where(is.double) | where(is.numeric)) %>% colnames()
  tax.numcols(numeric.columns)
  
  print("Load: loaded tax file")
}) %>% bindEvent(input$taxtab,input$tax_sep,input$tax_header)

output$taxcomplete <- renderText({
  req(input$taxtab)
  if(is.null(TaxTable$x)) {
    paste("Load: uploaded file is not an taxonomy table")
  } else {
    paste("Load: tax table uploaded")
  }
})

###################### USING EXAMPLE DATA ######################
observe({
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
  numeric.columns <- metfile %>% select(where(is.double) | where(is.numeric)) %>% colnames()
  met.numcols(numeric.columns)
  
  print("Load: loaded example metadata")
  
  otufile <- vroom::vroom('data/otu_table.csv',delim=',',
                          show_col_types = FALSE,
                          trim_ws=TRUE,
                          na = c("",".","NA","NULL","Unassigned","unassigned")
  )
  OTUTable$og <- OTUTable$x <- otufile
  print("Load: loaded example asv table")
  
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
  numeric.columns <- taxfile %>% select(where(is.double) | where(is.numeric)) %>% colnames()
  tax.numcols(numeric.columns)
  
  print("Load: loaded example tax file")
},priority=2) %>% bindEvent(input$load_ex)


## check that all the files are right to make a phyloseq object 
isgood <- reactive({
  if (is.null(MetadataTable$x)) { 
    "Load: Metadata are not in correct format or the wrong files are uploaded"
  }  else if (is.null(OTUTable$x)) {
    "Load: OTUs are not in correct format or the wrong files are uploaded"
  } else if (is.null(TaxTable$x)) {
    "Load: Taxonomy are not in correct format or the wrong files are uploaded"
  } else {"Load: Everything is great"}
  
  }) %>% bindEvent(input$Metadata,input$taxtab,input$otutab,ignoreNULL=FALSE)  

observe({
  shinyjs::disable(id = "assemble_ps")
  if (is.null(MetadataTable$x)) { 
    shinyjs::disable(id = "assemble_ps")
  }  else if (is.null(OTUTable$x)) {
    shinyjs::disable(id = "assemble_ps")
  } else if (is.null(TaxTable$x)) {
    shinyjs::disable(id = "assemble_ps")
  } else {
    "Load: Everything is great; all files uploaded"
    shinyjs::enable(id = "assemble_ps")
  }
}) %>% bindEvent(input$Metadata,input$taxtab,input$otutab,ignoreNULL=FALSE)  

ps.og <- reactive({
  
  #runif(input$assemble_ps)
  #runif(input$load_ex)
  #runif(input$go_addtroph)
  
  req(!is.null(MetadataTable$x))
  req(!is.null(OTUTable$x))
  req(!is.null(TaxTable$x))
  
  all <- tryCatch(assemble_ps(otu=OTUTable$x, # build the phyloseq object
                              tax=TaxTable$x,
                              #tax=new.tax,
                              meta=MetadataTable$x
  ), error=function(e){return(NULL)})
  if(isTruthy(all)) {
    print("Load: Dataset assembled")
    all
  } else {
    print("Load: Dataset not assembled")
    NULL
  }
}) %>% bindEvent(input$assemble_ps,
                 input$load_ex,
                 input$go_addtroph)


ps.all <- reactiveVal(NULL) 

observe({
  print("Load: assemble dataset clicked")
  
  #ps.all(NULL)
 # ps.sub(NULL) 
  #ps.norm(NULL) 
  #Meta_vars(NULL)
  #rnames(NULL)
  req(!is.null(MetadataTable$x))
  print("Load: Metadata table exists")
  
  req(!is.null(OTUTable$x))
  print("Load: Feature table exists")
  
  req(!is.null(TaxTable$x))
  print("Load: tax table exists")
  
  if(input$go_addtroph && !is.null(traits_tab$x)) {
    print("Load: adding traits to tax table")
    new.tax <- traits_tab$x
  } else {
    print("Load: using original tax table")
    new.tax <- TaxTable$x
  }
  
  all <- tryCatch(assemble_ps(otu=OTUTable$x, # build the phyloseq object
                              #tax=TaxTable$x,
                              tax=new.tax,
                              meta=MetadataTable$x
  ), 
  error=function(e){return(NULL)})
  
  if(isTruthy(all)) {
    print("Load: ps object assembled")
    ps.all(all)
  } else {
    ## check each component of the ps object individually
    ### start by going through the same process as in function-buildps.R
    ### check those outputs to make sure they are made correctly 
    
    ## sample data
    #sdtest <- as.data.frame(MetadataTable$x)
    #rownames(sdtest) <- sdtest[[1]]
    #sdfail <- tryCatch(sample_data(temp.metadata),
    #                   error=function(e) {return(e$message)})
    #if(!is.data.frame(sdfail)) {print(sdfail)}
    
    ## feature table  
    #otutest <- as.data.frame(OtuTable$x) #convert otu table upload to data frame
    #row.names(otutest) <- otutest[[1]] #add row names (which will be our sequence names)
    #otutest[[1]] <- NULL #remove row names so it can be turned into a matrix
    #otufail <- tryCatch(as.matrix(otutest) %>% 
    #                      otu_table(.,taxa_are_rows=TRUE),
    #                    error=function(e) {return(e$message)}) #otufail will either be a matrix (feature table was successfully turned into a ps component) or return error message  
    #if(!is.matrix(otufail)) {print(otufail)} #return the error message if it is not a matrix
    
    ## tax table  
    #taxtest <- as.data.frame(new.tax)
    #row.names(taxtest) <- taxtest[[1]]
    #taxtest[[1]] <- NULL
    #taxfail <- tryCatch(as.matrix(taxtest) %>% tax_table(.),
    #                    error=function(e) {return(e$message)})
    #if(!is.matrix(taxfail)) {print(taxfail)}
    
    ## check sample names match
    #if(sample_names())
    ## check tax names match  
    ermess <- tryCatch(assemble_ps(otu=OTUTable$x, # build the phyloseq object
                         #tax=TaxTable$x,
                         tax=new.tax,
                         meta=MetadataTable$x
    ), 
    error=function(e) {return(e$message)})
    showModal(modalDialog(ermess))
    ps.all(NULL)
  }
  
}) %>% bindEvent(input$assemble_ps,
                 input$load_ex,
                 input$go_addtroph, 
                 ignoreInit=TRUE, ignoreNULL=FALSE)

observe({
  req(!is.null(ps.all()))
  updateActionButton(session,"defaultgo",disabled=FALSE)
  print("Load: dataset uploaded; default option available")
  shinyjs::show("load_success")
  
  if(input$go_addtroph && !is.null(traits_tab$x)) {
    print("Load: trait info successfully added")
    shinyjs::show("trait_ps_success")
    }
}) %>% bindEvent(ps.all())

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

observe({
  #ps.all() <- NULL
  #ps.sub() <- NULL
  #ps.norm() <- NULL
  #Meta_vars() <- rnames() <- NULL
  print("Load: Dataset replaced; plots should be removed")
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
}) %>% bindEvent(c(input$assemble_ps,input$load_ex,input$go_addtroph), ignoreInit=TRUE)
