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
                          na = c("", "NA","NULL","Unassigned","unassigned")
  )
  MetadataTable$og <- MetadataTable$x <- metfile
  
  otufile <- vroom::vroom('data/otu_table.csv',delim=',',
                          show_col_types = FALSE,
                          trim_ws=TRUE,
                          na = c("", "NA","NULL","Unassigned","unassigned")
  )
  OTUTable$og <- OTUTable$x <- otufile
  
  taxfile <- vroom::vroom('data/tax_table.csv',delim=',',
                          show_col_types = FALSE,
                          trim_ws=TRUE,
                          na = c("", "NA","NULL","Unassigned","unassigned")
  )
  TaxTable$og <- TaxTable$x <- taxfile
})

Meta_vars <- reactive({ #read the metadata file and identify the variable names 
  #req(input$Metadata)
  return(names(MetadataTable$x))
}) %>% bindEvent(input$load_ex, #update meta_vars if the load button is pressed (new dataset)
                 input$Metadata, #update meta_vars if a new metadata table is uploaded manual
                 MetadataTable$x
)

rnames <- reactive({ #read the metadata file and identify the variable names 
  #req(input$Metadata)
  return(rank_names(ps.sub()))
}) %>% bindEvent(ps.sub()
)

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

subfunc <- function(ps,x) {
  tryCatch(x,error=function(e){return(FALSE)})
}

ps.sub <- reactive({
  req(ps.all())
  
  ps <- subfunc(ps.all(),
                ps.all() %>% 
                  prune_taxa(taxa_sums(.)>=input$prune_tax,.) %>% 
                  prune_samples(sample_sums(.)>=input$prune_samples,.)
  )
  
  if (!isTruthy(ps)) {
    showModal(
      modalDialog(title="ERROR",
                  "One or more of your subsetting criteria is too strict",
                  size='l',
                  easyClose = FALSE
      )
    ) 
  }
  
  req(ps)
  tnames <- rank_names(ps)

#####################################################################################
  if(isTruthy(input$rem_blanks)) { #if remove blanks is selected
    blakeep = NULL #make empty list of samples
    
    if(!is.null(input$blank_samples)) { #if there are blanks selected
      if(input$cols == "Sample") { #if the filtering parameter is sample id
        blasam = sample_names(ps)
        blarem = input$blank_samples #list of blank samples 
        blakeep = blasam[!blasam %in% blarem] #remove blanks from sample list 
      } else { #with any other filtering parameter
        blacolvals = as.data.frame(as.matrix(sample_data(ps))) %>% select(input$cols) #get sample_data of selected variable 
        blakeep = row.names(blacolvals %>% filter(!blacolvals[[1]] %in% input$blank_samples)) #list of sample names EXCEPT for those selected
      }
      ps <- prune_samples(blakeep,ps)
    }
    else {
      ps <- ps
    }
  }
####################################################################################   
  if(isTruthy(input$select_switch) && input$go) { #if user selects specific samples to vis
    runif(input$go)
    if(!is.null(input$select_samples)) {
      selkeep = NULL #make empty list of samples 
      
      if(input$cols2 == "Sample") { #if the filtering parameter is sample id
        selkeep = input$select_samples #list of selected samples 
      } else {
        selcolvals = as.data.frame(as.matrix(sample_data(ps))) %>% select(input$cols2) #get sample_data of selected variable 
        selkeep = row.names(selcolvals %>% filter(selcolvals[[1]] %in% input$select_samples)) #list of sample names selected
      }
      ps <- prune_samples(selkeep,ps)
    } else {
      ps <- ps
    }
  }
###################################################################################
  if(isTruthy(input$sub_taxa)) { #if user selects specific taxa to vis 
    taxkeep = NULL #make blank list
    taxsam = as.data.frame(as.matrix(tax_table(ps)))
    
    if(isTruthy(input$adjust)) { #if keep selected taxa is selected
      taxsamsel = select(tax.sam,input$ranks2)
      taxkeep = rownames(filter(taxsamsel, taxsamsel[[1]] %in% input$select_taxa))
    } else { #if remove selected taxa is selected
      taxsamsel = select(tax.sam,input$ranks2)
      taxkeep = rownames(filter(taxsamsel, !taxsamsel[[1]] %in% input$select_taxa))
    }
    ps <- prune_taxa(taxkeep,ps)
  }
  return(ps)
}) %>% bindEvent(input$prune_tax,input$prune_samples,
                 input$adjust,
                 input$rem_blanks,input$blank_samples,
                 input$assemble_ps,
                 input$select_switch,input$go,
                 input$load_ex)

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
})