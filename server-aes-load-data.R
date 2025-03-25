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
  
  metfile <- vroom::vroom('data/Mock_metadata.csv',delim=',',show_col_types = FALSE)
  MetadataTable$og <- MetadataTable$x <- metfile
  
  otufile <- vroom::vroom('data/otu_table.csv',delim=',',show_col_types = FALSE)
  OTUTable$og <- OTUTable$x <- otufile
  
  taxfile <- vroom::vroom('data/tax_table.csv',delim=',',show_col_types = FALSE)
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

ps.all <- reactive({
  #req(input$Metadata)
  #req(input$otutab)
  #req(input$taxtab)
  req(MetadataTable$x)
  req(OTUTable$x)
  req(TaxTable$x)
  assemble_ps(otu=OTUTable$x, # build the phyloseq object
              tax=TaxTable$x,
              meta=MetadataTable$x
  )
}) %>% bindEvent(OTUTable$x,TaxTable$x,MetadataTable$x,
                 input$load_ex)
ps.sub <- reactive({
  req(ps.all())
  ps <- ps.all()
  if(input$select_switch==TRUE && input$go) { 
    req(input$select_samples)
    new.ps <- subset_keep(ps,input$cols2,input$select_samples)
    if(input$adjust) { #if adjust is selected, remove unassigned sequences
      subset_taxa(new.ps,Kingdom!="Unassigned") %>% prune_samples(sample_sums(.)>1,.)
    } else { #otherwise just return new.ps
      new.ps
    }
  } else if(input$adjust && input$rem_blanks) { #if 'remove unassigned' and 'remove blanks' are both checked
    if(is.null(input$blank_samples)) { #but no blank samples are identified
      subset_taxa(ps,Kingdom!="Unassigned") %>% #only remove unassigned sequences
        prune_samples(sample_sums(.)>=1,.)
    } else
      subset_blanks_rem(ps,input$cols,input$blank_samples) %>% 
      subset_taxa(.,Kingdom!="Unassigned") %>% 
      prune_samples(sample_sums(.)>=1,.)
  } else
    if(input$adjust) { #if 'remove unassigned' is the only one checked
      subset_taxa(ps,Kingdom!="Unassigned") %>% 
        prune_samples(sample_sums(.)>=1,.)
    } else
      if(input$rem_blanks) { #if 'remove blanks' is the only one checked
        subset_blanks_rem(ps,input$cols,input$blank_samples) %>% 
          prune_samples(sample_sums(.)>=1,.)
      } else
        ps %>% prune_samples(sample_sums(.)>=1,.) #if neither is checked, just prune samples
}) %>% bindEvent(input$adjust,
                 input$rem_blanks,input$blank_samples,
                 input$Metadata,input$taxtab,input$otutab,
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




