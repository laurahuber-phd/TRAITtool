
############################################# Example dataset #############################################
Ex_Card <- card(id="ex_data",
                card_header("Use example data"),
                "Load the example dataset. You can use this dataset to explore the feautures of the app prior to using it on your own data.",
                actionButton("load_ex",
                             "Load"))
############################################# Metadata #############################################
Files_MetadataCard <- card(
  card_header("Add your metadata file here",
              tooltip(bs_icon("info-circle"),
                      tagList(p(style='text-decoration-line:underline; font-weight:bold; text-transform: uppercase;',"What is a metadata file?"),
                              #tags$style(HTML("text-align: left"),
                               p(style='text-align: left', "The first column of the file should be sequencing sample IDs, with at least one other column.The metadata file is what will bring biological relevance to the sequencing results, and at minimum is used to connect the sequencing sample to field sample."),
                               p(style='font-weight:bold',"See 'Help' tab for an example.")
                              )
                      )
              ),
  layout_columns(
    colwidths=c(4,8),
    fileInput("Metadata",
              label="",
              accept=c(".csv",".tsv") #only accept csv/tsv/text files
    ),
    layout_columns(
      colwidths=c(4,8),
      radioButtons("meta_sep", #select separator in the file
                   "File separator",
                   choices=c("Comma"=",","Semicolon"=";","Tab"="\t"),
                   selected=","),
      checkboxInput("meta_header","Header",TRUE)
    )
  ),
  textOutput("metacomplete")
)
############################################# From Scratch #############################################
Files_RawCard <- tagList(
  fileInput("FASTQfile",
            label="Upload sequence data",
            accept=c(".fastq",".fqz")),
  fileInput("BarcodeFile",
            label="Upload barcodes file",
            accept=c(".csv",".tsv"))
)
QFilterCard <- card(
  card_header(
    "Select filtering requirements"
  ),
  selectInput("Quality", 
              label="Quality Stringency", 
              choices=c("Loosest","Medium","Strictest"),
              selected = "Medium"),
  selectInput("Location",
              label="MPA Location",
              choices=c("Belize","Dominican Republic","Honduras","MAR"),
              selected = "MAR"),
  selectInput("HabitatType",
              label="Habitat Type",
              choices=c("Coral Reef","Mangrove","Seagrass","Any"),
              selected = "Any")
)
Files_ScratchCard <- tagList(
  p("Start here if you have raw sequencing data. You will need the files from the sequencing facility, usually in FASTQ or .fqz format, a tabular file that contains the indexing barcode sequences attached to each sample, and a metadata file (which you should have uploaded above).",
  ),
  br(),
  layout_column_wrap(
    width=1/2,
    fill=FALSE,
    Files_RawCard,
    QFilterCard
  )
)

############################################# Analysis-only #############################################
Card_OTU <- tagList(layout_columns(colwidths=c(8,4),
                                   fileInput("otutab",
                                             label="Upload OTU table",
                                             accept=c(".csv",".tsv")),
                                   layout_columns(
                                     radioButtons("otu_sep","File separator", #select separator in the file
                                                  choices=c("Comma"=",","Semicolon"=";","Tab"="/t"),
                                                  selected=","),
                                     checkboxInput("otu_header","Header",TRUE) #does the file have a header
                                     )))
Card_OTU_alt <- tagList(fileInput("otutab",
                                             label="Upload OTU table",
                                             accept=c(".csv",".tsv")),
                                   layout_columns(
                                     radioButtons("otu_sep","File separator", #select separator in the file
                                                  choices=c("Comma"=",","Semicolon"=";","Tab"="/t"),
                                                  selected=","),
                                     checkboxInput("otu_header","Header",TRUE) #does the file have a header
                                   ))
Card_TAX <- tagList(layout_columns(colwidths=c(8,4),
                                   fileInput("taxtab",
                                             label="Upload taxonomy table",
                                             accept=c(".csv",".tsv")),
                                   layout_columns(
                                     radioButtons("tax_sep","File separator", #select separator in the file
                                                  choices=c("Comma"=",","Semicolon"=";","Tab"="/t"),
                                                  selected=","),
                                     checkboxInput("tax_header","Header",TRUE) #does the file have a header
                                   )))

Files_CompletedProcessingCard <- tagList(
  p("Use this option if you have distilled your sequence data into a single sequence table and a table of taxonomic assignments. This can be because you've already gone through this app before, or using files generated by another method. If not generated by this app, double check your files have the same structure by navigating to the Help tab. " ),
  br(),
  Card_OTU,
  textOutput("otucomplete"),
  Card_TAX,
  textOutput("taxcomplete")
)
Files_PartialCard <- tagList(
  p("Use this option if you have distilled your sequence data into a single sequence table, but you still need to assign taxonomy to your sequences. You can do so here using one of our databases. "),
  br(),
  layout_columns(Card_OTU_alt,
                 QFilterCard) 
  )


##########################################################################################
uploadpage <- nav_panel("Files",
          shinyFeedback::useShinyFeedback(),
          open=TRUE,
          HTML("<h4 style='text-align:center;'>Select the option that fits your data best: </h4>"),
          layout_columns(col_widths=c(4,8),
                         radioGroupButtons(inputId = "selectupload", 
                                           width='100%', individual=TRUE,
                                           choiceNames = c(HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i>My data has been completely processed:</i></p> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>I only need analyses.</p>"),
                                                           HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i>My data has been partially processed:</i></p> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>I have an OTU table and I need to identify the species and analyze.</p>"), 
                                                           HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i>I have raw sequencing data:</i></p> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>My files are coming directly from the sequencing facility.</p>")
                                           ),
                                           choiceValues = c("choose_analysis","choose_partial","choose_raw"),
                                           direction = 'vertical',
                                           selected=NULL
                         ),
                         card(conditionalPanel(condition="input.selectupload == 'choose_raw'",
                                            Files_ScratchCard #card directing to raw fastq files
                           ),
                           conditionalPanel(condition="input.selectupload == 'choose_partial'",
                                            Files_PartialCard #placeholder
                           ),
                           conditionalPanel(condition="input.selectupload == 'choose_analysis'",
                                            Files_CompletedProcessingCard #card directing to otu and tax files
                           ))
                         ),
          h5("Make sure to upload a metadata file before proceeding. At minimum, this should include some way to connect sample names in the sequencing data."),
          Files_MetadataCard, #card directing to metadata csv file,
          br(),
          br(),
          br(),
         HTML("<h4>Or, alternatively, use our example dataset: </h4>"),
          Ex_Card,
          br(),
          actionButton("fileupload_nextpage_v2","Next step",width='100%')
)
##########################################################################################