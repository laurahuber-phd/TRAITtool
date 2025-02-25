
############################################# Example dataset #############################################
Ex_Card <- card(id="ex_data",
                card_header("Use example data"),
                "Load the example dataset to go through this app and explore what you can get out of it",
                actionButton("load_ex",
                             "Load"))
############################################# Metadata #############################################
Files_MetadataCard <- card(
  card_header("Add your metadata file here"),
  layout_columns(
    colwidths=c(4,8),
    fileInput("Metadata",
              label="Upload metadata",
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
############################################# ASV and tax assignment needed #############################################
Files_RawCard <- card(
  card_header(
    "Upload files here if you are starting from scratch"
  ),
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
Files_ScratchCard <- card(
  card_header("if you are starting from scratch, upload these files then proceed to the next tab:"),
  layout_column_wrap(
    width=1/2,
    fill=FALSE,
    Files_RawCard,
    QFilterCard
  )
)

############################################# Analysis-only #############################################
Files_CompletedProcessingCard <- card(
  card_header(
    "If you've already got a table and taxonomic assignments, upload those files here:"
  ),
  card_body(
    "if you've already gone through these steps and just want to analyze the results, upload these files and proceed to the Analysis tab"
  ),
  layout_columns(
    colwidths=c(8,4),
    fileInput("otutab",
              label="Upload OTU table",
              accept=c(".csv",".tsv")),
    layout_columns(
      radioButtons("otu_sep","File separator", #select separator in the file
                   choices=c("Comma"=",","Semicolon"=";","Tab"="/t"),
                   selected=","),
      checkboxInput("otu_header","Header",TRUE), #does the file have a header
    )
  ),
  textOutput("otucomplete"),
  layout_columns(
    colwidths=c(8,4),
    fileInput("taxtab",
              label="Upload taxonomy table",
              accept=c(".csv",".tsv")),
    layout_columns(
      radioButtons("tax_sep","File separator", #select separator in the file
                   choices=c("Comma"=",","Semicolon"=";","Tab"="/t"),
                   selected=","),
      checkboxInput("tax_header","Header",TRUE) #does the file have a header
    )
  ),
  textOutput("taxcomplete")
)

##########################################################################################
uploadpage <- nav_panel("Files",
          shinyFeedback::useShinyFeedback(),
          open=TRUE,
          Ex_Card,
          accordion(Files_MetadataCard, #card directing to metadata csv file
                    Files_CompletedProcessingCard, #card directing to otu and tax files
                    Files_ScratchCard), #card directing to raw fastq files
          actionButton("fileupload_nextpage_v2","Next step",width='100%')
)
##########################################################################################