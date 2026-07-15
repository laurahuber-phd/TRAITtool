source('ui/ui-defaultanalysis.R')
############################################# Example dataset #############################################
Ex_Card <- card(id="ex_data",
                card_header("Use example data"),
                "Load the example dataset. You can use this dataset to explore the feautures of the app prior to using it on your own data.",
                actionButton("load_ex",
                             "Load"),
                hidden( p(id='load_success',
                          bs_icon("patch-check",size='4em'),
                          "Success!"))
                )
############################################# Metadata #############################################
Files_MetadataCard <- tagList(
  card_header(h5("Add your metadata table here")),
  HTML("<p style='font-weight: bold'>This is one of the most important components. The information in this table will be used to identify patterns in the organisms that are found in your dataset and associate them with information you care about from the location and timepoint the samples were taken. </p>"),
  br(),
    p("Make sure to upload a metadata file before proceeding. At minimum, this should include some way to connect the sample that was sequenced to the sequencing output."),
  layout_columns(
    colwidths=c(4,8),
    fileInput("Metadata",
              label=tags$span("Upload metadata",
                              tags$i(tooltip(bs_icon("info-circle"),
                                             tagList(p(style='text-decoration-line:underline; font-weight:bold; text-transform: uppercase;',"What is a metadata file?"),
                                                     #tags$style(HTML("text-align: left"),
                                                     p(style='text-align: left', "The first column of the file should be sequencing sample IDs, with at least one other column.The metadata file is what will bring biological relevance to the sequencing results, and at minimum is used to connect the sequencing sample to field sample."),
                                                     p(style='font-weight:bold',"See 'Help' tab for an example.")
                                                     )
                                             )
                                     )),
              accept=c(".csv",".tsv") #only accept csv/tsv/text files
    ),
    layout_columns(
      colwidths=c(4,8),
      radioButtons("meta_sep", #select separator in the file
                   "File separator",
                   choices=c("Comma (.csv)"=",","Semicolon (.csv)"=";","Tab (.tsv)"="\t"),
                   selected=","),
      checkboxInput("meta_header","Header",TRUE)
    )
  ),
  textOutput("metacomplete")
)
############################################# From Scratch #############################################

############################################# Analysis-only #############################################
Card_OTU <- tagList(card_header(h5("Sequence table")),
                 p("Here, we are looking for a table of sequences (often in the form of ASVs, ESVs, or OTUs) and what samples they are found in"),
                 layout_columns(colwidths=c(8,4),
                                   fileInput("otutab",
                                             label=tags$span("Upload sequence table",
                                                             tags$i(tooltip(bs_icon("info-circle"),
                                                                            tagList(p(style='text-decoration-line:underline; font-weight:bold; text-transform: uppercase;',"What is a sequence table?"),
                                                                                    p(style='text-align: left', "Also known as an ASV table or OTU table, the feature data table is a table that records the number of times a taxonomic entity (ASV, OTU, ESV, etc) occurs in each sample. Each row of the table represents a unique taxonomic entity, and each column a sample."),
                                                                                    p(style='font-weight:bold',"See 'Help' tab for an example."))
                                                                            ))),
                                             accept=c(".csv",".tsv")),
                                   
                                   layout_columns(
                                     radioButtons("otu_sep","File separator", #select separator in the file
                                                  choices=c("Comma (.csv)"=",","Semicolon (.csv)"=";","Tab (.tsv)"="\t"),
                                                  selected=","),
                                     checkboxInput("otu_header","Header",TRUE) #does the file have a header
                                     )))

Card_TAX <- tagList(card_header(h5("Taxonomy")),
                    p("Here, we are looking for information about what species were found in your dataset and what sequences they belong to. This should come in the form of a table with a unique row per sequence and information about that sequence in columns"),
                    layout_columns(colwidths=c(8,4),
                                   fileInput("taxtab",
                                             label=tags$span("Upload taxonomy table",
                                                             tags$i(tooltip(bs_icon("info-circle"),
                                                                            tagList(p(style='text-decoration-line:underline; font-weight:bold; text-transform: uppercase;',"What is a taxonomy table?"),
                                                                                    p(style='text-align: left', "The taxonomy table is a table that reflects features associated with each taxonomic entity (ASV, OTU, ESV, etc) found in the dataset. Each row of the table represents a unique taxonomic entity, and each column a feature, often taxonomic rank."),
                                                                                    p(style='font-weight:bold',"See 'Help' tab for an example."))
                                                                            ))),
                                             accept=c(".csv",".tsv")),
                                   layout_columns(
                                     radioButtons("tax_sep","File separator", #select separator in the file
                                                  choices=c("Comma (.csv)"=",","Semicolon (.csv)"=";","Tab (.tsv)"="\t"),
                                                  selected=","),
                                     checkboxInput("tax_header","Header",TRUE) #does the file have a header
                                   )))

##########################################################################################
uploadpage <- nav_panel("Files",
          shinyFeedback::useShinyFeedback(),
          open=TRUE,
          
          # page navigation buttons 
          layout_column_wrap( 
            width=1/2,fill=TRUE,
            actionButton("fileupload_prevpage","Previous page",width='100%'),
            actionButton("fileupload_nextpage_v2","Next step",width='100%')
            ),
          br(),
          
          # header and description
          HTML("<h4 style='text-align:center;'>Select the option that fits your data best: </h4> <p>We have provided a few options for you to work through your data, depending on what state the data are currently in. Click the buttons on the left to decide which is best for you.</p> <p><em>To work through a demonstration, click the last button and load the example dataset. </em></p>"),
          br(),
          
          # Data processing selection 
          layout_columns(col_widths=c(4,8),
                         #left side of page; choice selection
                         radioGroupButtons(inputId = "selectupload", 
                                           width='100%', individual=TRUE,
                                           choiceNames = c(HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i><u>My data have been completely processed:</i></p></u> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>I only need analyses.</p>"),
                                                           HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i><u>I have already used this tool to assign traits and have my trait table:</i></p></u> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>I only need analyses.</p>"),
                                                           HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i><u>My data have been partially processed:</i></p></u> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>I have an OTU table and I need to identify the species and analyze.</p>"), 
                                                           HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i><u>I have raw sequencing data:</i></p></u> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>My files are coming directly from the sequencing facility.</p>"), 
                                                           HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i><u>Use example data:</i></p></u> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>You can use this dataset to explore the feautures of the app prior to using it on your own data.</p>")
                                                           ),
                                           choiceValues = c("choose_analysis",
                                                            "choose_return",
                                                            "choose_partial",
                                                            "choose_raw",
                                                            "choose_ex"),
                                           direction = 'vertical',
                                           selected=NULL
                         ),
                         
                         #right side of page; upload options
                         card(
                           conditionalPanel(condition="input.selectupload == 'choose_raw'",
                                            p("Start here if you have raw sequencing data. You will need the files from the sequencing facility, usually in FASTQ or .fqz format, a tabular file that contains the indexing barcode sequences attached to each sample, and a metadata file.",
                                            ),
                                            br(),
                                            h4("Go to the Info tab and review the following:"),
                                            actionBttn("view_infodemux",
                                                       label="Demultiplex and Quality Control",
                                                       style="stretch",
                                                       color="primary",
                                                       block=TRUE),
                                            actionBttn("view_infoseq",
                                                       label="Build Sequence Table",
                                                       style="stretch",
                                                       color="primary",
                                                       block=TRUE),
                                            actionBttn("view_infotax2",
                                                       label="Assign Taxonomy",
                                                       style="stretch",
                                                       color="primary",
                                                       block=TRUE)
                                            ),
                           conditionalPanel(condition="input.selectupload == 'choose_partial'",
                                            p("Use this option if you have distilled your sequence data into a single sequence table, but you still need to assign taxonomy to your sequences. "),
                                            br(),
                                            h4("Go to the Info tab and review the following:"),
                                            actionBttn("view_infotax",
                                                       label="Assign Taxonomy",
                                                       style="stretch",
                                                       color="primary",
                                                       block=TRUE)
                           ),
                           conditionalPanel(condition="input.selectupload == 'choose_analysis' | input.selectupload == 'choose_return' ",
                                            Files_MetadataCard,
                                            Card_OTU,
                                            Card_TAX,
                                            actionBttn("assemble_ps",
                                                       label="Build dataset",
                                                       style="unite",
                                                       color="success",
                                                       block=TRUE)
                           ),
                         conditionalPanel(condition="input.selectupload == 'choose_ex'",
                                          Ex_Card
                                          )
                         )
                         ),
          
          # bottom of page; default analysis inputs from ui/ui-defaultanalysis.R
          card(id='default',defaultanalysis),
          
          # bottom page navigation buttons
          layout_column_wrap(
            width=1/2,fill=TRUE,
            actionButton("bfileupload_prevpage","Previous page",width='100%'),
            actionButton("bfileupload_nextpage_v2","Next step",width='100%')
          )
)
##########################################################################################