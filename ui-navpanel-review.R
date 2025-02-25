source('module-readtable.R')

Review_MetadataCard <- layout_columns(
  #col_widths=c(12,12),
  #row_heights = c(1,5),
  card(
    #full_screen=TRUE,
    card_header("Metadata information"),
    card_body("We will use this information to connect your sequencing results to environmental data that you recorded."),
    tableUI('meta_table',editable=TRUE))
)
Review_otuCard <- card(
  #full_screen=TRUE,
  card_header("Overview of your sequence table"),
  tableUI('otu_table')
)
Review_TaxCard <- card(
  #full_screen=TRUE,
  card_header("Overview of your taxonomic assignments"),
  tableUI('tax_table')
)
Select_BlanksCard <- card( 
  full_screen=TRUE,
  card_header("Next, select which (if any) of your samples are blanks (sequencing, PCR, or experimental)"),
  height=400,
  layout_columns(
    col_widths=c(4,8), #max value of 12, so the ID card takes up 1/3 of the window space
    card(
      card_header("ID samples"),
      selectInput('cols', 'Which column is your sample names?',character())
      #uiOutput('meta_colnames') #reactive output variable (used to ID column names for metadata file and make a dropdown list)
    ), 
    card(
      card_header("Select Sample Blanks"),
      checkboxGroupInput("blank_samples",
                         label="Check boxes of each blank",
                         list())
      #uiOutput('blank_samples_check') #reactive output variable (used to select sample names from a list)
    )
  )
)

#############################################################################################################
reviewpage <- nav_panel("Review",
                        shinyFeedback::useShinyFeedback(),
                        card("We are going to look at a small sample of the files you uploaded: Does this look like your data?"), 
                        jqui_resizable(Review_MetadataCard), #This card is an overview of the metadata
                        jqui_resizable(Select_BlanksCard), #This card allows you to select the sequencing blanks
                        layout_column_wrap( #This card is 50/50 overview of otu/tax tables
                          width=1,
                          fill=FALSE,
                          jqui_resizable(Review_otuCard),
                          jqui_resizable(Review_TaxCard)
                        ),
                        layout_column_wrap(
                          width=1/2,fill=TRUE,
                          actionButton("reviewfiles_prevpage","Previous step"),
                          actionButton("reviewfiles_nextpage","Next step")
                        )
)
#############################################################################################################