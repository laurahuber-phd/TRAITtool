source('functions/module-readtable.R')

Review_MetadataCard <- layout_columns(
  #col_widths=c(12,12),
  #row_heights = c(1,5),
  card(
    #full_screen=TRUE,
    card_header("Review metadata file"),
    card_body("This file is needed to connect your sequencing results to the environmental data that you recorded when you were sampling. Make any necessary edits, and don't forget to save! You can reset any changes by clicking the 'Cancel' button. "),
    div(id="meta_table",
    tableUI('meta_table',editable=TRUE)))
)
Review_otuCard <- card(
  #full_screen=TRUE,
  card_header("Overview of your sequence table"),
  div(id="otu_table",
      tableUI('otu_table'))
)
Review_TaxCard <- card(
  #full_screen=TRUE,
  card_header("Overview of your taxonomic assignments"),
  div(id="tax_table",
      tableUI('tax_table'))
)


#############################################################################################################
reviewpage <- nav_panel("Review",
                        shinyFeedback::useShinyFeedback(),
                        layout_column_wrap(
                          width=1/2,fill=TRUE,
                          actionButton("review_prevpage","Previous step"),
                          actionButton("review_nextpage","Next step")
                        ),
                        br(),
                        card( HTML("<h2>Review Files</h2>"),
                              HTML("<p>Before analyzing your sequences, it is a good idea to double check that you have uploaded the right files and that they have uploaded correctly. Do sample names match? Does the metadata file contain all the information you need to answer the questions you have? </p>"),
                              HTML("<small'>As a reminder, the first column of the metadata file should be a set of unique identifiers. These identifiers will be the column names in the sequence table. </small>"),
                              br(),
                              br(),
                              HTML("<h4 style='text-align:center;'>We are going to look at a small sample of the files you uploaded: Does this look like your data? </h4>")
                            ), 
                        jqui_resizable(Review_MetadataCard), #This card is an overview of the metadata
                        br(),
                        HTML("<p>This next part is a sanity check - do the number of sequences make sense? Did every sample show up? If not, now is a good time to go back and figure out why. </p>"),
                        layout_columns( #This card is 50/50 overview of otu/tax tables
                          col_widths=c(6,6),
                          fill=FALSE,
                          jqui_resizable(Review_otuCard),
                          jqui_resizable(Review_TaxCard)
                        ),
                        layout_column_wrap(
                          width=1/2,fill=TRUE,
                          actionButton("breview_prevpage","Previous step"),
                          actionButton("breview_nextpage","Next step")
                        )
)
#############################################################################################################