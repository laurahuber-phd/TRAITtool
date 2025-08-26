source("functions/module-readtable.R",local=TRUE)

Trait_TaxReviewCard <- card(#preview table that you are assigning trophic info to 
  card_header("Overview of your taxonomic assignments"),
  max_height="500px",
  div(id="tax_table2",
      tableUI('tax_table2')
      )
)

Trait_TrophPullCard <- card(
                 selectInput("trait_select",label="Select what taxa to look at:",
                             choices=c("Fish","All species","None"),
                             selected = "Fish" #default selection is only fish species (only fishbase)
                 ),
                 actionButton("trait_assign","Assign trophic level data")
  # progress indicator
)

Trait_TrophReviewCard <- wellPanel(id='viewtrait_panel', #preview trait info 
                                   p("The following species were not found in the fishbase or sealifebase database:"),
                                   verbatimTextOutput("not.names"),
                                   br(),
  #DTOutput("trait_review"),
  downloadButton("dwnldtrait1", "Download trophic assignments", 
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
  ),
  downloadButton("dwnldtraittaxa1", "Download new taxa table", 
                 style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
  ),
  br(),
  h4("Direct output from fishbase and sealifebase:"),
  tableUI("trait_review",editable=FALSE),
  h4("Included with taxonomy table:"),
  tableUI("newtax_review",editable=FALSE),
  h3("If you want to use this table for further analysis, click here: "),
  h5("WARNING: If you go back to change filtering parameters, you will need to redo this step."),
  actionButton('go_addtroph',
               "Go: Add trait information to my taxa table",
               class = 'btn-warning',width="100%")
  )

#############################################################################################################
traitpage = nav_panel(
  useShinyjs(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("trait_prevpage","Previous step"),
    actionButton("trait_nextpage","Next step")
  ),
  br(),
  h2("Trophic Assignment"),
  h4("This page is used to pull trait information from Fishbase and Sealifebase."),
  p("Description of what that means"),
  br(),
  Trait_TaxReviewCard,
  br(),
  br(),
  Trait_TrophPullCard,
  Trait_TrophReviewCard,
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("btrait_prevpage","Previous step"),
    actionButton("btrait_nextpage","Next step")
  )
)
#############################################################################################################