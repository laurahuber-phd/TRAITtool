source("functions/module-readtable.R",local=TRUE)

Trait_TaxReviewCard <- card(#preview table that you are assigning trophic info to 
  card_header("Overview of your taxonomic assignments"),
  max_height="300px",
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
  # progress indicator?  
  
)

Trait_TrophReviewCard <- card( #preview trait info 
DTOutput("trait_review")
  )

#############################################################################################################
traitpage = nav_panel(
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