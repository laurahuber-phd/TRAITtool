source("functions/module-readtable.R",local=TRUE)

Trait_TaxReviewCard <- card(#preview table that you are assigning trophic info to 
  card_header("Overview of your taxonomic assignments"),
  max_height="500px",
  div(id="tax_table2",
      tableUI('tax_table2')
      )
)

Trait_TrophPullCard <- card(
  h5("Query information databases to source ecological information about the species in your dataset"),
                 layout_columns(col_widths=c(4,8),
                                selectInput("trait_select",label="Select what database to query:",
                                            choices=c("Both","FishBase","SeaLifeBase"),
                                            selected = "Both" #default selection is all databases
                                ),
                                HTML("<p><u>FishBase</u> collects and gathers information on the fishes of the world, including estimated trophic level, known habitat and distribution, and uses.</p> <p><u>SeaLifeBase</u> was developed as a similar database for non-fish marine species.</p> <p>Default query option is <u>Both</u>, which will query your species against both databases.</p>")
                                ),
                 actionButton("trait_assign","Assign trophic level data"),
                 #withSpinner(id="trait_spinner",
                #             ui_output=DTOutput("trait_summary")
                #             )
                hidden( p(id='p_fishquery',
                         bs_icon("binoculars-fill",size='2em'),
                         textOutput("n_fishquery"))
                        )
)

Trait_TrophReviewCard <- hidden(card(id="trait_trophreview_card",
                              h4("Loading ecological trait assignments gathered from FishBase and SeaLifeBase:"),
                              p("This uses the species assignments in your taxonomy table to compare against the species in Fishbase, the global database of fishes, and SeaLifeBase, the database for non-fish marine species"),
  hidden(wellPanel( #base/loading screen
    id='viewtrait_panel', #preview trait info 
    h5("The following species were not found in the FishBase or SeaLifeBase database:"),
    p("This does not necessarily mean that the species are not in those databases, but they may be under a synonym or spelled incorrectly. We recommend checking these species manually."),
    verbatimTextOutput("not.names"),
    br(),
    
    #DTOutput("trait_review"),
    downloadButton("dwnldtrait1", "Download trophic assignments", 
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
    downloadButton("dwnldtraittaxa1", "Download new taxa table", 
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
    br()
  )),
  hidden(wellPanel( #trait df 
    id="loading_traits",
    h4("Trait table:"),
    withSpinner(id="trait_spinner_review",
                ui=tableUI("trait_review",editable=FALSE))
  )),
  hidden(wellPanel( #new taxa df
    id="loading_traittax",
    h4("Taxonomy table with traits added:"),
    withSpinner(id="trait_spinner_taxreview",
                ui=tableUI("newtax_review",editable=FALSE)),
    br(),
    br()
  )),
  hidden(wellPanel( #add to ps object
    id="add_ps_trait",
    h3("If you want to use this table for further analysis, click here: "),
    h5("WARNING: If you go back to change any parameters, you will need to redo this step."),
    actionButton('go_addtroph',
                 "Go: Add trait information to my taxa table",
                 class = 'btn-warning',width="100%"),
    br()
  )),
  hidden( p(id='trait_ps_success',
            bs_icon("patch-check",size='4em'),
            "Success!"))
)
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
  h4("This page is used to pull trait information from FishBase and SeaLifeBase."),
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
  ),
  tags$footer(style="display: flex;justify-content: space-between;",
              tags$img(src='smiths_logoLong.png',height='40px',
                       alt="Smithsonian sun logo"),
              tags$img(#src='ShinyFooter.png',
                src='titlefooter.png',
                height='30px',
                alt="William H. Donner Foundation and Smithsonian Women's Committee logos"))
)
#############################################################################################################