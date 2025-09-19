library(shinyjs)
# ---- # Blanks # ----

## ---- select blanks ## ---- 
Select_BlanksCard <- card( 
  full_screen=TRUE,
  card_header("Select which (if any) of your samples are blanks (sequencing, PCR, or experimental)"),
  HTML("<p>During the process of collecting samples, filtering water, or performing PCR, you likely had 'blanks' for quality control and to track any sources of contamination. Here, you can identify which samples are blanks, so we can remove them from the analysis later on. </p>"),
  height=600,
  layout_columns(
    col_widths=c(4,8), #max value of 12, so the ID card takes up 1/3 of the window space
    card(
      card_header("ID samples"),
      layout_columns(col_widths = c(12,12),
                     row_heights=c(4,1),
                     selectInput('blank_cols', 'Which column is your sample names?',character()),
                     checkboxInput(
                       "rem_blanks", "Remove sample blanks?", value = FALSE)
                     #uiOutput('meta_colnames') #reactive output variable (used to ID column names for metadata file and make a dropdown list)
      )), 
    card(
      card_header("Select Sample Blanks"),
      checkboxGroupInput("blank_samples",
                         label="Check boxes of each blank",
                         list())
      #uiOutput('blank_samples_check') #reactive output variable (used to select sample names from a list)
    )
  ),
  conditionalPanel(condition="input.rem_blanks==true",
                        wellPanel(textOutput('remblank_bef'),textOutput('remblank_aft'),
                                  textOutput('remblanktax_bef'),textOutput('remblanktax_aft')
                                  ))
)

# ---- # Samples # ---- 

## ---- min num ## ---- 
SelectPruneCard <- card(
  full_screen = TRUE,
  card_header("Select pruning methods: ", tooltip(bs_icon("info-circle"),
                      "Use this section to choose a minimum number of times a taxa occurs and/or how small of a sample to include. "
                      ),
              ),
  "Removing sequences with only one occurrence can reduce the likelihood of overestimating diversity. Samples with dramatically fewer sequences can also bias analyses.",
  numericInput("prune_samples", "Remove samples with less than this many sequences:", value = 100),
  numericInput("prune_tax","Remove sequences with less than this many occurrences:",value = 1)
)

## ---- select samples ## ---- 
SelectSamplesCard <- card(
  full_screen=TRUE,
  card_header("Select which samples to analyze"),
  "Choose this if you only want to look at a couple specific samples. You don't need to do anything here if you are interested in viewing the entire dataset.",
  br(),
  h5("Click on the toggle to select samples. Clicking it again will reset all selections. "),
  useShinyjs(),
  materialSwitch(inputId='sample_switch', value=FALSE, label="Subset samples?"), #toggle to open accordion 
  
  conditionalPanel(condition="input.sample_switch==true",
                   splitLayout(cellWidths = c("50%","50%"),
                               verticalLayout(selectInput('sample_filterby',"Filter by the following:",
                                                          character()
                               ),
                               checkboxGroupInput('sample_select', label="Select samples",
                                                  list()
                               )),
                               verticalLayout(br(),
                                              actionButton('go_sample',"Go: Use selected samples"))
                               )
                               ),
  shinyjs::hidden(
         wellPanel(id="befaft_sample",textOutput("selsamp_bef"),textOutput("selsamp_aft"))
         )
)
# ---- # Taxa # ---- 
SelectTaxaCard <- card(
  full_screen=TRUE,
  card_header("Select which taxa to analyze"),
  "Choose this if you want to specify which taxa to include/exclude. You don't need to do anything here if you are interested in viewing the entire dataset.",
  br(),
  h5("Click on the toggle to select samples. Clicking it again will reset all selections."),
  materialSwitch(inputId='tax_switch', value=FALSE, label="Subset taxa?"), #toggle to open accordion 
  conditionalPanel(condition="input.tax_switch==true",
                   
                   splitLayout(cellWidths = c("50%","50%"),
                               verticalLayout(
                                 selectInput('tax_filterby',"Select rank",
                                             character() ),
                                 checkboxGroupInput('tax_select',label="Select taxa",
                                                    list())
                               ),
                               card("Keep or remove the selected taxa?",
                                              switchInput(inputId='tax_keepremove',
                                                          value=FALSE, width = "100%",
                                                          inline=TRUE,
                                                          onLabel = "Keep",
                                                          offLabel = "Remove"),
                                              actionButton('go_tax',"Go: prune selected taxa",class = 'btn-warning')
                               )
                   )
                   
  ),
  shinyjs::hidden(
    wellPanel(id="befaft_tax",textOutput("seltax_bef"),textOutput("seltax_aft"),textOutput("selrank"),textOutput('seltax'))
  )
)

# ---- Normalization # ---- 
SelectNormCard <- card(
  full_screen=TRUE,
  card_header("Select Normalization, if any:",tooltip(bs_icon('info-circle'),
                                                      "Normalizing a dataset is done to make findings comparable across samples with uneven sequencing depth. Relative abundance calculates the proportion of the total for each taxa. Rarefaction will randomly subsample the sequences in each sample to match the number of the smallest sample.")),
  radioButtons(
    "normoptions",label=NULL,
    choiceNames=list('None','Relative Abundance','Presence/Absence','Rarefaction','Taxa occurring in at least this many samples:','Taxa with relative abundance greater than:'),
    choiceValues = list("none","relabund",'presence',"raref","tax_count","tax_rel"),
    selected="none"
    ),
  conditionalPanel(condition="input.normoptions == 'tax_count'",
                   numericInput("abund_cutoff",label="Minimum number of samples a taxa should appear in",
                                value=0)
                   ),
  conditionalPanel(condition="input.normoptions == 'tax_rel'",
                   numericInput("mean_cutoff",label="Minimum total percentage of sequences a taxa should appear in",
                                value=0,min=0,max=1,step=0.01)
                   )
)

#############################################################################################################
filterpage <- nav_panel(
  useShinyjs(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("filter_prevpage","Previous step"),
    actionButton("filter_nextpage","Next step")
  ),
  HTML("<h2>Choose filtering options </h2>"),
  "Use the options on this page to define what part of your dataset to evaluate. You can choose only a few samples or taxa, for example, or remove blanks and singletons. ",
  br(),
  card(
  layout_columns(col_widths=c(6,6),
                 SelectPruneCard,
                 SelectNormCard
                 ),
  layout_columns(col_widths=c(6,6),
                 wellPanel(id='befaft_prune',
                             textOutput("prune_bef"),textOutput("prune_aft"),
                             textOutput("prunetax_bef"),textOutput("prunetax_aft")),
                 shinyjs::hidden(wellPanel(id="befaft_norm",
                                           textOutput("norm_bef"),textOutput("norm_aft")))
                 )
  ),
  jqui_resizable(Select_BlanksCard), #This card allows you to select the sequencing blanks,
  SelectSamplesCard,
  SelectTaxaCard,
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("bfilter_prevpage","Previous step"),
    actionButton("bfilter_nextpage","Next step")
  ),
  tags$footer(style="display: flex;justify-content: space-between;",
              tags$img(src='smiths_logoLong.png',height='40px',
                       alt="Smithsonian sun logo"),
              tags$img(#src='ShinyFooter.png',
                src='titlefooter.png',
                height='30px',
                alt="William H. Donner Foundation and Smithsonian Women's Committee logos"))
)