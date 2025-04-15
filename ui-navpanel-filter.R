
# ---- # Blanks # ----

## ---- select blanks ## ---- 
Select_BlanksCard <- card( 
  full_screen=TRUE,
  card_header("Select which (if any) of your samples are blanks (sequencing, PCR, or experimental)"),
  HTML("<p>During the process of collecting samples, filtering water, or performing PCR, you likely had 'blanks' for quality control and to track any sources of contamination. Here, you can identify which samples are blanks, so we can remove them from the analysis later on. </p>"),
  height=800,
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
                        textOutput('remblank_bef'),textOutput('remblank_aft'))
)

# ---- # Samples # ---- 

## ---- min num ## ---- 
SelectPruneCard <- card(
  full_screen = TRUE,
  card_header("Select pruning methods: ", tooltip(bs_icon("info-circle"),
                      "Use this section to choose a minimum number of times a taxa occurs and/or how small of a sample to include. "
                      ),
              ),
  numericInput("prune_samples", "Remove samples below:", value = 10),
  numericInput("prune_tax","Remove taxa below:",value = 10)
)

## ---- select samples ## ---- 
SelectSamplesCard <- card(
  full_screen=TRUE,
  "Toggle the switch to select only a few samples to look at",
  
  materialSwitch(inputId='sample_switch', value=FALSE, label="Choose"), #toggle to open accordion 
  accordion( id='sample_accord', open=FALSE,
    accordion_panel( id='select_panel',title="Pick samples",
      actionButton('go_samples',"Go: Use selected samples"),
      selectInput('sample_cols',"Select filtering group",
                  character()
                  ),
      checkboxGroupInput('select_samples', label="Select samples",
                         list()
                         )
    )
  ),
  conditionalPanel(condition="input.go_samples.onclick & input.sample_switch==true",
                   textOutput("selsamp_bef"),textOutput("selsamp_aft"))
)
# ---- # Taxa # ---- 
SelectTaxaCard <- card(
  full_screen=TRUE,
  "Toggle the switch to select only a few taxa to look at",
  materialSwitch(inputId='taxa_switch', value=FALSE, label="Choose"), #toggle to open accordion 
  accordion( id='select_taxaccord', open=FALSE,
    accordion_panel( id='select_taxpanel',title="Pick taxa",
      "Keep or remove the selected taxa?",
      switchInput(inputId='adjust',value=FALSE, width = "100%",
                  onLabel = "Keep",
                  offLabel = "Remove"),
      actionButton('go_taxa',"Go"),
      selectInput('select_rank',"Select rank",
                  character()
                  ),
      checkboxGroupInput('select_taxa',label="Select taxa",
                         list()
                         )
      )
  )
)

# ---- Normalization # ---- 
SelectNormCard <- card(
  full_screen=TRUE,
  card_header("Select Normalization, if any:",tooltip(bs_icon('info-circle'),
                                                      "Normalizing a dataset is done to make findings comparable across samples with uneven sequencing depth. Relative abundance calculates the proportion of the total for each taxa. Rarefaction will randomly subsample the sequences in each sample to match the number of the smallest sample.")),
  radioButtons(
    "normoptions",label=NULL,
    choiceNames=list('None','Relative Abundance','Presence/Absence','Rarefaction'),
    choiceValues = list("none","relabund",'presence',"raref"),
    selected="none"
    )
)

#############################################################################################################
filterpage <- nav_panel(
  card(card_header("Comparison"),
       layout_columns(col_widths = c(6,6),
                      textOutput("psog_filter"),
                      textOutput("pstext_filter")
                      )
       ),
  card(
  layout_columns(col_widths=c(6,6),
                 SelectPruneCard,
                 SelectNormCard
  ),
  textOutput("prune_bef"),
  textOutput("prune_aft")),
  jqui_resizable(Select_BlanksCard), #This card allows you to select the sequencing blanks,
  SelectSamplesCard,
  SelectTaxaCard
)