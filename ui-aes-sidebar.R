light <- bs_theme(#bootswatch = "morph",
  bootswatch = "lumen",
  #bootswatch = "flatly",
  #base_font = font_google("Merriweather"),
  heading_font = font_google("Lora"),
  base_font = font_google("Inter"),
  #heading_font = font_google("Archivo Black"),
) 
dark <- bs_theme(bootswatch="solar",
                 heading_font = font_google("Lora"),
                 base_font = font_collection(font_google("Inter"),"Montserrat"),
                 #bg="#CDDEF5"
)

SelectFiltersCard <- card( 
  full_screen=TRUE,
  card_header("Select filtering methods: ",
              tooltip(bs_icon("info-circle"),
                      "Removing unassigned sequences will make your dataset smaller and analyses run faster, but you will not get a complete picture of the diversity captured by your data. Removing the sample blanks will remove all OTUs found in any of your sample blanks. Generally, we don't recommend doing this, but removing them makes plotting more visually meaningful.")),
  checkboxInput(
    "adjust",
    "Remove unassigned sequences?",
    value = FALSE),
  checkboxInput(
    "rem_blanks",
    "Remove sample blanks?",
    value = FALSE)
)
SelectNormCard <- card(
  full_screen=TRUE,
  card_header("Select Normalization, if any:",
              tooltip(bs_icon('info-circle'),
                      "Normalizing a dataset is done to make findings comparable across samples with uneven sequencing depth. Relative abundance calculates the proportion of the total for each taxa. Rarefaction will randomly subsample the sequences in each sample to match the number of the smallest sample.")),
  #card_body("Note: we do not recommend rarefaction"),
  radioButtons(
    "normoptions",
    label=NULL,
    choiceNames=list('None','Relative Abundance','Presence/Absence','Rarefaction'),
    choiceValues = list("none","relabund",'presence',"raref"),
    selected="none"
  )
)
SelectSamplesCard <- card(
  full_screen=TRUE,
  "Toggle the switch to select only a few samples to look at",
  materialSwitch(inputId='select_switch',
                 value=FALSE,
                 label="Choose"),
  accordion(
    id='select_accord',
    open=FALSE,
    accordion_panel(
      id='select_panel',
      title="Filters",
      selectInput('cols2',
                  "Select filtering group",
                  character()
      ),
      checkboxGroupInput('select_samples',
                         label="Select samples",
                         list()
      ),
      actionButton('go',"Go")
    )
  )
)