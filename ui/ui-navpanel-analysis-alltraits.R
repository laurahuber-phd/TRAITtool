source("ui/ui-navpanel-analysis-temp.R",local=TRUE)
source("ui/ui-navpanel-analysis-trait.R",local=TRUE)

####################################### Trophic Level ################################################
Anlys_TraitIntroCard <- tagList(
                           h3("Trophic level analysis"),
                           p("Here you can view plots regarding patterns in mean trophic index of your data."),
                           br(),
                           card(h4("About Trophic Index"),
                                p(),
                                layout_columns(col_widths=c(8,4),
                                               p("Explanation of MTI and trophic level and how this can help inform fisheries management. Example phrasing includes: The marine trophic index measures the change in mean
trophic level of fisheries landings by region and globally. Trophic level is defined as the
position of an organism in the food chain, and ranges from a value of 1 for primary
producers up to a level of 5 for marine mammals and human."),
                                               tags$img(src='MTI.png',height='200px',width='200px',
                                                        alt="Fishing down the food web")
                                )
                           )
)

####################################### Temperature ################################################
Anlys_TempCard <- tagList(
  h3("Temperature range analysis"),
  p("Here you can view plots regarding species' temperature preference."),
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Temperature index plot",
                    Anlys_TempViolinCard
    ),
    accordion_panel(title="Temperature plot",
                    Anlys_TempChoiceViolinCard 
    ),
    accordion_panel(title="Lollipop plot",
                    Anlys_TempLolliCard
    )
  )
)

####################################### Size ################################################
Anlys_SizePlot <- tagList(
  layout_columns(col_widths=c(4,4,4),
                 shinyWidgets::dropdown(inputId="sizebar_plotgrouping",
                                        label="Grouping choices",
                                        icon=icon("gear"),
                                        pickerInput("choose_sizebar_axis",
                                                    label="Body size data",
                                        #            label="Groups to compare",
                                                    choices=list(),
                                                    multiple = FALSE,
                                                    options = pickerOptions(maxOptions = 1,
                                                                            container = "body", 
                                                                            actionsBox = TRUE,
                                                                            deselectAllText = "Clear selection"
                                                    )
                                        ),
                                        pickerInput("choose_sizebar_facet",
                                                    label="Groups to compare",
                                                    #label="Faceting Group",
                                                    choices=list(),
                                                    multiple = TRUE,
                                                    options = pickerOptions(maxOptions = 2,
                                                                            container = "body", 
                                                                            actionsBox = TRUE,
                                                                            deselectAllText = "Clear selection"
                                                    )
                                        )
                 ),
                 plotPrettyUI("sizebar_pretty"),
                 plotSizeUI('sizebar_size')
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_sizebar","Plot"),
                 downloadButton('sizebar_plotdwnld', label=NULL)
  ),
  withSpinner(plotOutput("sizebar_plot",click="sizebar_click"))
)

Anlys_SizeCard <- tagList(
  h3("Body characteristic analysis"),
  p("Here you can view plots regarding species' body size and time to maturity."),
  HTML("<p>Use the <u>'Grouping Choices'</u> button to change what body size measurement is visualized. You can also add up to two 'grouping' variables, which will separate the data into different plots based on which variables you group by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis. You can also change the plot and axes titles here. </p>"),
  HTML("<p>Use the <u>'Plot Size' </u> button to change the aspect ratio of the plot. </p>"),
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Figures",
                    Anlys_SizePlot)
  )
)

####################################### IUCN ################################################
Anlys_IUCNPlot <- tagList(
  layout_columns(col_widths=c(4,4,4),
                 shinyWidgets::dropdown(inputId="iucnbar_plotgrouping",
                                        label="Grouping choices",
                                        icon=icon("gear"),
                                        pickerInput("choose_iucnbar_axis",
                                                    label="IUCN data",
                                                    choices=list(),
                                                    multiple = FALSE,
                                                    options = pickerOptions(maxOptions = 1,
                                                                            container = "body", 
                                                                            actionsBox = TRUE,
                                                                            deselectAllText = "Clear selection"
                                                    )
                                        ),
                                        pickerInput("choose_iucnbar_facet",
                                                    label="Groups to compare",
                                                    #label="Faceting Group",
                                                    choices=list(),
                                                    multiple = TRUE,
                                                    options = pickerOptions(maxOptions = 2,
                                                                            container = "body", 
                                                                            actionsBox = TRUE,
                                                                            deselectAllText = "Clear selection"
                                                    )
                                        )
                 ),
                 plotPrettyUI("iucnbar_pretty"),
                 plotSizeUI('iucnbar_size')
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_iucnbar","Plot"),
                 downloadButton('iucnbar_plotdwnld', label=NULL)
  ),
  withSpinner(plotOutput("iucnbar_plot",click="iucnbar_click"))
)

Anlys_IUCNCard <- tagList(
  h3("IUCN Status"),
  p("Here you can view plots regarding IUCN Red List status of species in your dataset."),
  HTML("<p>Use the <u>'Grouping Choices'</u> button to change what body size measurement is visualized. You can also add up to two 'grouping' variables, which will separate the data into different plots based on which variables you group by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis. You can also change the plot and axes titles here. </p>"),
  HTML("<p>Use the <u>'Plot Size' </u> button to change the aspect ratio of the plot. </p>"),
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Figures",
                    Anlys_IUCNPlot)
  )
)
############################################# Alpha diversity #############################################
Anlys_AlphaCard <- tagList(
  layout_columns(
    shinyWidgets::dropdown(inputId="alpha_plotgrouping",
                           label="Grouping choices",
                           icon=icon("gear"),
                           
                           selectInput('alpha_xvar',
                                       label='Select the explanatory variable',
                                       choices=""),
                           selectInput('alpha_facet',
                                       label='Select faceting variable',
                                       choices="")
    ),
    selectInput("alphametrics",
                "Which alpha diversity measures?",
                c(
                  "Observed Richness"="Observed",
                  "Shannon"="Shannon",
                  "Chao" = "Chao1",
                  "ACE"="ACE", 
                  "Simpson Index" = "Simpson", 
                  "Inverse Simpson" = "InvSimpson", 
                  "Fisher"="Fisher"
                ),
                selected="Shannon",
                multiple=TRUE
    ),
    checkboxInput(
      "alpha_box", "Overlay box plot?", value = FALSE)
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_rich","Plot"),
                 downloadButton('rich_plotdwnld', label=NULL)),
  plotOutput("rich_a_plot",click="rich_click") 
)

###############################################################################################
allanalysispage = nav_panel(
  useShinyjs(),
  tags$style(HTML(".card2 {border: 10px solid black; border-radius: 1.5rem;}")), # creating class style
  tags$style(HTML(".card3 {border: 2px solid grey; border-radius: 15px 50px; border-style: dotted;}")), # creating class style
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("traitanalysis_prevpage","Previous step"),
    actionButton("traitanalysis_nextpage","Next step")
  ),
  h2("Trait analysis"),
  p("In this page, you can view plots for trait and ecological information."),
  p("We highlight mean trophic index, known temperature range, body size composition, and diversity as main components to look at."),
  br(),
  h5("For further plots and to dive deeper, see the Details page"),
  br(),
  p("To make any changes to these plots, scroll down to the appropriate section and use the controls to adjust."),
  br(),
  
  # Figure overview  
  card(h3("Trophic Composition"),
       layout_columns(col_widths=c(6,6),
                      card(h5("Mean Trophic Index"),
                           withSpinner(plotOutput("violin_s_plot")),
                           class="card3"),
                      card(h5("Feeding guilds"),
                           withSpinner(plotOutput("trophbar_s_plot")),
                           class="card3")
                      ),
       class="card2"       # <<<----- applying the new class
       ),
  card(h3("Temperature Preference"),
       layout_columns(col_widths=c(6,6),
                      card(withSpinner(plotOutput("tviolin_s_plot")),
                           class="card3"),
                      card(withSpinner(plotOutput("tviolinmean_s_plot")),
                           class="card3")#,
                      #withSpinner(plotOutput("tlolli_s_plot"))
                      ),
       class="card2"       # <<<----- applying the new class
       ),
  card(h3("Body Size and IUCN status"),
       layout_columns(col_widths=c(6,6),
                      card(h5("Adult Body Size"),
                           withSpinner(plotOutput("sizebar_s_plot")),
                           class="card3"),
                      card(h5("IUCN Red List Status"),
                           withSpinner(plotOutput("iucnbar_s_plot")),
                           class="card3")
                      ),
       class="card2"       # <<<----- applying the new class
       ),
  card(h3("Overall richness"),
       card(withSpinner(plotOutput("rich_s_plot")),
            class="card3"),
       class="card2"       # <<<----- applying the new class
  ),
  br(),
  # radar diagram 
  
  # trophic level
  card(
  Anlys_TraitIntroCard, 
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Mean trophic index",
                    Anlys_ViolinCard
                    ),
    accordion_panel(title="Feeding guild",
                    Anlys_TrophBarCard
                    )
    ),
  #Anlys_ViolinCard
  ), 
  br(),
  
  # temperature 
  Anlys_TempCard,
  br(),
  
  # body size
  Anlys_SizeCard,
  br(),
  
  # Feeding guild 
  #Anlys_TrophBarCard, 
  #br(),
  
  # Age at maturity 
  
  # IUCN status 
  Anlys_IUCNCard,
  br(),
  # Fisheries species 
  
  # Alpha diversity 
  #card(
  h3("Community Diversity"),
  HTML("<p>Here you can look at different ways of measuring biodiversity as a whole.</p>"),
  accordion(
    open=FALSE,
    multiple=TRUE,
    accordion_panel(title="Alpha diversity",
                    Anlys_AlphaCard
    )
  ), 
  
  
  br(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("btraitanalysis_prevpage","Previous step"),
    actionButton("btraitanalysis_nextpage","Next step")
  )
)