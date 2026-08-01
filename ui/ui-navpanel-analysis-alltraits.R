source("ui/ui-navpanel-analysis-temp.R",local=TRUE)

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
producers up to a level of 5 for marine mammals and human"),
                                               tags$img(src='MTI.png',height='200px',width='200px',
                                                        alt="Fishing down the food web")
                                )
                           )
)

Anlys_TraitCard <- tagList(
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Violin plot",
                    Anlys_ViolinCard
    ),
    accordion_panel(title="Bar plot",
                    Anlys_TrophBarCard
    ),
    accordion_panel(title="Line plot",
                    Anlys_TrophLineCard
    ),
    accordion_panel(title="Lollipop plot",
                    Anlys_TrophLolliCard
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
    accordion_panel(title="Violin plot",
                    Anlys_TempViolinCard
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
                                        label="Grouping choices (y-axis)",
                                        icon=icon("gear"),
                                        pickerInput("choose_sizebar_axis",
                                                    label="Groups to compare",
                                                    choices=list(),
                                                    multiple = FALSE,
                                                    options = pickerOptions(maxOptions = 1,
                                                                            container = "body", 
                                                                            actionsBox = TRUE,
                                                                            deselectAllText = "Clear selection"
                                                    )
                                        ),
                                        pickerInput("choose_sizebar_facet",label="Faceting Group",
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
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Figures",
                    Anlys_SizePlot)
  )
)

###############################################################################################
allanalysispage = nav_panel(
  useShinyjs(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("traitanalysis_prevpage","Previous step"),
    actionButton("traitanalysis_nextpage","Next step")
  ),
  h2("Mean Trophic Index and Trait analysis"),
  p("In this page, you can view plots for trait and ecological information."),
  p("We highlight mean trophic index, known temperature range, body size composition, and diversity as main components to look at"),
  p("For further plots and to dive deeper, see the Details page"),
  br(),
  br(),
  
  # Figure overview  
  card(h3("Figures"),
       layout_columns(col_widths=c(6,6),
                      row_heights=c(1,1,1),
                          withProgress(plotOutput("violin_s_plot")),
                          withProgress(plotOutput("trophbar_s_plot")),
                      
                      withProgress(plotOutput("tviolin_s_plot")),
                      withProgress(plotOutput("tlolli_s_plot")),
                          
                      withProgress(plotOutput("sizebar_s_plot")),
                      withProgress(plotOutput("rich_s_plot"))
                          )
       ),
  br(),
  # radar diagram 
  
  # trophic level
  card(
  Anlys_TraitIntroCard, 
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Violin plot",
                    Anlys_ViolinCard
                    ),
    accordion_panel(title="Feeding guild",
                    Anlys_TrophBarCard
                    )
    ),
  #Anlys_ViolinCard
  #Anlys_TraitCard
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
  
  # Fisheries species 
  
  # Alpha diversity 
  #card(
  h3("Diversity"),
  HTML("<p>Here you can look at different ways of measuring biodiversity as a whole</p>"),
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