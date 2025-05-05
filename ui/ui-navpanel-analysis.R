source('functions/module-plotting-general.R',local=TRUE)
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)

############################################# Alpha diversity #############################################
Anlys_AlphaCard <- tagList(
  layout_columns(uiOutput("alphametrics_var1"),  #reactive output variable (used to select variables from the metadata for the x axis)
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
                             )
                 ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_rich","Plot"),
                 downloadButton('rich_plotdwnld', label=NULL)),
  plotOutput("rich_plot",click="rich_click") 
)
############################################# Histo #############################################
Anlys_HistoCard <- tagList(
  h5("This tab will generate a histogram showing the occurrences of different taxa"),
  HTML("<p>Use the <u>'Controls'</u> button to change the taxonomic rank visualized or to alter what variable is used on the x-axis. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis.</p>"),
  HTML("<p>Use the <u>'Plot Size' </u> button to change the aspect ratio of the plot. </p>"),
  layout_columns(col_widths=c(4,4,4),
    plotControlsUI('histo_gen'),  #,color=TRUE,x=TRUE,y=FALSE,facet=TRUE
    plotPrettyUI("histo_pretty"),
    plotSizeUI('histo_size')
    ),
  layout_columns(col_widths=c(10,-1,1),
    actionButton("go_histoplot","Plot"),
    downloadButton('histo_plotdwnld', label=NULL)
    ),
  withSpinner(plotOutput("histo_plot",click="histo_click"))
)
############################################# Ordination ##############################################
Ordinate_Card <- layout_column_wrap(
  card(full_screen=TRUE,
    layout_columns(col_widths=c(4,4,4),
                   selectInput("ord_meth",label="Select ordination method",
                               choices=c("NMDS","PCoA","CCA", "RDA", "CAP", "DCA", "DPCoA","MDS"),
                               selected = "NMDS" #default selection is NMDS
                               ),
                   radioButtons("ord_dist",label="Select distance matrix",
                                choices=c("jaccard","bray"),
                                selected = "bray", #default selection is bray
                                inline=TRUE #choices are laid out horizontally
                                ),
                   radioButtons("ord_vis",label="Ordination of samples or taxa?",
                                choices=c("samples","taxa"),
                                selected="samples",
                                inline=TRUE
                                )
                   ),
    actionButton("go_ord","Calculate"),
    conditionalPanel(condition='output.ord_complete',
                     textOutput("ord_complete")
                     )
  )
)
##########################################################################
Anlys_BetaCard <- accordion(
    open=c("Ordination Plot","Ordination Visualization Options"),
    accordion_panel("Ordination Plot",
                    Ordinate_Card,
                    br(),
                    card( full_screen=TRUE,
                          tags$head(tags$style(HTML("
                                                    .bs-select-all {
                                                    display: none;
                                                    }
                                                    .bs-deselect-all {
                                                    width: 100%;
                                                    }
                                                    "))),
                          card_header( layout_columns(col_widths=c(6,6),
                        shinyWidgets::dropdown(inputId="ord_plotgrouping",
                                                     label="Grouping choices",
                                                     icon=icon("gear"),
                      pickerInput("choose_ord_color",label="Color",
                                  choices=list(),
                                  multiple = TRUE,
                                  options = pickerOptions(maxOptions = 1,
                                                          container = "body", 
                                                          actionsBox = TRUE,
                                                          deselectAllText = "Clear selection")),
                      pickerInput("choose_ord_shape",label="Shape",
                                  choices=list(),
                                  multiple = TRUE,
                                  options = pickerOptions(maxOptions = 1,
                                                          container = "body", 
                                                          actionsBox = TRUE,
                                                          deselectAllText = "Clear selection")),
                      radioGroupButtons(inputId = "choose_ord_ellipse", 
                                           label = "Draw ellipse around which variable:",
                                           choices = c("None","Color","Shape"),
                                        checkIcon = list(
                                          yes = tags$i(class = "fa fa-check-square", 
                                                       style = "color: steelblue"),
                                          no = tags$i(class = "fa fa-square-o", 
                                                      style = "color: steelblue")),
                                           selected = "None",
                                        justified=TRUE,
                                           disabled=TRUE)
                      ),
                    shinyWidgets::dropdownButton(inputId="ord_plotsizes",
                                                 label="Size choices",
                                                 circle=FALSE,
                                                 icon=icon("expand"),
                                                 numericInput("ord_plottext", "Plot text size",
                                                              value=16 #starting value 
                                                 ),
                                                 numericInput("ord_plotpoint", "Plot point size",
                                                              value=5 
                                                 ),
                                                 numericInput("ord_plotheight",label="Height of plot",
                                                              value=5
                                                 ),
                                                 numericInput("ord_plotwidth", label="Width of plot",
                                                              value=10
                                                 )
                    ))
                    ),
                    actionButton("go_ord","Plot"),
                      withSpinner(plotOutput("ord_plot",click="ord_click"))
                    )
    ),
    accordion_panel("Stress Plots",
                    open=FALSE, #all dropdown menus are closed to start
                    actionButton("ReturnStress","Show how well the ordination fits"),
                    withSpinner(verbatimTextOutput("converged")),
                    actionButton("PlotStress","Draw a stress plot to show fit of the ordination"),
                    withSpinner(plotOutput("ordstress"))
    ),
    accordion_panel("Scree Plots",
                    open=FALSE, #all dropdown menus are closed to start
                    actionButton("PlotScree","Draw a scree plot to show fit of the ordination"),
                    withSpinner(plotOutput("ordscree"))
    )
  )

############################################# Heat Map #############################################
Anlys_hmCard <- tagList(
  h5("This tab will generate a heatmap to show patterns in when a taxa occurs"),
  HTML("<p>Use the <u>'Controls'</u> button to change the taxonomic rank visualized or to alter what variable is used on the x-axis. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis.</p>"),
  HTML("<p>Use the <u>'Plot Size' </u> button to change the aspect ratio of the plot. </p>"),
  layout_columns(
    plotControlsUI('hm_gen',color=TRUE,x=TRUE,y=FALSE,facet=TRUE
    ),
    plotPrettyUI("hm_pretty"),
    plotSizeUI('hm_size')
  ),
    actionButton("go_hm","Plot"),
    downloadButton('hm_plotdwnld', label=NULL),
    withSpinner(plotOutput("hm_plot",click="hm_click")) #add a reaction when the plot is clicked
  
)

############################################# Violin plot #############################################
Anlys_ViolinCard <- tagList(
  h5("This tab will generate a violin plot"),
  HTML("<p>Use the <u>'Controls'</u> button to change the taxonomic rank visualized or to alter what variable is used on the x-axis. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis.</p>"),
  HTML("<p>Use the <u>'Plot Size'</u> button to change the aspect ratio of the plot. </p>"),
  layout_columns(
    plotControlsUI('violin_gen',color=FALSE,x=TRUE,y=TRUE,facet=TRUE
    ),
    plotPrettyUI("violin_pretty"),
    plotSizeUI('violin_size')
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_violin","Plot"),
                 downloadButton('violin_plotdwnld', label=NULL)),
  withSpinner(plotOutput("violin_plot",click="violin_click")) #add a reaction when the plot is clicked
) 

############################################# Trophic diversity #############################################
Anlys_TrophCard <- layout_sidebar(
  sidebar=sidebar(
    "Select the explanatory variable", #title of sidebar (maintained across all tabs in the panel)
    uiOutput("alphametrics_var")  #reactive output variable (used to select variables from the metadata for the x axis)
  )#,
  #tooltip(bsicons::bs_icon("question-circle"),
  #        "Short explanatory sentence if user has a question",
  #        placement="right"),
  #popover(bsicons::bs_icon("gear", class = "ms-auto"),
  #        #selection options to graph by
  #        title="Use the following measure:"),
)

#############################################################################################################
analysispage <- nav_panel("Analysis",
  layout_column_wrap(width=1/2,fill=TRUE,
    actionButton("analysis_prevpage","Previous step"),
    actionButton("analysis_nextpage","Next step")
    ),
  br(),
  HTML("<h2>Generate figures </h2>"),
  navset_card_tab(id="anlys", 
                nav_panel(id='alpha', title="Species richness",
                          Anlys_AlphaCard
                          ),
                nav_panel(id="taxcomp",title="Taxonomic composition", full_screen=TRUE,
                          Anlys_HistoCard
                          ),
                nav_panel(id="beta", title="Beta biversity",
                          Anlys_BetaCard
                          ),
                nav_panel(id="hm", title="Heat map",
                          Anlys_hmCard
                          ),
                #nav_panel(id="trophic", title="Trophic composition",
                #          Anlys_TrophCard
                #          ),
                nav_panel(id="violin", title="Trophic composition",
                          Anlys_ViolinCard
                )
                )
)
#############################################################################################################