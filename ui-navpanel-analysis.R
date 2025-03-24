#source('mod-plot2.R')
source('module-plotting-general.R',local=TRUE)
source('module-plotting-pretty.R',local=TRUE)
source('module-plotting-size.R',local=TRUE)

############################################# Alpha diversity #############################################
Anlys_AlphaCard <- tagList(#layout_sidebar(
  #sidebar=sidebar(
  #  "Select the explanatory variable", #title of sidebar (maintained across all tabs in the panel)
  #  uiOutput("alphametrics_var1")  #reactive output variable (used to select variables from the metadata for the x axis)
  #),
  layout_columns(#"Select the explanatory variable", #title of sidebar (maintained across all tabs in the panel)
                 uiOutput("alphametrics_var1"),  #reactive output variable (used to select variables from the metadata for the x axis)
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
  layout_columns(
    col_widths=c(10,-1,1),
    actionButton("DrawAlphaPlot","Plot"),
            downloadButton('download_richplot', label=NULL)),
  plotOutput("richness",click="rich_click") #add a reaction when the plot is clicked
  
)

############################################# Ordination ##############################################
PlottingChoices_Card <- card(
  numericInput("disp_text",  
               "Plot text size",
               min=1,
               max=100,
               #max=50,
               step=1,
               value=16 #starting value 
  ),
  numericInput("disp_point", 
               "Plot point size",
               min=1,
               max=100,
               #max=50,
               step=1,
               value=5 #starting value 
  )
)
Ordinate_Card <- layout_column_wrap(
  card(
    full_screen=TRUE,
    #card_header("Ordination"),
    layout_columns(col_widths=c(4,4,4),
                   selectInput("ordmeth",
                               label="Select ordination method",
                               choices=c("NMDS","PCoA","CCA", "RDA", "CAP", "DCA", "DPCoA","MDS"),
                               selected = "NMDS" #default selection is NMDS
                               ),
                   radioButtons("orddist",
                                label="Select distance matrix",
                                choices=c("jaccard","bray"),
                                selected = "bray", #default selection is bray
                                inline=TRUE #choices are laid out horizontally
                                ),
                   radioButtons("ordvis",
                                label="Ordination of samples or taxa?",
                                choices=c("samples","taxa"),
                                selected="samples",
                                inline=TRUE
                                )
                   ),
    actionButton("calc_ord","Calculate"),
    conditionalPanel(condition='output.ord_complete',
                     textOutput("report_ord")
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
                        shinyWidgets::dropdownButton(inputId="ordgen_controls",
                                                     label="Grouping choices",
                                                     circle=FALSE,
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
                      #pickerInput("choose_ord_ellipse",label="Ellipse grouping",
                      #            choices=list(),
                      #            multiple = TRUE,
                      #            options = pickerOptions(maxOptions = 1,
                      #                                    container = "body", 
                      #                                    actionsBox = TRUE,
                      #                                    deselectAllText = "Clear selection"))
                      ),
                    shinyWidgets::dropdownButton(inputId="ordgen_controls",
                                                 label="Size choices",
                                                 circle=FALSE,
                                                 icon=icon("expand"),
                                                 numericInput("disp_text", "Plot text size",
                                                              value=16 #starting value 
                                                 ),
                                                 numericInput("disp_point", "Plot point size",
                                                              value=5 
                                                 ),
                                                 numericInput("ordplot_height",label="Height of plot",
                                                              value=5
                                                 ),
                                                 numericInput("ordplot_width", label="Width of plot",
                                                              value=10
                                                 )
                    ))
                    ),
                      withSpinner(plotOutput("ord",click="ord_click")),
                      actionButton("DrawPlot","Plot")
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
analysispage <- nav_panel(
  "Analysis",
  navset_card_tab(id="anlys", 
                nav_panel(id='alpha',
                          title="Alpha Diversity",
                          Anlys_AlphaCard
                ),
                nav_panel(id="taxcomp",
                          title="Taxonomic composition",full_screen=TRUE,
                          layout_columns(
                            plotControlsUI('histo_gen'#,color=TRUE,x=TRUE,y=FALSE,facet=TRUE
                            ),
                              plotPrettyUI("histo_pretty"),
                              plotSizeUI('histo_size')
                            ),
                            layout_columns(
                              col_widths=c(10,-1,1),
                              actionButton("plot_histo","Plot"),
                              downloadButton('download_histoplot', label=NULL)),
                            withSpinner(plotOutput("histo",click="histo_click"))
                ),
                nav_panel(id="beta",
                          title="Beta Diversity",
                          Anlys_BetaCard
                ),
                nav_panel(id="trophic",
                          title="Trophic composition",
                          Anlys_TrophCard
                )
                ), actionButton("analysis_prevpage","Previous step",width='100%')
)
#############################################################################################################