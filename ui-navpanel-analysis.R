#source('mod-plot2.R')

############################################# Alpha diversity #############################################
Anlys_AlphaCard <- layout_sidebar(
  sidebar=sidebar(
    "Select the explanatory variable", #title of sidebar (maintained across all tabs in the panel)
    uiOutput("alphametrics_var1")  #reactive output variable (used to select variables from the metadata for the x axis)
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
              selected="Shannon"
  ),
  actionButton("DrawAlphaPlot","Plot"),
  plotOutput("richness",click="rich_click") #add a reaction when the plot is clicked
  
)
Anlys_HistoCard <- layout_sidebar(
  sidebar=sidebar(
    "Select the explanatory variable", #title of sidebar (maintained across all tabs in the panel)
    uiOutput("alphametrics_var2")  #reactive output variable (used to select variables from the metadata for the x axis)
  ),
  selectInput("taxa",
              label="Choose a taxonomic rank to visualize:",
              choices=c("Kingdom","Phylum","Class","Order","Family","Genus",'Species'),
              multiple=FALSE
  ),
  actionButton("DrawTaxPlot","Plot"),
  withSpinner(plotOutput("histo", click="histo_click" #,hover = "histohover" #goal is to have the taxa name show up when the mouse is hovered over
  )), #includes loading animation while plotting
  #verbatimTextOutput("histoinfo")
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
  )
)
##########################################################################
Anlys_BetaCard <- layout_sidebar(
  sidebar=accordion(accordion_panel("Ordination",Ordinate_Card),
                    accordion_panel("Plotting aesthetics",PlottingChoices_Card)
  ),
  accordion(
    open=c("Ordination Plot","Ordination Visualization Options"),
    accordion_panel("Ordination Plot",
                    card(
                      full_screen=TRUE,
                      card_header(
                        tooltip(bsicons::bs_icon("question-circle"),
                                "Ordination is a way to look at patterns in your samples",
                                placement="left"),
                        "Visualize how your sequences group together",
                        popover(bsicons::bs_icon("gear", class = "ms-auto"),
                                placement="right",
                                selectInput("choose_color",label="Color",choices=list()),
                                selectInput("choose_shape",label="Shape",choices=list()),
                                selectInput("choose_fill",label="Fill",choices=list()),
                                selectInput("choose_ellipse",label="Ellipse grouping",choices=list()),
                                title="Change grouping color of:")
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
  navset_card_tab(id="anlys", #name of the tabset card
                nav_panel(id='alpha',
                          title="Alpha Diversity",
                          Anlys_AlphaCard
                ),
                nav_panel(id="taxcomp",
                          title="Taxonomic composition",full_screen=TRUE,
                          plotUI("histo")
                          #Anlys_HistoCard
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