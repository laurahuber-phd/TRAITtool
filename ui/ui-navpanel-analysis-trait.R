
Trait_AnalysisCard <- tagList(layout_columns(col_widths = c()),
                              h3()
                              )

Trait_FiguresCard <- tagList()


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
traitanalysispage = nav_panel(
  useShinyjs(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("traitanalysis_prevpage","Previous step"),
    actionButton("traitanalysis_nextpage","Next step")
  ),
  br(),
  
  h2("Trait Analysis"),
  h4("In this page, you can view the analysis for trait and ecological information."),
  p("Description of what that means"),
  Trait_AnalysisCard, 
  br(),
  Trait_FiguresCard,
  br(),
  Anlys_ViolinCard,
  #Anlys_TrophCard,
  
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("btraitanalysis_prevpage","Previous step"),
    actionButton("btraitanalysis_nextpage","Next step")
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