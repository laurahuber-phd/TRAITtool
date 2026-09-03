
############################################# Violin plot #############################################
Anlys_ViolinCard <- tagList(
  h5("This tab will generate a violin plot"),
  HTML("<p>Use the <u>'Controls'</u> button to change the taxonomic rank visualized or to alter what variable is used on the x-axis. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis.</p>"),
  HTML("<p>Use the <u>'Plot Size'</u> button to change the aspect ratio of the plot. </p>"),
  br(),
  layout_columns(
    plotControlsUI('violin_gen',color=FALSE,x=TRUE,y=TRUE,facet=TRUE,
                   xlabel="Groups to compare (x-axis)",
                   ylabel="Numeric MTI to plot",
                   facetlabel="Faceting variable"
    ),
    plotPrettyUI("violin_pretty"),
    plotSizeUI('violin_size')
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_violin","Plot"),
                 downloadButton('violin_plotdwnld', label=NULL)),
  withSpinner(plotOutput("violin_plot",click="violin_click")) #add a reaction when the plot is clicked
) 

############################################# Bar plot (percentage) #############################################
Anlys_TrophBarCard <- tagList(
  h5("This tab will generate a bar plot with relative abundance composition for each trophic group"),
  p("Separated into the groups selected for the y-axis"),
  br(),
  HTML("<p>Use the <u>'Grouping Choices'</u> button to choose which grouping variable to use on the y-axis. You can also select subgroups to facet by. </p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis.</p>"),
  HTML("<p>Use the <u>'Plot Size'</u> button to change the aspect ratio of the plot.</p>"),
  br(),
  layout_columns(col_widths=c(4,4,4),
                              shinyWidgets::dropdown(inputId="trophbar_plotgrouping",
                                                     label="Grouping choices",
                                                     icon=icon("gear"),
                                                    # pickerInput("choose_trophbar_axis",
                                                    #             label="Trophic level data",
                                                    #             #label="Groups to compare",
                                                    #             choices=list(),
                                                    #            multiple = FALSE,
                                                    #             options = pickerOptions(maxOptions = 1,
                                                    #                                     container = "body", 
                                                    #                                     actionsBox = TRUE,
                                                    #                                     deselectAllText = "Clear selection"
                                                    #                                     )
                                                    #             ),
                                                     pickerInput("choose_trophbar_facet",
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
                              plotPrettyUI("trophbar_pretty"),
                              plotSizeUI('trophbar_size')
                              ),
        layout_columns(col_widths=c(10,-1,1),
                       actionButton("go_trophbar","Plot"),
                       downloadButton('trophbar_plotdwnld', label=NULL)
        ),
        withSpinner(plotOutput("trophbar_plot",click="trophbar_click"))
  )

############################################# Line plot #############################################
Anlys_TrophLineCard <- tagList(
  h5("This tab will generate a line plot comparing trophic level data against abundance."),
  p("You can "),
  br(),
  HTML("<p>Use the <u>'Controls'</u> button to change which measure of trophic information is used on the x-axis. Here you can also update how groups are visualized with different linetype and color grouping. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis.</p>"),
  HTML("<p>Use the <u>'Plot Size'</u> button to change the aspect ratio of the plot. </p>"),
  br(),
  layout_columns(col_widths=c(4,4,4),
                 plotControlsUI("tline_gen",color=TRUE,x=TRUE,y=TRUE,facet=TRUE,
                                colorlabel="Numeric MTI to plot",
                                xlabel="Groups to compare by color",
                                ylabel="Groups to compare by line type",
                                facetlabel="Faceting variable"),
                 plotPrettyUI("tline_pretty"),
                 plotSizeUI('tline_size')
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_tline","Plot"),
                 downloadButton('tline_plotdwnld', label=NULL)
  ),
  withSpinner(plotOutput("tline_plot",click="tline_click"))
  )

############################################# Lollipop plot #############################################
Anlys_TrophLolliCard <- tagList(
  h5("This tab will generate a plot highlighting specific groups"),
  p("Description"),
  br(),
  HTML("<p>Use the <u>'Grouping Choices'</u> button to change which category is visualized on the y-axis. You also have the option to select specific members of the category to highlight in the plot. Also in this button, you can select a numeric category to plot on the x-axis. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis.</p>"),
  HTML("<p>Use the <u>'Plot Size'</u> button to change the aspect ratio of the plot. </p>"),
  br(),
  HTML("<p>Note: if one of the members selected does not have any information associated with it on the x-axis, no values will appear on the plot. </p>"),
  card(height=800,
        page_sidebar(fillable=FALSE,
        sidebar=sidebar(
          position="left",
          open=TRUE,
          shinyWidgets::dropdown(inputId="lolli_plotgrouping",
                                 label="Grouping choices",
                                 icon=icon("gear"),
                                 pickerInput("choose_lolli_yaxis",
                                             label="Category Group (y-axis)",
                                             choices=list(),
                                             multiple = FALSE,
                                             options = pickerOptions(maxOptions = 1,
                                                                     container = "body", 
                                                                     actionsBox = TRUE,
                                                                     deselectAllText = "Clear selection"
                                                                     )
                                             ),
                                 pickerInput("choose_lolli_xaxis",
                                             label="Numeric Group (x-axis)",
                                             choices=list(),
                                             multiple = FALSE,
                                             options = pickerOptions(maxOptions = 1,
                                                                     container = "body", 
                                                                     actionsBox = TRUE,
                                                                     deselectAllText = "Clear selection"
                                                                     )
                                             ),
                                 pickerInput("choose_lolli_facet",label="Faceting Group",
                                             choices=list(),
                                             multiple = TRUE,
                                             options = pickerOptions(maxOptions = 2,
                                                                     container = "body", 
                                                                     actionsBox = TRUE,
                                                                     deselectAllText = "Clear selection"
                                                                     )
                                             )
                                 ),
                                    plotPrettyUI("lolli_pretty"),
                                    plotSizeUI('lolli_size'),
          checkboxGroupInput("lolli_samples",
                             label="Check boxes of each group you want to focus on",
                             list())
        ),
        layout_columns(col_widths=c(10,-1,1),
                       actionButton("go_lolli","Plot"),
                       downloadButton('lolli_plotdwnld', label=NULL)
        ),
        withSpinner(plotOutput("lolli_plot",click="lolli_click"))
  )))

#############################################################################################################
traitanalysispage = nav_panel(
  useShinyjs(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("traitanalysis_prevpage","Previous step"),
    actionButton("traitanalysis_nextpage","Next step")
  ),
  br(),
  h2("Mean Trophic Index and Trait analysis"),
  h4("In this page, you can view plots for trait and ecological information."),
  p("Trophic level "),
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
       ),
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Violin plot",
                    Anlys_ViolinCard
                    ),
    accordion_panel(title="Bar plot",
                    Anlys_TrophBarCard
    ),
    #accordion_panel(title="Line plot",
    #                Anlys_TrophLineCard
    #),
    accordion_panel(title="Lollipop plot",
                    Anlys_TrophLolliCard
    )
  ),
  br(),
  br(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("btraitanalysis_prevpage","Previous step"),
    actionButton("btraitanalysis_nextpage","Next step")
  )
)
#############################################################################################################