
############################################# Troph violin plot #############################################
Anlys_ViolinCard <- tagList(
  h5("This tab will generate a violin plot"),
  HTML("<p>Use the <u>'Controls'</u> button to change the taxonomic rank visualized or to alter what variable is used on the x-axis. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis. You can also change the plot and axes titles here. </p>"),
  HTML("<p>Use the <u>'Plot Size'</u> button to change the aspect ratio of the plot. </p>"),
  br(),
  layout_columns(
    plotControlsUI('violin_gen',color=FALSE,x=TRUE,y=TRUE,facet=TRUE,
                   xlabel="Groups to compare (x-axis)",
                   ylabel="Numeric MTI to plot",
                   facetlabel="Faceting variable"
    ),
    plotStatUI("violin_stat"),
    plotPrettyUI("violin_pretty"),
    plotSizeUI('violin_size')
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_violin","Plot"),
                 downloadButton('violin_plotdwnld', label=NULL)),
  withSpinner(plotOutput("violin_a_plot",click="violin_click")) #add a reaction when the plot is clicked
) 

############################################# Troph Bar plot (percentage) #############################################
Anlys_TrophBarCard <- tagList(
  h5("This tab will generate a bar plot with relative abundance composition for each trophic group"),
  p("Separated into the groups selected for the y-axis"),
  br(),
  HTML("<p>Use the <u>'Grouping Choices'</u> button to choose which grouping variable to use on the y-axis. You can add up to two variables, which will separate the data into different plots based on which variables you group by. </p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis. You can also change the plot and axes titles here. </p>"),
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

############################################# Troph line plot #############################################
Anlys_TrophLineCard <- tagList(
  h5("This tab will generate a line plot comparing trophic level data against abundance."),
  p("You can "),
  br(),
  HTML("<p>Use the <u>'Controls'</u> button to change which measure of trophic information is used on the x-axis. Here you can also update how groups are visualized with different linetype and color grouping. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis. You can also change the plot and axes titles here. </p>"),
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

############################################# Troph lollipop plot #############################################
Anlys_TrophLolliCard <- tagList(
  h5("This tab will generate a plot highlighting specific groups"),
  p("Description"),
  br(),
  HTML("<p>Use the <u>'Grouping Choices'</u> button to change which category is visualized on the y-axis. You also have the option to select specific members of the category to highlight in the plot. Also in this button, you can select a numeric category to plot on the x-axis. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis. You can also change the plot and axes titles here. </p>"),
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


############################################# Temperature Index violin plot #############################################
Anlys_TempViolinCard <- tagList(
  h5("This tab will generate a violin plot"),
  HTML("<p>Use the <u>'Controls'</u> button to change the taxonomic rank visualized or to alter what variable is used on the x-axis. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis.</p>"),
  HTML("<p>Use the <u>'Plot Size'</u> button to change the aspect ratio of the plot. </p>"),
  br(),
  layout_columns(
    plotControlsUI('tviolin_general',color=FALSE,
                   x=TRUE,y=FALSE,#y=TRUE,
                   facet=TRUE,
                   xlabel="Groups to compare (x-axis)",
                   #ylabel="Numeric MTI to plot",
                   facetlabel="Faceting variable"
    ),
    plotStatUI("tviolin_stat"),
    plotPrettyUI("tviolin_pretty"),
    plotSizeUI('tviolin_size')
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_tviolin","Plot"),
                 downloadButton('tviolin_plotdwnld', label=NULL)),
  withSpinner(plotOutput("tviolin_plot",click="tviolin_click")), #add a reaction when the plot is clicked
) 

Anlys_TempChoiceViolinCard <- tagList(
  layout_columns(
    plotControlsUI('tviolinmean_general',color=FALSE,
                   x=TRUE,y=TRUE,
                   facet=TRUE,
                   xlabel="Groups to compare (x-axis)",
                   #ylabel="Numeric MTI to plot",
                   facetlabel="Faceting variable"
    ),
    plotStatUI("tviolinmean_stat"),
    plotPrettyUI("tviolinmean_pretty"),
    plotSizeUI('tviolinmean_size')
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_tviolinmean","Plot"),
                 downloadButton('tviolinmean_plotdwnld', label=NULL)),
  withSpinner(plotOutput("tviolinmean_plot",click="tviolinmean_click")) #add a reaction when the plot is clicked
)

############################################# Temperature lollipop plot #############################################
Anlys_TempLolliCard <- tagList(
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
                      shinyWidgets::dropdown(inputId="tlolli_plotgrouping",
                                             label="Grouping choices",
                                             icon=icon("gear"),
                                             pickerInput("choose_tlolli_yaxis",
                                                         label="Category Group (y-axis)",
                                                         choices=list(),
                                                         multiple = FALSE,
                                                         options = pickerOptions(maxOptions = 1,
                                                                                 container = "body", 
                                                                                 actionsBox = TRUE,
                                                                                 deselectAllText = "Clear selection"
                                                         )
                                             ),
                                             # pickerInput("choose_tlolli_xaxis",
                                             #             label="Numeric Group (x-axis)",
                                             #             choices=list(),
                                             #             multiple = FALSE,
                                             #             options = pickerOptions(maxOptions = 1,
                                             #                                    container = "body", 
                                             #                                     actionsBox = TRUE,
                                             #                                     deselectAllText = "Clear selection"
                                             #             )
                                             # ),
                                             pickerInput("choose_tlolli_facet",label="Faceting Group",
                                                         choices=list(),
                                                         multiple = TRUE,
                                                         options = pickerOptions(maxOptions = 2,
                                                                                 container = "body", 
                                                                                 actionsBox = TRUE,
                                                                                 deselectAllText = "Clear selection"
                                                         )
                                             )
                      ),
                      plotPrettyUI("tlolli_pretty"),
                      plotSizeUI('tlolli_size'),
                      checkboxGroupInput("tlolli_samples",
                                         label="Check boxes of each group you want to focus on",
                                         list())
                    ),
                    card(
                      p("Select range of values to highlight (for example, average temperature range of your site)"),
                      layout_columns(col_widths=c(6,6),
                                     numericInput("temprange_min",
                                                  "Minimum temperature value:",
                                                  value = NA),
                                     numericInput("temprange_max",
                                                  "Maximum temperature value:",
                                                  value = NA),
                      )),
                    layout_columns(col_widths=c(10,-1,1),
                                   actionButton("go_tlolli","Plot"),
                                   downloadButton('tlolli_plotdwnld', label=NULL)
                    ),
                    withSpinner(plotOutput("tlolli_plot",click="tlolli_click"))
       )))

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
