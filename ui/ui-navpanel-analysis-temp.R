############################################# Violin plot #############################################
Anlys_TempViolinCard <- tagList(
  h5("This tab will generate a violin plot"),
  HTML("<p>Use the <u>'Controls'</u> button to change the taxonomic rank visualized or to alter what variable is used on the x-axis. You can also add up to two 'faceting' variables, which will separate the data into different plots based on which variables you facet by.</p>"),
  HTML("<p>Use the <u>'Aesthetics'</u> button to change aesthetic elements such as the plotting theme, which will influence how the background and gridlines look. Here you can also change the position of the legend, text size, and the direction of text on the x-axis.</p>"),
  HTML("<p>Use the <u>'Plot Size'</u> button to change the aspect ratio of the plot. </p>"),
  br(),
  layout_columns(
    plotControlsUI('tviolin_general',color=FALSE,
                   x=TRUE,y=TRUE,
                   facet=TRUE,
                   xlabel="Groups to compare (x-axis)",
                   #ylabel="Numeric MTI to plot",
                   facetlabel="Faceting variable"
    ),
    plotPrettyUI("tviolin_pretty"),
    plotSizeUI('tviolin_size')
  ),
  layout_columns(col_widths=c(10,-1,1),
                 actionButton("go_tviolin","Plot"),
                 downloadButton('tviolin_plotdwnld', label=NULL)),
  withSpinner(plotOutput("tviolin_plot",click="tviolin_click")) #add a reaction when the plot is clicked
) 

############################################# Lollipop plot #############################################
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

#############################################################################################################
tempanalysispage = nav_panel(
  useShinyjs(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("tempanalysis_prevpage","Previous step"),
    actionButton("tempanalysis_nextpage","Next step")
  ),
  br(),
  h2("Temperature range analysis"),
  h4("In this page, you can view plots depicting information gathered from FishBase regarding species temperature preference."),
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Violin plot",
                    Anlys_TempViolinCard
    ),
    #accordion_panel(title="Bar plot",
    #                Anlys_TempBarCard
    #),
    #accordion_panel(title="Line plot",
    #                Anlys_TempLineCard
    #),
    accordion_panel(title="Lollipop plot",
                    Anlys_TempLolliCard
    )
  ),
  br(),
  br(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("btempanalysis_prevpage","Previous step"),
    actionButton("btempanalysis_nextpage","Next step")
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