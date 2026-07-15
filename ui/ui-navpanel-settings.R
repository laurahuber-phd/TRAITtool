
settingspage <- tagList(
  h3("Settings"),
  h5("Explore the qualities of your data and plotting colors"),
  br(),
  h6("You can choose the color scheme your plots are generated with:"),
  layout_columns(col_widths=c(4,8),
                 shinyWidgets::radioGroupButtons("gblcol",
                                                 label="Plotting color schemes",
                                                 c("viridis" = 'D',
                                                   "magma" = 'A',
                                                   "plasma" = 'C',
                                                   "mako" = 'G'
                                                   ),
                                                 selected='D',
                                                 direction="vertical",
                                                 justified = TRUE,
                                                 ),
                 plotOutput("viscolors")),
  br(),
  h5("About your data"),
  p("About the original dataset: "),
  verbatimTextOutput('psog'),
  br(),
  
  p("About the subset dataset analyzed: "),
  HTML("<p><i> (This may be the same as the original dataset if you did not choose to remove blanks or unassigned sequences.) </i></p>"),
  verbatimTextOutput("pstext"),
  br(),
  
  p("These are the column names of the metadata file uploaded. These will be used as variables to explore trends in the data."),
  verbatimTextOutput("metavar_return"), #available metadata column names
  br(),
  
  p("These are the rank names in the taxonomy table. These can be used to visualize taxa at different levels and groupings."),
  verbatimTextOutput("rnames_return"), #available rank names
  br(),
  
  h4("If something here doesn't look right, go back to that section and check what was entered as input."),
  br(),
  br() 
                        )
#display.brewer.all(colorblindFriendly = TRUE)
## View a single RColorBrewer palette by specifying its name
#display.brewer.pal(n = 8, name = 'Dark2')