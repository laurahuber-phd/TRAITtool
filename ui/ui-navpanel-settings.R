
settingspage <- tagList(shinyWidgets::radioGroupButtons("gblcol",
                                    label="Plotting color schemes",
                                    c("viridis" = 'D',
                                      "magma" = 'A',
                                      "plasma" = 'C',
                                      "mako" = 'G'
                                    ),
                                    selected='D',
                                    direction="vertical",
                                    justified = TRUE,
                                    )
                        )
#display.brewer.all(colorblindFriendly = TRUE)
## View a single RColorBrewer palette by specifying its name
#display.brewer.pal(n = 8, name = 'Dark2')