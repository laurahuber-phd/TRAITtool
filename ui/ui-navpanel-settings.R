
settingspage <- tagList(
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
  tags$footer(style="display: flex;justify-content: space-between;",
              tags$img(src='smiths_logoLong.png',height='40px',
                       alt="Smithsonian sun logo"),
              tags$img(#src='ShinyFooter.png',
                src='titlefooter.png',
                height='30px',
                alt="William H. Donner Foundation and Smithsonian Women's Committee logos")) 
                        )
#display.brewer.all(colorblindFriendly = TRUE)
## View a single RColorBrewer palette by specifying its name
#display.brewer.pal(n = 8, name = 'Dark2')