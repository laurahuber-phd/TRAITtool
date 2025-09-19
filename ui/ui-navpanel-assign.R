#  nav_panel("Page 2",
#            textOutput("selected_Quality"),
#            textOutput("selected_Location"),
#            textOutput("selected_HabitatType")),

qcpanel <- tagList(h3("Demultiplex and Quality Control"),
                 layout_column_wrap(
                   width=1/2,fill=TRUE,
                   actionButton("demux_prevpage","Previous step"),
                   actionButton("demux_nextpage","Next step")
                 ),
                          p("In progress, please check back later"),
                 tags$footer(style="display: flex;justify-content: space-between;",
                             tags$img(src='smiths_logoLong.png',height='40px',
                                      alt="Smithsonian sun logo"),
                             tags$img(#src='ShinyFooter.png',
                               src='titlefooter.png',
                               height='30px',
                               alt="William H. Donner Foundation and Smithsonian Women's Committee logos"))
                 
                   )
assignasvpanel <- tagList(h3("Build Sequence Table"),
                          layout_column_wrap(
                   width=1/2,fill=TRUE,
                   actionButton("asv_prevpage","Previous step"),
                   actionButton("asv_nextpage","Next step")
                 ),
                          p("In progress, please check back later"),
                 tags$footer(style="display: flex;justify-content: space-between;",
                             tags$img(src='smiths_logoLong.png',height='40px',
                                      alt="Smithsonian sun logo"),
                             tags$img(#src='ShinyFooter.png',
                               src='titlefooter.png',
                               height='30px',
                               alt="William H. Donner Foundation and Smithsonian Women's Committee logos"))
                 )

assigntaxapanel <-tagList(h3("Assign Taxonomy"),
                 layout_column_wrap(
                   width=1/2,fill=TRUE,
                   actionButton("tax_prevpage","Previous step"),
                   actionButton("tax_nextpage","Next step")
                 ),
                 p("In progress, please check back later"),
                 tags$footer(style="display: flex;justify-content: space-between;",
                             tags$img(src='smiths_logoLong.png',height='40px',
                                      alt="Smithsonian sun logo"),
                             tags$img(#src='ShinyFooter.png',
                               src='titlefooter.png',
                               height='30px',
                               alt="William H. Donner Foundation and Smithsonian Women's Committee logos"))
                 )