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
                 p("In progress, please check back later")
                 )
assignasvpanel <- tagList(h3("Build Sequence Table"),
                 layout_column_wrap(
                   width=1/2,fill=TRUE,
                   actionButton("asv_prevpage","Previous step"),
                   actionButton("asv_nextpage","Next step")
                 ),
                 p("In progress, please check back later")
                 )
assigntaxapanel <-tagList(h3("Assign Taxonomy"),
                 layout_column_wrap(
                   width=1/2,fill=TRUE,
                   actionButton("tax_prevpage","Previous step"),
                   actionButton("tax_nextpage","Next step")
                 ),
                 p("In progress, please check back later")
                 )