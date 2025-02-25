#  nav_panel("Page 2",
#            textOutput("selected_Quality"),
#            textOutput("selected_Location"),
#            textOutput("selected_HabitatType")),

qcpanel <- nav_panel("Demultiplex and Quality Control",
                 layout_column_wrap(
                   width=1/2,fill=TRUE,
                   actionButton("demux_prevpage","Previous step"),
                   actionButton("demux_nextpage","Next step")
                 ))
assignasvpanel <- nav_panel("Assign OTU",
                 layout_column_wrap(
                   width=1/2,fill=TRUE,
                   actionButton("asv_prevpage","Previous step"),
                   actionButton("asv_nextpage","Next step")
                 ))
assigntaxapanel <-nav_panel("Assign Taxonomy",
                 layout_column_wrap(
                   width=1/2,fill=TRUE,
                   actionButton("tax_prevpage","Previous step"),
                   actionButton("tax_nextpage","Next step")
                 ))