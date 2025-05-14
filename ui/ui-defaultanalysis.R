defaultanalysis <- nav_panel("Default",
                             useShinyjs(),
                             actionButton("defaultgo","Run with default settings",width='100%'),
                             shinyjs::hidden(
                               wellPanel(id="def_prune",
                                         "Used default pruning settings of removing samples with less than 100 sequences and removing taxa occurring less than 10 times. Dataset was converted to relative abundance")
                             )
                             
)