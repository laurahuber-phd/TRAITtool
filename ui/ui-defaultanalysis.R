defaultanalysis <- tagList(
                           tags$span(h4("Default analysis inputs"),
                                     tags$i(tooltip(bs_icon("bar-chart-line-fill"),
                                                    tagList(p(style='text-align: left',
                                                              "The default analysis will return each plot generated in this tool with its most basic settings. This will provide a general overview of what figures can be returned, but is unlikely to be useful for natural resource management. This page will report what settings were used as calculations occur, then automatically take you to the report page when all analyses are completed."))
                                                    )
                                            )),
                           "After generating these figures, you will automatically be directed to the report page",
                             useShinyjs(),
                             actionButton("defaultgo","Run with default settings?",width='100%',disabled=TRUE),
                             shinyjs::hidden(
                               wellPanel(id="def_prune",
                                         "Used default pruning settings of removing samples with less than 100 sequences and retaining all taxa. Dataset was converted to relative abundance"),
                               wellPanel(id="def_plotrank",
                                         "Used highest taxonomic rank to plot relative abundance of each taxa in the dataset"),
                               wellPanel(id="def_plotrich",
                                         "Calculated Shannon richness with default sample groups"),
                               wellPanel(id="def_plothisto",
                                         "Used highest taxonomic rank to plot relative composition of taxa in each sample"),
                               wellPanel(id="def_plothm",
                                         "Used highest taxonomic rank to visualize samples in a heatmap"),
                               wellPanel(id="def_plotord",
                                         "Used Jaccard distance to plot NMDS ordination of samples"),
                               wellPanel(id="def_plotvio",
                                         "Visualize trophic index distribution")
                             )
)