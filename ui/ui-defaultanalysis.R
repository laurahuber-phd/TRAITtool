defaultanalysis <- nav_panel("Default",
                             useShinyjs(),
                             actionButton("defaultgo","Run with default settings",width='100%'),
                             shinyjs::hidden(
                               wellPanel(id="def_prune",
                                         "Used default pruning settings of removing samples with less than 100 sequences and retaining all taxa. Dataset was converted to relative abundance"),
                               wellPanel(id="def_plotrank",
                                         "Used highest taxonomic rank to plot relative abundance of each taxa in the dataset"),
                               wellPanel(id="def_plotrich",
                                         "Calculated Shannon richness with default sample groups"),
                               wellPanel(id="def_plothisto",
                                         "Used highest taxonomic rank to plot relative composition of taxa in each sample"),
                               wellPanel(id="def_plotord",
                                         "Used Jaccard distance to plot NMDS ordination of samples")
                             )
)