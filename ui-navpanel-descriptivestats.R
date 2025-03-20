source('module-plotting-pretty.R',local=TRUE)
source('module-plotting-size.R',local=TRUE)
source('module-plotting-general.R',local=TRUE)


Basics_NumCard <- navset_card_tab(
  sidebar=sidebar(title="Original dataset overview",
                  position="right",
                  open="closed", 
                  accordion(
                    accordion_panel(title="Summary",
                            withSpinner(tableOutput("summ_seqs")) #max/min no of sequences in a sample, mean and median 
                  ),
                  accordion_panel(title="Sequence breakdown",
                                  textOutput("N_seq_total"),
                                  textOutput("N_seq_assigned"),
                                  textOutput("N_seq_blanks")
                                  ),
                  accordion_panel(title="OTU breakdown",
                                  textOutput("N_otu_total"),
                                  textOutput("N_otu_assigned"),
                                  textOutput("N_otu_blanks")
                                  )
                  )
  ),
  nav_panel(id="SamSumNavTab",
            title="Sample Summary Table",
            dataTableOutput("N_sample")), #table of no. seqs in each sample
  nav_panel(id="UniqGrpNavTab",
            title="Unique Groups",
            card(
              full_screen=TRUE,
              card_header("Number of unique groups at each taxonomic rank"),
              dataTableOutput("N_taxa") #column 2 is a card that shows the number of unique taxa at each level
            )
            ), #Number of members in each taxonomic rank
  nav_panel(id="SeqSumNavTab",
            title="Dataset Overview",
            accordion(
              open="Summary",
              accordion_panel(title="Summary",
                              withSpinner(tableOutput("summ_seqs_sel")) #max/min no of sequences in a sample, mean and median 
              ),
              accordion_panel(title="Sequence breakdown",
                              textOutput("N_sel_seq_total"),
                              textOutput("N_sel_seq_assigned"),
                              textOutput("N_sel_seq_blanks")),
              accordion_panel(title="OTU breakdown",
                              textOutput("N_sel_otu_total"),
                              textOutput("N_sel_otu_assigned"),
                              textOutput("N_sel_otu_blanks"))
            )),
)
Basics_TaxCard <- card(
  #full_screen=TRUE,
  card_header("Taxonomy summary"),
  pickerInput("ranks","Taxonomic Ranks",character(),width='fit',inline=TRUE),
  navset_card_tab(
    nav_panel(title="List members of a taxonomic group present in the dataset",
              dataTableOutput("listtaxa")),
    nav_panel(title="View percent assigned",
              #position="right",
              open="closed",
              #bg="#a8c3bc",
              #width=('45%'),
              layout_columns(
                plotPrettyUI("basics_pretty"),
                plotSizeUI('basics_size')
              ),
              layout_columns(col_widths=c(10,-1,1),
                             actionButton("plot_rankperc","Plot"),
                             downloadButton('download_rankperc_Plot', label=NULL)),
              withSpinner(plotOutput("N_otu_byrank_plot",click="notu_click"))
              #withSpinner(dataTableOutput("N_otu_byrank"))
    )
  )
)
#############################################################################################################
descriptivepage = tagList("Basic Stats",
          Basics_NumCard,
          Basics_TaxCard,
          layout_column_wrap(
            width=1/2,fill=TRUE,
            actionButton("basics_prevpage","Previous step"),
            actionButton("basics_nextpage","Next step")
          )
          #include a section to output names of taxa at each rank/their relative abundances
)
#############################################################################################################