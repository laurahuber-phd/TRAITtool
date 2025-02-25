
Basics_NumCard <- navset_card_tab(
  sidebar=sidebar(title="Original dataset",
                  position="right",
                  open="closed", 
                  bg="#a8c3bc",
                  accordion(title="Summary",
                            withSpinner(tableOutput("summ_seqs")) #max/min no of sequences in a sample, mean and median 
                  ),
                  accordion_panel(title="Sequence breakdown",
                                  textOutput("N_seq_total"),
                                  textOutput("N_seq_assigned"),
                                  textOutput("N_seq_blanks")),
                  accordion_panel(title="OTU breakdown",
                                  textOutput("N_otu_total"),
                                  textOutput("N_otu_assigned"),
                                  textOutput("N_otu_blanks"))
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
  card_header("List members of a taxonomic group present in the dataset"),
  layout_sidebar(
    sidebar=sidebar(title="Percent assigned",
                    position="right",
                    open="closed",
                    bg="#a8c3bc",
                    width=('45%'),
                    withSpinner(plotOutput("N_otu_byrank_plot",click="notu_click")),
                    layout_column_wrap(width=1/2,
                                       sliderInput("rankper_height",label="Height of plot",min=1,max=10,value=5,step=1),
                                       sliderInput("rankper_width",label="Width of plot",min=1,max=10,value=5,step=1)),
                    layout_columns(col_widths=c(8,4),
                                   sliderInput("rankper_text",label="Text size",min=1,max=10,value=5,step=1),
                                   numericInput("rankper_legend",label="Number of legend columns",value=5)),
                    downloadButton('download_rankperc_Plot', 'Download')
                    #withSpinner(dataTableOutput("N_otu_byrank"))
    ),
    pickerInput("ranks","Taxonomic Ranks",character()),
    dataTableOutput("listtaxa")#,
  )
)
#############################################################################################################
descriptivepage = nav_panel("Basic Stats",
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