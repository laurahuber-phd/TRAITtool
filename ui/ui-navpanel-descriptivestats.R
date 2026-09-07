source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
source('functions/module-plotting-general.R',local=TRUE)

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
                  accordion_panel(title="ASV breakdown",
                                  textOutput("N_otu_total"),
                                  textOutput("N_otu_assigned"),
                                  textOutput("N_otu_blanks")
                                  )
                  )
  ),
  nav_panel(id="SamSumNavTab",
            title="Sample Summary Table",
            p("This is a table that shows each of the samples in the dataset you are looking at and how many sequences are in that sample. We want to know if one sample has way more (or less) sequences than the rest of the dataset - unbalanced data makes it difficult to see real patterns."),
                             DTOutput('N_sample_placeholder'), #show placeholder when no data is loaded
                             withSpinner(dataTableOutput("N_sample")) #number of sequences per sample
            ), #table of no. seqs in each sample

  nav_panel(id="UniqGrpNavTab",
            title="Unique Groups",
            p("This is a table showing how many unique taxonomic groups can be found at each rank. This is a useful way to look at the general diversity of your data, and ask if it matches up with what you'd expect from the marker you chose. Do you expect a variety of phyla? Or were you using specifically targetted primers? "),
            card(
              full_screen=TRUE,
              card_header("Number of unique groups at each taxonomic rank"),
              DTOutput('N_taxa_placeholder'), #show placeholder when no data is loaded
              dataTableOutput("N_taxa") #column 2 is a card that shows the number of unique taxa at each level
            )
            ), #Number of members in each taxonomic rank
  nav_panel(id="SeqSumNavTab",
            title="Dataset Overview",
            p("This table is an overview of sequences, ASVs, and how that changes depending on the filtering parameters. You probably don't need to look too far into this, but it will tell you how well your sequences clustered into distinct units/ASVs, how much contamination was found in the blanks (if you defined blanks in the review section), and the spread/evenness of sequences within the dataset. "),
            br(),
            p("This is by far the most in the weeds - really, it's here to look at patterns, and to compare with the patterns in the sidebar."),
            accordion(
              open="Summary",
              accordion_panel(title="Summary",
                              withSpinner(tableOutput("summ_seqs_sel")) #max/min no of sequences in a sample, mean and median 
              ),
              accordion_panel(title="Sequence breakdown",
                              textOutput("N_sel_seq_total"),
                              textOutput("N_sel_seq_assigned"),
                              textOutput("N_sel_seq_blanks")),
              accordion_panel(title="ASV breakdown",
                              textOutput("N_sel_otu_total"),
                              textOutput("N_sel_otu_assigned"),
                              textOutput("N_sel_otu_blanks"))
            )),
)
Basics_TaxCard <- card(
  #full_screen=TRUE,
  card_header("Taxonomy summary"),
  p("Use the dropdown button to select a taxonomic rank, and the tabs below will fill out information on it."),
  pickerInput("basics_ranks","Taxonomic Ranks",character(),width='fit',inline=TRUE),
  navset_card_tab(
    nav_panel(title="Members",
              h5("List members of a taxonomic group present in the dataset"),
              p("The table here will list the various taxa found in your dataset up to the rank you chose in the dropdown button above."),
              DTOutput('listtaxa_placeholder'),
              dataTableOutput("basics_listtaxa")),
    nav_panel(title="Percent Composition",
              p("Using the taxonomic rank selected in the dropdown button above, visualize proportionally how much each taxa appears in the dataset. What taxa are most common? Do you have rare taxa? "),
              br(),
              p("Use the buttons to make aesthetic changes, then click 'Plot'. "),
              #position="right",
              open="closed",
              #bg="#a8c3bc",
              #width=('45%'),
              layout_columns(
                plotPrettyUI("basics_pretty"),
                plotSizeUI('basics_size')
              ),
              layout_columns(col_widths=c(10,-1,1),
                             actionButton("go_rankperc","Plot"),
                             downloadButton('rankperc_plotdwnld', label=NULL)),
              withSpinner(plotOutput("rankperc_plot",click="rankperc_click"))
              #withSpinner(dataTableOutput("N_otu_byrank"))
    )
  )
)
#############################################################################################################
descriptivepage = tagList(
  #layout_column_wrap(
  #  width=1/2,fill=TRUE,
  #  actionButton("basics_prevpage","Previous step"),
  #  actionButton("basics_nextpage","Next step")
  #),
  #br(),
  h2("Data Overview"),
  h4("This page is essentially a sanity check."),
  p("The information we get from this page is useful because it can be used for troubleshooting and can tell us about the quality and reliability of the data we're looking at. Large, dramatic changes here could be the reason why you see differences in what species are showing up or why community composition appears to change. We want to know if the changes are real, biological changes or due to the dataset."),
  br(),
  br(),
  HTML("<h5>So first, look at the <u>number</u> of sequences.</h5>"),
          Basics_NumCard,
  br(),
  HTML("<h5>Next, look at taxonomy</h5>"),
  p(),
          Basics_TaxCard#,
          #include a section to output names of taxa at each rank/their relative abundances
  #layout_column_wrap(
  #  width=1/2,fill=TRUE,
  #  actionButton("bbasics_prevpage","Previous step"),
  #  actionButton("bbasics_nextpage","Next step")
  #)
)
#############################################################################################################