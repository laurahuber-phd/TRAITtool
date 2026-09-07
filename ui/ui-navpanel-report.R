reportpage <- nav_panel(
  "Report",
  useShinyjs(),
  actionButton("report_prevpage","Previous step",width='20%'),
  br(),
  br(),
  h2("Collect everything into one report"),
  "Use this page to check that you have completed all analyses that you were interested in and that figures look the way you want them to before generating the report, which can be computationally intensive and annoying to do multiple times.",
  br(),
  br(),
  HTML("<i><b>Before generating the full report, check that you have calculated everything you need in the panels below: </i></b>"),
  br(),
  card(
    accordion(id="report_accord",open=FALSE,
              accordion_panel(title="Filtering Report",
                              p("Minimum sample size:", textOutput("rep_pnsamp",inline=TRUE)),
                              p("Minimum number occurrences to keep an ASV: ", textOutput("rep_pntax",inline=TRUE)),
                              p("Selected blanks: ",textOutput("rep_blanks",inline=TRUE)),
                              p("Blanks were removed: ",textOutput("rep_rembl",inline=TRUE)),
                              p("Samples removed: ",textOutput("rep_remsam",inline=TRUE)),
                              p("Dataset was normalized with: ",textOutput("rep_norm",inline=TRUE)),
                              textOutput("rep_unass"),
                              verbatimTextOutput("rep_remtax")
                              ),
              accordion_panel(title="Data Descriptions",
                              HTML("<p><i> (For the original dataset)</i> <u>Minimum and maximum number of sequences per sample: </u></p>"),
                              tableOutput("rep_numog"),
                              
                              HTML("<p><i> (For the analyzed dataset)</i> <u>Minimum and maximum number of sequences per sample: </u></p>"),
                              tableOutput("rep_numsam"),
                              ),
              accordion_panel(title="Trait Assignment",
                              tableOutput("rep_syn.names"),
                              tableOutput("traits_report"),
                              "Species queried", textOutput("rep_trait.nona"),
                              "Valid names",textOutput("rep_trait.valid"),
                              "Invalid names",textOutput("rep_trait.invalid")
                              ),
              accordion_panel(title="Figures",
                              accordion(id='fig_accord',open=FALSE,
                                        accordion_panel(title="Trophic index",
                                                        withSpinner(plotOutput("rep_vioplot")),
                                                        actionButton("repaccd_vioplot",
                                                                     label="Go back to this page")
                                        ),
                                        accordion_panel(title="Feeding guilds",
                                                        withSpinner(plotOutput("rep_tbarplot")),
                                                        actionButton("repaccd_tbarplot",
                                                                     label="Go back to this page")
                                        ),
                                        accordion_panel(title="Body size",
                                                        withSpinner(plotOutput("rep_sizebarplot")),
                                                        actionButton("repaccd_sizebarplot",
                                                                     label="Go back to this page")
                                        ),
                                        #accordion_panel(title="Trophic index",
                                        #                withSpinner(plotOutput("rep_lolliplot")),
                                        #                actionButton("repaccd_lolliplot",
                                        #                             label="Go back to this page")
                                        #),
                                        accordion_panel(title="Temperature density",
                                                        withSpinner(plotOutput("rep_tviolinplot")),
                                                        actionButton("repaccd_tviolinplot",
                                                                     label="Go back to this page")),
                                        accordion_panel(title="Temperature range",
                                                        withSpinner(plotOutput("rep_tlolliplot")),
                                                        actionButton("repaccd_tlolliplot",
                                                                     label="Go back to this page")
                                        ),
                                        accordion_panel(title="Richness",
                                                        withSpinner(plotOutput("rep_richplot")),
                                                        actionButton("repaccd_richplot",label="Go back to this page")
                                        ),
                                accordion_panel(title="Percent composition",
                                                withSpinner(plotOutput("rep_percplot")),
                                                actionButton("repaccd_percplot",label="Go back to this page")
                                                ),
                                accordion_panel(title="Taxonomic diversity",
                                                withSpinner(plotOutput("rep_histplot")),
                                                actionButton("repaccd_histplot",label="Go back to this page")
                                                ),
                                accordion_panel(title="Heatmap",
                                                withSpinner(plotOutput("rep_hmplot")),
                                                actionButton("repaccd_hmplot",label="Go back to this page")
                                                ),
                                accordion_panel(title="Ordination plot",
                                                withSpinner(plotOutput("rep_ordplot")),
                                                actionButton("repaccd_ordplot",label="Go back to this page")
                                                )
                              ))
              )
  ),
  br(),
  HTML("<h2>Download report </h2>"),
  "Click on the button to gather all the analyses together and generate the report. This will likely take awhile.",
  layout_columns(
    col_widths=c(6,6),
    actionButton('gen_onepg_s', 'Generate one-page summary (static)'),
    actionButton('gen_onepg_i', 'Generate one-page summary (interactive)')),
  layout_columns(
    col_widths=c(6,6),
  actionButton('gen_report_s', 'Generate report (static)'),
  actionButton('gen_report_i', 'Generate report (interactive)')),
  br(),
  br(),
  br(),
  card(id='dwnldviewreport',
  #conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                   layout_columns(
                     col_widths=c(6,6),
                     card("View the report in-app before downloading:",
                          br(),
                          br(),
                          actionButton(inputId = 'view_report',label="View report")),
                     card(radioButtons('format', 'Document format', c( 'HTML'),#, 'PDF','Word'),
                                       inline = TRUE,width='100%'),
                   downloadButton("download_report", "Download report", 
                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                   ),
                   br(),
                   br(),
                   shinyjs::hidden(wellPanel(id="viewfinalreport",htmlOutput('markdown')))
                   ),
  h4("Download individual files used in the analysis"),
  card(card_header("Files generated:"),
       downloadButton("download_zip", label="All figures (.zip)"),
       downloadButton('norm_meltdwnld', label="Full dataset:"),
       downloadButton('norm_asvdwnld', label="Sequence table:"),
       downloadButton('norm_metadwnld', label="Metadata table:"),
       downloadButton('norm_taxdwnld', label="Taxonomy table:")
       ),
  actionButton("breport_prevpage","Previous step",width='20%')
)
