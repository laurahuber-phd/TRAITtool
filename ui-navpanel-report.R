reportpage <- nav_panel(
  "Report",
  useShinyjs(),
  actionButton("report_prevpage","Previous step",width='20%'),
  br(),
  br(),br(),
  h2("Collect everything into one report"),
  "Use this page to check that you have completed all analyses that you were interested in and that figures look the way you want them to before generating the report, which can be computationally intensive and annoying to do multiple times.",
  br(),
  "Before generating the full report, check that you have calculated everything you need in the panels below: ",
  br(),
  card(
    accordion(id="report_accord",
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
              accordion_panel(title="Data Descriptions"),
              accordion_panel(title="Figures",
                              accordion(
                                accordion_panel(title="Percent composition"),
                                accordion_panel(title="Richness"),
                                accordion_panel(title="Taxonomic diversity"),
                                accordion_panel(title="Heatmap"),
                                accordion_panel(title="Ordination plot")
                              ))
              )
  ),
  br(),
  HTML("<h2>Download report </h2>"),
  "Click on the button to gather all the analyses together and generate the report. This will likely take awhile.",
  actionButton('gen_report', 'Generate report'),
  br(),
  br(),
  br(),
  conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                   layout_columns(
                     col_widths=c(6,6),
                     card("View the report in-app before downloading:",
                          br(),
                          br(),
                          br(),
                          actionButton(inputId = 'view_report',label="View report")),
                     card(radioButtons('format', 'Document format', c('PDF', 'HTML'),#, 'Word'),
                                       inline = TRUE,width='100%'),
                   downloadButton("download_report", "Download report", 
                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                   ),
                   br(),
                   br(),
                   shinyjs::hidden(wellPanel(id="viewfinalreport",htmlOutput('markdown')))
                   )
)