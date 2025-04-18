reportpage <- nav_panel(
  "Report",
  useShinyjs(),
  actionButton("report_prevpage","Previous step",width='20%'),
  br(),
  br(),
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