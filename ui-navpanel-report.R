reportpage <- nav_panel(
  "Report",
  "Download the report here",
  actionButton('gen_report', 'Generate report'),
  br(),
  conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                   layout_columns(
                     col_widths=c(6,6),
                     card("View the report in-app before downloading:",
                          actionButton(inputId = 'view_report',label="View report")),
                     card(radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                                       inline = TRUE,width='100%'),
                   downloadButton("download_report", "Download report", 
                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                   ),
                   br(),
                   br(),
                   htmlOutput('markdown')
                   )
)