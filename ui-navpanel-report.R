reportpage <- nav_panel(
  "Report",
  card("Download the report here",
       radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                    inline = TRUE),
       #downloadButton("download_report","Download report")),
       actionButton('gen_report', 'Generate report')),
  conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                   downloadButton("download_report", "Download report", 
                                  style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
  card(uiOutput('markdown'))
  
)