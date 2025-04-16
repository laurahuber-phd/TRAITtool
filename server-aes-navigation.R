# --- # Page Navigation # --------
# modal dialog boxes to walk user through steps
observe({
  req(input$FASTQFile) #if the FASTQ and barcode files are both uploaded, take the user to the demux page
  req(input$BarcodeFile)
  showModal(modalDialog(title="Move on to process your data",
                        size='m',
                        actionButton("rawupload_nextpage","Next step",width='100%'),
                        easyClose = TRUE,
                        footer=NULL
  )
  )
}) %>% bindEvent(input$FASTQFile,input$BarcodeFile,input$Metadata)
observe({
  req(MetadataTable$x) #if the otu table and taxonomy table are both uploaded, take the user to the review page
  req(OTUTable$x)
  req(TaxTable$x)
  showModal(modalDialog(title="Data uploaded. Move on to review your files",
                        size='l',
                        actionButton("fileupload_nextpage","Go to review your files",width='100%'),
                        actionButton("fileupload_nextpage_v3","Go directly to stats",width='100%'),
                        actionButton('fileupload_nextpage_v4',"Go to filter page",width='50%'),
                        easyClose = TRUE,
                        footer=NULL
  )
  )
}) %>% bindEvent(input$assemble_ps)

# page switch actions
switch_page <- function(i) {
  nav_select(id='pages',selected=i)
}

# navigation from home
observeEvent(input$titlepg_start, switch_page("uploadtab"))

# navigation from upload page
observeEvent(input$fileupload_nextpage, switch_page("reviewtab")) #modal dialog box from file upload page takes you to review page
observeEvent(input$fileupload_nextpage_v2, switch_page("reviewtab")) #next button from file upload page takes you to review page
observeEvent(input$fileupload_nextpage_v3, switch_page("basicstab")) #modal dialog box from file upload page takes you to stats page
observeEvent(input$fileupload_nextpage_v4, switch_page("filtertab")) 
observeEvent(input$fileupload_prevpage,switch_page("introtab"))

# navigation from review page
observeEvent(input$reviewfiles_nextpage, switch_page("filtertab")) #next button from review page takes you to basic stats page
observeEvent(input$reviewfiles_prevpage, switch_page("uploadtab")) #previous button from review page takes you back to file upload page

# navigation from filtering page
observeEvent(input$filter_nextpage, switch_page("basicstab"))
observeEvent(input$filter_prevpage, switch_page("reviewtab"))

# navigation from basics page
observeEvent(input$basics_nextpage, switch_page("statstab"))
observeEvent(input$basics_prevpage, switch_page("filtertab")) 

# navigation from stats page
observeEvent(input$analysis_nextpage, switch_page("reporttab"))
observeEvent(input$analysis_prevpage, switch_page("basicstab")) 

# navigation from report page
observeEvent(input$report_prevpage,switch_page("statstab"))

# other navigation
observeEvent(input$psfail_goback,switch_page("uploadtab"))
observeEvent(input$filespage,switch_page('test'))

# navigation sidebar
observeEvent(input$go_home, switch_page("introtab"))
observeEvent(input$go_upload, switch_page("uploadtab"))
observeEvent(input$go_review, switch_page("reviewtab"))
observeEvent(input$go_filter, switch_page("filtertab"))
observeEvent(input$go_desc, switch_page("basicstab"))
observeEvent(input$go_stat, switch_page("statstab"))
observeEvent(input$go_help, switch_page("helptab"))
observeEvent(input$go_settings, switch_page("settingstab"))
observeEvent(input$go_report, switch_page("reporttab"))

# not using yet
observeEvent(input$demux_nextpage, switch_page("Assign OTU"))
observeEvent(input$demux_prevpage, switch_page("uploadtab")) 

observeEvent(input$asv_nextpage, switch_page("Assign Taxonomy")) 
observeEvent(input$asv_prevpage, switch_page("Demultiplex and Quality Control")) 

observeEvent(input$tax_nextpage, switch_page("reviewtab")) 
observeEvent(input$tax_prevpage, switch_page("Assign OTU")) 

# remove modal dialog boxes
observe({
  removeModal() #remove the modal dialog when the user selects something in one of the dialog boxes
}) %>% bindEvent(input$rawupload_nextpage,
                 input$fileupload_nextpage,
                 input$fileupload_nextpage_v3,
                 input$psfail_goback,
                 input$fileupload_nextpage_v4
)

observe({
  if (!isTruthy(MetadataTable$x) || !isTruthy(OTUTable$x) || !isTruthy(TaxTable$x)) {
    showModal(
      modalDialog(title="You do not have all the appropriate files to continue.",
                  paste("The issue is: ", isgood()),
                  size='l',
                  easyClose = FALSE
      )
    ) 
  }
}) %>% bindEvent(input$assemble_ps)