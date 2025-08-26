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
                        layout_columns(col_widths = c(6,6),
                          actionButton("fileupload_nextpage","Go to review files"),
                          actionButton('fileupload_nextpage_v4',"Go to filter page")),
                        actionButton("fileupload_nextpage_v3","Go directly to stats",width='100%'),
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
observeEvent(input$fileupload_prevpage, switch_page("introtab")) #top button
observeEvent(input$bfileupload_prevpage, switch_page("introtab")) #bottom button

observeEvent(input$fileupload_nextpage_v2, switch_page("reviewtab")) 
observeEvent(input$bfileupload_nextpage_v2, switch_page("reviewtab")) 

# navigation from modal dialog box after file upload
observeEvent(input$fileupload_nextpage, switch_page("reviewtab")) #modal dialog box from file upload page takes you to review page
observeEvent(input$fileupload_nextpage_v3, switch_page("basicstab")) #modal dialog box from file upload page takes you to stats page
observeEvent(input$fileupload_nextpage_v4, switch_page("filtertab")) 

# navigation from review page
observeEvent(input$review_nextpage, switch_page("traittab")) 
observeEvent(input$review_prevpage, switch_page("uploadtab")) 
observeEvent(input$breview_nextpage, switch_page("traittab")) 
observeEvent(input$breview_prevpage, switch_page("uploadtab")) 

# navigation from trait page
observeEvent(input$trait_nextpage, switch_page("filtertab"))
observeEvent(input$trait_prevpage, switch_page("reviewtab")) 
observeEvent(input$btrait_nextpage, switch_page("filtertab"))
observeEvent(input$btrait_prevpage, switch_page("reviewtab")) 

# navigation from filtering page
observeEvent(input$filter_nextpage, switch_page("basicstab"))
observeEvent(input$filter_prevpage, switch_page("traittab"))
observeEvent(input$bfilter_nextpage, switch_page("basicstab"))
observeEvent(input$bfilter_prevpage, switch_page("traittab"))

# navigation from basics page
observeEvent(input$basics_nextpage, switch_page("statstab"))
observeEvent(input$basics_prevpage, switch_page("filtertab")) 
observeEvent(input$bbasics_nextpage, switch_page("statstab"))
observeEvent(input$bbasics_prevpage, switch_page("filtertab")) 

# navigation from stats page
observeEvent(input$analysis_nextpage, switch_page("reporttab"))
observeEvent(input$analysis_prevpage, switch_page("basicstab")) 
observeEvent(input$banalysis_nextpage, switch_page("reporttab"))
observeEvent(input$banalysis_prevpage, switch_page("basicstab")) 

# navigation from report page
observeEvent(input$report_prevpage,switch_page("statstab"))
observeEvent(input$breport_prevpage,switch_page("statstab"))

# other navigation
observeEvent(input$psfail_goback,switch_page("uploadtab"))
observeEvent(input$filespage,switch_page('test'))

# navigation sidebar
observeEvent(input$go_home, switch_page("introtab"))
observeEvent(input$go_upload, switch_page("uploadtab"))
observeEvent(input$go_review, switch_page("reviewtab"))
observeEvent(input$go_filter, switch_page("filtertab"))
observeEvent(input$go_desc, switch_page("basicstab"))
observeEvent(input$go_trait, switch_page("traittab"))
observeEvent(input$go_stat, switch_page("statstab"))
observeEvent(input$go_help, switch_page("helptab"))
observeEvent(input$go_settings, switch_page("settingstab"))
observeEvent(input$go_report, switch_page("reporttab"))

# report accordion
observeEvent(input$repaccd_percplot,switch_page("basicstab"))
observeEvent(input$repaccd_richplot, switch_page("statstab"))
observeEvent(input$repaccd_ordplot, switch_page("statstab"))
observeEvent(input$repaccd_histplot, switch_page("statstab"))
observeEvent(input$repaccd_hmplot,switch_page("statstab"))
observeEvent(input$repaccd_vioplot,switch_page("statstab"))

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

# --- # Sidebar # --------
observe({
  sidebar_toggle(
    id="sidebar_main",
    open=FALSE #sidebar with selection to remove blanks and unassigned sequences is hidden by default
  )
  if(input$pages == "Basic Stats" || input$pages == "Analysis") { #blank/unassigned selection sidebar is open for these two pages
    sidebar_toggle(
      id="sidebar_main",
      open = TRUE
    ) 
    if (!isTruthy(MetadataTable$x) || !isTruthy(OTUTable$x) || !isTruthy(TaxTable$x)) {
      showModal(
        modalDialog(title="You do not have all the appropriate files for this page, go back to the beginning and check files",
                    paste("The issue is: ", isgood()),
                    size='l',
                    actionButton("psfail_goback","Go to review your files",width='100%'),
                    easyClose = FALSE
        )
      ) 
    } else if (!is.null(ps.all())) {removeModal()} 
  } else {
    sidebar_toggle(
      id="sidebar_main",
      open = FALSE
    )
  }
},priority=1) %>% bindEvent(input$pages)