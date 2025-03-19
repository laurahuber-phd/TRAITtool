
#knit rmd file in app
#output$markdown <- renderUI({
#  HTML(markdown::mark_html(knitr::knit("ProjectReport.Rmd",
#                                       quiet=TRUE)))
#}) 

report <- reactiveValues(pdffilepath=NULL,
                         htmlfilepath=NULL,
                         docxfilepath=NULL) #This creates a short-term storage location for a filepath
params <- reactiveValues(blanks=NA, #create list of parameters to read into rmarkdown
                         unass=NA,
                         rembl=NA,
                         norm=NA,
                         ps=NA,
                         ntaxa=NA,
                         numsam=NA,
                         numog=NA,
                         richplot=NA,
                         percplot=NA,
                         ordmeth=NA,
                         ordtype=NA,
                         ordplot=NA,
                         N_seq_total_num_sel=NA,
                         N_seq_assigned_num_sel=NA,
                         N_otu_total_num_sel=NA,
                         N_otu_assigned_num_sel=NA,
                         N_seq_blanks_num_sel=NA,
                         N_otu_blanks_num_sel=NA,
                         richmet=NA
)

observeEvent(input$gen_report, {
  
  #tryCatch will check to see if the 'function' exists, and if it does not, returns NA as an error
  params[["ps"]] <- tryCatch(ps.sub(),error=function(e){return(as.numeric(NA))}) 
  params[["richplot"]] <- tryCatch(richplot(),error=function(e){return(as.numeric(NA))})
  params[["percplot"]] <- tryCatch(histoplot(),error=function(e){return(as.numeric(NA))})
  params[["ordplot"]] <- tryCatch(ordplot(),error=function(e){return(as.numeric(NA))})
  params$blanks <- tryCatch(input$blank_samples,error=function(e){return("No blanks selected")})
  params$unass <- tryCatch(input$adjust,error=function(e){return(as.numeric(NA))})
  params$rembl <- tryCatch(input$rem_blanks,error=function(e){return(as.numeric(NA))})
  params$norm <- tryCatch(input$normoptions,error=function(e){return("No normalization performed")})
  params$ordmeth <- tryCatch(input$ordmeth,error=function(e){return(as.numeric(NA))})
  params$ordtype <- tryCatch(input$ordvis,error=function(e){return(as.numeric(NA))})
  params$ntaxa <- tryCatch(N_taxa_tab(),error=function(e){return(as.numeric(NA))})
  params$numsam <- tryCatch(Summ_seqs_sel(),error=function(e){return(as.numeric(NA))})
  params$numog <- tryCatch(Summ_seqs(),error=function(e){return(as.numeric(NA))})
  params$N_seq_total_num_sel <- tryCatch(N_seq_total_num_sel(),error=function(e){return(as.numeric(NA))})
  params$N_seq_assigned_num_sel <- tryCatch(N_seq_assigned_num_sel(),error=function(e){return(as.numeric(NA))})
  params$N_otu_total_num_sel <- tryCatch(N_otu_total_num_sel(),error=function(e){return(as.numeric(NA))})
  params$N_otu_assigned_num_sel <- tryCatch(N_otu_assigned_num_sel(),error=function(e){return(as.numeric(NA))})
  params$N_seq_blanks_num_sel <- tryCatch(N_seq_blanks_num_sel(),error=function(e){return(as.numeric(NA))})
  params$N_otu_blanks_num_sel <- tryCatch(N_otu_blanks_num_sel(),error=function(e){return(as.numeric(NA))})
  params$richmet <- tryCatch(input$alphametrics, error=function(e){return(as.numeric(NA))})
  
  paramvals <- reactiveValuesToList(params) #convert reactive values to list to read into render
  #params[params=="NULL"] <- NA #any NULL values (because input$x does not exist) are replaced with NA because markdown does not like NULL in the YAML header
  
  library(rmarkdown)
  withProgress(
    message = "Gathering data and building report. \n", 
    detail = "This may take a while. This window will disappear when the report is ready.",
    value=0, {
      render('ProjectReport.Rmd',
             output_format = c('pdf_document','html_document','word_document'), 
             params= paramvals, 
         #switch(input$format,
        #        PDF = pdf_document(),
        #        HTML = html_document(),
        #        Word = word_document()
        # ), 
         output_file = "tmp_file",
        output_dir = tempdir(),
         envir = new.env(parent=globalenv()),
         intermediates_dir = tempdir() #On your own computer, you don't need to set intermediates_dir = tempdir(), but you will need to do this if you want to deploy your app on a shiny server. When rmarkdown renders an Rmd file, it creates several intermediate files in the working directory and then deletes them. you have permission to write to the app's working directory on your own computer, but might not on a shiny server. tempdir() is almost always a safe place to write temporary files to.
      )}
        )
  
  #  render("report/GA_report.Rmd", 
  #         output_format = "all", 
  #         output_file = tmp_file,
  #         params = params, 
  #         envir = new.env())
  
  report$pdffilepath <- list.files(tempdir(), pattern="tmp_file.pdf", full.names = TRUE)
  report$htmlfilepath <- list.files(tempdir(), pattern="tmp_file.html", full.names = TRUE)
  report$docxfilepath <- list.files(tempdir(), pattern="tmp_file.docx", full.names = TRUE) #Assigning in the temp file where the .pdf is located to the reactive file created above
  
})


# Hide download button until report is generated
output$reportbuilt <- reactive({
  return(!is.null(report$filepath))
})
outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)

#Download report  
output$download_report <- downloadHandler(
  
  # This function returns a string which tells the client browser what name to use when saving the file.
  filename=function() {
    paste('ShinyReport', Sys.Date(), sep=".", switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  # This function should write data to a file given to it by the argument 'file'.
  content = function(file) {
    file.copy(switch(input$format,
                     PDF = report$pdffilepath,
                     HTML = report$htmlfilepath,
                     Word = report$docxfilepath), file)
  }
)

#View report in app
observeEvent(input$view_report, {
  req(report$htmlfilepath)
  addResourcePath('vwfile',tempdir())
  
  output$markdown <- renderUI({
    tags$iframe(seamless='seamless', src='vwfile/tmp_file.html',width=800,height=800)
    #includeHTML(report$htmlfilepath)
  }) 
  
})
