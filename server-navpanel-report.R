
#knit rmd file in app
output$markdown <- renderUI({
  HTML(markdown::mark_html(knitr::knit("ProjectReport.Rmd",
                                       quiet=TRUE)))
}) 

report <- reactiveValues(filepath=NULL) #This creates a short-term storage location for a filepath

observeEvent(input$gen_report, {
  
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  progress$set(message = "Gathering data and building report.", 
               detail = "This may take a while. This window will disappear  
                     when the report is ready.", value = 1)
  
  #create list of parameters to read into rmarkdown
  #any reactive that is read in will throw a warning that stops creation of the list, so we need to add them separately
  params <- list(
    blanks=input$blank_samples,
    unass=input$adjust,
    rembl=input$rem_blanks,
    norm=input$normoptions,
    ps=NA, #ps=ps.sub(),
    richplot=NA, #richplot=richplot(),
    percplot=NA,  #percplot=histo_plot(),
    ordmeth=input$ordmeth,
    ordtype=input$ordvis,
    ordplot=NA #ordplot=ordplot()
  )
  
  #tryCatch will check to see if the 'function' exists, and if it does not, returns NA as an error
  params[["ps"]] <- tryCatch(ps.sub(),error=function(e){return(as.numeric(NA))}) 
  params[["richplot"]] <- tryCatch(richplot(),error=function(e){return(as.numeric(NA))})
  params[["percplot"]] <- tryCatch(histo_plot(),error=function(e){return(as.numeric(NA))})
  params[["ordplot"]] <- tryCatch(ordplot(),error=function(e){return(as.numeric(NA))})
  
  params[params=="NULL"] <- NA #any NULL values (because input$x does not exist) are replaced with NA because markdown does not like NULL in the YAML header
  
  tmp_file <- paste0(tempfile(),  switch(
    input$format, PDF = '.pdf', HTML = '.html', Word = '.docx'
  )
  ) #Creating the temp where the file is going to be stored
  
  library(rmarkdown)
  render('ProjectReport.Rmd',
         params= params,
         switch(input$format,
                PDF = pdf_document(),
                HTML = html_document(),
                Word = word_document()
         ), 
         output_file = tmp_file,
         envir = new.env(parent=globalenv()),
         intermediates_dir = tempdir() #On your own computer, you don't need to set intermediates_dir = tempdir(), but you will need to do this if you want to deploy your app on a shiny server. When rmarkdown renders an Rmd file, it creates several intermediate files in the working directory and then deletes them. you have permission to write to the app's working directory on your own computer, but might not on a shiny server. tempdir() is almost always a safe place to write temporary files to.
  )
  
  #  render("report/GA_report.Rmd", 
  #         output_format = "all", 
  #         output_file = tmp_file,
  #         params = params, 
  #         envir = new.env())
  
  report$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above
  
  
})


# Hide download button until report is generated
output$reportbuilt <- reactive({
  return(!is.null(report$filepath))
})
outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)

#Download report  
output$download_report <- downloadHandler(
  
  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename=function() {
    paste('ShinyReport', Sys.Date(), sep=".", switch(
      input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
    ))
  },
  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
    
    file.copy(report$filepath, file)
    
  }
)


