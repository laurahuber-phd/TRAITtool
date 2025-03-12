observeEvent(input$gen_report, {
  progress <- shiny::Progress$new()
  # Make sure it closes when we exit this reactive, even if there's an error
  on.exit(progress$close())
  progress$set(message = "Gathering data and building report.", 
               detail = "This may take a while. This window will disappear  
                     when the report is ready.", value = 1)
  
  #src <- normalizePath('ProjectReport.Rmd')
  # temporarily switch to the temp dir, in case you do not have write
  # permission to the current working directory
  #owd <- setwd(tempdir())
  #on.exit(setwd(owd))
  #file.copy(src, 'ProjectReport.Rmd', overwrite = TRUE)
  
  library(rmarkdown)
  tempReport <- file.path(tempdir(),"ProjectReport.Rmd")
  file.copy("ProjectReport.Rmd",tempReport,overwrite=TRUE)
  
  params <- list(
    blanks=input$blank_samples,
    unass=input$adjust,
    rembl=input$rem_blanks,
    norm=input$normoptions,
    ps=ps.sub(),
    richplot=richplot(),
    percplot=histo_plot(),
    ordmeth=input$ordmeth,
    ordtype=input$ordvis,
    ordplot=ordplot()
  )
  
  report <- reactiveValues(filepath=NULL)
  
  render('ProjectReport.Rmd',tempReport,
                output_file=tmp_file,
                switch(input$format,
                       PDF=pdf_document(),
                       HTML=html_document(),
                       Word=word_document()))
  
  report$filepath <- tmp_file 
  
  # Hide download button until report is generated
  output$reportbuilt <- reactive({
    return(!is.null(report$filepath))
  })
  outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)
  
})


output$download_report <- downloadHandler(
  #filename="ProjectReport.html",
  filename = function() {
    paste('my-report',sep=".",
          switch(input$format,
                 PDF="pdf",HTML="html",Word="docx")
          )
  },
  content = function(file) {
    #src <- normalizePath('ProjectReport.Rmd')
    # temporarily switch to the temp dir, in case you do not have write
    # permission to the current working directory
    #owd <- setwd(tempdir())
    #on.exit(setwd(owd))
    #file.copy(src, 'ProjectReport.Rmd', overwrite = TRUE)
    
    library(rmarkdown)
    #tempReport <- file.path(tempdir(),"ProjectReport.Rmd")
    #file.copy("ProjectReport.Rmd",tempReport,overwrite=TRUE)
    
    params <- list(
      blanks=input$blank_samples,
      unass=input$adjust,
      rembl=input$rem_blanks,
      norm=input$normoptions,
      ps=ps.sub(),
      richplot=richplot(),
      percplot=histo_plot(),
      ordmeth=input$ordmeth,
      ordtype=input$ordvis,
      ordplot=ordplot()
    )
    
    
    file.rename(out,file)
    
    # Knit the document, passing in the `params` list, and eval it in a
    # child of the global environment (this isolates the code in the document
    # from the code in this app).
    #rmarkdown::render(tempReport,
    #                  #"ProjectReport.Rmd",
    #                  output_file=file,
    #                  #tempReport,
    #                  #output_file=filename,
    #                  params = params,
    #                  envir = new.env(parent=globalenv())#, #Setting the envir argument to new.env() makes sure that settings in your Shiny app, such as a default ggplot theme, can't affect the rendered file, but also makes it so that the R Markdown code will not have access to any objects from your app, such as input or v$summary_data, unless you pass them as parameters.
    #                  #intermediates_dir = tempdir() #On your own computer, you don't need to set intermediates_dir = tempdir(), but you will need to do this if you want to deploy your app on a shiny server. When rmarkdown renders an Rmd file, it creates several intermediate files in the working directory and then deletes them. you have permission to write to the app's working directory on your own computer, but might not on a shiny server. tempdir() is almost always a safe place to write temporary files to.
    #)
  }
)

output$markdown <- renderUI({
  HTML(markdown::mark_html(knitr::knit("ProjectReport.Rmd",
                                       quiet=TRUE)))
}) 