
#knit rmd file in app
#output$markdown <- renderUI({
#  HTML(markdown::mark_html(knitr::knit("ProjectReport.Rmd",
#                                       quiet=TRUE)))
#}) 
##### report parameters #####
report <- reactiveValues(pdffilepath=NULL,
                         htmlfilepath=NULL
) 

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
                         nseq=NA,
                         notu=NA,
                         richmet=NA,
                         pnsamp=NA,
                         pntax=NA,
                         remtax=NA,
                         remsam=NA,
                         histplot=NA,
                         hmplot=NA,
                         samsum=NA
)

mkdwnrptfun <- function(x) { #tryCatch will check to see if the 'function' exists, and if it does not, returns NA as an error
  tryCatch(x,error=function(e){return(as.numeric(NA))}) 
}

observeEvent(input$gen_report, {
  report$pdffilepath <- report$htmlfilepath <- NULL
  shinyjs::hide("viewfinalreport")
  params[["ps"]] <- mkdwnrptfun(ps.sub())
  params[["richplot"]] <- mkdwnrptfun(richplot())
  params[["percplot"]] <- mkdwnrptfun(rankperc_plot())
  params[["ordplot"]] <- mkdwnrptfun(ordplot())
  params[["histplot"]] <- mkdwnrptfun(histoplot())
  params[["hmplot"]] <- mkdwnrptfun(hmplot())
  
  params$pnsamp <- mkdwnrptfun(input$prune_samples) #min sample number
  params$pntax <- mkdwnrptfun(input$prune_tax) #min tax occurrence 
  params$blanks <- tryCatch(input$blank_samples, #blanks selected
                            error=function(e){return("No blanks selected")}) 
  params$rembl <- mkdwnrptfun(input$rem_blanks) #blanks removed?
  
  params$unass <- if(isTruthy(input$tax_keepremove)) {"Keep"} else if (!isTruthy(input$tax_keepremove) && isTruthy(input$tax_switch)) {"Remove"} else {as.numeric(NA)}  #keep or remove taxa?
  #params$remtax <- mkdwnrptfun(as.list(input$tax_select)) #list of ranks and names of taxa used to filter  
  params$remtax <- mkdwnrptfun(reactiveValuesToList(taxfilter_list)) #list of ranks and names of taxa used to filter  
  params$remsam <- mkdwnrptfun(input$sample_select) #list of samples removed 
  
  params$norm <- tryCatch(input$normoptions, #normalized? 
                          error=function(e){return("No normalization performed")})
  
  params$ordmeth <- mkdwnrptfun(input$ord_meth) #ordination method
  params$ordtype <- mkdwnrptfun(input$ord_vis) #ordination visualization method
  
  params$ntaxa <- mkdwnrptfun(N_taxa_tab())
  params$numsam <- mkdwnrptfun(Summ_seqs_sel())
  params$numog <- mkdwnrptfun(Summ_seqs())
  params$samsum <- mkdwnrptfun(N_sample_df())
  
  params$nseq <- mkdwnrptfun(reactiveValuestoList(Num_seq_subset_df))
  params$notu <- mkdwnrptfun(reactiveValuestoList(Num_otu_subset_df))
  
  params$richmet <- mkdwnrptfun(input$alphametrics)
  paramvals <- reactiveValuesToList(params) #convert reactive values to list to read into render
  #params[params=="NULL"] <- NA #any NULL values (because input$x does not exist) are replaced with NA because markdown does not like NULL in the YAML header
  
  library(rmarkdown)
  withProgress(
    message = "Gathering data and building report. \n", 
    detail = "This may take a while. This window will disappear when the report is ready.",
    value=0, {
      render('ProjectReport.Rmd',
             output_format = c('pdf_document','html_document'),#,'word_document'), 
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
  #report$docxfilepath <- list.files(tempdir(), pattern="tmp_file.docx", full.names = TRUE) #Assigning in the temp file where the .pdf is located to the reactive file created above
  
})


# Hide download button until report is generated
output$reportbuilt <- reactive({
  return(!is.null(report$htmlfilepath))
}) %>% bindEvent(input$gen_report)
outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)

#Download report  
output$download_report <- downloadHandler(
  
  # This function returns a string which tells the client browser what name to use when saving the file.
  filename=function() {
    paste('ShinyReport', Sys.Date(), sep=".", switch(
      input$format, PDF = 'pdf', HTML = 'html'#, Word = 'docx'
    ))
  },
  # This function should write data to a file given to it by the argument 'file'.
  content = function(file) {
    file.copy(switch(input$format,
                     PDF = report$pdffilepath,
                     HTML = report$htmlfilepath#,
                     #Word = report$docxfilepath
                     ), file)
  }
)

#View report in app
observeEvent(input$view_report, {
  shinyjs::show("viewfinalreport")
  
  req(report$htmlfilepath)
  addResourcePath('vwfile',tempdir())
  
  output$markdown <- renderUI({
    tags$iframe(seamless='seamless', src='vwfile/tmp_file.html',width=800,height=800)
    #includeHTML(report$htmlfilepath)
  }) 
  
})
