
#knit rmd file in app
#output$markdown <- renderUI({
#  HTML(markdown::mark_html(knitr::knit("ProjectReport.Rmd",
#                                       quiet=TRUE)))
#}) 

#### checks #### 
#output$rep_ps <- mkdwnrptfun(ps.sub())
observe({
  req(ps.norm())
  req(input$pages=="reporttab")
  
  # core plots  
  output$rep_tbarplot <- renderPlot({trophbarplot()})
  print("Report: Feeding guild pie chart rendered")
  #
  output$rep_tviolinplot <- renderPlot({tviolinplot()})
  print("Report: temperature index violin plot rendered")
  #
  output$rep_tviolinmeanplot <- renderPlot({tviolinmeanplot()})
  print("Report: temperature choice violin plot rendered")
  #
  output$rep_sizebarplot <- renderPlot({sizebarplot_base()+sizebarplot_size$aspectRatio()})
  print("Report: Body size pie chart rendered")
  #
  output$rep_vioplot <- renderPlot({violinplot()})
  print("Report: violinplot rendered")
  #
  output$rep_tlolliplot <- renderPlot({tlolliplot()})
  print("Report: Temperature lolliplot rendered")
  #
  output$rep_richplot <- renderPlot({richplot()})
  print("Report: richplot rendered")
  
  ## extra plots
  output$rep_percplot <- renderPlot({rankperc_plot()})
  print("Report: rankperc_plot rendered")
  
  output$rep_histplot <- renderPlot({histoplot()})
  print("Report: histoplot rendered")
  output$rep_hmplot <- renderPlot({hmplot()})
  print("Report: hmplot rendered")
  output$rep_ordplot <- renderPlot({ordplot()})
  print("Report: ordplot rendered")
  #output$rep_tlineplot <- renderPlot({tlineplot()})
  #print("Report: trophic line plot rendered")
  #output$rep_lolliplot <- renderPlot({lolliplot()})
  #print("Report: Trophic level lolliplot rendered")
  
  output$rep_pnsamp <- renderText({mkdwnrptfun(input$prune_samples)}) #min sample number
  print("Report: minimum sample number rendered")
  output$rep_pntax <- renderText({mkdwnrptfun(input$prune_tax)}) #min tax occurrence 
  print("Report: minimum taxa occurrence rendered")
  output$rep_blanks <- renderText({tryCatch(input$blank_samples, #blanks selected
                                            error=function(e){return("No blanks selected")})})
  print("Report: blank selection rendered")
  output$rep_rembl <- renderText({mkdwnrptfun(input$rem_blanks)}) #blanks removed?
  print("Report: blank removal choice recorded")
  
  output$rep_unass <- renderText({if(isTruthy(input$tax_keepremove)) {"The following taxa were kept:"} else if (!isTruthy(input$tax_keepremove) && isTruthy(input$tax_switch)) {"The following taxa were removed:"} else {"No taxa were specifically identified for special analysis"} }) #keep or remove taxa?
  print("Report: Whether taxa were selected for keep/removal recorded")
  output$rep_remtax <- renderPrint({mkdwnrptfun(reactiveValuesToList(taxfilter_list))}) #list of ranks and names of taxa used to filter  
  
  output$rep_remsam <- renderText({mkdwnrptfun(input$sample_select)}) #list of samples removed 
  print("Report: list of samples removed recorded")
  output$rep_norm <- renderText({tryCatch(input$normoptions, #normalized? 
                                          error=function(e){return("No normalization performed")})})
  print("Report: whether normalization happened recorded")
  #output$rep_ordmeth <- mkdwnrptfun(input$ord_meth) #ordination method
  #output$rep_ordtype <- mkdwnrptfun(input$ord_vis) #ordination visualization method
  
  output$rep_numsam <- renderTable({mkdwnrptfun(Summ_seqs_sel())},rownames = TRUE,digits=0) #max/min/avg seqs (subset)
  output$rep_numog <- renderTable({mkdwnrptfun(Summ_seqs())},rownames = TRUE,digits=0) #max/min/avg seqs (original)
  
  #output$rep_richmet <- mkdwnrptfun(input$alphametrics)
})

##### report parameters #####
report <- reactiveValues(pdffilepath=NULL,
                         htmlfilepath=NULL
                         ) 

params <- reactiveValues(blanks=NA, #create list of parameters to read into rmarkdown
                         unass=NA,
                         rembl=NA,
                         sampleselect=NA,
                         norm=NA,
                         ps=NA,
                         ntaxa=NA,
                         numsam=NA,
                         numog=NA,
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
                         vioplot=NA,
                         tbarplot=NA,
                         tlolplot=NA,
                         tvioplot=NA,
                         tviomeanplot=NA,
                         richplot=NA,
                         histplot=NA,
                         hmplot=NA,
                         lolplot=NA,
                         lineplot=NA,
                         samsum=NA,
                         N_seq_total_num_sel=NA,
                         N_seq_assigned_num_sel=NA,
                         N_otu_total_num_sel=NA,
                         N_otu_assigned_num_sel=NA,
                         N_seq_blanks_num_sel=NA,
                         N_otu_blanks_num_sel=NA
)

mkdwnrptfun <- function(x) { #tryCatch will check to see if the 'function' exists, and if it does not, returns NA as an error
  tryCatch(x,error=function(e){return(as.numeric(NA))}) 
}

observe({
  print("Report: generate report button selected")

  report$pdffilepath <- report$htmlfilepath <- NULL
  shinyjs::hide("viewfinalreport")
  params[["ps"]] <- mkdwnrptfun(ps.norm())
  
  params[["vioplot"]] <- mkdwnrptfun(violinplot())
  params[["tbarplot"]] <- mkdwnrptfun(trophbarplot())
  params[["tvioplot"]] <- mkdwnrptfun(tviolinplot())
  params[["tviomeanplot"]] <- mkdwnrptfun(tviolinmeanplot())
  params[["tlolplot"]] <- mkdwnrptfun(tlolliplot())
  params[["sizebarplot"]] <- mkdwnrptfun(sizebarplot_base()+sizebarplot_size$aspectRatio())
  params[["richplot"]] <- mkdwnrptfun(richplot())
  
  params[["percplot"]] <- mkdwnrptfun(rankperc_plot())
  params[["ordplot"]] <- mkdwnrptfun(ordplot())
  params[["histplot"]] <- mkdwnrptfun(histoplot())
  params[["hmplot"]] <- mkdwnrptfun(hmplot())
  #params[["lineplot"]] <- mkdwnrptfun(tlineplot())
  #params[["lolplot"]] <- mkdwnrptfun(lolliplot())
  print("Report: plots saved to params list")
  
  params$pnsamp <- mkdwnrptfun(input$prune_samples) #min sample number
  params$pntax <- mkdwnrptfun(input$prune_tax) #min tax occurrence 
  params$blanks <- tryCatch(input$blank_samples, #blanks selected
                            error=function(e){return("No blanks selected")}) 
  params$rembl <- mkdwnrptfun(input$rem_blanks) #blanks removed?
  
  params$unass <- if(isTruthy(input$sam_keepremove)) {"Keep"} else if (!isTruthy(input$sam_keepremove) && isTruthy(input$sample_switch)) {"Remove"} else {as.numeric(NA)}  #keep or remove taxa?
  params$remtax <- mkdwnrptfun(reactiveValuesToList(taxfilter_list)) #list of ranks and names of taxa used to filter  
  params$sampleselect <- if(isTruthy(input$tax_keepremove)) {"Keep"} else if (!isTruthy(input$tax_keepremove) && isTruthy(input$tax_switch)) {"Remove"} else {as.numeric(NA)}  #keep or remove taxa?
  params$remsam <- mkdwnrptfun(input$sample_select) #list of samples removed/kept   
  
  params$norm <- tryCatch(input$normoptions, #normalized? 
                          error=function(e){return("No normalization performed")})
  
  params$ordmeth <- mkdwnrptfun(input$ord_meth) #ordination method
  params$ordtype <- mkdwnrptfun(input$ord_vis) #ordination visualization method

  params$ntaxa <- mkdwnrptfun(N_taxa_tab())
  params$numsam <- mkdwnrptfun(Summ_seqs_sel())
  params$numog <- mkdwnrptfun(Summ_seqs())
  params$samsum <- mkdwnrptfun(N_sample_df())
  
  params$nseq <- mkdwnrptfun(reactiveValuestoList(Num_seq_subset))
  params$notu <- mkdwnrptfun(reactiveValuestoList(Num_otu_subset))
  
  params$N_seq_total_num_sel <- mkdwnrptfun(Num_seq_subset$N_seq_num)
  params$N_seq_assigned_num_sel <- mkdwnrptfun(Num_seq_subset$N_seq_assigned) 
  
  params$N_otu_total_num_sel <- mkdwnrptfun(Num_otu_subset$N_otu_num) 
  params$N_otu_assigned_num_sel <- mkdwnrptfun(Num_otu_subset$N_otu_assigned) 
  
  params$N_seq_blanks_num_sel <- mkdwnrptfun(Num_seq_subset$N_seq_blanks)
  params$N_otu_blanks_num_sel <- mkdwnrptfun(Num_otu_subset$N_otu_blanks)
  
  params$richmet <- mkdwnrptfun(input$alphametrics)
  paramvals <- reactiveValuesToList(params) #convert reactive values to list to read into render
  #params[params=="NULL"] <- NA #any NULL values (because input$x does not exist) are replaced with NA because markdown does not like NULL in the YAML header
  print("Report: variables saved to params list")
  
  library(rmarkdown)
  
  if(input$gen_onepg_s) {
    withProgress(
      message = "Gathering data and building static report. \n", 
      detail = "This may take a while. This window will disappear when the report is ready.",
      value=0, {
        render('OnePageSummary.Rmd',
               output_format = c('html_document'),#,'pdf_document','word_document'), 
               params= paramvals, 
               output_file = "tmp_file",
               output_dir = tempdir(),
               envir = new.env(parent=globalenv()),
               intermediates_dir = tempdir() 
        )}
    )
  }
  
  if(input$gen_onepg_i) {
    withProgress(
      message = "Gathering data and building interactive report. \n", 
      detail = "This may take a while. This window will disappear when the report is ready.",
      value=0, {
        render('OnePageSummaryPlotly.Rmd',
               output_format = c('html_document'),#,'pdf_document','word_document'), 
               params= paramvals, 
               output_file = "tmp_file",
               output_dir = tempdir(),
               envir = new.env(parent=globalenv()),
               intermediates_dir = tempdir() 
        )}
    )
  }
  
  if(input$gen_report_s) {
    withProgress(
      message = "Gathering data and building static report. \n", 
      detail = "This may take a while. This window will disappear when the report is ready.",
      value=0, {
        render('ProjectReport.Rmd',
               output_format = c('html_document'),#,'pdf_document','word_document'), 
               params= paramvals, 
               output_file = "tmp_file",
               output_dir = tempdir(),
               envir = new.env(parent=globalenv()),
               intermediates_dir = tempdir() 
        )}
    )
  }
  
  if(input$gen_report_i) {
    withProgress(
      message = "Gathering data and building interactive report. \n", 
      detail = "This may take a while. This window will disappear when the report is ready.",
      value=0, {
        render('ProjectReportPlotly.Rmd',
               output_format = c('html_document'),#,'pdf_document','word_document'), 
               params= paramvals, 
               output_file = "tmp_file",
               output_dir = tempdir(),
               envir = new.env(parent=globalenv()),
               intermediates_dir = tempdir() 
        )}
    )
  }
  
  report$pdffilepath <- list.files(tempdir(), pattern="tmp_file.pdf", full.names = TRUE)
  report$htmlfilepath <- list.files(tempdir(), pattern="tmp_file.html", full.names = TRUE)
  #report$docxfilepath <- list.files(tempdir(), pattern="tmp_file.docx", full.names = TRUE) #Assigning in the temp file where the .pdf is located to the reactive file created above
  
}) %>% bindEvent(input$gen_onepg_s,
                 input$gen_onepg_i,
                 input$gen_report_s,
                 input$gen_report_i,
                 ignoreInit=TRUE)

observe({
  if(is.null(input$gen_report_s) && is.null(input$gen_report_i) && is.null(input$gen_onepg_i) && is.null(input$gen_onepg_s)) shinyjs::hide("dwnldviewreport")
  if(!is.null(report$htmlfilepath)) shinyjs::show("dwnldviewreport")
})
# Hide download button until report is generated
#output$reportbuilt <- reactive({
#  return(!is.null(report$htmlfilepath))
#}) %>% bindEvent(input$gen_report)
#outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)

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

#Hide report if ps.norm changes
observeEvent(ps.norm(),{
  print("Report: ps.norm changed; final report and download report hidden")
  shinyjs::hide("viewfinalreport")
  shinyjs::hide("dwnldviewreport")
},ignoreInit = TRUE)

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

observeEvent(input$norm_meltdwnld,{
  print("Download: dataset")
})
observeEvent(input$norm_taxdwnld,{
  print("Download: tax data")
})
observeEvent(input$norm_asvdwnld,{
  print("Download: asv data")
})
observeEvent(input$norm_metadwnld,{
  print("Download: metadata")
})

output$norm_meltdwnld <- downloadHandler(
    filename = function() {
      paste("ShinyeDNA_fullDataset", ".csv", sep="")
    },
    content = function(file) {
      write.csv(ps.norm.melt(),file)
    }
  )
  
  output$norm_taxdwnld <- downloadHandler(
    filename = function() {
      paste("ShinyeDNA_taxdataset", ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(as.matrix(tax_table(ps.norm()))),
                file)
    }
  )
  
  output$norm_asvdwnld <- downloadHandler(
    filename = function() {
      paste("ShinyeDNA_asvdataset", ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(as.matrix(otu_table(ps.norm()))),
                file)
    }
  )
  
  output$norm_metadwnld <- downloadHandler(
    filename = function() {
      paste("ShinyeDNA_metadataset", ".csv", sep="")
    },
    content = function(file) {
      write.csv(as.data.frame(sample_data(ps.norm())),
                file)
    }
  )
  
  observeEvent(input$download_zip, {
    
    figlist <- reactive(list( #list of plots where nonexistent plots return "NA"
      
      mkdwnrptfun(trophbarplot()),
      mkdwnrptfun(violinplot()),
      mkdwnrptfun(tviolinplot()),
      mkdwnrptfun(tviolinmeanplot()),
      mkdwnrptfun(tlolliplot())),
      mkdwnrptfun(sizebarplot_base()+sizebarplot_size$aspectRatio()),
      
      mkdwnrptfun(richplot()),
      mkdwnrptfun(rankperc_plot()),
      mkdwnrptfun(ordplot()),
      mkdwnrptfun(histoplot()),
      mkdwnrptfun(hmplot())
    )
    
    myfigs <- reactive( #remove NAs from figlist()
      lapply(figlist(), function(x) x[!is.na(x)])
    )
    
    nplots <- reactive(length(myfigs()))
    
    lapply(1:nplots(), function(i){
      ggsave(paste0("yplot",i,".png"), 
             plot(mydata()[[i]]),
             path=fig_dir(),
             scale=2)
    })
  }, ignoreInit = TRUE)
  
  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("ShinyeDNA_figures", ".zip", sep="")
    },
    content = function(file) {
      
      files_to_zip <- list.files(tempdir(), pattern=".png", full.names = TRUE)
      print(files_to_zip)
      
      zip(zipfile = file, 
          files = files_to_zip, 
          flags = "-j")
    },
    contentType = "application/zip"
  )
  
