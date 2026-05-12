library(shiny)
library(bslib)
library(DT)
library(dplyr)
library(shinycssloaders)
library(shinyWidgets)
library(vroom)
library(shinyFeedback)
library(shinyBS)
library(shinyjqui)


#render_dt = function(data, editable = 'cell', server = TRUE,  ...) {
#  renderDT(data, selection = 'none', server = server, editable = editable, ...)
#}

tableUI <- function(id,editable=FALSE) {
  if(editable==TRUE) {
    card(
      useShinyjs(),#set up shinyjs
      full_screen = TRUE,
      card_body(fill=FALSE,
                layout_columns(col_widths=c(11,1),
      #card_header(layout_columns(col_widths=c(11,1),
                                 " ",
                                 #"To edit individual cells, double click on the table. Save edits with Control + Enter",
                                 layout_columns(
                                   popover(bsicons::bs_icon("ban", class = "ms-auto"),
                                           id=NS(id,'resetbutton'),
                                           placement="right",
                                           actionButton(NS(id,'reset'),'Reset Modifications')
                                   ),
                                   popover(bsicons::bs_icon("download", class = "ms-auto"),
                                           id=NS(id,'dwnldbutton'),
                                           placement="right",
                                           downloadButton(NS(id,'download'),'Download Modifications'))
                                 )
      )),
      DTOutput(NS(id,"failtable")),DTOutput(NS(id,"table"))
                            
         )
    } else if(editable==FALSE) {
           card(
             full_screen = TRUE,
             card_body(fill=FALSE,
                       layout_columns(col_widths=c(11,1),
             #card_header(layout_columns(col_widths=c(11,1),
                                        " ",
                                        #Expand or save table",
                                        popover(bsicons::bs_icon("download", class = "ms-auto"),
                                                id=NS(id,'dwnldbutton'),
                                                placement="right",
                                                downloadButton(NS(id,'download'),'Download'))
             )),
             DTOutput(NS(id,"failtable")),DTOutput(NS(id,"table_noedit"))
             
           )}
}

tableServer <- function(id,data,editable=FALSE) {
  moduleServer(id, function(input,output,session) {
    ns <- session$ns

    values <- reactiveValues()
    values$table <- values$empty <- NULL
    
    observeEvent(data$x, {
      values$table <- values$empty <- NULL #resets values
      if(is.null(data$x)) { #if there is no input data to make a table out of
    #    output$goodtable <- FALSE
        values$empty <- TRUE 
        values$table <- FALSE
        print(c(id," table is null"))
      } else if (!is.null(data$x)) {
    #    output$goodtable <- TRUE
        values$empty <- FALSE
        values$table <- TRUE
        print(c(id," table is not null"))
      }
    }, ignoreNULL = FALSE)
    
 output$badtable <- reactive({
          return(values$empty)
        }) %>% bindEvent(data$x)
 
 output$goodtable <- reactive({
          return(values$table)
        }) %>% bindEvent(data$x)
  
    output$failtable <- renderDT({
      data.frame(ID=c("Sample1","Sample2","Sample3"),Var1=c("","Placeholder",""),Var2=c("","",""),Var3=c("","",""))
    })
    outputOptions(output, "failtable", suspendWhenHidden = FALSE) 
    #outputOptions(output, "table", suspendWhenHidden = FALSE) 
    #outputOptions(output, "table_noedit", suspendWhenHidden = FALSE) 

    observeEvent(values$table, {
      if(values$table==TRUE)  {
        print(c(id,"table panel shown"))
        hide('failtable')
        show('table')
        show('table_noedit')
        } else {
          print(c(id,"table panel hidden"))
          show('failtable')
          hide('table')
          hide('table_noedit')
          }
    })
    
    observe({
    output$table <- renderDT(data$x,
                              editable='all',rownames=FALSE,
                             caption = htmltools::tags$caption(
                               style = 'caption-side: top; text-align: center; color:black; font-size:1.5vw; font-weight:bold;',
                               'Edit your table by double clicking inside a cell, save edits with Control + Enter'))
    print(c(id,"editable table rendered"))
    })
    observe({
      output$table_noedit <- renderDT(data$x,rownames=FALSE,
                                      caption = htmltools::tags$caption(
                                        style = 'caption-side: top; text-align: center; color:black; font-size:1.5vw; font-weight:bold;',
                                        'Save or expand your table')
                                      )
      print(c(id,"non-editable table rendered"))
    })

    proxy=dataTableProxy('table')
    observeEvent(input$table_cell_edit, {
      print(c(id,"table edited"))
      info = input$table_cell_edit
      str(info)
      i=info$row
      j=info$col
      v=info$value
      data$x[i,j] <<- v 
    })
    
    output$download <-  downloadHandler(
      filename = function() {
        paste(Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(data$x,file)
      }
    ) 
    observeEvent(input$reset, {
      print(c(id,"data reset"))
      data$x <- data$og
    })
    
    observe({
      if(editable==TRUE) {
        output$bigTable <- renderDT(data$x,
                                    caption = htmltools::tags$caption(
                                      style = 'caption-side: top; text-align: center; color:black; font-size:1.5vw; font-weight:bold;',
                                      'Edit your table by double clicking inside a cell, save edits with Control + Enter'),
                                    editable='all',
                                    #filter='top',
                                    rownames=FALSE)
        print(c(id,"large editable table rendered"))
      } else {
      output$bigTable <- renderDT(data$x,
                                  rownames=FALSE)
      print(c(id,"large non-editable table rendered"))
      }
    }) 
    # The function to show the popup screen.
      dataModal <- function(ns) {
        modalDialog(
          DTOutput(NS(id,'bigTable')),
            #renderDataTable("bigTable"),
            footer=downloadButton(NS(id,'bigdownload'), 'Download'),
            size="xl",
            easyClose = TRUE
        ) # end of modalDialog
      } # end of function
      
      observeEvent(input$show,  # Show modal when button is clicked
                   { showModal(dataModal(session$ns))
                     toggle_popover('expndbutton') #close the expansion popup when clicked
                     })
      
    #  # Get changed data from table on popup and store in dataframe
      proxy2=dataTableProxy('bigTable')
      observeEvent(input$bigTable_cell_edit, {
        info = input$bigTable_cell_edit
        str(info)
        i = info$row
        j = info$col
        k = info$value
        v = info$value
        #data$x[i,j+1] <<- v 
        data$x <- editData(data$x,
                           info,
                           #proxy=proxy2,
                           rownames=FALSE)
        #data$x[i, j+1] <<- DT::coerceValue(k, data$x[i, j])
        #print(data$x)
      })
observeEvent(input$download, {toggle_popover('dwnldbutton')}) #close the download popup when clicked
observeEvent(input$reset, {toggle_popover('resetbutton')}) #close the reset popup when clicked
  })
}
