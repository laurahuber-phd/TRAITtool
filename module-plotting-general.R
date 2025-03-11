library(ggplot2)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(phyloseq)

plotControlsUI <- function(id,color=TRUE,x=TRUE,y=FALSE,facet=TRUE) {
#what will be in the dropdown menu
  if (color==TRUE & x==TRUE & y==FALSE & facet==TRUE) {
    tagList(
      shinyWidgets::dropdownButton(
        #tags$h3("List of Inputs"),
        inputId=NS(id,"mydropdown"),
        label="controls",
        circle=FALSE,
        icon = icon("gear"), 
        selectInput(inputId = NS(id,"choose_color"),label="Color",choices=list()),
        selectInput(inputId = NS(id,"choose_x"),label="X-axis variable",choices=list()),
        selectizeInput(inputId = NS(id,"facet"),label="Faceting variable",choices=list(),multiple=TRUE,options = list(maxItems = 2))
      )
    )
  } else if (color==TRUE & x==TRUE & y==TRUE & facet==TRUE) {
     tagList(
       shinyWidgets::dropdownButton(
         #tags$h3("List of Inputs"),
         inputId=NS(id,"mydropdown"),
         label="controls",
         circle=FALSE,
         icon = icon("gear"), 
         selectInput(inputId = NS(id,"choose_color"),label="Color",choices=list()),
         selectInput(inputId = NS(id,"choose_x"),label="X-axis variable",choices=list()),
         selectInput(inputId = NS(id,"choose_y"),label="y-axis variable",choices=list()),
         selectizeInput(inputId = NS(id,"facet"),label="Faceting variable",choices=list(),multiple=TRUE,options = list(maxItems = 2))
       )
     )
 } else if (color==TRUE & x==TRUE & y==TRUE & facet==FALSE) {
   tagList(
     shinyWidgets::dropdownButton(
       #tags$h3("List of Inputs"),
       inputId=NS(id,"mydropdown"),
       label="controls",
       circle=FALSE,
       icon = icon("gear"), 
       selectInput(inputId = NS(id,"choose_color"),label="Color",choices=list()),
       selectInput(inputId = NS(id,"choose_x"),label="X-axis variable",choices=list()),
       selectInput(inputId = NS(id,"choose_y"),label="y-axis variable",choices=list())
     )
   )
 } else if (color==TRUE & x==FALSE & y==FALSE & facet==FALSE) {
   tagList(
     shinyWidgets::dropdownButton(
       #tags$h3("List of Inputs"),
       inputId=NS(id,"mydropdown"),
       label="controls",
       circle=FALSE,
       icon = icon("gear"), 
       selectInput(inputId = NS(id,"choose_color"),label="Color",choices=list())
     )
   )
 } else if (color==FALSE & x==FALSE & y==FALSE & facet==TRUE) {
   tagList(
     shinyWidgets::dropdownButton(
       #tags$h3("List of Inputs"),
       inputId=NS(id,"mydropdown"),
       label="controls",
       circle=FALSE,
       icon = icon("gear"), 
       selectizeInput(inputId = NS(id,"facet"),label="Faceting variable",choices=list(),multiple=TRUE,options = list(maxItems = 2))
     )
   )
 } else {
     tagList(
       shinyWidgets::dropdownButton(
         #tags$h3("List of Inputs"),
         inputId=NS(id,"mydropdown"),
         label="controls",
         circle=FALSE,
         icon = icon("gear"), 
         selectInput(inputId = NS(id,"choose_color"),label="Color",choices=list()),
         selectInput(inputId = NS(id,"choose_x"),label="X-axis variable",choices=list()),
         selectizeInput(inputId = NS(id,"facet"),label="Faceting variable",choices=list(),multiple=TRUE,options = list(maxItems = 2))
       )
     )
   }
}

plotControlsServer <- function(id,df,ranks,samplevars) {
  moduleServer(id, function(input,output,session) {
      ns <- session$ns
     # plotControlsOutput <- reactiveValues(color=NULL,
    #                                       x=NULL,
    #                                       y=NULL,
    #                                       facet=NULL)

      observe({ 
        req(df)
        updateSelectInput(session,
                          "choose_color", #response=return column names as color choice options
                          choices=ranks)
        updateSelectInput(session,
                          "choose_x", #use column names as fill options
                          choices=samplevars)
        updateSelectInput(session,
                          "choose_y", #use column names as fill options
                          choices=samplevars)
        updateSelectizeInput(session,
                          "facet", #use column names as fill options
                          choices=samplevars)
      }) 
      
      faceting <- reactive({
        if (length(input$facet)==1) {
          facet_wrap(~.data[[input$facet]],scales="free_x") 
        } else if (length(input$facet)==2) {
          facet_grid(rows=vars(.data[[input$facet[1]]]),
                     cols= vars(.data[[input$facet[2]]]),
                     scales="free") 
        } else { NULL } 
      }) %>% bindEvent(input$facet,
                       ignoreNULL=FALSE)
      
      # observe({
      #   if (length(input$facet)==1) {
      #      #plotting() + 
      #        faceting <- facet_wrap(~.data[[input$facet]],scales="free_x") 
      #    } else if (length(input$facet)==2) {
      #      #plotting() + 
      #        faceting <- facet_grid(rows=vars(.data[[input$facet[1]]]),
      #                   cols= vars(.data[[input$facet[2]]]),
      #                   scales="free") 
      #    } else { faceting <- NULL } 
      #  }) %>% bindEvent(input$facet,
      #                   ignoreNULL=FALSE)
      
      #reactive({list(
      #  color = input$choose_color,
      #  x = input$choose_x,
      #  y = input$choose_y,
      #  facet = faceting()
      #)})
      
      #observe({
      #  rv$color = input$choose_color
      #  rv$x = input$choose_x
      #  rv$y = input$choose_y
      #  rv$facet = faceting()
      #})
      
    return(list(
      color = reactive(input$choose_color),
      x = reactive(input$choose_x),
      y = reactive(input$choose_y),
      facet = reactive(faceting())
      ))
      
    }
  )
}