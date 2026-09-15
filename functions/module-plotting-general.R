library(ggplot2)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(phyloseq)

plotControlsUI <- function(id,color=TRUE,x=TRUE,y=FALSE,facet=TRUE,
                           colorlabel="Taxa category",
                           xlabel="x-axis variable",
                           ylabel="y-axis variable",
                           facetlabel="Faceting variable") {

#what will be in the dropdown menu

  if (color==TRUE & x==TRUE & y==FALSE & facet==TRUE) {
    tagList(
      shinyWidgets::dropdownButton(
        #tags$h3("List of Inputs"),
        inputId=NS(id,"mydropdown"),
        label="controls",
        circle=FALSE,
        icon = icon("gear"), 
        selectInput(inputId = NS(id,"choose_color"),label=colorlabel,choices=list()),
        selectInput(inputId = NS(id,"choose_x"),label=xlabel,choices=list()),
        selectizeInput(inputId = NS(id,"facet"),label=facetlabel,choices=list(),multiple=TRUE,options = list(maxItems = 2))
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

        selectInput(inputId = NS(id,"choose_color"),label=colorlabel,choices=list()),
        selectInput(inputId = NS(id,"choose_x"),label=xlabel,choices=list()),
        selectInput(inputId = NS(id,"choose_y"),label=ylabel,choices=list()),
        selectizeInput(inputId = NS(id,"facet"),label=facetlabel,choices=list(),multiple=TRUE,options = list(maxItems = 2))
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

        selectInput(inputId = NS(id,"choose_color"),label=colorlabel,choices=list()),
        selectInput(inputId = NS(id,"choose_x"),label=xlabel,choices=list()),
        selectInput(inputId = NS(id,"choose_y"),label=ylabel,choices=list())
      )
    )
  } else if (color==TRUE & x==TRUE & y==FALSE & facet==FALSE) {
    tagList(
      shinyWidgets::dropdownButton(
        #tags$h3("List of Inputs"),
        inputId=NS(id,"mydropdown"),
        label="controls",
        circle=FALSE,
        icon = icon("gear"), 

        selectInput(inputId = NS(id,"choose_color"),label=colorlabel,choices=list()),
        selectInput(inputId = NS(id,"choose_x"),label=xlabel,choices=list())
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

        selectInput(inputId = NS(id,"choose_color"),label=colorlabel,choices=list())
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
        selectizeInput(inputId = NS(id,"facet"),label=facetlabel,choices=list(),multiple=TRUE,options = list(maxItems = 2))
      )
    )

  } else if(color==FALSE & x==TRUE & y==TRUE & facet==TRUE) { 
    tagList(
      shinyWidgets::dropdownButton(
        #tags$h3("List of Inputs"),
        inputId=NS(id,"mydropdown"),
        label="controls",
        circle=FALSE,
        icon = icon("gear"), 
        selectInput(inputId = NS(id,"choose_x"),label=xlabel,choices=list()),
        selectInput(inputId = NS(id,"choose_y"),label=ylabel,choices=list()), 
        selectizeInput(inputId = NS(id,"facet"),label=facetlabel,choices=list(),multiple=TRUE,options = list(maxItems = 2))
      )
    )
  } else if(color==FALSE & x==TRUE & y==FALSE & facet==TRUE) { 
    tagList(
      shinyWidgets::dropdownButton(
        #tags$h3("List of Inputs"),
        inputId=NS(id,"mydropdown"),
        label="controls",
        circle=FALSE,
        icon = icon("gear"), 
        selectInput(inputId = NS(id,"choose_x"),label=xlabel,choices=list()),
        selectizeInput(inputId = NS(id,"facet"),label=facetlabel,choices=list(),multiple=TRUE,options = list(maxItems = 2))
      )
    )
  } else { #color==FALSE & x==TRUE & y==FALSE & facet==FALSE
    tagList(
      shinyWidgets::dropdownButton(
        #tags$h3("List of Inputs"),
        inputId=NS(id,"mydropdown"),
        label="controls",
        circle=FALSE,
        icon = icon("gear"), 
        selectInput(inputId = NS(id,"choose_x"),label=xlabel,choices=list())
      )
    )
  }
}


plotControlsServer <- function(id,df,var1=NULL,var2=NULL,var3=NULL,var4=NULL) {
  moduleServer(id, function(input,output,session) {
    ns <- session$ns
    # plotControlsOutput <- reactiveValues(color=NULL,
    #                                       x=NULL,
    #                                       y=NULL,
    #                                       facet=NULL)
    req(df)
    req(!is.null(df))
    
    observe({
      req(!is.null(var1))
      req(is.reactive(var1))
      updateSelectInput(session,
                        "choose_color", #response=return column names as color choice options
                        choices=var1())
    })
    observe({
      req(!is.null(var2))
      req(is.reactive(var2))
      updateSelectInput(session,
                        "choose_x", #use column names as fill options
                        choices=var2())
    })
    observe({
      req(!is.null(var3))
      req(is.reactive(var3))
      updateSelectInput(session,
                        "choose_y", #use column names as fill options
                        choices=var3())
    }) 
    observe({
      req(!is.null(var4))
      req(is.reactive(var4))
      updateSelectizeInput(session,
                           "facet", #use column names as fill options
                           choices=var4())
    }) 


    faceting <- reactiveVal(NULL)
    
    observeEvent(input$facet, {
      
        if (length(input$facet)==1) {
          faceting(facet_wrap(~.data[[input$facet]],scales="free_x")) 
        } else if (length(input$facet)==2) {
          faceting(facet_grid(rows=vars(.data[[input$facet[1]]]),
                     cols= vars(.data[[input$facet[2]]]),
                     scales="free")) 
        } else if (is.null(input$facet) || length(input$facet==0) || input$facet == "")  { 
          faceting(NULL) 
          } else {faceting(NULL)}

    },ignoreNULL = FALSE)

    return(list(
      color = reactive(input$choose_color),
      x = reactive(input$choose_x),
      y = reactive(input$choose_y),
      facet = reactive(faceting())

    ))
    
  }
  )
}
