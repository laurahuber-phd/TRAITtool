library(ggplot2)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(phyloseq)

plotControlsUI <- function(id,color=TRUE,x=TRUE,y=FALSE,facet=TRUE) {
#what will be in the dropdown menu
   if (color==TRUE & x==TRUE & y==TRUE & facet==TRUE) {
   controls <- c(
     selectInput(inputId = NS(id,"choose_color"),label="Color",choices=list()),
     selectInput(inputId = NS(id,"choose_x"),label="X-axis variable",choices=list()),
     selectInput(inputId = NS(id,"choose_y"),label="y-axis variable",choices=list()),
     selectizeInput(inputId = NS(id,"facet"),label="Faceting variable",choices=list(),multiple=TRUE,options = list(maxItems = 2))
     )
 } else if (color==TRUE & x==TRUE & y==TRUE & facet==FALSE) {
    controls <- c(
      selectInput(inputId = NS(id,"choose_color"),label="Color",choices=list()),
      selectInput(inputId = NS(id,"choose_x"),label="X-axis variable",choices=list()),
      selectInput(inputId = NS(id,"choose_y"),label="y-axis variable",choices=list())
    )
 } else if (color==TRUE & x==FALSE & y==FALSE & facet==FALSE) {
    controls <- c(
      selectInput(inputId = NS(id,"choose_color"),label="Color",choices=list())
    )
 } else if (color==FALSE & x==FALSE & y==FALSE & facet==TRUE) {
    controls <- c(
      selectizeInput(inputId = NS(id,"facet"),label="Faceting variable",choices=list(),multiple=TRUE,options = list(maxItems = 2))
    )
  }
  
  tagList(
    shinyWidgets::dropdownButton(
      #tags$h3("List of Inputs"),
      inputId=NS(id,"mydropdown"),
      label="controls",
      circle=FALSE,
      icon = icon("gear"), 
       controls
    )
  )
}
  
plotControlsServer <- function(id,df) {
  moduleServer(id, function(input,output,session) {
      ns <- session$ns

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

    }
  )
}