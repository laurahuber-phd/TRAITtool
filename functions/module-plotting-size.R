library(ggplot2)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(phyloseq)

plotSizeUI <- function(id) {
  tagList(
    shinyWidgets::dropdownButton(inputId = NS(id,"visbutton"),
                                 label="change plot size",
                                 circle=FALSE,
                                 icon=icon("expand"),
                                 numericInput(NS(id,"plot_height"),
                                             label="Height of plot",
                                             value=5),
                                 
                                 numericInput(NS(id,"plot_width"),
                                             label="Width of plot",
                                             value=10))
  )
}
  
plotSizeServer <- function(id) {
  moduleServer(id, function(input,output,session) {
      ns <- session$ns
      
      aspect_theme <- reactive({ 
          theme(
            aspect.ratio=(input$plot_height/input$plot_width)
          ) 
        }) %>% bindEvent(input$plot_height,input$plot_width)

return(list(
  aspectRatio=reactive(aspect_theme()),
  height=reactive(input$plot_height),
  width=reactive(input$plot_width)
)
  
)

    }
  )
}