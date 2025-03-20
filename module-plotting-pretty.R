library(ggplot2)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(phyloseq)

plotPrettyUI <- function(id) {
  tagList(
    shinyWidgets::dropdownButton(inputId=NS(id,"pretty"),
                                 label="Aesthetic changes",
                                 circle=FALSE,
                                 icon=icon("paintbrush"),
                                 selectInput(NS(id,"choose_theme"),
                                             "Select plotting theme",
                                             choices=list(
                                               "Default"="1",
                                               "B/W"="2",
                                               "Minimal"="3",
                                               "Classic"="4"
                                             ), selected="4"),
                                 
                                 selectInput(NS(id,"pos_legend"),
                                             "Legend position",
                                             choices=list(
                                               "bottom","top",
                                               "right", "left",
                                               "none"
                                             ), selected="right"),
                                 
                                 numericInput(NS(id,"textsize"),
                                              label="Text size",
                                              value=8),

                                 numericInput(NS(id,"textdir"),
                                              label="Axis text rotation",
                                              value=90)#,
                                 
                                 #numericInput(NS(id,"pointsize"),
                                 #             label="Point size",
                                 #             value=5),
                                 
                                 #numericInput(NS(id,"rankper_legend"),
                                 #            label="Number of legend columns",
                                 #             value=4)
    )
  )
}

plotPrettyServer <- function(id) {
  moduleServer(id, function(input,output,session) {
      ns <- session$ns
#common theme
      newtheme <- reactive({switch(input$choose_theme,
                         "1"=theme_gray(),
                         "2"=theme_bw(),
                         "3"=theme_minimal(),
                         "4"=theme_classic())
        }) %>% bindEvent(input$choose_theme)
      
#add updates
      chart_theme <- reactive({
        newtheme() +
          theme(
            legend.position = input$pos_legend,
            text = element_text(size=(input$textsize+5)),
            legend.key.height = unit(input$textsize,"pt"),
            legend.key.width = unit(input$textsize,"pt"),
            axis.text.x = element_text(angle=input$textdir,hjust=0)
          ) 
        }) %>% bindEvent(input$pos_legend,
                         input$textsize,
                         input$textdir,
                         newtheme()
                         )

      return(reactive({chart_theme()}))
    }
  )
}