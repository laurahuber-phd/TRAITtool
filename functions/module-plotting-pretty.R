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
                                             ), selected="none"),
                                 
                                 numericInput(NS(id,"textsize"),
                                              label="Text size",
                                              value=8),

                                 numericInput(NS(id,"textdir"),
                                              label="Axis text rotation",
                                              value=90),
                                 
                                 textInput(NS(id,"plottitle"), 
                                           label="Plot Title:",
                                           value=NA),
                                 textInput(NS(id,"plotsubtitle"), 
                                           label="Plot Subtitle:",
                                           value=NA),
                                 textInput(NS(id,"plotcaption"), 
                                           label="Plot Caption:",
                                           value=NA),
                                 textInput(NS(id,"xaxistitle"), 
                                           label="X-axis Title:",
                                           value=NA),
                                 textInput(NS(id,"yaxistitle"), 
                                           label="Y-axis Title:",
                                           value=NA)
                                 
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
      
#labels  
      newlabs <- reactive({
        
        plot_title <- if (is.na(input$plottitle) || is.null(input$plottitle) || input$plottitle == "") { waiver() } else { input$plottitle }
        
        plot_subtitle <- if (is.na(input$plotsubtitle) ||is.null(input$plotsubtitle) || input$plotsubtitle == "") { waiver() } else { input$plotsubtitle}
        
        plot_caption <- if (is.na(input$plotcaption) ||is.null(input$plotcaption) || input$plotcaption == "") { waiver() } else { input$plotcaption}
        
        plot_x <- if (is.na(input$xaxistitle) ||is.null(input$xaxistitle) || input$xaxistitle == "") { waiver() } else { input$xaxistitle}
        
        plot_y <- if (is.na(input$yaxistitle) ||is.null(input$yaxistitle) || input$yaxistitle == "") { waiver() } else { input$yaxistitle}

        baselabs <- labs(
         title = plot_title,
         subtitle = plot_subtitle,
         caption = plot_caption, 
         x = plot_x,
         y = plot_y
        )
        
        baselabs
      }) %>% bindEvent(input$plottitle,
                       input$plotsubtitle,
                       input$plotcaption,
                       input$xaxistitle,
                       input$yaxistitle
                       )
#add updates
      chart_theme <- reactive({
        list(
        newtheme(),
          newlabs(),
          theme(
            legend.position = input$pos_legend,
            text = element_text(size=(input$textsize+5)),
            legend.key.height = unit(input$textsize,"pt"),
            legend.key.width = unit(input$textsize,"pt"),
            axis.text.x = element_text(angle=input$textdir,hjust=1)
          ) )
        }) %>% bindEvent(input$pos_legend,
                         input$textsize,
                         input$textdir,
                         newlabs(),
                         newtheme()
                         )

      return(reactive({chart_theme()}))
    }
  )
}