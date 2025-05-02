library(ggplot2)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(phyloseq)

plotUI <- function(id) {
  fluidRow(
    #tags$h2("Dropdown Button"),
    #br(),
    layout_columns(shinyWidgets::dropdownButton(
      #tags$h3("List of Inputs"),
      inputId=NS(id,"mydropdown"),
      label="controls",
      circle=FALSE,
      icon = icon("gear"), 
      selectInput(inputId = NS(id,"choose_color"),label="Color",choices=list()),
      selectInput(inputId = NS(id,"choose_x"),label="X-axis variable",choices=list()),
      selectizeInput(inputId = NS(id,"facet"),label="Faceting variable",choices=list(),multiple=TRUE,
                     options = list(maxItems = 2))
    ), #end of spinner
    shinyWidgets::dropdownButton(inputId = NS(id,"visbutton"),
                                 label="change plot size",
                                 circle=FALSE,
                                 icon=icon("expand"),
                                 numericInput(NS(id,"rankper_height"),
                                             label="Height of plot",
                                             value=5),
                                 
                                 numericInput(NS(id,"rankper_width"),
                                             label="Width of plot",
                                             value=10)),
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
                                             ), selected="bottom"),
                                 
                                 numericInput(NS(id,"textsize"),
                                             label="Text size",
                                             value=8),
                                 
                                 numericInput(NS(id,"pointsize"),
                                             label="Point size",
                                             value=5),
                                 
                                 numericInput(NS(id,"rankper_legend"),
                                              label="Number of legend columns",
                                              value=4))),
    actionButton(inputId = NS(id,"gobttn"),
                 label="Go"),
  withSpinner( plotOutput(NS(id,"plot"),click=NS(id,'click')) )
  )
}
  
plotServer <- function(id,df,type,ranks,samplevars) {
  moduleServer(id, function(input,output,session) {
      ns <- session$ns
#common theme
      newtheme <- reactive({switch(input$choose_theme,
                         "1"=theme_gray(),
                         "2"=theme_bw(),
                         "3"=theme_minimal(),
                         "4"=theme_classic())
        }) %>% bindEvent(input$choose_theme)
      
      chart_theme <- reactive({newtheme() + 
          theme(
            legend.position = input$pos_legend,
            legend.box.margin = margin(5,5,5,5),
            legend.spacing = unit(0,"pt"),
            plot.title = element_text(face="bold"),
            plot.caption = element_text(face="italic"),
            text = element_text(size=(input$textsize+5)),
            legend.key.height = unit(input$textsize,"pt"),
            legend.key.width = unit(input$textsize,"pt"),
            aspect.ratio=(input$rankper_height/input$rankper_width),
          ) 
        }) %>% bindEvent(input$pos_legnd,
                         input$textsize,
                         input$rankper_height,input$rankper_width,
                         newtheme()
                         )

      observe({ 
        req(df)
        updateSelectInput(session,
                          "choose_color", #response=return column names as color choice options
                          choices=ranks)
        updateSelectInput(session,
                          "choose_x", #use column names as fill options
                          choices=samplevars)
        updateSelectizeInput(session,
                          "facet", #use column names as fill options
                          choices=samplevars)
      }) 

      
plotting <- reactive({
  phyloseq::plot_bar(df,
                     x=input$choose_x,
                     fill=input$choose_color) + 
    geom_bar(stat='identity',position='stack') + 
    scale_color_viridis_d() + scale_fill_viridis_d()
}) %>% bindEvent(input$choose_color,
                 input$choose_x,
                 chart_theme())      


faceting <- reactive({
  if (length(input$facet)==1) {
    plotting() + facet_wrap(input$facet,scales="free_x") 
  } else if (length(input$facet)==2) {
    plotting() + facet_grid(rows=vars(.data[[input$facet[1]]]),cols= vars(.data[[input$facet[2]]]),scales="free") 
  } else { plotting() } 
}) %>% bindEvent(input$facet,
                 plotting(),
                 ignoreNULL=FALSE)
      
switchplot <- eventReactive(input$gobttn, {
  faceting() +
    guides(fill=guide_legend(ncol=input$rankper_legend)) + 
    chart_theme()
  }) 

output$plot <- renderPlot({switchplot()}) #%>% bindCache(input$choose_x,input$choose_y,chart_theme()) 


    }
  )
}