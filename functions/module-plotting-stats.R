library(ggplot2)
library(shiny)
library(shinyWidgets)
library(RColorBrewer)
library(ggpubr)

plotStatUI <- function(id,provideChoices=FALSE) {
  
  if(provideChoices==FALSE) {
    tagList(
      shinyWidgets::dropdownButton(inputId = NS(id,"statbutton"),
                                   label="Statistics",
                                   circle=FALSE,
                                   icon=icon("chart-simple"),
                                   checkboxInput(inputId = NS(id,"include_stat"),
                                                      label="Include statistics?", 
                                                 value=FALSE
                                                 ),
                                   checkboxInput(inputId = NS(id,"include_NS"), 
                                                 label="Include non-significant?", 
                                                 value = FALSE) 
      )
    )
  } else {
  tagList(
    shinyWidgets::dropdownButton(inputId = NS(id,"statbutton"),
                                 label="Statistics",
                                 circle=FALSE,
                                 icon=icon("chart-simple"),
                                 checkboxGroupInput(inputId = NS(id,"include_stat"),
                                                    label="Include statistics?", 
                                                    choiceNames=c("None",
                                                              "Default",
                                                              "Wilcox",
                                                              "T-test"
                                                              ),
                                                    choiceValues=c(FALSE,
                                                                   "wilcox_test",
                                                                   "wilcox_test",
                                                                   "t_test"),
                                                    selected = "None"),
                                 checkboxInput(inputId = NS(id,"include_NS"), 
                                               label="Include non-significant?", 
                                               value = FALSE) 
                                 )
  )
    }
}

plotStatServer <- function(id) {
  moduleServer(id, function(input,output,session) {
    ns <- session$ns
    
    IncludeStat <- reactiveVal(NULL)
    
    observeEvent({input$include_stat;input$include_NS},{
      req(!is.null(input$include_stat))
      req(!is.null(input$include_NS))
      
      print(c("Stats included?",input$include_stat))
      print(c("NS included?",input$include_NS))
      
      stat <- input$include_stat #can be FALSE, TRUE, or name a statistical test
      ns <- input$include_NS #can be FALSE or TRUE
      hide_ns_val <- !ns
      #combinations include 
      #ns=FALSE, stat=FALSE -> no geom_pwc 
      #ns=TRUE, stat=FALSE -> stat=wilcox, ns=TRUE
      #ns=FALSE & stat=TRUE -> stat=wilcox, ns=FALSE
      #ns=TRUE & stat=TRUE -> stat=wilcox, ns=TRUE
      #ns=TRUE & stat="x" -> stat="x",ns=TRUE
      
      if(stat==FALSE && hide_ns_val==FALSE) {  #option 1
        print("No statistics requested")
        IncludeStat(NULL)
        } else if(stat==FALSE && hide_ns_val==TRUE) { #option 2
        print("user requested to report non-significant statistics but did not check box to include stats")
        IncludeStat(ggpubr::geom_pwc(method = "wilcox_test",
                                     hide.ns = hide_ns_val)
                    )
        #updateCheckboxInput(session, 
        #                    "include_stat",
        #                    value=TRUE)
      } else if(stat==TRUE) {
        print("User requested stats")
        IncludeStat(ggpubr::geom_pwc(method = "wilcox_test",
                                     hide.ns = hide_ns_val)
        )
      } else { #if(hide_ns_val==TRUE && stat!=FALSE) 
        print("user requested specific statistical test")
        IncludeStat(ggpubr::geom_pwc(method = stat,
                                     hide.ns = hide_ns_val)
        )
      } #else if(hide_ns_val!=TRUE && stat!=FALSE) {
        #IncludeStat(ggpubr::geom_pwc(method = stat,
        #                             hide.ns = hide_ns_val)
        #)
      #} 
    },ignoreNULL=FALSE)
    
    return(reactive(IncludeStat()))
    
  }
  )
}