options(shiny.maxRequestSize=10*1024^10)

library(shiny)
library(bslib)
library(bsicons)
library(ggplot2)
library(phyloseq)
library(vegan)
library(htmltools)
library(dplyr)
library(shinycssloaders)
library(shinyWidgets)
library(DT)
library(vroom)
library(shinyFeedback)
library(shinyBS)
library(shinyjqui)
library(colourpicker)
#
# source scripts --------
source("function-buildps.R")
source("function-upload.R")

source("module-readtable.R")
source('module-plotting.R')
source('module-plotting-pretty.R',local=TRUE)
source('module-plotting-size.R',local=TRUE)
source('module-plotting-general.R',local=TRUE)

source('ui-aes-sidebar.R',local=TRUE)

source('ui-navpanel-assign.R')
source('ui-navpanel-upload.R')
source('ui-navpanel-review.R')
source('ui-navpanel-descriptivestats.R')
source('ui-navpanel-analysis.R')
source('ui-navpanel-help.R')

# define UI --------  
ui <- page_navbar(id="pages",
                  title="Title",
                  theme=light,
                  fillable=FALSE,
  sidebar=sidebar(id="sidebar_main",
                  title="Sidebar",
                  open=FALSE,
    materialSwitch('dark_mode',"Dark mode",
                   right=TRUE),
    SelectFiltersCard,
    SelectNormCard,
    SelectSamplesCard
  ),
## Panel 1 # File upload  --------  
  nav_panel("Files",
            uploadpage),
## Panel 2 # Review files  --------  
  nav_panel("Review",
            reviewpage
            ),
## Hidden panels # analysis from raw --------  
nav_panel_hidden("Demultiplex and Quality Control",qcpanel),
nav_panel_hidden("Assign OTU",assignasvpanel),
nav_panel_hidden("Assign Taxonomy",assigntaxapanel),
## Panel # Basic Stats  --------  
  nav_panel("Basic Stats",
            descriptivepage),
## Panel # Analysis  --------  
  nav_panel("Analysis", 
            analysispage ),
nav_panel("Help",
          helppage)
)
# define server logic -----
server <- function(input,output,session) {
# --- # Overall themes # --------
observe(session$setCurrentTheme(
  if(isTRUE(input$dark_mode)) dark else light
))
source('server-aes-vis.R',local=TRUE) #base theme for ggplot2
# --- # Page Navigation # --------
source('server-aes-navigation.R',local=TRUE)
# --- # Load Data # --------
source('server-aes-load-data.R',local=TRUE) #loads the functionality to read in the files and go through any manipulations to normalize or subset the ps object
# --- # Sidebar # --------
  observe({
    sidebar_toggle(
      id="sidebar_main",
      open=FALSE #sidebar with selection to remove blanks and unassigned sequences is hidden by default
    )
    if(input$pages == "Basic Stats" || input$pages == "Analysis") { #blank/unassigned selection sidebar is open for these two pages
      sidebar_toggle(
        id="sidebar_main",
        open = TRUE
        ) 
      if (!isTruthy(MetadataTable$x) || !isTruthy(OTUTable$x) || !isTruthy(TaxTable$x)) {
        showModal(
          modalDialog(title="You do not have all the appropriate files for this page, go back to the beginning and check files",
                      paste("The issue is: ", isgood()),
                      size='l',
                      actionButton("psfail_goback","Go to review your files",width='100%'),
                      easyClose = FALSE
          )
        ) 
      } else if (!is.null(ps.all())) {removeModal()} 
    } else {
        sidebar_toggle(
          id="sidebar_main",
          open = FALSE
        )
      }
  },priority=1) %>% bindEvent(input$pages)
# --- # Upload Page # --------
source('server-navpanel-upload.R',local=TRUE)
# --- # Review Page # --------
source('server-navpanel-review.R',local=TRUE)
  # --- # Page 2 # --------
  output$selected_Quality <- renderText({ 
    paste('Selected stringency: ',input$Quality)  # return the values selected from the checkboxes
    }) 
  output$selected_Location <- renderText({ 
    paste('Selected location: ',input$Location)  # return the values selected from the checkboxes
  }) 
  output$selected_HabitatType <- renderText({ 
    paste('Selected habitat type: ',input$HabitatType)  # return the values selected from the checkboxes
  }) 
# --- # Descriptive Stats Page  # --------  
 source("server-navpanel-descriptivestats.R",local=TRUE)
## --- # Richness Tab # --------  
source('server-navpanel-analysis-alpha.R',local=TRUE)
## --- # Distance Matrices Tab # --------  
source('server-navpanel-analysis-ord.R',local=TRUE)
  #
  #
}
#
#
#
# run the app  --------
shinyApp(ui=ui,server=server)
