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
library(knitr)
library(markdown)
library(rmarkdown)
#
# source scripts --------
source("function-buildps.R")
source("function-upload.R")
source('ps_div.R')

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
source('ui-navpanel-report.R')
source('ui-navpanel-help.R')
source('ui-navpanel-settings.R')

# define UI --------  
ui <- page_navbar(id="pages",
                  title="",
                  theme=bs_theme(bootswatch='lux'),
                  tags$head(
                    tags$style(HTML("
                      [data-bs-theme=light] .btn {
                      background-color:rgba(var(--bs-body-color-rgb), 0.1);
                      color: #0c3e53;
                      border-width: 3px;
                      border-radius: 5rem;
                      border-color: rgba(var(--bs-body-color-rgb), 0.2);
                      font-weight: 1200;
                      }
                      [data-bs-theme=light] .btn:hover {
                      color: #0c3e53;
                      background-color: #ced4da;
                      border-color: #0c3e53;
                      }
                      [data-bs-theme=dark] .btn {
                      background-color: #919aa1;
                      color: #0c3e53;
                      border-color: transparent;
                      }
                      [data-bs-theme=dark] .btn:hover {
                      color: #0c3e53;
                      background-color: #adb5bd;
                      }
                      [data-bs-theme=dark] .card {
                      background-color: rgba(72,77,83);
                      }
                      .card-header {
                      background-color:rgba(var(--bs-body-color-rgb), 0.06);
                      }
                      [data-bs-theme=dark] .nav-tabs .nav-link {
                      color: var(--bs-secondary-text-emphasis);
                      border-width: 1;
                      border-color:var(--bs-secondary-bg);
                      background-color:#828a90;
                      }
                      [data-bs-theme=dark] .nav-tabs .nav-link:hover {
                      background-color: #656b70;
                      }
                      [data-bs-theme=dark] .nav-tabs .nav-link.active,
                      .nav-tabs .nav-item.show .nav-link {
                      color: var(--bs-nav-tabs-link-active-color);
                      background-color: var(--bs-nav-tabs-link-active-bg);
                      border-color: var(--bs-nav-tabs-link-active-border-color);
                      }
                    "))
                  ),
                  fillable=FALSE,
  sidebar=sidebar(id="sidebar_main",
                  title="Sidebar",
                  input_dark_mode(id='dark_mode',mode='light'),
                  open=FALSE,
    SelectFiltersCard,
    SelectNormCard,
    SelectSamplesCard
  ),
## Group 1 # Starting information  --------  
  nav_menu(title=div(class="title",
                     img(src="MCL_SERC.png", 
                         height=40, width=40, 
                         align = "left",
                         style="margin-bottom:5px; margin-left:1px;margin-top:1px;margin-right:1px;"
                         )),
           ),
## Group 2 # Upload and review files  --------  
nav_menu("Files", 
         nav_panel("Upload",value="uploadtab",
                   uploadpage),
         nav_panel("Review",value="reviewtab",
                   reviewpage)
         ),
## Group 3 # Process from raw  --------  
nav_menu("From Scratch",
         nav_panel("Demultiplex and Quality Control"#,qcpanel
         ),
         nav_panel("Assign OTU"#,assignasvpanel
         ),
         nav_panel("Assign Taxonomy"#,assigntaxapanel
         )
),
## Group 4 # Analysis --------  
nav_menu(title="Analysis",
         nav_panel("The Basics",value="basicstab",
                   descriptivepage),
         nav_panel("The Stats", value='statstab',
                   analysispage)
         ),
nav_spacer(),
## Group 5 # Report and help -------- 
nav_menu("Info",
         icon=bs_icon("info-square",size="1.2em"),
         nav_panel("Help",value='helptab',
                   helppage),
         nav_panel("Settings",value='settingstab',
                   settingspage)
         ),
nav_panel(title=bs_icon("printer-fill",size="2em"), value='reporttab',
          reportpage)
)
# define server logic -----
server <- function(input,output,session) {
# --- # Overall themes # --------
#observe(session$setCurrentTheme(
#  if(isTRUE(input$dark_mode)) dark else light
#))
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
## --- # Report Tab # --------  
source('server-navpanel-report.R',local=TRUE)
  
  #
  #
}
#
#
#
# run the app  --------
shinyApp(ui=ui,server=server)
