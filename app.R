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
library(stringr)
library(shinyjs)
library(rfishbase)

#
# Specify the application port
options(shiny.host = "0.0.0.0")
options(shiny.port = 8180)
# source scripts --------
source('functions/function-buildps.R')
source('functions/function-upload.R')
source('functions/ps_div.R')

source('functions/module-readtable.R')
source('functions/module-plotting-pretty.R',local=TRUE)
source('functions/module-plotting-size.R',local=TRUE)
source('functions/module-plotting-general.R',local=TRUE)

source('ui/ui-aes-sidebar.R',local=TRUE)

source('ui/ui-navpanel-intro.R')
source('ui/ui-navpanel-assign.R')
source('ui/ui-navpanel-upload.R')
source('ui/ui-navpanel-review.R')
source('ui/ui-navpanel-filter.R')
source('ui/ui-defaultanalysis.R')
source('ui/ui-navpanel-trait.R')
source('ui/ui-navpanel-descriptivestats.R')
source('ui/ui-navpanel-analysis.R')
source('ui/ui-navpanel-report.R')
source('ui/ui-navpanel-help.R')
source('ui/ui-navpanel-settings.R')
source('ui/ui-navpanel-preprocessing.R')

# define UI --------  
ui <- page_navbar(id="pages",
                  title="",
                  useShinyjs(),
                  theme=bs_theme(bootswatch='lux'),
                  tags$head(
                    tags$style(HTML("
                    tr, td {
                    padding: 0px;
                    height: 0px;
                    border-spacing: -2px;
                    margin: 0px;
                    white-space: nowrap;
                    }
                    p {
                    margin: 0px;
                    }
                      [data-bs-theme=light] .accordion {
                      --bs-accordion-btn-color: #0c3e53;
                      --bs-accordion-border-width: 1.5px;
                      --bs-accordion-border-radius: 1rem;
                      --bs-accordion-border-color: rgba(var(--bs-body-color-rgb), 0.5)
                      }
                      [data-bs-theme=dark] .accordion {
                      color: #0c3e53;
                      border-width: 3px;
                      border-radius: 5rem;
                      border-color: rgba(var(--bs-body-color-rgb), 0.2);
                      }
                      [data-bs-theme=light] .btn {
                      background-color:rgba(var(--bs-body-color-rgb), 0.1);
                      color: #0c3e53;
                      border-width: 3px;
                      border-radius: 0.5rem;
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
                 layout_columns(col_widths = c(8,4),
                                "Light/Dark mode:",
                                input_dark_mode(id='dark_mode',mode='light')),
                  br(),
                  navigation,
                  open=FALSE
  ),
## Group 1 # Starting information  --------  
  nav_menu(title=div(class="title",
                     img(src="MCL_SERC.png", 
                         height=40, width=40, 
                         align = "left",
                         style="margin-bottom:5px; margin-left:1px;margin-top:1px;margin-right:1px;"
                         )),
           nav_panel("Introduction",value='introtab',
                     titlepage)
           ),
## Group 2 # Upload and review files  --------  
nav_menu("Files", 
         nav_panel("Upload",value="uploadtab",
                   uploadpage),
         nav_panel("Review",value="reviewtab",
                   reviewpage)
         ),
## Group 3 # Process from raw  --------  
nav_menu("Trait Data", 
  nav_panel("Assign ecological data",value='traittab',
            traitpage)
),
## Group 4 # Analysis --------  
nav_menu("Filter", #filtering options 
         nav_panel("Filter",value='filtertab',
                   #defaultanalysis,
                   filterpage)#,
         #nav_panel("Default",value='defaulttab',
         #          defaultanalysis)
),
nav_menu(title="Analysis",
         nav_panel("At a Glance",value="basicstab",
                   descriptivepage),
         nav_panel("Figures", value='statstab',
                   analysispage)
         ),
nav_spacer(),
## Group 5 # Report and help -------- 
nav_menu("Info",
         icon=bs_icon("info-square",size="1.2em"),
         nav_panel("Help",value='helptab',
                   helppage),
         nav_panel("Settings",value='settingstab',
                   settingspage),
         nav_panel("Demultiplex and Quality Control",value="infodemux",
                   qcpanel
         ),
         nav_panel("Build Sequence Table",value="infoassignseq",
                   assignasvpanel
         ),
         nav_panel("Assign Taxonomy",value="infoassigntax",
                   assigntaxapanel
         )
         ),
nav_panel(title=bs_icon("printer-fill",size="2em"), value='reporttab',
          reportpage)
)
# define server logic -----
server <- function(input,output,session) {
  source('server/server-globalvars.R',local=TRUE)
  source('server/server-navpanel-settings.R',local=TRUE)
  #source('server/server-navpanel-preprocessing.R',local=TRUE)
# --- # Overall themes # --------
#observe(session$setCurrentTheme(
#  if(isTRUE(input$dark_mode)) dark else light
#))
source('server/server-globalvars.R',local=TRUE) #global variables
source('server/server-aes-vis.R',local=TRUE) #base theme for ggplot2
# --- # Page Navigation # --------
source('server/server-aes-navigation.R',local=TRUE)
# --- # Load Data # --------
source('server/server-aes-load-data.R',local=TRUE) #loads the functionality to read in the files and go through any manipulations to normalize or subset the ps object
# --- # Upload Page # --------
source('server/server-navpanel-upload.R',local=TRUE)
# --- # Review Page # --------
source('server/server-navpanel-review.R',local=TRUE)
# --- # Filter Page # --------
source('server/server-navpanel-filter.R',local=TRUE)  
  source('server/server-defaultanalysis.R',local=TRUE)
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
source('server/server-navpanel-trait.R',local=TRUE)
# --- # Descriptive Stats Page  # --------  
 source('server/server-navpanel-descriptivestats.R',local=TRUE)
## --- # Analysis Page # --------  

source('server/server-navpanel-analysis-alpha.R',local=TRUE)
source('server/server-navpanel-analysis-ord.R',local=TRUE)
source('server/server-navpanel-analysis-hm.R',local=TRUE)
source('server/server-navpanel-analysis-violin.R',local=TRUE)
## --- # Report Tab # --------  
source('server/server-navpanel-report.R',local=TRUE)
  
  #
  #
}
#
#
#
# run the app  --------
shinyApp(ui=ui,server=server)
