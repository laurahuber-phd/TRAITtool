titlepage <- tagList(
  HTML("<h1 style='text-align:center;'>Environmental DNA management tool</h1>"),
  tags$figure(class="centerFigure",
      tags$img(src="Placeholder_Artwork_long.png", 
          width="100%",
          alt="Picture of an island Belizean mangrove caye with the Marine Conservation Lab logo superseded on the left",
      #align = "center",
      #style='flex-grow:1; margin-left: 20px;resize:both;overflow:none;'
      ),
      tags$figcaption(HTML("<p style='text-align:right'><small><i>Image of mangrove caye by Dr. Hannah Morrissette, 2024. Logo by ____ </i></small></p>"))),
  actionBttn(
    inputId = "titlepg_start",
    label = "Get started!", 
    style = "gradient",block=TRUE,
    icon=bs_icon("balloon-fill"),
    color = "success"
  ),
  br(),
  layout_columns(col_widths=c(4,8),
                 card(HTML("<h3>Welcome!</h3>"),
                      HTML("<h5>What does this app do?</h5>"),
                      "The goal of this app is to make the use of environmental DNA (eDNA) accessible to users on the ground working in conservation management. No prior experience with bioinformatics is necessary, and minimal prior data manipulation.",
                      br(),
                      br(),
                      "All you need to use this app is DNA amplicon sequencing output and a metadata file connecting the sequencing data to your samples. ",
                      br(),
                      br(),
                      "With this, you will get information regarding what species are in your samples, how the community composition and diversity differs between samples, and follow patterns in specific taxa."
                      ),
                 card("Example use case",
                      #some pretty graphs that you can generate with this app
                      )
                 ),
  HTML("<p style='text-align:center;margin-bottom:1px'><i>Marine Conservation Lab: Smithsonian Environmental Research Center</i></p>"),
tags$footer(style="display: flex;justify-content: space-between;",
            tags$img(src='smiths_logo.png',height='25px',
                          alt="Smithsonian sun logo"),
            HTML("<small><i>Developed with the generous support of the William H. Donner Foundation</i></small>"))
)