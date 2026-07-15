titlepage <- tagList(
  HTML("<h2 style='text-align:center;'>Welcome to TRAIT</h2>"),
  HTML("<h4 style='text-align:center;'><i>Trophic Rapid Assesssment and Integration Tool </i></h4>"),
  tags$figure(class="centerFigure",
      tags$img(src="MCL_Shiny.png", 
          width="100%",height="75%",
          alt="Picture of an island Belizean mangrove caye with the Marine Conservation Lab logo superseded on the left",
      #align = "center",
      #style='max-height:300px;width:auto;'
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
                 card(HTML("<h5>The main goal of this tool is to translate and synthesize taxonomic data into ecosystem structure information. </h5>"),
                      "The number of species in highly diverse ecosystems can complicate our understanding of the health and functioning of a system.",
                      "By looking at the ecological traits associated with each species in a system, this tool allows the user to infer ecosystem function",
                      "The final product of biodiversity monitoring is rarely intended to be a list of species, which often are difficult to interpret or do not get at the broader ecological environmental trends that are happening, but instead to get at the functional level.",
                      br(),
                      br(),
                      "By distilling taxonomic information into ecological traits, we are getting to what managers/etc are actually interested in when it comes to conservation: what is happening in the ecosystem, and are interventions having the intended effect?"),
              
                 layout_columns(col_widths=c(12,12),
                 card(HTML("<h3>Welcome!</h3>"),
                      HTML("<h5>What does this app do?</h5>"),
                      p("The goal of this app is to make the use of environmental DNA (eDNA) accessible to users on the ground working in conservation management. No prior experience with bioinformatics is necessary, and minimal prior data manipulation."),
                      br(),
                      HTML("<p>All you need to use this app is <u style='font-weight: bold'>DNA amplicon sequencing output</u> and a metadata file connecting the sequencing data to your samples. </p>"),
                      br(),
                      p("With this, you will get information regarding what species are in your samples, how the community composition and diversity differs between samples, and follow patterns in specific taxa."),
                      HTML("<h5>What does this app <u>NOT</u> do?</h5>"),
                      p("This app will not ... .")
                 ),
                 br(),
                 card(HTML("<h5>Example use cases and returns:</h5>"),
                      #some pretty graphs that you can generate with this app
                      layout_columns(col_widths=c(12,12,12),
                                     tags$figure(tags$img(src="violinplot.png", 
                                                          width="100%",
                                                          #alt="Picture of an island Belizean mangrove caye with the Marine Conservation Lab logo superseded on the left",
                                                          align = "left",
                                                          style='flex-grow:1; margin-top: 1px;margin-bottom: 1em;resize:both;overflow:none;'
                                     ),
                                     tags$figcaption(HTML("<p style='text-align:left'><small><i>Visualize patterns in trophic levels</i></small></p>"))
                                     ),
                                     tags$figure(tags$img(src="ord_plot.png", 
                                                          width="100%",
                                                          #alt="Picture of an island Belizean mangrove caye with the Marine Conservation Lab logo superseded on the left",
                                                          align = "right",
                                                          style='flex-grow:1; margin-top: 1px;margin-bottom: 1em;resize:both;overflow:none;'
                                     ),
                                     tags$figcaption(HTML("<p style='text-align:left'><small><i>Visualize how communities compare to one another</i></small></p>"))
                                     ),
                                     tags$figure(class="centerFigure",
                                                 tags$img(src="richness_plot.png", 
                                                          width="100%",
                                                          #alt="Picture of an island Belizean mangrove caye with the Marine Conservation Lab logo superseded on the left",
                                                          align = "center",
                                                          style='flex-grow:1; margin-top: 1px;margin-bottom: 1em;resize:both;overflow:none;'
                                                 ),
                                                 tags$figcaption(HTML("<p style='text-align:left'><small><i>Get records of richness at each site</i></small></p>"))
                                                 )
                                     )
                      )
                 )
                 
                 ),
  HTML("<p style='text-align:center;margin-bottom:1px'><i>Marine Conservation Lab: Smithsonian Environmental Research Center</i></p>"),
  br(),
tags$footer(style="display: flex;justify-content: space-between;",
            tags$img(src='smiths_logoLong.png',height='40px',
                     alt="Smithsonian sun logo"),
            HTML("<small><i>Developed with the generous support of our sponsors and collaborators</i></small>"),
            tags$img(#src='ShinyFooter.png',
                     src='titlefooter.png',
                     height='30px',
                     alt="William H. Donner Foundation and Smithsonian Women's Committee logos"))
)
