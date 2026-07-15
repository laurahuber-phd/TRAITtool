
aboutpanel <- tagList(
  h3("About the team"),
  h5("We are grateful to the teams who helped contribute to making this application possible"),
  #p("Fisheries Department"),
  #p("Forestry Department"),
  #p("TASA"),
  #p("CZMI"),
  #p("EB"),
  p("University of Belize Environmental Research Institute"),
  p("Marine Conservation Lab at the Smithsonian Environmental Research Center"),
  p("Marine Global Earth Observatory (MarineGEO), directed by the Smithsonian’s Tennenbaum Marine Observatories Network (TMON)"),
  p("OceanDNA at the Smithsonian National Museum of Natural History"),
  br(),
  h3("About our funders"),
  p("The development of this application was funded in part by the William H. Donner Foundation, OceanDNA, and the Smithsonian Women's Committee"),
  br(),
  br(),
  HTML("<p>For questions, comments, or feature requests, reach out to <i>xyz@si.edu</i> </p>"),
  br()
)

rmpanel <- tagList(
  h3("Definitions, Caveats, and Assumptions"),
  HTML("<p><u>Definitions:</u> Define phrases used in the tool </p>"),
  HTML("<p><u>Caveats:</u> What DOESN'T the tool do </p>"),
  HTML("<p><u>Assumptions:</u> What assumptions are made in the development and use of the tool, to make figures, etc </p>"),
  h3("References"),
  p("This tool was developed in RStudio using RShiny v.x.y.z"),
  p("IUCN data were made available with permission under the Red List Terms and Conditions of Use for non-commercial use"),
  h5("Packages"),
  HTML("<p>Special thanks to the curators of xyz packages, which were used in the development of this tool</p>")
)
