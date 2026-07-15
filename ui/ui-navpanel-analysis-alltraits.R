
####################################### Trophic Level ################################################
Anlys_TraitIntroCard <- tagList(
                           h3("Trophic level analysis"),
                           p("Here you can view plots regarding patterns in mean trophic index of your data."),
                           br(),
                           card(h4("About Trophic Index"),
                                p(),
                                layout_columns(col_widths=c(8,4),
                                               p("Explanation of MTI and trophic level and how this can help inform fisheries management. Example phrasing includes: The marine trophic index measures the change in mean
trophic level of fisheries landings by region and globally. Trophic level is defined as the
position of an organism in the food chain, and ranges from a value of 1 for primary
producers up to a level of 5 for marine mammals and human"),
                                               tags$img(src='MTI.png',height='200px',width='200px',
                                                        alt="Fishing down the food web")
                                )
                           )
)

Anlys_TraitCard <- tagList(
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    accordion_panel(title="Violin plot",
                    Anlys_ViolinCard
    ),
    accordion_panel(title="Bar plot",
                    Anlys_TrophBarCard
    ),
    accordion_panel(title="Line plot",
                    Anlys_TrophLineCard
    ),
    accordion_panel(title="Lollipop plot",
                    Anlys_TrophLolliCard
    )
  )
)

####################################### Temperature ################################################
Anlys_TempCard <- tagList(
  h3("Temperature range analysis"),
  p("Here you can view plots regarding species temperature preference."),
  accordion(
    open=FALSE, #start with all panels closed
    multiple=TRUE, #multiple panels can be open at once
    #accordion_panel(title="Violin plot",
    #                Anlys_TempViolinCard
    #),
    accordion_panel(title="Lollipop plot",
                    Anlys_TempLolliCard
    )
  )
)

###############################################################################################
allanalysispage = nav_panel(
  useShinyjs(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("traitanalysis_prevpage","Previous step"),
    actionButton("traitanalysis_nextpage","Next step")
  ),
  h2("Mean Trophic Index and Trait analysis"),
  p("In this page, you can view plots for trait and ecological information."),
  br(),
  br(),
  
  # radar diagram 
  
  # trophic level
  card(
  Anlys_TraitIntroCard, 
  Anlys_TraitCard), 
  br(),
  
  # temperature 
  card(Anlys_TempCard),
  br(),
  
  # body size
  
  # Alpha diversity 
  #card(
    h5("Diversity"),
    HTML("<p>Here you can look at different ways of measuring biodiversity as a whole</p>"),
    navset_bar(fillable=TRUE,#full_screen=FALSE,
               fluid=TRUE,
      sidebar=sidebar(title="Original dataset overview",
                      position="right",
                      open="closed"
                      ), 
      nav_panel(id="testb",
                title="Sample Summary Table",
                Anlys_AlphaCard)
   #   )
    ), 
  
  br(),
  layout_column_wrap(
    width=1/2,fill=TRUE,
    actionButton("btraitanalysis_prevpage","Previous step"),
    actionButton("btraitanalysis_nextpage","Next step")
  )
)