source('ui/ui-navpanel-assigntrait.R')

source('ui/ui-navpanel-analysis.R')
source('ui/ui-navpanel-analysis-trait.R')
source('ui/ui-navpanel-descriptivestats.R')

############################################# Summarize Trophic #############################################
Card_SumTrophic <- tagList(
  br(),
  p("This gathers everything you will need to build a picture of the trophic distribution of the organisms identified in your dataset. This information can help elucidate the health of the ecosystem and fishery sustainability. Overfishing often results in shortening of the food chain, reducing the number of high trophic level organisms and resulting in a proportionally higher number of lower trophic level taxa."),
  br(),
  h5("First, if you have not already, use the following to assign natural history information to your data"),
  Trait_TrophPullCard,
  Trait_TrophReviewCard,
  br(),
  h5("Next, we will build a graphical depiction of the proportions of levels of consumers in your dataset"),
  card(Anlys_ViolinCard),
  br(),
  h5("Following that, we have a few more plots that will delve deeper into some trends in the trophic level data"),
  card(Anlys_TrophBarCard),
  card(Anlys_TrophLineCard),
  card(Anlys_TrophLolliCard)
                        )

############################################# Summarize Community #############################################
Card_SumCommunity <- tagList(
  br(),
  p("This develops a general overview of the community structure of organisms identified in your dataset. This includes information on species richness and evenness, which is a reflection of the biodiversity of the system. Here you can also visualize what taxa are in the data and how that differs by sample and group."),
  br(),
  #h5(""),
  #br(),
  card(Anlys_AlphaCard),
  card(Anlys_HistoCard),
  card(Anlys_BetaCard),
  card(Anlys_hmCard)
                          )

############################################# Summarize Management #############################################
Card_SumManage <- tagList(
  br(),
  h5("In progress"),
  br(),
  "Check back later."
)


##########################################################################################
choicepage <- nav_panel("Choose",
                        shinyFeedback::useShinyFeedback(),
                        open=TRUE,
                        
                        # page navigation buttons 
                        layout_column_wrap( 
                          width=1/2,fill=TRUE,
                          actionButton("summarychoice_prevpage","Previous page",width='100%'),
                          actionButton("summarychoice_nextpage_v2","Next step",width='100%')
                        ),
                        br(),
                        
                        # page info  
                        radioGroupButtons(inputId = "selectsummary", 
                                          #width='100%', 
                                          individual=TRUE,
                                          direction = 'horizontal',
                                          size="normal",
                                          justified=TRUE,
                                          choiceNames = c(HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i><u>I am interested in the food webs of my data:</i></p></u> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>Summarize the trophic structure of my data.</p>"),
                                                          HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i><u>I am interested in the health of the community:</i></p></u> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>Summarize the community metrics of my data.</p>"), 
                                                          HTML("<p style='margin-bottom:0;font-size:1.5vw;'><i><u>I am interested in fisheries and fishery health:</i></p></u> <p style='padding-top:0; margin-bottom: 0;font-size:1.5vw;'>Summarize information of management relevance.</p>")
                                          ),
                                          choiceValues = c("choose_sumtrophic",
                                                           "choose_sumcomm",
                                                           "choose_summanage"),
                                          selected=NULL
                        ), 
                          conditionalPanel(condition="input.selectsummary == 'choose_sumtrophic'",
                                           Card_SumTrophic
                          ),
                          conditionalPanel(condition="input.selectsummary == 'choose_sumcomm'",
                                           Card_SumCommunity
                          ),
                          conditionalPanel(condition="input.selectsummary == 'choose_summanage' ",
                                           Card_SumManage
                          ),
                        br(),
                        br(),
                        # bottom page navigation buttons
                        layout_column_wrap(
                          width=1/2,fill=TRUE,
                          actionButton("bsummarychoice_prevpage","Previous page",width='100%'),
                          actionButton("bsummarychoice_nextpage_v2","Next step",width='100%')
                        ),
                        
                        # footer
                        tags$footer(style="display: flex;justify-content: space-between;",
                                    tags$img(src='smiths_logoLong.png',height='40px',
                                             alt="Smithsonian sun logo"),
                                    tags$img(#src='ShinyFooter.png',
                                      src='titlefooter.png',
                                      height='30px',
                                      alt="William H. Donner Foundation and Smithsonian Women's Committee logos")))