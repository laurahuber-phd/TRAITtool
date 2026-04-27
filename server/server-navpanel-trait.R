source("functions/module-readtable.R",local=TRUE)

tableServer(id='tax_table2',
            data=TaxTable 
) 

traits_var <- reactiveValues(x=NULL, #final data frame 
                             serv=NULL, #database to query 
                             choose=NULL, #list of species to query 
                             nona=NULL, #list of species to query without na values 
                             
                             syn.fish.tab=NULL, #rfishbase::synonyms() output for fish
                             syn.sl.tab=NULL, #rfishbase::synonyms() output for non-fish
                             syn.tab=NULL, #synonym table showing queried values with valid names
                            
                             fish.troph=NULL, #rfishbase::ecology output for fish
                             sl.troph=NULL, #rfishbase::ecology output for non-fish
                             troph=NULL, # rfishbase::ecology output
                             
                             fbdf=NULL, #combined rfishbase::ecosystem() and species() dataframes
                             traits.df=NULL, #combined ecology, ecosystem, and species dataframe
                             alltraits.df=NULL, #subsetted ecology, ecosystem, and speciesdataframe
                             taxa_traits_df=NULL, #combined fish/nonfish df
                             taxa_traits_sub=NULL, #final df used for ps
                             
                             valid.fish.names=NULL, #validated fishbase names
                             invalid.fish.names=NULL, #invalid fishbase names
                             sl.check=NULL, #all non-chordate spp names and invalid names for fishbase 
                             valid.sl.names=NULL, #validated sealifebase names
                             invalid.sl.names=NULL, #invalid names in sealifebase 
                             
                             valid=NULL, #all validated names 
                             invalid=NULL, #invalid names not queried
                             syn.names=NULL, #synonym names identified
                             
                             fish.eco=NULL, #rfishbase::ecology() output for fishbase
                             sl.eco=NULL, #rfishbase::ecology() output for sealifebase
                             eco.df=NULL, #final rfishbase::ecology() output
                             
                             fish.spp=NULL, #rfishbase::species() output for fishbase
                             sl.spp=NULL, #rfishbase::species() output for sealifebase
                             spp.df=NULL, #final rfishbase::ecology() output 
                             
                             eco.sub=NULL, #subsetted ecology df
                             spp.sub=NULL #subsetted species df 
)

traits_tab <- reactiveValues(x=NULL)

observeEvent(input$trait_select, {
  req(TaxTable$x)
  
  shinyjs::hide(id="trait_trophreview_card")
  shinyjs::hide(id="p_fishquery")
  
  traits_var$serv <- switch(input$trait_select,
         "Both" = c("fishbase","sealifebase"),
         "FishBase" = "fishbase",
         "SeaLifeBase" = "sealifebase",
         )
})

observeEvent(input$trait_assign, {
  
  ################## reset values when assign button is clicked  ################## 
  req(TaxTable$x)
  taxa_df <- TaxTable$x
  
 # shinyjs::show(id="viewtrait_panel")
  shinyjs::show(id="trait_trophreview_card")
  shinyjs::show(id="loading_traits")
  
  # reset all values to NULL
  traits_var$x <- traits_var$choose <- traits_var$nona <- traits_var$syn.fish.tab <- traits_var$syn.sl.tab <- traits_var$syn.tab <- traits_var$fish.troph <- traits_var$sl.troph <- traits_var$troph <- traits_var$fbdf <- traits_var$traits.df <- traits_var$alltraits.df <- traits_var$taxa_traits_df <- traits_var$taxa_traits_sub <- traits_var$valid.fish.names <- traits_var$invalid.fish.names <- traits_var$sl.check <- traits_var$valid.sl.names <- traits_var$invalid.sl.names <- traits_var$valid <- traits_var$invalid <- traits_var$syn.names <- traits_var$fish.eco <- traits_var$sl.eco <- traits_var$eco.df <- traits_var$fish.spp <- traits_var$sl.spp <- traits_var$spp.df <- traits_var$eco.sub <- traits_var$spp.sub <- NULL
  
  traits_tab$x <- NULL

  # get a list of species to query based on the selected database 
  traits_var$choose <- switch(input$trait_select, 
                             "Both" = c(taxa_df %>% 
                                          filter(!is.na(Species)) %>%
                                          .$Species %>% unique()),
                             "FishBase" = c(taxa_df %>%
                                              filter(!is.na(Species) & str_detect(Phylum,"hord")) %>% 
                                              .$Species %>% unique()),
                             "SeaLifeBase" = c(taxa_df %>%
                                                 filter(!is.na(Species) & !str_detect(Class,"ctinop")) %>% 
                                                 .$Species %>% unique())
  )
  
  # check that the number of species to query is greater than 0
  # if so, assign the species to a list and remove NA species assignments
  if(length(traits_var$choose[!is.na(traits_var$choose)]) > 0) { 
    traits_var$nona <- traits_var$choose[!is.na(traits_var$choose)] 
    
    # show the panel listing number of species queried
    shinyjs::show(id="p_fishquery")
    output$n_fishquery <-  renderText({paste("Number of species queried: ",length(traits_var$nona))})
    
  } else {
    showModal(modalDialog("No species are assigned in this table. Check you are using the correct dataframe on the review tab."))
  }
  
  # only proceed if traits_var$nona is made 
  req(traits_var$nona)
  #
  ##
  ###
  ####
  ##### At this point the species list is queried against one or both of the databases ##### 
  ##### This is based on user selection with input$trait_select #####
  ####
  ###
  ##
  #
  ################## When both db is selected  ################## 
  if(input$trait_select=="Both") {
    
    ##### First, check with FishBase ##### 
    # Compare species list to database and retain only valid names in the database (traits_var$valid.fish.names)
    # Invalid names are assigned to a new value (traits_var$invalid.fish.names)
    # Invalid names are then checked for synonym names (var: syn.fish)
    # Valid synonyms are written to a new variable (var: syn.fish.names)
    # Valid synonyms are added to the valid names list (traits_var$valid.fish.names) 
    # Invalid names with valid synonyms are removed from the invalid names list (traits_var$invalid.fish.names)
    # Table to report synonyms is recorded (traits_var$syn.fish.tab)
    # Valid names are then queried for trophic information against FishBase (var: fish.troph)
    
    ##### Then, query the invalid names against SeaLifeBase ##### 
    # Combine invalid names and names without info in FishBase into a single list (traits_var$sl.check)
    # Compare species list to database and retain only valid names in the database (traits_var$valid.sl.names)
    # Invalid names are assigned to a new value (traits_var$invalid.sl.names)
    # Invalid names are then checked for synonym names (var: syn.sl)
    # Valid synonyms are written to a new variable (var: syn.sl.names)
    # Valid synonyms are added to the valid names list (traits_var$valid.sl.names)
    # Invalid names with valid synonyms are removed from the invalid names list (traits_var$invalid.sl.names)
    # Table to report synonyms is recorded (traits_var$syn.sl.tab)
    # Valid names are then queried for trophic information against FishBase (traits_var$sl.troph)
    
    # All trophic tables are combined (traits_var$troph)
    
    # check if tax names are valid/found in the databases and remove NA values from species #
    valid.fish.names <- traits_var$nona[!is.na(traits_var$nona)] %>% 
      rfishbase::validate_names(server="fishbase") 
    
    # make list of species that did not pass 
    invalid.fish.names <- traits_var$nona[!traits_var$nona%in%valid.fish.names & !is.na(traits_var$nona)] 
    
    # Check the species names that were not found in fishbase for synonyms
    syn.fish <- rfishbase::synonyms(invalid.fish.names,
                                    server="fishbase") %>% 
      filter(Status=="synonym")
    
    syn.fish.names <- syn.fish[!is.na(syn.fish$Species),]$Species
    
    syn.fish.tab <- syn.fish %>% 
      select(c(Species,synonym)) %>% 
      rename("Valid Name" = "Species", 
             "Queried Name" = "synonym" )
    
    # add synonyms to valid list and remove the queried names from invalid list 
    valid.fish.names <- c(valid.fish.names[!valid.fish.names%in%syn.fish.names], 
                            syn.fish.names) 
    invalid.fish.names <- invalid.fish.names[!invalid.fish.names%in%syn.fish$synonym]
    
    #### get trophic info using valid list ### 
    fish.troph <- rfishbase::estimate(species_list=valid.fish.names,
                                      server="fishbase")
    
    ##### SeaLifeBase ##### 

    # check if tax names are valid/found in the databases and remove NA values from species #
    sl.check <- c(valid.fish.names[!valid.fish.names%in%fish.troph$Species],
                  invalid.fish.names)
    
    valid.sl.names <- sl.check %>% 
      rfishbase::validate_names(server="sealifebase") 
    
    # make list of species that did not pass 
    invalid.sl.names <- sl.check[!sl.check%in%valid.sl.names]
    
    # Check the species names that were not found in fishbase for synonyms
    syn.sl <- rfishbase::synonyms(invalid.sl.names,
                                  server="sealifebase") %>% 
      filter(Status=="synonym")
    
    syn.sl.names <- syn.sl[!is.na(syn.sl$Species),]$Species
    syn.sl.tab <- syn.sl %>% 
      select(c(Species,synonym)) %>% 
      rename("Valid Name" = "Species", 
             "Queried Name" = "synonym" )
    
    # add synonyms to valid list and remove the queried names from invalid list 
    valid.sl.names <- c(valid.sl.names[!valid.sl.names%in%syn.sl.names], 
                            syn.sl.names) 
    
    invalid.sl.names <- c(invalid.sl.names,
                                 invalid.sl.names[!invalid.sl.names%in%syn.sl$synonym])
    
    #### get trophic info ### 
    sl.troph <- rfishbase::estimate(species_list=valid.sl.names,
                                      server="sealifebase")
    
    
    ################### combine the two df ##################
    # standardize the column names  
    colnames(fish.troph) <- str_to_title(colnames(fish.troph))
    colnames(sl.troph) <- str_to_title(colnames(sl.troph))
    
    # combine the two dataframes with bind_rows, because that will fill any missing values with NA  
    # include 'db' as an ID column so we can tell later what database the results came from  
    traits.df <- bind_rows(fish.troph, 
                           sl.troph,
                           .id="db") %>% 
      filter(!duplicated(Species)) %>%
      mutate(Syn=case_when(Species%in%syn.fish$Status ~ "synonym"))
    
    
    #### Keep some vars #### 
    traits_var$valid.fish.names <- valid.fish.names
    traits_var$invalid.fish.names <- invalid.fish.names
    traits_var$syn.fish.tab <- syn.fish.tab
    traits_var$fish.troph <- fish.troph
    
    traits_var$valid.sl.names <- valid.sl.names
    traits_var$invalid.sl.names <- invalid.sl.names 
    traits_var$syn.sl.tab <- syn.sl.tab
    traits_var$sl.troph <- sl.troph
    
    traits_var$valid <- c(valid.fish.names,valid.sl.names) %>% unique()
    traits_var$invalid <- unique(invalid.sl.names) #because this value encompasses anything that wasn't found in fishbase, I don't need to include both invalids
    traits_var$syn.tab <- bind_rows(syn.fish.tab, 
                                     syn.sl.tab,
                                     .id="db") #%>% filter(!duplicated(Species)) 
    traits_var$syn.names <- c(syn.fish.names,syn.sl.names) %>% unique()
    
    traits_var$troph <- (traits.df %>%
                            tibble::rownames_to_column(var="rownames")
                          ) 
    
    ################## Add other ecosystem data  ################## 
    
    # get table of location/ecosystem preferences 
    fish.eco <- rfishbase::ecosystem(species_list=valid.fish.names,
                                     server="fishbase") 
    sl.eco <- rfishbase::ecosystem(species_list=valid.sl.names,
                                   server="sealifebase") 
    
    eco.df <- bind_rows(fish.eco,sl.eco,.id="db") 
    
    # get species table information
    fish.spp <- rfishbase::species(species_list=valid.fish.names,
                                   server="fishbase") 
    sl.spp <- rfishbase::species(species_list=valid.sl.names,
                                 server="sealifebase") 
    
    spp.df <- bind_rows(fish.spp,sl.spp,.id="db")
    
    # ecosystem table is large 
    # want to keep unique values for Climate, Salinity, and EcosystemType, AverageDepth, TempSurface
    eco.sub <- eco.df %>% 
      group_by(Species) %>% 
      select(c(#Status,
        #EcosystemName,
        #EcosystemType,
        Salinity,Climate,
        Polar,Temperate,Subtropical,Tropical,
      )) %>% 
      mutate(across(where(is.character),str_to_sentence)) %>% #change all column names to sentence case
      mutate(Salinity=paste(sort(unique(Salinity)),collapse=", "),
             Climate=paste(sort(unique(Climate)),collapse=", ")) %>%
      mutate(across(where(is.integer),\(x) replace_na(x,0))) %>% #replace all NA values in the binary columns with 0
      ungroup() %>%
      group_by(Species,#EcosystemType,
               Climate,
               Salinity) %>%
      summarize(across(c("Polar","Temperate","Subtropical","Tropical"), #we don't need to keep every area that a fish is present in, collate all the values so we can convert back to a binary
                       sum, na.rm=TRUE)) %>%
      mutate(across(c("Polar","Temperate","Subtropical","Tropical"), #convert to presence/absence
                    ~ case_when(. == 0 ~ 0, .default=-1))) %>%
      unique() %>%
      #filter(!duplicated(EcosystemType)) %>% 
      filter(#!is.na(EcosystemType) & 
        !is.na(Salinity)) %>%
      relocate(Species) 
    
    # subset species table to just include target information 
    spp.sub <- spp.df %>% 
      group_by(Species) %>% 
      select(c(FBname,
               Fresh,Brack,Saltwater,
               DemersPelag,
               AnaCat,
               Vulnerability,VulnerabilityClimate,
               Length,
               Importance,
               Landings,LandingStatistics #, starts_with("M")
      )) %>% 
      mutate(across(where(is.integer),replace_na,0)) %>% #replace all NA values in the binary columns with 0
      mutate(across(where(is.character),str_to_sentence)) %>% #change all column names to sentence case
      relocate(Species) 
    
    # combine all trait information: trophic level from estimate, ecosystem information from ecosystem, species qualities from species  
    # fish.troph and fish.eco and fish.spp 
    fbdf <- merge(eco.sub,
                  spp.sub,
                  .by=Species,all = TRUE)
    
    alltraits.df <- merge((traits.df %>% 
                             select(c(db,Species,
                                      Maxlengthsl,Maxlengthtl,
                                      Tlobserved,
                                      contains("roph"),
                                      contains("epth"),
                                      contains("atio"),
                                      contains("emp"),
                                      contains("eed"),
    ))),
    fbdf,
    .by=Species,all=TRUE)
    
  } else { 
    
    ################## When a single db is selected  ################## 
    
    valid.fish.names <- traits_var$nona[!is.na(traits_var$nona)] %>% 
      rfishbase::validate_names(server=traits_var$serv) 
    
    # make list of species that did not pass 
    invalid.fish.names <- traits_var$nona[!traits_var$nona%in%valid.fish.names & !is.na(traits_var$nona)] 
    
    # Check the species names that were not found in fishbase for synonyms
    syn.fish <- rfishbase::synonyms(invalid.fish.names,server=traits_var$serv) %>% 
      filter(Status=="synonym")
    
    syn.fish.names <- syn.fish[!is.na(syn.fish$Species),]$Species
    
    syn.fish.tab <- syn.fish %>% 
      select(c(Species,synonym)) %>% 
      rename("Valid Name" = "Species", 
             "Queried Name" = "synonym" )
    
    # add synonyms to valid list and remove the queried names from invalid list 
    valid.fish.names <- c(valid.fish.names[!valid.fish.names%in%syn.fish.names], 
                          syn.fish.names) 
    invalid.fish.names <- invalid.fish.names[!invalid.fish.names%in%syn.fish$synonym]
    
    #### get trophic info using valid list ### 
    fish.troph <- rfishbase::estimate(species_list=valid.fish.names,
                                      server=traits_var$serv)
  
    colnames(fish.troph) <- str_to_title(colnames(fish.troph))
    traits.df <- fish.troph %>%
      filter(!duplicated(Species)) %>%
      mutate(Syn=case_when(Species%in%syn.fish$Status ~ "synonym"))
    
    #### Keep some vars #### 
    #traits_var$valid.fish.names <- valid.fish.names
    #traits_var$invalid.fish.names <- invalid.fish.names
    #traits_var$syn.fish.tab <- syn.fish.tab
    traits_var$fish.troph <- fish.troph
    
    traits_var$valid <- unique(valid.fish.names)
    traits_var$invalid <- unique(invalid.fish.names) 
    traits_var$syn.tab <- syn.fish.tab
    traits_var$syn.names <- unique(syn.fish.names)
    
    traits_var$troph <- (traits.df %>%
                           tibble::rownames_to_column(var="rownames")
    ) 
    
    ################## Add other ecosystem data  ################## 
    
    # get table of location/ecosystem preferences 
    eco.df <- rfishbase::ecosystem(species_list=valid.fish.names,
                                     server=traits_var$serv) 
    
    # get species table information
    spp.df <- rfishbase::species(species_list=valid.fish.names,
                                   server=traits_var$serv) 
    
    # ecosystem table is large 
    # want to keep unique values for Climate, Salinity, and EcosystemType, AverageDepth, TempSurface
    eco.sub <- eco.df %>% 
      group_by(Species) %>% 
      select(c(#Status,
        #EcosystemName,
        #EcosystemType,
        Salinity,Climate,
        Polar,Temperate,Subtropical,Tropical,
      )) %>% 
      mutate(across(where(is.character),str_to_sentence)) %>% #change all column names to sentence case
      mutate(Salinity=paste(sort(unique(Salinity)),collapse=", "),
             Climate=paste(sort(unique(Climate)),collapse=", ")) %>%
      mutate(across(where(is.integer),\(x) replace_na(x,0))) %>% #replace all NA values in the binary columns with 0
      ungroup() %>%
      group_by(Species,#EcosystemType,
               Climate,
               Salinity) %>%
      summarize(across(c("Polar","Temperate","Subtropical","Tropical"), #we don't need to keep every area that a fish is present in, collate all the values so we can convert back to a binary
                       sum, na.rm=TRUE)) %>%
      mutate(across(c("Polar","Temperate","Subtropical","Tropical"), #convert to presence/absence
                    ~ case_when(. == 0 ~ 0, .default=-1))) %>%
      unique() %>%
      #filter(!duplicated(EcosystemType)) %>% 
      filter(#!is.na(EcosystemType) & 
        !is.na(Salinity)) %>%
      relocate(Species) 
    
    # subset species table to just include target information 
    spp.sub <- spp.df %>% 
      group_by(Species) %>% 
      select(c(FBname,
               Fresh,Brack,Saltwater,
               DemersPelag,
               AnaCat,
               Vulnerability,VulnerabilityClimate,
               Length,
               Importance,
               Landings,LandingStatistics #, starts_with("M")
      )) %>% 
      mutate(across(where(is.integer),replace_na,0)) %>% #replace all NA values in the binary columns with 0
      mutate(across(where(is.character),str_to_sentence)) %>% #change all column names to sentence case
      relocate(Species) 
    
    # combine all trait information: trophic level from estimate, ecosystem information from ecosystem, species qualities from species  
    # fish.troph and fish.eco and fish.spp 
    fbdf <- merge(eco.sub,
                  spp.sub,
                  .by=Species,all = TRUE)
    
    alltraits.df <- merge((traits.df %>% 
                             select(c(Species,
                                      contains("length"),
                                      contains("roph"),
                                      contains("epth"),
                                      contains("atio"),
                                      contains("emp"),
                                      contains("eed"),
                             ))),
                          fbdf,
                          .by=Species,all=TRUE)
  }
 
  
  ################## Combine with tax table  ################## 
  traits_var$x <- traits_var$alltraits.df <- (alltraits.df %>% 
                                               tibble::rownames_to_column(var="rownames")) #combined fish/nonfish df
  
  # merge with the taxa table  
  taxa_traits_df <- merge(taxa_df,
                          alltraits.df,
                          by="Species",
                          all.x=TRUE)
  
  taxa_traits_df <- relocate(taxa_traits_df,
                             "Species",
                             .after="Genus") 
  
  #rownames(taxa_traits_df) <- taxa_traits_df[[1]]
  
  # save 
  traits_tab$x <- traits_var$taxa_traits_df <- (taxa_traits_df #%>% tibble::rownames_to_column(var="rownames")
                                                 ) #combined fish/nonfish df with taxonomy table 
  
  taxa_traits_sub <- select(taxa_traits_df, -c(1:14))
  traits_var$taxa_traits_sub <- (taxa_traits_sub #%>% tibble::rownames_to_column(var="rownames")
                                  )  #final data frame used for ps
  
})

  # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~ # make report table # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~  
  
  #check if all species were returned
#  nrow(fish.troph)
#  length(valid.fish.names)

  # check lengths  
#  length(fish.names)
#  length(valid.fish.names)
  
  # check numbers  
#  length(valid.fish.names)
#  length(valid.nonfish.names)
  
  # is the size of the combined traits df from fishbase and sealifebase the same size as the two incoming?  
#  nrow(fish.troph)
#  nrow(traits_var$sl.troph)
#  nrow(alltraits.df)
  
  # is the size of the combined traits df the same size as the original taxa df?  
#  nrow(taxa_traits_df)
#  nrow(taxa_df)
traits_report <- reactive({
  req(!is.null(traits_var$taxa_traits_df))
  
  tibble("Number of species queried" = n_distinct(traits_var$nona,na.rm=TRUE),
         "Number of synonyms identified" = n_distinct(traits_var$syn.names,na.rm=TRUE),
         "Number of valid names found" = n_distinct(traits_var$valid %>% unique(),na.rm=TRUE),
         "Number of names not found in database" = n_distinct(traits_var$invalid,na.rm=TRUE),
         ) 
  
}) %>% bindEvent(traits_var$taxa_traits_df)

 # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~ # download trait table # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~  

observe({
  if(!is.null(traits_var$valid)) {shinyjs::show("loading_traittax")} #after traits are assigned show the progress on the next table
  if(!is.null(traits_var$invalid)) {shinyjs::show("viewtrait_panel")} #show loading screen once invalid names list is complete
})

#observe(ps.all(), {
#  if(input$go_addtroph) {shinyjs::show("trait_ps_success")}
#})

################## Popup dialog box ################## 
observe({
  #if(is.null(traits_var$x)) {shinyjs::hide(id="viewtrait_panel")}
  if(!is.null(traits_var$alltraits.df)) {
    shinyjs::delay(5000,
  showModal(
    modalDialog(title="Save data",
                downloadButton("dwnldtrait2", "Download trophic assignments", 
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                ),
                downloadButton("dwnldtraittaxa2", "Download new taxa table", 
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                ),
                size='l',
                easyClose = FALSE )
  )
)
    shinyjs::show(id="add_ps_trait")
    #shinyjs::show(id="viewtrait_panel")
    }
}) %>% bindEvent(input$trait_assign,ignoreNULL=FALSE)

################## Download buttons ################## 
output$dwnldtrait1 <- output$dwnldtrait2 <- downloadHandler(
  filename = function() {
      paste("trait_table_only", ".csv", sep="")
    },
    content = function(file) {
      write.csv(traits_var$x,file)
    }
  )

output$dwnldtraittaxa1 <- output$dwnldtraittaxa2 <- downloadHandler(
  filename = function() {
    paste("trait_taxa_table", ".csv", sep="")
  },
  content = function(file) {
    write.csv(traits_tab$x,file)
  }
)
  # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~ # outputs # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~  
  
#specifically just look at the trait df  
showSpinner("trait_spinner_review", {
  tableServer(id='trait_review',
              data=traits_var #module was written to auto read the $x value of the table
              )
  })

#look at the entire new taxa df ?
showSpinner("trait_spinner_taxreview", {
  tableServer(id='newtax_review',
              data=traits_tab
              )
  })
  
  output$not.names <- renderPrint(traits_var$invalid)
  output$syn.names <- renderTable(traits_var$syn.tab)
  output$trait.tab <- renderTable(traits_report())
  
  output$trait.nona <- renderText(sort(traits_var$nona),sep = ", ")
  output$trait.syn.names <- renderText(sort(traits_var$syn.names),sep = ", ")
  output$trait.valid <- renderText(sort(traits_var$valid),sep = ", ")
  output$trait.invalid <- renderText(sort(traits_var$invalid),sep = ", ")
#  valid.nonfish.names


