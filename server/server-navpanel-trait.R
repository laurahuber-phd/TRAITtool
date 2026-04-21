source("functions/module-readtable.R",local=TRUE)

tableServer(id='tax_table2',
            data=TaxTable 
) 

traits_vals <- reactiveValues(x=NULL, #final data frame 
                              fish.troph=NULL, #rfishbase::ecology for fish
                              nonfish.troph=NULL, #rfishbase::ecology for non-fish
                              fishbase.colnames=NULL, #column names of the rfishbase::ecology output
                              invalid.nonfish.names=NULL, #what species names were not found in either of the data frames?
                              taxa_traits_df=NULL, #combined fish/nonfish df
                              taxa_traits_sub=NULL #final df used for ps
                              )  
traits_tab <- reactiveValues(x=NULL)
traits_query <- reactiveValues(serv=NULL, #database to query 
                               choose=NULL, #list of species to query 
                               nona=NULL, #list of species to query without na values 
                               valid=NULL, #validated names from nona list
                               sl.query=NULL, #validated names for fishbase including synonyms
                               invalid.fish=NULL, #invalid names in fishbase
                               invalid.sl=NULL, #invalid names in sealifebase
                               invalid=NULL, #invalid names not queried
                               syn.names=NULL, #synonym names identified
                               syn.tab=NULL, #synonym table showing queried values with valid names
                        all=NULL #all species? I think I'm not using this anymore but have not deleted yet
                             )

observeEvent(input$trait_select, {
  req(TaxTable$x)
  
  shinyjs::hide(id="trait_trophreview_card")
  shinyjs::hide(id="p_fishquery")
  
  traits_query$serv <- switch(input$trait_select,
         "Both" = c("fishbase","sealifebase"),
         "FishBase" = "fishbase",
         "SeaLifeBase" = "sealifebase",
         )
})

observeEvent(input$trait_assign, {
  req(TaxTable$x)
 # shinyjs::show(id="viewtrait_panel")
  shinyjs::show(id="trait_trophreview_card")
  shinyjs::show(id="loading_traits")
  
  
  ################## reset all reactive values  ################## 
  traits_vals$x <- traits_vals$fish.troph <- traits_vals$nonfish.troph <- traits_vals$fishbase.colnames <- traits_vals$invalid.nonfish.names <- traits_vals$taxa_traits_df <- traits_vals$taxa_traits_sub <- NULL
  
  traits_tab$x <- NULL
  
  traits_query$serv <- traits_query$all <- traits_query$choose <- traits_query$nona <- traits_query$valid <- traits_query$sl.query <- traits_query$invalid <- traits_query$invalid.fish <- traits_query$invalid.sl <- traits_query$syn.names <- traits_query$syn.tab <- NULL
  
  taxa_df <- TaxTable$x
  
  ################## update vars to show what db we're querying  ################## 
  traits_query$choose <- switch(input$trait_select, 
                                #based on the selected database, get a list of species  
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
  
  if(length(traits_query$choose[!is.na(traits_query$choose)]) > 0) { #number of species to query has to be more than 0
    #make a list of species from the selection (remove NA species)
    traits_query$nona <- traits_query$choose[!is.na(traits_query$choose)] 
    
    shinyjs::show(id="p_fishquery") #report the number of species queried 
    output$n_fishquery <-  renderText({paste("Number of species queried: ",length(traits_query$nona))})
    
  } else {
    showModal(modalDialog("No species are assigned in this table. Check you are using the correct dataframe on the review tab."))
  }
  
  # only proceed if traits_query$nona is made 
  req(traits_query$nona)
  
  ################## When both db is selected  ################## 
  if(input$trait_select=="Both") {
    
    ##### FishBase ##### 
    
    # check if tax names are valid/found in the databases and remove NA values from species #
    valid.fish.names <- traits_query$nona[!is.na(traits_query$nona)] %>% 
      rfishbase::validate_names(server="fishbase") 
    
    # make list of species that did not pass 
    invalid.fish.names <- traits_query$nona[!traits_query$nona%in%valid.fish.names] 
    
    # Check the species names that were not found in fishbase for synonyms
    syn.fish.names.tab <- rfishbase::synonyms(invalid.fish.names,server="fishbase")
    syn.fish.names <- syn.fish.names.tab %>% 
      filter(Status=="synonym")
    
    traits_query$syn.names <- syn.fish.names[!is.na(syn.fish.names$Species),]$Species
    traits_query$syn.tab <- syn.fish.names %>% 
      select(c(Species,synonym)) %>% 
      rename("Valid Name" = "Species", 
             "Queried Name" = "synonym" )
    
    # add synonyms to valid list and remove the queried names from invalid list 
    traits_query$valid <- c(valid.fish.names[!valid.fish.names%in%syn.fish.names$Species], 
                            traits_query$syn.names) 
    traits_query$invalid.fish <- invalid.fish.names[!invalid.fish.names%in%syn.fish.names$synonym]
    
    #### get trophic info ### 
    fish.troph <- rfishbase::estimate(species_list=traits_query$valid,
                                      server="fishbase")
    
    ##### SeaLifeBase ##### 

    # check if tax names are valid/found in the databases and remove NA values from species #
    sl.check <- c(traits_query$valid[!traits_query$valid%in%fish.troph$Species],
                  traits_query$invalid.fish)
    
    valid.sl.names <- sl.check %>% 
      rfishbase::validate_names(server="sealifebase") 
    
    # make list of species that did not pass 
    invalid.sl.names <- sl.check[!sl.check%in%valid.sl.names]
    
    # Check the species names that were not found in fishbase for synonyms
    syn.sl.names.tab <- rfishbase::synonyms(invalid.sl.names,server="sealifebase")
    syn.sl.names <- syn.sl.names.tab %>% 
      filter(Status=="synonym")
    
    traits_query$syn.names <- syn.sl.names[!is.na(syn.sl.names$Species),]$Species
    traits_query$syn.tab <- syn.sl.names %>% 
      select(c(Species,synonym)) %>% 
      rename("Valid Name" = "Species", 
             "Queried Name" = "synonym" )
    
    # add synonyms to valid list and remove the queried names from invalid list 
    traits_query$sl.query <- c(valid.sl.names[!valid.sl.names%in%syn.sl.names$Species], 
                            traits_query$syn.names) 
    
    traits_query$invalid.sl <- c(traits_query$invalid,
                              invalid.sl.names[!invalid.sl.names%in%syn.sl.names$synonym])
    
    #### get trophic info ### 
    nonfish.troph <- rfishbase::estimate(species_list=traits_query$sl.query,
                                      server="sealifebase")
    
    ################### combine the two df ##################
    # standardize the column names  
    colnames(fish.troph) <- str_to_title(colnames(fish.troph))
    colnames(nonfish.troph) <- str_to_title(colnames(nonfish.troph))
    
    # combine the two dataframes with bind_rows, because that will fill any missing values with NA  
    # include 'db' as an ID column so we can tell later what database the results came from  
    traits.df <- bind_rows(fish.troph, 
                           nonfish.troph,
                           .id="db") %>% 
      filter(!duplicated(Species)) %>%
      mutate(Syn=case_when(Species%in%syn.fish.names$Species ~ "synonym"))
    
    traits_vals$fish.troph <- (traits.df %>% tibble::rownames_to_column(var="rownames")) 
    
    #add the ones that weren't found to traits_query$invalid
    traits_query$invalid <- traits_query$invalid.sl #because this value encompasses anything that wasn't found in fishbase, I don't need to include both invalids
    
    ################## Add other ecosystem data  ################## 
    
    # get table of location/ecosystem preferences 
    fish.eco <- rfishbase::ecosystem(species_list=traits_query$valid,
                                     server="fishbase") 
    sl.eco <- rfishbase::ecosystem(species_list=traits_query$sl.query,
                                   server="sealifebase") 
    
    eco.df <- bind_rows(fish.eco,sl.eco,.id="db") 
    
    # get species table information
    fish.spp <- rfishbase::species(species_list=traits_query$valid,
                                   server="fishbase") 
    sl.spp <- rfishbase::species(species_list=traits_query$sl.query,
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
    fbdf <- merge(eco.sub,spp.sub,.by=Species,all = TRUE)
    
    alltraits.df <- merge((traits.df %>% select(c(db,Species,
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
    
    ### check if tax names are valid/found in the databases and remove NA values from species ### 
    valid.fish.names <- traits_query$nona[!is.na(traits_query$nona)] %>% 
      rfishbase::validate_names(server=traits_query$serv) 
    
    # make list of species that did not pass 
    invalid.fish.names <- traits_query$nona[!traits_query$nona%in%valid.fish.names] 
    
    # Check for synonyms
    syn.fish.names.tab <- rfishbase::synonyms(invalid.fish.names,
                                              server=traits_query$serv)
    syn.fish.names <- syn.fish.names.tab %>%
      filter(Status=="synonym")
    
    traits_query$syn.names <- syn.fish.names[!is.na(syn.fish.names$Species),]$Species
    traits_query$syn.tab <- syn.fish.names %>%
      select(c(Species,synonym)) %>%
      rename("Valid Name" = "Species", 
             "Queried Name" = "synonym" )
    
    # add synonyms to valid list and remove the queried names from invalid list 
    traits_query$valid <- c(valid.fish.names[!valid.fish.names%in%syn.fish.names$Species], 
                            traits_query$syn.names) 
    traits_query$invalid <- invalid.fish.names[!invalid.fish.names%in%syn.fish.names$synonym]
    
    # get the trophic info for those species  
    traits.df <- rfishbase::estimate(#species_list=traits_query$nona,
                                     species_list=traits_query$valid,
                                      server=traits_query$serv)
    
    # assign to reactiveValues to use later  
    colnames(traits.df) <- str_to_title(colnames(traits.df))
    traits_vals$fish.troph <- (traits.df %>% tibble::rownames_to_column(var="rownames")) #rfishbase::ecology for fish
    
    ################## Add other ecosystem data  ################## 
    
    # get table of location/ecosystem preferences 
    fish.eco <- rfishbase::ecosystem(species_list=traits_query$valid,
                                     server=traits_query$serv) 
    
    # get species table information
    fish.spp <- rfishbase::species(species_list=traits_query$valid,
                                   server=traits_query$serv) 
    # ecosystem table is large 
    # want to keep unique values for Climate, Salinity, and EcosystemType, AverageDepth, TempSurface
    eco.sub <- fish.eco %>% 
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
    spp.sub <- fish.spp %>% 
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
    fbdf <- merge(eco.sub,spp.sub,.by=Species,all = TRUE)
    
    alltraits.df <- merge((traits.df %>% select(c(Species,
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
    
  }
 
  
  ################## Combine with tax table  ################## 
  traits_vals$x <- traits_vals$alltraits.df <- (alltraits.df %>% 
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
  traits_tab$x <- traits_vals$taxa_traits_df <- (taxa_traits_df #%>% tibble::rownames_to_column(var="rownames")
                                                 ) #combined fish/nonfish df with taxonomy table 
  
  taxa_traits_sub <- select(taxa_traits_df, -c(1:14))
  traits_vals$taxa_traits_sub <- (taxa_traits_sub #%>% tibble::rownames_to_column(var="rownames")
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
#  nrow(nonfish.troph)
#  nrow(alltraits.df)
  
  # is the size of the combined traits df the same size as the original taxa df?  
#  nrow(taxa_traits_df)
#  nrow(taxa_df)

 # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~ # download trait table # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~  

observe({
  if(!is.null(traits_query$valid)) {shinyjs::show("loading_traittax")} #after traits are assigned show the progress on the next table
  if(!is.null(traits_query$invalid)) {shinyjs::show("viewtrait_panel")} #show loading screen once invalid names list is complete
})

#observe(ps.all(), {
#  if(input$go_addtroph) {shinyjs::show("trait_ps_success")}
#})

################## Popup dialog box ################## 
observe({
  #if(is.null(traits_vals$x)) {shinyjs::hide(id="viewtrait_panel")}
  if(!is.null(traits_vals$alltraits.df)) {
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
      write.csv(traits_vals$x,file)
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
              data=traits_vals #module was written to auto read the $x value of the table
              )
  })

#look at the entire new taxa df ?
showSpinner("trait_spinner_taxreview", {
  tableServer(id='newtax_review',
              data=traits_tab
              )
  })
  
  output$not.names <- renderPrint(traits_query$invalid)
  output$syn.names <- renderTable(traits_query$syn.tab)
#  valid.nonfish.names


