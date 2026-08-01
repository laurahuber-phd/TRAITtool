source("functions/module-readtable.R",local=TRUE)

tableServer(id='tax_table2',
            data=TaxTable 
) 

################## set initial reactive values  ################## 
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

################## ID taxa to query  ################## 
# use the taxa table to check what taxonomic levels can be used to query trophic information
# provide option to query trophic information based on higher taxa levels

observe({
  taxcols <- colnames(TaxTable$x) #get column names of the tax table 
  seltax <- taxcols[str_detect(taxcols,"peci") | str_detect(taxcols,"enus") | str_detect(taxcols,"amil")] #look for genus, family, and species 
  
  # then update the selection box accordingly  
  updateSelectInput(session,
                    "trait_tax_select",#for blank selection
                    choices=c(rev(seltax))) 
  
  print(c("Trait: Query options include:",seltax))
}) %>% bindEvent(TaxTable$x)

observe({
  print(c("Trait: Query selection is",input$trait_tax_select))
}) %>% bindEvent(input$trait_tax_select, ignoreInit=TRUE)

# when the user selections fishbase/sealifebase/both, hide previous queries and update database string for query 
observeEvent(input$trait_select, {
  req(TaxTable$x)

  shinyjs::hide(id="trait_trophreview_card")
  shinyjs::hide(id="p_fishquery")
  print("Traits: Hid trophic review cards")
  
  traits_var$serv <- switch(input$trait_select,
         "Both" = c("fishbase","sealifebase"),
         "FishBase" = "fishbase",
         "SeaLifeBase" = "sealifebase",
         )
  print("Traits: traits_query$serv updated")
})

################## when assign button is clicked  ################## 
observeEvent(input$trait_assign, {
  
### reset the following values when assign button is clicked
  req(TaxTable$x)
  taxa_df <- TaxTable$x
  rownames(taxa_df) <- taxa_df[[1]]
  keep <- colnames(taxa_df)[1]
  
 # shinyjs::show(id="viewtrait_panel")
  shinyjs::show(id="trait_trophreview_card")
  shinyjs::show(id="loading_traits")
  
  # reset all values to NULL
  traits_var$x <- traits_var$choose <- traits_var$nona <- traits_var$syn.fish.tab <- traits_var$syn.sl.tab <- traits_var$syn.tab <- traits_var$fish.troph <- traits_var$sl.troph <- traits_var$troph <- traits_var$fbdf <- traits_var$traits.df <- traits_var$alltraits.df <- traits_var$taxa_traits_df <- traits_var$taxa_traits_sub <- traits_var$valid.fish.names <- traits_var$invalid.fish.names <- traits_var$sl.check <- traits_var$valid.sl.names <- traits_var$invalid.sl.names <- traits_var$valid <- traits_var$invalid <- traits_var$syn.names <- traits_var$fish.eco <- traits_var$sl.eco <- traits_var$eco.df <- traits_var$fish.spp <- traits_var$sl.spp <- traits_var$spp.df <- traits_var$eco.sub <- traits_var$spp.sub <- NULL
  
  traits_tab$x <- NULL
  traits_var_spplist <- c()
print("Traits: reactive value variables reset to NULL")

################## Get taxa list to query  ################## 

##### Using levels higher than species  ##### 
if(input$trait_tax_select != "Species") {
  print(c("Trait: Querying the following",input$trait_tax_select))
  
  traits_var_taxlist <- taxa_df %>% 
    select(all_of(input$trait_tax_select)) %>% 
    unlist() %>%
    #as.list() %>%
    unique() 
  
  #spptbls <- fb_tbl("species")
  #famtbls <- fb_tbl("families")
  allspptbl <- load_taxa()
  
  req(!is.null(traits_var_taxlist))
  for (i in 1:length(traits_var_taxlist)) {
    #querylist <- species_list(input$trait_tax_select = traits_var_taxlist[i])
    querylist <- allspptbl %>% 
      filter(.data[[input$trait_tax_select]] == traits_var_taxlist[i]) %>% 
      .$Species %>% 
      unique()
    
    traits_var_spplist <- c(traits_var_spplist,querylist)
  }
  
  traits_var$choose <- traits_var_spplist
  print(c("Number of",input$trait_tax_select,"queried:",length(traits_var_taxlist)))
  print(c("Resulting in",length(traits_var_spplist),"species queried"))
  print(traits_var_spplist)
} else {
  
  ##### Using species level ##### 
  # get a list of species to query based on the selected database 
  traits_var$choose <- switch(input$trait_select, 
                             "Both" = c(taxa_df %>% 
                                          #filter(!is.na(Species)) %>%
                                          .$Species %>% unique()),
                             "FishBase" = c(taxa_df %>%
                                              #filter(!is.na(Species) & str_detect(Phylum,"hord")) %>% 
                                              .$Species %>% unique()),
                             "SeaLifeBase" = c(taxa_df %>%
                                                 #filter(!is.na(Species) & !str_detect(Class,"ctinop")) %>% 
                                                 .$Species %>% unique())
  )
  print("Traits: trait taxa selected")
  }
  
  # check that the number of species to query is greater than 0
  # if so, assign the species to a list and remove NA species assignments
  if(length(traits_var$choose[!is.na(traits_var$choose)]) > 0) { 
    traits_var$nona <- traits_var$choose[!is.na(traits_var$choose)] 
    print(c("Number of species queried: ",length(traits_var$nona)))
    # show the panel listing number of species queried
    shinyjs::show(id="p_fishquery")
    output$n_fishquery <-  renderText({paste("Number of species queried: ",length(traits_var$nona))})
    
  } else {
    showModal(modalDialog("No species are assigned in this table. Check you are using the correct dataframe on the review tab."))
    print("Traits: traits_query$choose is 0 or NA")
  }
  
  # only proceed if traits_var$nona is made 
  req(traits_var$nona)
  print("Traits: traits_query$nona exists")
  
################## Query list against one or both of the databases ##################    

  ###### When both db is selected  ##### 
  if(input$trait_select=="Both") {
    print("Traits: Both databases selected for trait query")
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
    valid.fish.names <- traits_var$nona %>% 
      rfishbase::validate_names(server="fishbase") 
    valid.fish.names <- valid.fish.names[!is.na(valid.fish.names)]
    print("Traits: Names validated in FishBase")
    
    # make list of species that did not pass 
    invalid.fish.names <- traits_var$nona[!traits_var$nona%in%valid.fish.names]
    
    #if (length(invalid.fish.names)>1) {
    #  showModal(modalDialog("These names were not found."),
    #            renderText(invalid.fish.names))
    #}
    
    # Check the species names that were not found in fishbase for synonyms
    syn.fish <- rfishbase::synonyms(invalid.fish.names,
                                    server="fishbase") %>% 
      filter(Status=="synonym")
    print("Traits: Synonyms queried from FishBase")
    
    # retain synonym names 
    syn.fish.names <- syn.fish[!is.na(syn.fish$Species),]$Species
    
    # make a table showing what synonyms were found and what their original names from the tax dataframe were
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
    print("Traits: trophic information gathered from FishBase")
    
    ##### SeaLifeBase ##### 

    # check if tax names are valid/found in the databases and remove NA values from species #
    sl.check <- c(valid.fish.names[!valid.fish.names%in%fish.troph$Species],
                  invalid.fish.names)
    
    valid.sl.names <- sl.check %>% 
      rfishbase::validate_names(server="sealifebase") 
    valid.sl.names <- valid.sl.names[!is.na(valid.sl.names)]
    print("Traits: Names validated in SeaLifeBase")
    
    # make list of species that did not pass 
    invalid.sl.names <- sl.check[!sl.check%in%valid.sl.names]
    
    # Check the species names that were not found in fishbase for synonyms
    syn.sl <- rfishbase::synonyms(invalid.sl.names,
                                  server="sealifebase") %>% 
      filter(Status=="synonym")
    print("Traits: Synonyms queried from SeaLifeBase")
    
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
    print("Traits: Trophic information gathered from SeaLifeBase")
    
    ################### combine the two df ##################
    # standardize the column names  
    colnames(fish.troph) <- str_to_title(colnames(fish.troph))
    colnames(sl.troph) <- str_to_title(colnames(sl.troph))
    
    # combine the two dataframes with bind_rows, because that will fill any missing values with NA  
    # include 'db' as an ID column so we can tell later what database the results came from  
    traits.df <- bind_rows(fish.troph, 
                           sl.troph,
                           .id="traitdb") %>% 
      filter(!duplicated(Species)) %>%
      mutate(Syn=case_when(Species%in%syn.fish$Status ~ "synonym")) %>% as.data.frame()
    print("Traits: trophic level information combined from both databases")
    
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
                                     .id="traitdb") #%>% filter(!duplicated(Species)) 
    traits_var$syn.names <- c(syn.fish.names,syn.sl.names) %>% unique()
    
    traits_var$troph <- (traits.df #%>%
                            #tibble::rownames_to_column(var="Row.Name")
                          ) 
    
################## Add other ecosystem data  ################## 
    
    # only check for other ecosystem data when we're querying the species level
    if(input$trait_tax_select != "Species"){ 
      #alltraits.df <- traits.df 
      alltraits.df <- merge(traits.df, 
                            (allspptbl %>% 
                              filter(Species %in% traits_var_spplist) %>% 
                              select(c(Species,
                                       .data[[input$trait_tax_select]]
                                       ))),
                            .by="Species"
                            )
    print("Traits: higher taxa level queried; no further natural history information queried beyond MTI")
    print(c("colnames of old table:",colnames(traits.df)))
    print(c("colnames of new table:",colnames(alltraits.df)))
    print(c("nrows of old table:",nrow(traits.df)))
    print(c("nrows of new table:",nrow(alltraits.df)))
        
      } else {
    # get table of location/ecosystem preferences 
    fish.eco <- rfishbase::ecosystem(species_list=valid.fish.names,
                                     server="fishbase") 
    print("Traits: Ecosystem data queried from FishBase")
    sl.eco <- rfishbase::ecosystem(species_list=valid.sl.names,
                                   server="sealifebase") 
    print("Traits: Ecosystem data queried from SeaLifeBase")
    eco.df <- bind_rows(fish.eco,sl.eco,.id="traitdb") 
    
    # get species table information
    fish.spp <- rfishbase::species(species_list=valid.fish.names,
                                   server="fishbase") 
    print("Traits: Species table data queried from FishBase")
    sl.spp <- rfishbase::species(species_list=valid.sl.names,
                                 server="sealifebase") 
    print("Traits: Species table data queried from SeaLifeBase")
    
    spp.df <- bind_rows(fish.spp,sl.spp,.id="traitdb")
    
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
        !is.na(Salinity))# %>%
      #relocate(Species) 
    
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
      mutate(across(where(is.character),str_to_sentence))# %>% #change all column names to sentence case
      #relocate(Species) 
    
    # combine all trait information: trophic level from estimate, ecosystem information from ecosystem, species qualities from species  
    # fish.troph and fish.eco and fish.spp 
    fbdf <- merge(eco.sub,
                  spp.sub,
                  .by=Species,all = TRUE) %>% as.data.frame()
    print("Traits: Ecology and Species data combined into one table")
    
    alltraits.df <- merge((traits.df %>% 
                             #rownames_to_column(.,var="Row.Name") %>% 
                             select(c(#all_of(keep),
                                      traitdb,
                                      #Row.Name, 
                                      Species,
                                      Maxlengthsl,Maxlengthtl,
                                      Tlobserved,
                                      contains("roph"),
                                      contains("epth"),
                                      contains("atio"),
                                      contains("emp"),
                                      contains("eed"),
    ))),
    fbdf,
    .by=Species,all=TRUE)}
    print("Traits: All trait data merged with taxa table")
  
    } else { 
    print("Traits: only one database selected")
    
  ################## When a single db is selected  ################## 
    
    valid.fish.names <- traits_var$nona %>% 
      rfishbase::validate_names(server=traits_var$serv) 
    print("Traits: Names validated with database")
    
    # make list of species that did not pass 
    invalid.fish.names <- traits_var$nona[!traits_var$nona%in%valid.fish.names] 
    
    # Check the species names that were not found in fishbase for synonyms
    syn.fish <- rfishbase::synonyms(invalid.fish.names,server=traits_var$serv) %>% 
      filter(Status=="synonym")
    print("Traits: Checked invalidated names for synonyms")
    
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
    print("Traits: Trait information gathered")
    
    colnames(fish.troph) <- str_to_title(colnames(fish.troph))
    traits.df <- fish.troph %>%
      filter(!duplicated(Species)) %>%
      mutate(Syn=case_when(Species%in%syn.fish$Status ~ "synonym")) %>% as.data.frame()
    
    #### Keep some vars #### 
    #traits_var$valid.fish.names <- valid.fish.names
    #traits_var$invalid.fish.names <- invalid.fish.names
    #traits_var$syn.fish.tab <- syn.fish.tab
    traits_var$fish.troph <- fish.troph
    
    traits_var$valid <- unique(valid.fish.names)
    traits_var$invalid <- unique(invalid.fish.names) 
    traits_var$syn.tab <- syn.fish.tab
    traits_var$syn.names <- unique(syn.fish.names)
    
    traits_var$troph <- (traits.df #%>%
                           #tibble::rownames_to_column(var="Row.Name")
    ) 
    
    ################## Add other ecosystem data  ################## 
    if (input$trait_tax_select != "Species") { 
      
      alltraits.df <- traits.df 
      print("Traits: higher taxa level queried; no further natural history information queried beyond MTI")
      
      } else {
    # get table of location/ecosystem preferences 
    eco.df <- rfishbase::ecosystem(species_list=valid.fish.names,
                                     server=traits_var$serv) 
    print("Traits: Ecosystem data gathered")
    
    # get species table information
    spp.df <- rfishbase::species(species_list=valid.fish.names,
                                   server=traits_var$serv) 
    print("Traits: Species trait data gathered")
    
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
        !is.na(Salinity)) #%>%
      #relocate(Species) 
    
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
      mutate(across(where(is.character),str_to_sentence)) #%>% #change all column names to sentence case
      #relocate(Species) 
    
    # combine all trait information: trophic level from estimate, ecosystem information from ecosystem, species qualities from species  
    # fish.troph and fish.eco and fish.spp 
    fbdf <- merge(eco.sub,
                  spp.sub,
                  .by=Species,all = TRUE) %>% as.data.frame()
    
    alltraits.df <- merge((traits.df %>% 
                             #rownames_to_column(.,var="Row.Name") %>% 
                             select(c(#all_of(keep),
                                      #Row.Name, 
                                      Species,
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
  }
  ################## After trait assignment is completed  ##################   
 req(!is.null(traits_var$syn.tab))
 req(alltraits.df)
 
 ################## Average MTI for higher taxa level and combine with tax df ##################   
if (input$trait_tax_select != "Species") { 
  alltraits.df <- alltraits.df %>% ungroup() %>%
   group_by(.data[[input$trait_tax_select]]) %>% 
   summarize("Troph" = mean(Troph),
             "Setroph" = mean(Setroph),
             "Trophobserved" = mean(Trophobserved),
             "Trophpredicted" = mean(Trophpredicted),
             "Setrophpredicted" = mean(Setrophpredicted),
             "MeanCalc" = "MTI calculated as mean of all MTI from taxonomic level identified")
  
  print(c("colnames of new table:",colnames(alltraits.df)))
  print(c("nrows of new table:",nrow(alltraits.df)))
  # check if there's a way to do this without specifying the column names to make it a bit more robust
  }
 
  ################## Add synonym information to tax table  ################## 
 if (input$trait_tax_select=="Species"){
  t.syn.tab <- traits_var$syn.tab %>% rename(Synonym = "Valid Name", #Change the name of the columns so we can combine this table with the alltraits.df table by Species 
                                  Species = "Queried Name") 
  print("Traits: Synonym table headers renamed")
  
  alltraits.df <- alltraits.df %>% 
    merge(.,t.syn.tab, all.x=TRUE) %>% 
    mutate(Synonym= case_when(Species %in% t.syn.tab$Synonym ~ Species, #add the synonym found in the database as an extra column in the trait df 
                              .default=Synonym)) 
  print("Traits: Synonym table added to trait df")
  
  alltraits.df <- alltraits.df %>% 
    filter(.,!is.na(Synonym)==TRUE) %>% #temporarily remove the taxa we did not find synonyms for
    group_by(Synonym) %>% 
    fill(-Species, .direction="downup") %>% #then we can fill in the values for the original query using values found from the synonym name 
    bind_rows(alltraits.df %>% filter(is.na(Synonym))) %>% #add the other taxa back in 
    #filter(rowname %in% rownames(otu.tab))
    filter(Species != Synonym | is.na(Synonym)) %>% #then remove the synonym rows 
    as.data.frame() 
  print("Traits: Synonym data used to fill trait df")
 }
 
  if(!is.null(alltraits.df$Troph)) {
    alltraits.df <- alltraits.df %>% 
      mutate(Trophic.val=case_when(Troph < 2 ~ "Primary producers and detritus",
                                   Troph >= 2 & Troph < 2.2 ~ "Primary consumers", 
                                   Troph >= 2.2 & Troph < 2.8 ~ "Omnivores",
                                   Troph >=2.8 ~ "Carnivores", 
                                   .default="Not Assigned"))
  }
  print("Traits: Numeric trophic level data converted to descriptive")
  
  ################## Combine with tax table  ################## 
  if(input$trait_tax_select != "Species") {
    #need to take average of everything in the Genus/Family
    #then add back into tax table based on tax level queried
  traits_var$x <- traits_var$alltraits.df <- (alltraits.df #%>% tibble::rownames_to_column(var="Row.Name")
                                              ) #combined fish/nonfish df
  
  # merge with the taxa table  
  taxa_traits_df <- merge(taxa_df,
                          alltraits.df,
                          by = input$trait_tax_select,
                          #by=.data[[input$trait_tax_select]],
                          all.x=TRUE)
  
  print(c("colnames of old table:",colnames(alltraits.df)))
  print(c("colnames of new table:",colnames(taxa_traits_df)))
  print(c("nrows of old table:",nrow(alltraits.df)))
  print(c("nrows of new table:",nrow(taxa_traits_df)))
  
  numeric.cols <- taxa_traits_df %>% select(where(is.double) | where(is.numeric)) %>% colnames()
  tax.numcols(numeric.cols)
  print("Traits: trophic information added to tax table") 
  traits_tab$x <- traits_var$taxa_traits_df <- (taxa_traits_df %>% relocate(all_of(keep))  #%>% tibble::rownames_to_column(var="Row.Name")
  ) #combined fish/nonfish df with taxonomy table 
  
  taxa_traits_sub <- taxa_traits_df #select(taxa_traits_df, -c(1:14))
  traits_var$taxa_traits_sub <- (taxa_traits_sub %>% relocate(all_of(keep))  #%>% tibble::rownames_to_column(var="Row.Name")
  )  #final data frame used for ps
  
  }
  else {
  traits_var$x <- traits_var$alltraits.df <- (alltraits.df #%>% tibble::rownames_to_column(var="Row.Name")
                                              ) #combined fish/nonfish df
  
  # merge with the taxa table  
  taxa_traits_df <- merge(taxa_df,
                          alltraits.df,
                          by="Species",
                          all.x=TRUE)
  numeric.cols <- taxa_traits_df %>% select(where(is.double) | where(is.numeric)) %>% colnames()
  tax.numcols(numeric.cols)
  #taxa_traits_df <- relocate(taxa_traits_df,
  #                           "Species",
  #                           .after="Genus") 
  print("Traits: trophic information added to tax table") 
  
  #rownames(taxa_traits_df) <- taxa_traits_df[[1]]
  
  # save 
  traits_tab$x <- traits_var$taxa_traits_df <- (taxa_traits_df %>% relocate(all_of(keep))  #%>% tibble::rownames_to_column(var="Row.Name")
                                                 ) #combined fish/nonfish df with taxonomy table 
  
  taxa_traits_sub <- taxa_traits_df #select(taxa_traits_df, -c(1:14))
  traits_var$taxa_traits_sub <- (taxa_traits_sub %>% relocate(all_of(keep))  #%>% tibble::rownames_to_column(var="Row.Name")
                                  )  #final data frame used for ps
  }
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
  
 data.frame( Data =c("Number of species queried",
                         "Number of synonyms identified",
                         "Number of valid names found",
                         "Number of names not found in database"
 ),
             Values = c(n_distinct(traits_var$nona,na.rm=TRUE),
                        n_distinct(traits_var$syn.names,na.rm=TRUE),
                        n_distinct(traits_var$valid %>% unique(),na.rm=TRUE),
                        n_distinct(traits_var$invalid,na.rm=TRUE)
                        )
         ) 
  
}) %>% bindEvent(traits_var$taxa_traits_df)

 # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~ # download trait table # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~  

observe({
  if(!is.null(traits_query$valid)) {shinyjs::show("loading_traittax")} #after traits are assigned show the progress on the next table
  print("Traits: traits_query$valid is no longer null, show loading_traittax")
  if(!is.null(traits_query$invalid)) {shinyjs::show("viewtrait_panel")} #show loading screen once invalid names list is complete
  print("Traits: traits_query$invalid is not null; show viewtrait_panel")
},suspended = TRUE)

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
      write.csv(traits_var$x,file,row.names=FALSE)
    }
  )

output$dwnldtraittaxa1 <- output$dwnldtraittaxa2 <- downloadHandler(
  filename = function() {
    paste("trait_taxa_table", ".csv", sep="")
  },
  content = function(file) {
    write.csv(traits_tab$x,file,row.names=FALSE)
  }
)
  # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~ # outputs # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~  
  

  output$not.names <- renderPrint(traits_var$invalid)
  output$syn.names <- renderTable(traits_var$syn.tab)
  output$trait.tab <- renderTable(traits_report())
  
  output$trait.nona <- renderText(sort(traits_var$nona),sep = ", ")
  output$trait.syn.names <- renderText(sort(traits_var$syn.names),sep = ", ")
  output$trait.valid <- renderText(sort(traits_var$valid),sep = ", ")
  output$trait.invalid <- renderText(sort(traits_var$invalid),sep = ", ")
#  valid.nonfish.names


