source("functions/module-readtable.R",local=TRUE)

tableServer(id='tax_table2',
            data=TaxTable 
) 

observeEvent(input$trait_assign, {
  req(TaxTable$x)
  
  taxa_df <- TaxTable$x
  
  # get chordate species  
  get.fish.names <- taxa_df %>% 
    filter(Phylum=="Chordata" & !is.na(Species)) %>% 
    .$Species %>% unique() 

  if(length(get.fish.names[!is.na(get.fish.names)]) > 0) {
    fish.names <- get.fish.names[!is.na(get.fish.names)]
  } else {
    showModal(modalDialog("No fish species are assigned in this table. Try again without selecting fish, or check you are using the correct dataframe on the review tab."))
  }
  
  req(fish.names)
  # select only valid species in fishbase   
  valid.fish.names <- fish.names %>% rfishbase::validate_names(server="fishbase") 
#  valid.fish.names <- valid.fish.names[!is.na(valid.fish.names)]
  
  # make new list of species that did not pass 
  invalid.fish.names <- fish.names[!fish.names%in%valid.fish.names] 
  
  # get the ecology info for those species  
#  fish.troph <- rfishbase::ecology(species_list=valid.fish.names,
#                                   server="fishbase")
  
  #add the ones that weren't to invalid.fish.names
#  invalid.fish.names <- c(invalid.fish.names,valid.fish.names[!valid.fish.names%in%fish.troph$Species])
  
  # make a variable with the column names  
#  fishbase.colnames <- colnames(fish.troph)
  
  # this only gets us the fish though, and only species that are valid in fishbase  
  # so next we will look for non-fish species 
  
  # ~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~ # non-fish (sealifebase) # ~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~
  # get  species  
#  names <- taxa_df %>% 
#    filter(Phylum!="Chordata" & !is.na(Species)) %>% 
#    .$Species %>% unique() 
  
  # add the species that didn't pass the previous version in case they're available in sealifebase  
#  all.names <- c(invalid.fish.names,names)
  
  # select only valid species in fishbase   
#  valid.nonfish.names <- all.names %>% rfishbase::validate_names(server="sealifebase")
#  valid.nonfish.names <- valid.nonfish.names[!is.na(valid.nonfish.names)]
  
#  invalid.nonfish.names <- all.names[!all.names%in%valid.nonfish.names]
  
  # get the ecology info for those species  
#  nonfish.troph <- rfishbase::ecology(species_list=valid.nonfish.names,
#                                      server="sealifebase"
#  )
  
  # make a variable with the column names  
#  sealifebase.colnames <- colnames(nonfish.troph)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~~~ # combine # ~~~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~~~
  
  # standardize the column names  
#  colnames(fish.troph) <- str_to_title(colnames(fish.troph))
#  colnames(nonfish.troph) <- str_to_title(colnames(nonfish.troph))
  
  # combine the two dataframes with bind_rows, because that will fill any missing values with NA  
  # include 'db' as an ID column so we can tell later what database the results came from  
#  traits.df <- bind_rows(fish.troph, nonfish.troph,.id="db") 
  
  # merge with the taxa table  
#  taxa_traits_df <- merge(taxa_df,traits.df,by="Species",all.x=TRUE)
  
#  taxa_traits_df <- relocate(taxa_traits_df,"Species",.after="Genus") 
#  rownames(taxa_traits_df) <- taxa_traits_df$seqid
#  taxa_traits_sub <- select(taxa_traits_df, -c(1:14))
  
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
#  nrow(traits.df)
  
  # is the size of the combined traits df the same size as the original taxa df?  
#  nrow(taxa_traits_df)
#  nrow(taxa_df)
  
  # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~ # outputs # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~  
  
#  output$trait_review <- renderDT(taxa_traits_sub)
  
  output$fish.names <- renderText(valid.fish.names)
  output$not.names <- renderText(invalid.fish.names)
#  valid.nonfish.names
}) 

