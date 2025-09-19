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
                               invalid=NULL, #invalid names not queried
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
  
  traits_query$serv <- traits_query$all <- traits_query$choose <- traits_query$nona <- traits_query$valid <- traits_query$invalid <- NULL
  
  taxa_df <- TaxTable$x
  
  ################## update vars to show what db we're querying  ################## 
  traits_query$choose <- switch(input$trait_select, 
                                #based on the selected database, get a list of species  
                             "Both" = c(taxa_df %>% filter(!is.na(Species)) %>% .$Species %>% unique()),
                             "FishBase" = c(taxa_df %>% filter(!is.na(Species) & str_detect(Phylum,"hord")) %>% .$Species %>% unique()),
                             "SeaLifeBase" = c(taxa_df %>% filter(!is.na(Species)) %>% .$Species %>% unique())
  )
  
  if(length(traits_query$choose[!is.na(traits_query$choose)]) > 0) { #number of species to query has to be more than 0
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
    # get valid species from each database 
    fishlist <-  traits_query$nona %>% rfishbase::validate_names(server="fishbase") 
    sealist <-  traits_query$nona %>% rfishbase::validate_names(server="sealifebase") 
    # combine the two lists and remove any na values  
    bothlist <- c(fishlist,sealist[!sealist%in%fishlist])
    traits_query$valid <- bothlist[!is.na(bothlist)] 
    # make list of species that did not pass 
    traits_query$invalid <- traits_query$nona[!traits_query$nona%in%traits_query$valid] 
    
    ################## get trophic info   ################## 
    fish.troph <- rfishbase::estimate(species_list=traits_query$valid,
                                      server="fishbase")
    nonfish.troph <- rfishbase::estimate(species_list=traits_query$valid[!traits_query$valid%in%fish.troph$Species],
                                      server="sealifebase")
    
    # combine the two df
    # standardize the column names  
    colnames(fish.troph) <- str_to_title(colnames(fish.troph))
    colnames(nonfish.troph) <- str_to_title(colnames(nonfish.troph))
    
    # combine the two dataframes with bind_rows, because that will fill any missing values with NA  
    # include 'db' as an ID column so we can tell later what database the results came from  
    traits.df <- bind_rows(fish.troph, 
                           nonfish.troph,
                           .id="db") 
    
    traits_vals$fish.troph <- (traits.df %>% tibble::rownames_to_column(var="rownames")) 
    
    #add the ones that weren't found to traits_query$invalid
    traits_query$invalid <- c(traits_query$invalid,traits_query$valid[!traits_query$valid%in%traits.df$Species])
    
  } else { 
    
    ################## When a single db is selected  ################## 
    # select only valid species   
    traits_query$valid <- traits_query$nona %>% rfishbase::validate_names(server=traits_query$serv) 
    traits_query$valid <- traits_query$valid[!is.na(traits_query$valid)]
    
    # make new list of species that did not pass 
    traits_query$invalid <- traits_query$nona[!traits_query$nona%in%traits_query$valid] 
    
    # get the trophic info for those species  
    traits.df <- rfishbase::estimate(species_list=traits_query$valid,
                                      server=traits_query$serv)
    # assign to reactiveValues to use later  
    colnames(traits.df) <- str_to_title(colnames(traits.df))
    traits_vals$fish.troph <- (traits.df %>% tibble::rownames_to_column(var="rownames")) #rfishbase::ecology for fish
    
    #add the ones that weren't found to traits_query$invalid
    traits_query$invalid <- c(traits_query$invalid,traits_query$valid[!traits_query$valid%in%traits.df$Species])
    
  }
  
  ################## Combine with tax table  ################## 
  traits_vals$x <- traits_vals$traits.df <- (traits.df %>% 
                                               tibble::rownames_to_column(var="rownames")) #combined fish/nonfish df
  
  # merge with the taxa table  
  taxa_traits_df <- merge(taxa_df,
                          traits.df,
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
#  nrow(traits.df)
  
  # is the size of the combined traits df the same size as the original taxa df?  
#  nrow(taxa_traits_df)
#  nrow(taxa_df)

 # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~ # download trait table # ~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~  

observe({
  if(!is.null(traits_vals$fish.troph)) {shinyjs::show("loading_traittax")} #after traits are assigned show the progress on the next table
  if(!is.null(traits_vals$invalid.nonfish.names)) {shinyjs::show("viewtrait_panel")} #show loading screen once invalid names list is complete
})

#observe(ps.all(), {
#  if(input$go_addtroph) {shinyjs::show("trait_ps_success")}
#})

################## Popup dialog box ################## 
observe({
  #if(is.null(traits_vals$x)) {shinyjs::hide(id="viewtrait_panel")}
  if(!is.null(traits_vals$traits.df)) {
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
  
  output$not.names <- renderPrint(traits_vals$invalid.nonfish.names)
#  valid.nonfish.names


