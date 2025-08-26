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

observeEvent(input$trait_assign, {
  req(TaxTable$x)
  
  traits_vals$x <- traits_vals$fish.troph <- traits_vals$nonfish.troph <- traits_vals$fishbase.colnames <- traits_vals$invalid.nonfish.names <- traits_vals$taxa_traits_df <- traits_vals$taxa_traits_sub <- NULL
  traits_tab$x <- NULL
  
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
  
  # only proceed if fish.names is made
  
  req(fish.names)
  # select only valid species in fishbase   
  valid.fish.names <- fish.names %>% rfishbase::validate_names(server="fishbase") 
  valid.fish.names <- valid.fish.names[!is.na(valid.fish.names)]
  
  # make new list of species that did not pass 
  invalid.fish.names <- fish.names[!fish.names%in%valid.fish.names] 
  
  # get the ecology info for those species  
  fish.troph <- rfishbase::ecology(species_list=valid.fish.names,
                                   server="fishbase")
  
  #add the ones that weren't found to invalid.fish.names
  invalid.fish.names <- c(invalid.fish.names,valid.fish.names[!valid.fish.names%in%fish.troph$Species])
  
  # make a variable with the column names  
  fishbase.colnames <- colnames(fish.troph)
  
  # this only gets us the fish though, and only species that are valid in fishbase  
  # so next we will look for non-fish species 
  
  # ~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~ # non-fish (sealifebase) # ~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~
  # get  species  
  names <- taxa_df %>% 
    filter(Phylum!="Chordata" & !is.na(Species)) %>% 
    .$Species %>% unique() 
  
  # add the species that didn't pass the previous version in case they're available in sealifebase  
  all.names <- c(invalid.fish.names,names)
  
  # select only valid species in fishbase   
  valid.nonfish.names <- all.names %>% rfishbase::validate_names(server="sealifebase")
  valid.nonfish.names <- valid.nonfish.names[!is.na(valid.nonfish.names)]
  
  invalid.nonfish.names <- all.names[!all.names%in%valid.nonfish.names]
  
  # get the ecology info for those species  
  nonfish.troph <- rfishbase::ecology(species_list=valid.nonfish.names,
                                      server="sealifebase"
  )
  
  # ~~~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~~~ # combine # ~~~~~~~~~~~~~~~~~~~~~~~ # ~~~~~~~~~~~~~~~~~~~~~~~
  
  # standardize the column names  
  colnames(fish.troph) <- str_to_title(colnames(fish.troph))
  colnames(nonfish.troph) <- str_to_title(colnames(nonfish.troph))
  
  # combine the two dataframes with bind_rows, because that will fill any missing values with NA  
  # include 'db' as an ID column so we can tell later what database the results came from  
  traits.df <- bind_rows(fish.troph, nonfish.troph,.id="db") 
  
  # merge with the taxa table  
  taxa_traits_df <- merge(taxa_df,traits.df,by="Species",all.x=TRUE)
  
  taxa_traits_df <- relocate(taxa_traits_df,"Species",.after="Genus") 
  rownames(taxa_traits_df) <- taxa_traits_df[[1]]
  taxa_traits_sub <- select(taxa_traits_df, -c(1:14))
  
  # assign to reactiveValues to use later  
  traits_vals$fish.troph <- (fish.troph %>% tibble::rownames_to_column(var="rownames")) #rfishbase::ecology for fish
  traits_vals$nonfish.troph <- (nonfish.troph %>% tibble::rownames_to_column(var="rownames")) #rfishbase::ecology for non-fish
  
  traits_vals$fishbase.colnames <- (fishbase.colnames) #column names of the rfishbase::ecology output
  
  traits_vals$invalid.nonfish.names <- (invalid.nonfish.names) #what species names were not found in either of the data frames?  
  
  traits_vals$traits.df <- (traits.df %>% tibble::rownames_to_column(var="rownames")) #combined fish/nonfish df
  traits_vals$taxa_traits_df <- (taxa_traits_df %>% tibble::rownames_to_column(var="rownames")) #combined fish/nonfish df with taxonomy table 
  traits_vals$taxa_traits_sub <- (taxa_traits_sub %>% tibble::rownames_to_column(var="rownames"))  #final data frame used for ps
  
  traits_vals$x <- (traits.df %>% tibble::rownames_to_column(var="rownames")) #combined fish/nonfish df
  traits_tab$x <- (taxa_traits_df %>% tibble::rownames_to_column(var="rownames") ) #final data frame used for ps
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

#observe({
#  if(is.null(traits_vals$x)) {shinyjs::hide("viewtrait_panel")}
#  if(!is.null(traits_vals$x)) {shinyjs::show("viewtrait_panel")}
#}) %>% bindEvent(input$trait_assign)

observe({
  if(is.null(traits_vals$x)) {shinyjs::hide(id="viewtrait_panel")}
  if(!is.null(traits_vals$traits.df)) {
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
    shinyjs::show(id="viewtrait_panel")
    }
}) %>% bindEvent(input$trait_assign,ignoreNULL=FALSE)

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
  tableServer(id='trait_review',
              data=traits_vals
  )

#look at the entire new taxa df ?
  tableServer(id='newtax_review',
              data=traits_tab
  )
  
  output$not.names <- renderPrint(traits_vals$invalid.nonfish.names)
#  valid.nonfish.names


