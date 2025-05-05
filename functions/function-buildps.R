
assemble_ps <- function(otu,tax,meta) {
  temp.otu <- as.data.frame(otu)
  row.names(temp.otu) <- temp.otu[[1]]
  temp.otu[[1]] <- NULL
  #row.names(temp.otu) <- temp.otu$X
  #temp.otu$X <- NULL
  
  #navals <- c("Unassigned","unassigned",NULL,"Not assigned","")
  temp.tax <- as.data.frame(tax)
  #temp.tax <- data.frame(lapply(temp.tax,function(x) {#replace all unassigned with NA
   # stringr::str_replace_all(x,paste(navals,collapse = "|"),as.character(NA))
   # gsub(paste(navals,collapse = "|"),NA,x)
  #}))
  row.names(temp.tax) <- temp.tax[[1]]
  temp.tax[[1]] <- NULL
  #row.names(temp.tax) <- temp.tax$X
  #temp.tax$X <- NULL

  temp.metadata <- as.data.frame(meta)
#  rownames(temp.metadata) <- temp.metadata$Barcode #need to change this eventually
  rownames(temp.metadata) <- temp.metadata[[1]]
  
  temp.OTU <- as.matrix(temp.otu) %>% otu_table(., taxa_are_rows = TRUE)
  temp.TAX <- as.matrix(temp.tax) %>% tax_table(.)
  temp.META <- sample_data(temp.metadata) 
  temp.ps <- phyloseq(temp.OTU, temp.TAX, temp.META)
  temp.ps
}
subset_keep <- function(ps,var,samples) { #need the ps object, the variable name/column name of the metadata column with your defined samples, and the selected samples that are blanks
  varlist <- as.character(get_variable(ps,var)) #list the components of the defined variable
  keepsamples <- varlist[varlist %in% samples] #keep the components that are selected
  
  newsampledata <- filter(as.data.frame(as.matrix(sample_data(ps))), #extract the metadata from your phyloseq object as a data frame
                          .data[[var]]%in%keepsamples #tell R we are searching for 'var' inside the data frame, then we want all of chosen samples
  )
  
  new.ps <- ps #make a new ps object so we aren't editing the old one
  sample_data(new.ps) <- sample_data(newsampledata) #assign our new sample data (sans blanks) to the new ps object
  new.ps <- new.ps %>% prune_taxa(taxa_sums(.)>0,.) #remove any taxa that were only in the blanks
  new.ps
  
}

subset_blanks <- function(ps,var,samples) { #need the ps object, the variable name/column name of the metadata column with your defined samples, and the selected samples that are blanks
  varlist <- as.character(get_variable(ps,var)) #list the components of the defined variable
  notblasamples <- varlist[!varlist %in% samples] #keep the components that are NOT blanks
  
  newsampledata <- filter(as.data.frame(as.matrix(sample_data(ps))), #extract the metadata from your phyloseq object as a data frame
                          .data[[var]]%in%notblasamples #tell R we are searching for 'var' inside the data frame, then we want all of NON blank samples
                          )
  
  new.ps <- ps #make a new ps object so we aren't editing the old one
  sample_data(new.ps) <- sample_data(newsampledata) #assign our new sample data (sans blanks) to the new ps object
  new.ps <- new.ps %>% prune_taxa(taxa_sums(.)>0,.) #remove any taxa that were only in the blanks
  new.ps

}

subset_blanks_rem <- function(ps,var,samples,prop) { #need the ps object, the variable name/column name of the metadata column with your defined samples, the selected samples that are blanks, and whether you want to remove the taxa found in the blanks from the other samples by proportion, abundance, or completely
  if (missing(prop)) {prop <- "all"}
  varlist <- as.character(get_variable(ps,var)) #list the components of the defined variable
  notblasamples <- varlist[!varlist %in% samples] #keep the components that are NOT blanks
  blasamples <- varlist[varlist %in% samples]
  
  newsampledata <- filter(as.data.frame(as.matrix(sample_data(ps))), #extract the metadata from your phyloseq object as a data frame
                          .data[[var]]%in%notblasamples #tell R we are searching for 'var' inside the data frame, then we want all of NON blank samples
                          )
  blametadata <- filter(as.data.frame(as.matrix(sample_data(ps))), #extract the metadata from your phyloseq object as a data frame
                          .data[[var]]%in%blasamples #tell R we are searching for 'var' inside the data frame, then we want all of blank samples
                          )
  #now we have two data frames, one of the metadata for the ps object we are going to keep, and one with the information for the blank samples
  blataxa <- prune_samples(sample_names(ps)%in%row.names({{blametadata}}),
                           ps) %>% 
    prune_taxa(taxa_sums(.)>0,.) %>% prune_samples(sample_sums(.)>0,.) %>%
    taxa_names(.) #results in a list of the taxa names that were found in the blanks
  
  #new.ps <- ps 
  
  #if(prop=="Proportion" | prop=="prop"){}
  if(prop=="ALL" | prop=="all" | is.na(prop) | missing(prop)) {
    new.ps <- ps #make a new ps object so we aren't editing the old one
    new.ps <- prune_taxa(!taxa_names(new.ps) %in% blataxa, new.ps) 
    sample_data(new.ps) <- sample_data(newsampledata) #assign our new sample data (sans blanks) to the new ps object
    new.ps <- new.ps %>% prune_taxa(taxa_sums(.)>0,.) #remove any taxa that were only in the blanks; I think we don't need to do this but leaving it in in case we have issues downstream
    new.ps <- prune_samples(sample_sums(new.ps)>0,new.ps)
    }
  #else if(prop=="Abundance" | prop=="abund") {
  #  #need to remove those taxa_names from the original ps object
  #do something with otu table 
  #  sample_data(new.ps) <- sample_data(newsampledata) #assign our new sample data (sans blanks) to the new ps object
  #  #new.ps <- 
  #}
  else {
    new.ps <- ps
    print("Available transformations to remove blanks include 'abund' (Abundance; remove the absolute number of the ASV found in the blank from each sample), 'all' (All; remove all the ASVs found in the blanks from each sample). No input will result in all ASVs found in defined blanks being removed from the samples")
    }
   new.ps %>% prune_samples(sample_sums(.)>0,.) #return new.ps
}

TRANSFORM <- function(ps,TRANSFORMATION,abund_cutoff=NULL,mean_cutoff=NULL) {
  if (TRANSFORMATION == "none") {
    ps %>% 
      prune_samples(sample_sums(.) > 0,.) %>% 
      prune_taxa(taxa_sums(.) > 0,.)
  } 
  else if (TRANSFORMATION == "tax_count") {
    ps %>% filter_taxa(function(x)sum(x>0)>=abund_cutoff,prune=TRUE)
  } 
  else if (TRANSFORMATION == "tax_rel") {
    ps %>% transform_sample_counts(function(x) x/sum(x)) %>% 
      filter_taxa(function(x){mean(x)>=mean_cutoff},prune=TRUE)
  }
  else if (TRANSFORMATION == "log") {
    ps %>% 
      prune_samples(sample_sums(.) > 0,.) %>% 
      transform_sample_counts(., function(x) log(1 + x)) %>% 
      prune_samples(sample_sums(.) > 0,.)
  }
  else if (TRANSFORMATION == "rank") {
    ps %>% 
      prune_samples(sample_sums(.) > 0,.) %>% 
      transform_sample_counts(.,rank,ties.method="min") %>% 
      prune_samples(sample_sums(.) > 0,.)
  }
  else if (TRANSFORMATION == "presence") {
    ps %>% 
      prune_samples(sample_sums(.) > 0,.) %>% 
      transform_sample_counts(., function(abund) 1*(abund > 0)) %>% 
      prune_samples(sample_sums(.) > 0,.)
  }
  else if (TRANSFORMATION == "relabund" | TRANSFORMATION == "relative") {
    ps %>% 
      prune_samples(sample_sums(.) > 0,.) %>% 
      transform_sample_counts(., function(x) x/sum(x)) %>% 
      prune_samples(sample_sums(.) > 0,.) 
  }
  else { print("Available transformations include 'Log', 'Rank', 'Relative' (aka Relative abundance), and 'Presence' (aka Presence/Absence)") }
  
}
