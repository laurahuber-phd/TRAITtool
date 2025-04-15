source("module-readtable.R")

tableServer(id='meta_table',
            editable=TRUE,
            #data=MetadataTable(x) #Error in MetadataTable(x) : could not find function "MetadataTable"
            data=MetadataTable #doesnt crash the app but won't upload
            #data=MetadataTable$x
            #data=reactive(MetadataTable$x)
) 
tableServer(id='otu_table',
            data=OTUTable #doesnt crash the app but won't upload
) 
tableServer(id='tax_table',
            data=TaxTable #doesnt crash the app but won't upload
) 