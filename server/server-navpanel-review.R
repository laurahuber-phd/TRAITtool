source("functions/module-readtable.R")

tableServer(id='meta_table',
            editable=TRUE,
            data=MetadataTable 
) 
tableServer(id='otu_table',
            data=OTUTable 
) 
tableServer(id='tax_table',
            data=TaxTable
) 