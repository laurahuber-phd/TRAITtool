output$viscolors <- renderPlot({
  ggplot(iris, aes(x=Sepal.Length, y=Sepal.Width, color=Sepal.Width)) + geom_point(size=6) 
})


output$psog <- renderPrint({ps.og()})
output$pstext <- renderPrint({ps.norm()})
output$rnames_return <- renderPrint({rnames()})
output$metavar_return <- renderPrint({Meta_vars()})

# these show up in ui-navpanel-help.R
## sample names
output$asv_colnames <- renderPrint({
  req(!is.null(OTUTable$x))
  colnames(OTUTable$x) %>% sort()
})
output$meta_rownames <- renderPrint({
  req(!is.null(MetadataTable$x))
  MetadataTable$x[[1]] %>% sort()
})
### match
output$helpsamplenames <- renderPrint({
int <- intersect(colnames(OTUTable$x),MetadataTable$x[[1]])
paste("Number of sample names in common:",length(int), 
       "Number of samples in metadata table: ", nrow(MetadataTable$x), 
       "Number of samples in feature table", ncol(OTUTable$x),
       "Names in common: ", list(int))
})

## taxa names
output$asv_rownames <- renderPrint({
  req(!is.null(OTUTable$x))
  OTUTable$x[[1]] %>% sort()
})
output$tax_rownames <- renderPrint({
  req(!is.null(TaxTable$x))
  TaxTable$x[[1]] %>% sort()
})
### match 
output$helpseqnames <- renderPrint({
  int <- intersect(OTUTable$x[[1]],TaxTable$x[[1]])
  paste("Number of taxa names in common:",length(int), 
         "Number of taxa in taxonomy table: ", nrow(TaxTable$x), 
         "Number of taxa in feature table", nrow(OTUTable$x),
         "Names in common: ", list(int))
})