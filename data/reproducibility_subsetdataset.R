library(dplyr)
library(phyloseq)

load("/Users/lauragivens/Desktop/R/eDNA_methods_chapter_files/RData/MockCommTests.Rdata")

set.seed(10)
# subsample ps object
subset.ps <- merge_phyloseq(
  (prune_samples(sample_sums(Mock.ps)<1000,Mock.ps) %>% prune_taxa(taxa_sums(.)>0,.)),
  rarefy_even_depth(Mock.ps,sample.size = 1000)
)

write.csv(otu_table(subset.ps),"/Users/lauragivens/Desktop/R/IntroRShiny/DNAbase/data/otu_table.csv")
write.csv(tax_table(subset.ps),"/Users/lauragivens/Desktop/R/IntroRShiny/DNAbase/data/tax_table.csv")

