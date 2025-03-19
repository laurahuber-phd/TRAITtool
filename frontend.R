library(tidyverse)
library(gridExtra)
library(Biostrings)
library(dada2)

#start with cutadapt
# need to add this functionality in later I think

#move to DADA2 pipeline tutorial 
#official here
#https://benjjneb.github.io/dada2/tutorial.html
# pipeline walkthrough https://www.bioconductor.org/packages/devel/bioc/vignettes/dada2/inst/doc/dada2-intro.html
#samples must be demultiplexed
#primers,adapters, non-biological nucleotides have been removed
#f and r fastq files contain reads in matched order