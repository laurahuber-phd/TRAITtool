# Trophic Rapid Assessment and Integration Toolkit (TRAITtool)  

TRAITtool is an RShiny analysis toolkit for biodiversity survey data to draw a connection between species and the habitats they rely on using natural history information. It provides analyses that can be used directly with little bioinformatic knowledge needed.

TRAITtool has two components:  
1. Trait assignment - this takes a table of taxonomic assignments and queries taxa against the FishBase and/or SeaLifeBase databases. This returns a table of traits associated with the taxa in your dataset, that can then be downloaded or integrated. 
2. Trait visualization - after assigning trait-level characteristics to your data, you also have the option to visualize these traits using a set of predetermined figures. These figures can be updated with different visual criteria to reflect your needs and downloaded.   
You will also have the ability to download a report that shares what dataset manipulations were made in generating these visualizaitons, which can then be used for reproducibility if you upload data into the tool again or for future reporting.  


To use TRAITtool, clone this repository. All required files to run TRAITtool on your personal machine should be included in the repository.   

# Overview  
1. Upload biodiversity survey count, metadata, and taxonomy files in tabular format
2. Use the built in tool function to assign natural history trait information to each species
3. Decide what samples or taxonomic groups to focus on for your analysis  
4. Determine how to group data based on your metadata file and plot
5. Make any necessary visual changes or fine tuned updates to plots
6. Save figures, files, and generate a report to record what data manipulations were done     









# This was built using the following package versions:   
 R version 4.2.3 (2023-03-15)
  
## attached packages:
cowplot_1.1.3         
kableExtra_1.4.0     
knitr_1.50            
rmarkdown_2.29       
RColorBrewer_1.1-3    
rfishbase_5.0.3      
shinyjs_2.1.0         
markdown_2.0         
colourpicker_1.3.0    
shinyjqui_0.4.1      
shinyBS_0.61.1        
shinyFeedback_0.4.0  
vroom_1.6.5           
DT_0.33              
shinyWidgets_0.9.0    
shinycssloaders_1.1.0
lubridate_1.9.4       
forcats_1.0.0        
stringr_1.6.0         
dplyr_1.1.4          
purrr_1.2.2           
readr_2.1.5          
tidyr_1.3.1           
tibble_3.3.1         
tidyverse_2.0.0       
htmltools_0.5.8.1    
vegan_2.6-4           
lattice_0.22-7       
permute_0.9-7         
phyloseq_1.41.1      
bsicons_0.1.2         
plotly_4.10.4        
ggplot2_3.5.2        


bslib_0.11.0         
 [35] shiny_1.10.0   
