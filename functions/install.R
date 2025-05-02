# This script should be the first thing run 
# It will check that all dependencies are installed to ensure the app works correctly

# check R version
R_req_version = "4.2.3" #version Shiny app was built under 
R_comp_version = paste0(R.Version()$major, ".", R.Version()$minor) #current version used

if(compareVersion(R_req_version, R_comp_version) > 0){
  stop("You do not have the minimum required version of R installed.\n", 
       "Go to http://cran.r-project.org/ and update your version of R.")
}
if(compareVersion(R_req_version, R_comp_version) < 0){
       paste0("Note: you are using a higher version of R than the package was built under: ",R_req_version)
}

if('renv'%in%rownames(installed.packages())){require(renv)} else {install.packages('renv')}
if('devtools'%in%rownames(installed.packages())){require(devtools)} else {install.packages('devtools')}

#install.packages('devtools')
#install.packages("renv")
#install.packages("BiocManager") #this will install or update BiocManager to the latest versions  

#renv::restore()
#renv::install()

# list of required packages and versions  
req_packages <- c(
  cowplot = "1.1.3",
  kableExtra = "1.4.0",     
  RColorBrewer = "1.1-3",
  shinyjs = "2.1.0",        
  stringr = "1.5.1",
  rmarkdown = "2.29",       
  markdown = "2.0",
  knitr = "1.49",           
  colourpicker = "1.3.0",
  shinyjqui = "0.4.1",      
  shinyBS = "0.61.1",
  shinyFeedback = "0.4.0",  
  vroom = "1.6.5", 
  DT = "0.33",              
  shinyWidgets = "0.9.0",
  shinycssloaders = "1.1.0",
  dplyr = "1.1.4",
  htmltools = "0.5.8.1",    
  vegan = "2.6.4",
  lattice = "0.22.7",       
  permute = "0.9.7",
  phyloseq = "1.42.0",     
  ggplot2 = "3.5.2",
  bsicons = "0.1.2",        
  bslib = "0.9.0",
  shiny = "1.10.0",
  tinytex='0.57'
)
pkgnm <- names(req_packages)

for (i in 1:length(pkgnm)){
  if (!pkgnm[i]%in%rownames(installed.packages())) {
    devtools::install_version(package=names(req_packages[i]),
                              version=req_packages[i],
                              type='source',repos = "http://cran.us.r-project.org", upgrade="never"
                              )
    require(pkgnm[i],character.only = TRUE)
    if(pkgnm[i]=="tinytex") {tinytex::install_tinytex()}
    }
  if (packageVersion(pkgnm[i])<req_packages[i]) {
    devtools::install_version(package=names(req_packages[i]),
                              version=req_packages[i],
                              type='source',repos = "http://cran.us.r-project.org", upgrade="never"
                              )
    require(pkgnm[i],character.only = TRUE)
    if(pkgnm[i]=="tinytex") {tinytex::install_tinytex()}
    }
  else {require(pkgnm[i],character.only = TRUE)
        paste0("Package ",pkgnm[i], "was already installed")
        }
}

#package_version("")>"version"
#may need to follow this link
#https://forum.posit.co/t/shiny-server-renv/71879/4
   