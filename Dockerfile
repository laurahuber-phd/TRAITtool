# Base R Shiny image
FROM rocker/shiny

# Make a directory in the container
RUN mkdir /home/shiny-app

# Install R dependencies
RUN R -e "install.packages('devtools',dependencies=TRUE)"
RUN R -e "install.packages('BiocManager',dependencies=TRUE)"
RUN R -e "require(devtools)"

RUN R -e "devtools::install_version('cowplot',version='1.1.3',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('kableExtra',version='1.4.0',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('RColorBrewer',version='1.1-3',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('colourpicker',version='1.3.0',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"

RUN R -e "devtools::install_version('shinyjs',version='2.1.0',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('shinyjqui',version='0.4.1',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('shinyBS',version='0.61.1',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('shinyFeedback',version='0.4.0',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('shinyWidgets',version='0.9.0',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('shinycssloaders',version='1.1.0',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"

RUN R -e "devtools::install_version('stringr',version='1.5.1',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('rmarkdown',version='2.29',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('markdown',version='2.0',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('knitr',version='1.49',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"

RUN R -e "devtools::install_version('vroom',version='1.6.5',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('DT',version='0.33',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('dplyr',version='1.1.4',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"

RUN R -e "devtools::install_version('htmltools',version='0.5.8.1',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"    
RUN R -e "devtools::install_version('vegan',version='2.6.4',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('lattice',version='0.22.7',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"       
RUN R -e "devtools::install_version('permute',version='0.9.7',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "BiocManager::install('phyloseq')"     
RUN R -e "devtools::install_version('ggplot2',version='3.5.2',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('bsicons',version='0.1.2',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"        
RUN R -e "devtools::install_version('bslib',version='0.9.0',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('shiny',version='1.10.0',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "devtools::install_version('tinytex',version='0.57',type='source',repos = 'http://cran.us.r-project.org',dependencies=TRUE)"
RUN R -e "tinytex::install_tinytex()"


# Copy the Shiny app code
COPY app.R /home/shiny-app/app.R
COPY . /home/shiny-app/

# Expose the application port
EXPOSE 8180

# Run the R Shiny app
#CMD Rscript /home/shiny-app/app.R
CMD ["R", "-e", "shiny::runApp('/home/shiny-app')"]