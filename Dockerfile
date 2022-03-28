# get shiny server and R from the rocker project
FROM rocker/shiny-verse:latest

# system libraries
# Try to only install system libraries you actually need
# Package Manager is a good resource to help discover system deps
RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev
  

# install R packages required 
# Change the packages list to suit your needs
RUN R -e 'install.packages(c("tidyverse","DT"), repos="http://cran.rstudio.com/")'


# copy the app directory into the image
COPY ./*.R /srv/shiny-server/
COPY ./*.Rdata /srv/shiny-server/

# run app
CMD ["/usr/bin/shiny-server"]
