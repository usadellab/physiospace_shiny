FROM rocker/r-ver:3.6.3

MAINTAINER Dr. Asis Hallab "a.hallab@fz-juelich.de"

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    git \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libjpeg-dev \
    libxml2-dev \
    libxt-dev \
    xtail \
    wget

# Download and install shiny server
RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown','remotes','BiocManager'), repos='https://cran.rstudio.com/')" && \
    R -e "require(remotes); BiocManager::install('SummarizedExperiment'); remotes::install_github('JRC-COMBINE/PhysioSpaceMethods'); remotes::install_github('JRC-COMBINE/PlantPhysioSpace');" \
    R -e "BiocManager::install(c('dplyr','stringr','png','shinyjs','DT','visNetwork','rintrojs','shinydashboard'))" \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    chown shiny:shiny /var/lib/shiny-server

EXPOSE 3838

RUN rm /srv/shiny-server/*
COPY ./app.R /srv/shiny-server/
COPY ./carouselPanel.R /srv/shiny-server/
COPY shiny-server.sh /usr/bin/shiny-server.sh
COPY ./www /srv/shiny-server/www
RUN chmod u+x /usr/bin/shiny-server.sh

RUN chown shiny.shiny /srv/shiny-server/

CMD ["/usr/bin/shiny-server.sh"]
