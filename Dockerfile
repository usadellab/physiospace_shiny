FROM r-base:3.5.1

MAINTAINER Dr. Asis Hallab "a.hallab@fz-juelich.de"

## Install dependencies and Download and install shiny server

## See https://www.rstudio.com/products/shiny/download-server/ for the
## instructions followed here.

RUN apt-get update && apt-get dist-upgrade -y && \
    apt-get install -y -t unstable libssl-dev libxml2-dev \
    sudo gdebi-core pandoc pandoc-citeproc libcurl4-gnutls-dev \
    libcairo2-dev/unstable libxt-dev git subversion && \
    apt-get autoremove -y && \
    wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    R -e "install.packages(c('shiny', 'rmarkdown', 'devtools'), repos='https://cran.rstudio.com/')" && \
    R -e "require(devtools); devtools::install_bioc('BiocInstaller'); devtools::install_bioc('SummarizedExperiment'); devtools::install_github('JRC-COMBINE/PhysioSpaceMethods'); devtools::install_github('JRC-COMBINE/HumanPhysioSpace'); devtools::install_github('JRC-COMBINE/PlantPhysioSpace');" \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    rm -rf /var/lib/apt/lists/*

EXPOSE 3838

RUN rm /srv/shiny-server/*
COPY ./app.R /srv/shiny-server/
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod u+x /usr/bin/shiny-server.sh

## Uncomment the line below to include a custom configuration file. You can download the default file at
## https://raw.githubusercontent.com/rstudio/shiny-server/master/config/default.config
## (The line below assumes that you have downloaded the file above to ./shiny-customized.config)
## Documentation on configuration options is available at
## http://docs.rstudio.com/shiny-server/

# COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

CMD ["/usr/bin/shiny-server.sh"]
