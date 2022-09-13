# Based on https://raw.githubusercontent.com/Sage-Bionetworks/shiny-module-gallery/feature-47-Docker_file/Dockerfile
FROM rocker/r-base:latest
LABEL maintainer="Anthony anthony.williams@sagebase.org"
RUN apt-get update && apt-get install -y --no-install-recommends \
  sudo \
  libcurl4-gnutls-dev \
  libcairo2-dev \
  libxt-dev \
  libssl-dev \
  libssh2-1-dev \
  libxml2-dev \
  git \
  && rm -rf /var/lib/apt/lists/*
  
# add app and code
WORKDIR /home/app
RUN git clone https://github.com/Sage-Bionetworks/data_curator -b schematic-rest-api

WORKDIR /home/app/data_curator
# set up r packages via renv
# This step takes forever, so do early to avoid invalidating cache with code changes
RUN  R --vanilla -e 'install.packages("renv", repos = "https://cloud.r-project.org/"); renv::restore()' 

RUN addgroup --system app \
&& adduser --system --ingroup app app

#RUN echo "local(options(shiny.port = 8100, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN chown app:app -R /home/app
USER app
EXPOSE 8100
CMD ["R", "--vanilla", "-e", "shiny::runApp('/home/app/data_curator', port=8100, host='0.0.0.0')"]
