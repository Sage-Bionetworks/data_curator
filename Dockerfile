# Based on https://github.com/Sage-Bionetworks/shiny-module-gallery/blob/feature-47-Docker_file/Dockerfile
# and https://github.com/Sage-Bionetworks/kubernetes-deployments/blob/add-shiny/shiny/example_shiny/Dockerfile
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
    && rm -rf /var/lib/apt/lists/*

# add app and code
WORKDIR /home/app
COPY renv.lock ./

# set up r packages via renv
# This step takes forever, so do early to avoid invalidating cache with code changes
RUN  R -e 'install.packages("renv")' \
     && R -e 'renv::restore()'
     
COPY server.R ui.R global.R oauth_config.yml schematic_config.yml ./
ADD www/ www/
ADD functions/ functions/
ADD modules/ modules/
ADD R/ R/

# set up python venv
#RUN apt-get update && apt-get install -y --no-install-recommends \
#    pip \
#    python3-venv \
#    && python3 -m venv .venv \
#    && chmod 755 .venv \
#    && . .venv/bin/activate \
#    && pip3 install schematicpy

RUN addgroup --system app \
    && adduser --system --ingroup app app

RUN echo "local(options(shiny.port = 8100, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
RUN chown app:app -R /home/app
USER app
EXPOSE 8100
CMD ["R", "-e", "shiny::runApp('/home/app')"]

