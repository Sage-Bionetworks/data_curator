# Based on https://github.com/Sage-Bionetworks/shiny-module-gallery/blob/feature-47-Docker_file/Dockerfile
# and https://github.com/Sage-Bionetworks/kubernetes-deployments/blob/add-shiny/shiny/example_shiny/Dockerfile
FROM rocker/r-ver:latest
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
RUN git clone https://github.com/sage-bionetworks/data_curator --branch schematic-rest-api
WORKDIR data_curator
#COPY renv.lock ./

# set up r packages via renv
# This step takes forever, so do early to avoid invalidating cache with code changes
RUN R --vanilla -e 'install.packages("renv", repos = "https://cloud.r-project.org/"); renv::restore(); remotes::install_local("./")'

RUN addgroup --system app \
    && adduser --system --ingroup app app

#RUN echo "local(options(shiny.port = 8100, shiny.host = '0.0.0.0'))" > /usr/lib/R/etc/Rprofile.site
#RUN echo "local(options(shiny.port = 8100, shiny.host = '0.0.0.0'))" > .Rprofile
RUN chown app:app -R /home/app
USER app
EXPOSE 8100
CMD ["R", "--vanilla", "-e", "shiny::runApp('/home/app/data_curator', port=8100, host='0.0.0.0')"]

