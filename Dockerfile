FROM sagebionetworks/shiny-base:release-1.0
LABEL maintainer="Anthony anthony.williams@sagebase.org"

WORKDIR /srv/shiny-server/app
COPY --chown=shiny ./ ./

# set up r packages via renv
RUN Rscript -e 'install.packages(c("remotes", "renv"), repos = "https://cloud.r-project.org/"); source("renv/activate.R"); renv::restore(); remotes::install_github("https://github.com/sage-bionetworks/data_curator", ref="schematic-rest-api")'

CMD ["./dca_startup.sh"]

