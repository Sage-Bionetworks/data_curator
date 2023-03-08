FROM sagebionetworks/shiny-base:release-1.0
LABEL maintainer="Anthony anthony.williams@sagebase.org"

WORKDIR /srv/shiny-server/app
COPY --chown=shiny ./ ./

# set up r packages via renv
RUN Rscript -e 'source("renv/activate.R"); renv::restore(repos="https://packagemanager.rstudio.com/all/__linux__/jammy/latest"); renv::install("./")'

CMD ["./dca_startup.sh"]