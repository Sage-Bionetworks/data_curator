FROM ghcr.io/afwillia/shiny-base:release-0.3
LABEL maintainer="Anthony anthony.williams@sagebase.org"

WORKDIR /srv/shiny-server/app
COPY --chown=shiny ./ ./

# set up r packages via renv
RUN Rscript -e 'renv::restore(); renv::install("./")'

CMD ["./dca_startup.sh"]
