FROM sagebionetworks/shiny-base:release-1.1
LABEL maintainer="Anthony anthony.williams@sagebase.org"

USER root
RUN apt-get update
RUN apt-get install -y libxml2 libglpk-dev libicu-dev libicu70
USER shiny

WORKDIR /srv/shiny-server/app
COPY --chown=shiny ./ ./

# set up r packages via renv
RUN Rscript -e 'renv::restore(); renv::install("./")'

CMD ["./dca_startup.sh"]
