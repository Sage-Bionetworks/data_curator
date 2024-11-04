FROM ghcr.io/afwillia/shiny-base:release-1.13

# add version tag as a build argument
ARG DCA_VERSION

ENV DCA_VERSION=$DCA_VERSION

USER root
RUN apt-get update
RUN apt-get install -y libxml2 libglpk-dev libicu-dev curl

# overwrite the default config with our modified copy
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN chmod 777 /etc/shiny-server/shiny-server.conf

USER shiny

WORKDIR /srv/shiny-server/app
COPY --chown=shiny ./ ./

# set up r packages via renv. Use binary lib matching the shiny-base ubuntu version
# to speed up installatioon.
RUN Rscript -e 'renv::restore(repos="https://packagemanager.posit.co/cran/__linux__/noble/latest"); renv::install("./")'

CMD ["./dca_startup.sh"]
