FROM ghcr.io/afwillia/shiny-base:release-update-node
LABEL maintainer="Anthony anthony.williams@sagebase.org"

USER root
RUN apt-get update
RUN apt-get install -y libxml2 libglpk-dev libicu-dev libicu70 curl

# overwrite the default config with our modified copy
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN chmod 777 /etc/shiny-server/shiny-server.conf

# Update node. https://github.com/nodesource/distributions
RUN apt-get remove nodejs
RUN curl -fsSL https://deb.nodesource.com/setup_16.x | sudo -E bash - && apt-get install -y nodejs

USER shiny

WORKDIR /srv/shiny-server/app
COPY --chown=shiny ./ ./

# set up r packages via renv. Use binary lib matching the shiny-base ubuntu version
# to speed up installatioon.
RUN Rscript -e 'renv::restore(repos="https://packagemanager.rstudio.com/all/__linux__/jammy/latest"); renv::install("./")'

CMD ["./dca_startup.sh"]
