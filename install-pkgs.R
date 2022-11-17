
cran <- c(
  "ellipsis==0.3.2",
  "shiny==1.7.1",
  "fontawesome==0.3.0",
  "httr==1.4.2",
  "yaml==2.2.1",
  "shinyjs==2.1.0",
  "dplyr==1.0.7",
  "shinythemes==1.2.0",
  "shinydashboard==0.7.2",
  "stringr==1.4.0",
  "DT==0.20",
  "jsonlite==1.7.3",
  "reticulate==1.23",
  # "shinydashboardPlus==2.0.3",
  "waiter==0.2.5",
  "readr==2.1.1",
  "sass==0.4.1",
  "remotes==2.4.2",
  "rsconnect==0.8.25",
  "png==0.1.7",
  "tidyr==1.1.4"
)
gh <- c(
  "dreamRs/shinypop",
  # switch back to use cran install 'shinydashboardPlus'
  # once they make a release to fix icons
  # https://github.com/RinteRface/shinydashboardPlus
  "RinteRface/shinydashboardPlus"
)

# The binary package distributions from R Studio dramatically speed up installation time
# For Ubuntu 18.04 (Bionic) it's https://packagemanager.rstudio.com/all/__linux__/bionic/latest
# For Ubuntu 20.04 (Focal)  it's https://packagemanager.rstudio.com/all/__linux__/focal/latest
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/focal/latest", getOption("repos")))

install.packages("remotes")
invisible(
  lapply(strsplit(cran, "=="), function(cran_pkg) {
    remotes::install_version(cran_pkg[1], version = cran_pkg[2])
  })
)
remotes::install_github(gh)
