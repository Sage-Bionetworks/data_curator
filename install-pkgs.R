cran <- c(
"ellipsis",
"shiny",
"httr",
"rjson",
"yaml",
"shinyjs",
"dplyr",
"shinythemes",
"shinydashboard",
"stringr",
"DT",
"jsonlite",
"reticulate",
"ggplot2",
"purrr",
"plotly",
"shinydashboardPlus",
"waiter",
"readr",
"sass",
"remotes",
"rsconnect",
"png")

gh <- "dreamRs/shinypop"

# The binary package distributions from R Studio dramatically speed up installation time
# For Ubuntu 18.04 (Bionic) it's https://packagemanager.rstudio.com/all/__linux__/bionic/latest
# For Ubuntu 20.04 (Focal)  it's https://packagemanager.rstudio.com/all/__linux__/focal/latest
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest", getOption("repos")))
install.packages(cran)
remotes::install_github(gh)
