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
options(repos = c(REPO_NAME = "https://packagemanager.rstudio.com/all/__linux__/bionic/latest", getOption("repos")))
install.packages(cran)
remotes::install_github(gh)
