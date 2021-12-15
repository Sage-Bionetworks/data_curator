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
"rsconnect")

gh <- "dreamRs/shinypop"

install.packages(cran)
# auth_token=NULL only works for public repo's
remotes::install_github(gh, auth_token=NULL)
