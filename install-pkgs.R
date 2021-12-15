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

# We will only install public repo's from Github
Sys.unsetenv("GITHUB_PAT")
remotes::install_github(gh)
