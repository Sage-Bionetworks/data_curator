
gh <- "dreamRs/shinypop"

# We will only install public repo's from Github
Sys.unsetenv("GITHUB_PAT")
remotes::install_github(gh, auth_token=NULL)
