# require github cli package - 'gh': https://cli.github.com/manual/

args <- commandArgs(trailingOnly = TRUE)

repo <- basename(Sys.getenv("PWD"))

# Read OAuth credential
oauth_client <- yaml::yaml.load_file(args[1])
client_id <- toString(oauth_client$CLIENT_ID)
client_secret <- toString(oauth_client$CLIENT_SECRET)

# Read credential files locations
secret_lists <- yaml::yaml.load_file(args[2])$definitions
# Change working directory to the directory of config file
setwd(dirname(args[2]))

# Validation
# Ensure client credentials are not empty
if (is.null(client_id) || nchar(client_id) == 0) stop(args[1], " is missing CLIENT_ID")
if (is.null(client_secret) || nchar(client_secret) == 0) stop(args[1], " is missing CLIENT_SECRET")
# Ensure it is not schematic or data-model repos
if (!grepl("data[-|_]curator", repo)) stop("You are not in the data curator folder !!!")
# Ensure all credential files exist
diff <- setdiff(c("synapse_config", "creds_path", "token_pickle", "service_acct_creds"), names(secret_lists))
if (length(diff) > 0) {
  stop(paste(diff, "not found in the config file", collapse = "\n"))
}

# Add OAuth secrets via gh CLI
system(
  sprintf(
    "
    gh secret set OAUTH_CLIENT_ID --body %s;
    gh secret set OAUTH_CLIENT_SECRET --body %s;
    ",
    sQuote(client_id), sQuote(client_secret)
  )
)

# Set secret names SCHEMATIC_*
secret_names <- toupper(paste0("schematic_", names(secret_lists)))

# Add schematic secrets via gh CLI
for (i in seq_along(secret_lists)) {
  if (!file.exists(toString(secret_lists[i]))) stop(secret_lists[i], " not found")
  if (secret_names[i] == "SCHEMATIC_TOKEN_PICKLE") {
    # token.pickle is a binary file
    # If you want to manually add to the secret:
    # 1. copy below code snippet and run it in the terminal
    # 2. copy content from 'token.pickle.b64' to your github secret
    token_str <- base64enc::base64encode(secret_lists[i])
    system(
      sprintf(
        "
        gh secret set %s --body %s;
        ",
        secret_names[i], token_str
      )
    )
  } else {
    system(
      sprintf(
        "gh secret set %s < %s",
        secret_names[i], secret_lists[i]
      )
    )
  }
}

cat("
                <<<<<<<< Important >>>>>>>>
Please manually add 'REPO_PAT', 'RSCONNECT_USER', 'RSCONNECT_SECRET', 'RSCONNECT_TOKEN'
check 'shinyapps_deploy.md' for details\n")