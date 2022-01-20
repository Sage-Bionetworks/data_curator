# require github cli package - 'gh': https://cli.github.com/manual/

args <- commandArgs(trailingOnly = TRUE)

secret_lists <- yaml::yaml.load_file(toString(args[1]))$definitions

for (f in c("synapse_config", "creds_path", "service_acct_creds")) {
  system(
    sprintf(
      "gh secret set %s < %s",
      toupper(paste0("schematic_", f)), secret_lists[f]
    )
  )
}

# token.pickle is a binary file
# If you want to manually add to the secret:
# 1. copy below code snippet and run it in the terminal
# 2. copy content from 'token.pickle.b64' to your github secret
system(
  "
  less token.pickle | base64 --wrap=0 > token.pickle.b64;
  gh secret set SCHEMATIC_TOKEN_PICKLE < token.pickle.b64;
  "
)
unlink("token.pickle.b64")