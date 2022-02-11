# require github cli package - 'gh': https://cli.github.com/manual/

args <- commandArgs(trailingOnly = TRUE)

repo <- basename(Sys.getenv("PWD"))

if (repo %in% c("schematic", "data-models")) stop("You are not in the data curator folder !!!")

secret_lists <- yaml::yaml.load_file(toString(args[1]))$definitions
message("Assuming all definition files are stored in the schematic folder ...")

for (f in c("synapse_config", "creds_path", "service_acct_creds")) {
  system(
    sprintf(
      "gh secret set %s < %s",
      toupper(paste0("schematic_", f)), file.path("schematic", secret_lists[f])
    )
  )
}

# token.pickle is a binary file
# If you want to manually add to the secret:
# 1. copy below code snippet and run it in the terminal
# 2. copy content from 'token.pickle.b64' to your github secret
system(
  "
  less schematic/token.pickle | base64 --wrap=0 > schematic/token.pickle.b64;
  gh secret set SCHEMATIC_TOKEN_PICKLE < schematic/token.pickle.b64;
  "
)
unlink("schematic/token.pickle.b64")