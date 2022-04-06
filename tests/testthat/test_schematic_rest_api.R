context("test schematic rest api wrappers")

### Test that schematic server is online. Make sure schem_url matches the actual
### schematic server URL https://github.com/Sage-Bionetworks/schematic/tree/develop/api
### If not available, skip these tests.

schem_url <- "http://127.0.0.1:3001/"
ping <- try(httr::GET(schem_url), silent = TRUE)
skip_it <- function(skip=ping) {
  if (inherits(ping, "try-error")) skip(sprintf("schematic server URL unavailable (%s). Is it running locally?", schem_url)) #nolint
}

pass_csv <- system.file("testdata", "HTAN-Biospecimen-Tier-1-2-pass.csv",
                        package = "datacurator")
fail_csv <- system.file("testdata", "HTAN-Biospecimen-Tier-1-2-fail.csv",
                        package = "datacurator")

test_that("manifest_generate returns a URL if sucessful", {
  skip_it()
  
  url <- manifest_generate(title="Test biospecimen", data_type="Biospecimen",
                  dataset_id="syn20977135")
  expect_true(grepl("^https://docs.google", url))
})

test_that("manifest_populate returns a google sheet link with records filled", {
  skip_it()
  req <- manifest_populate(data_type="Biospecimen", title="Example",
                           csv_file = pass_csv)
})
  
test_that("manifest_validate passes and fails correctly", {
  skip_it()
  
  pass <- manifest_validate(data_type="Biospecimen", csv_file=pass_csv)
  expect_identical(pass, list())
  
  fail <- manifest_validate(data_type="Biospecimen", csv_file=fail_csv)
  expect_true(length(unlist(fail)) > 0L)
})

test_that("model_submit successfully uploads to synapse", {
  skip_it()
  
  submit <- model_submit(data_type="Biospecimen", dataset_id="syn20977135",
                      input_token=Sys.getenv("SYNAPSE_PAT"), csv_file=pass_csv)
  expect_true(submit)
})

test_that("storage_project_datasets returns available datasets", {
  skip_it()
  storage_project_datasets(asset_view="syn23643253",
                           project_id="syn26251192",
                           input_token=Sys.getenv("SYNAPSE_PAT"))
})

test_that("storage_projects returns available projects", {
  skip_it()
  storage_projects(asset_view="syn23643253",
                   input_token=Sys.getenv("SYNAPSE_PAT"))
})

test_that("storage_dataset_files returns files", {
  storage_dataset_files(asset_view = "syn23643253",
                        dataset_id = "syn23643250",
                        input_token=Sys.getenv("SYNAPSE_PAT"))
})
