context("test schatic rest api wrappers")

pass_csv <- system.file("testdata", "HTAN-Biospecimen-Tier-1-2-pass.csv",
                        package = "datacurator")
fail_csv <- system.file("testdata", "HTAN-Biospecimen-Tier-1-2-fail.csv",
                        package = "datacurator")

test_that("manifest_generate returns a URL if sucessful", {
  url <- manifest_generate(title="Test biospecimen", data_type="Biospecimen",
                  dataset_id="syn20977135")
  expect_true(grepl("^http", url))
})
  
test_that("manifest validate passes and fails correctly", {
  pass <- manifest_validate(data_type="Biospecimen", csv_file=pass_csv)
  expect_identical(pass, list())
  
  fail <- manifest_validate(data_type="Biospecimen", csv_file=fail_csv)
  expect_true(length(unlist(fail)) > 0L)
})

test_that("model submit successfully uploads to synapse", {
  submit <- model_submit(data_type="Biospecimen", dataset_id="syn20977135",
                      input_token=Sys.getenv("SYNAPSE_PAT"), csv_file=pass_csv)
  expect_true(submit)
})

