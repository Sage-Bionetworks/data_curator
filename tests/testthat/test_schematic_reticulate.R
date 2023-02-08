context("schematic reticulate API wrappers")

good_manifest_path <- system.file("testdata", "example-patient-pass.csv",
                                  package = "datacurator")

# Set up virtualenv and schematic
# Creates synapse_driver and schema_generator objects in global env
setup_synapse_driver()

test_that("storage_projects returns asset view info.", {
  sp <- storage_projects_py(synapse_driver=synapse_driver,
                      access_token = Sys.getenv("SYNAPSE_PAT"))
  xpt <- list(list("syn33715357", "FAIR demo data"))
  expect_equal(sp, xpt)
})

test_that("storage_projects_datasets returns a list of synIDs and dataset names", {
  spd <- storage_projects_datasets_py(synapse_driver = synapse_driver,
                                  project_id = "syn33715357")
  xpt <- list(list("syn35294937", "biospecimen"))
  expect_equal(spd[1], xpt)
})

test_that("storage_dataset_files returns an asset view synID and label", {
  sdf <- storage_dataset_files_py("syn33715357")
  xpt <- list(list("syn33715412", "FAIR demo data/mock dca fileview"))
  expect_equal(sdf, xpt)
})

test_that("manifest_generate creates google sheet url", {
  mg <- manifest_generate_py(title = "test", rootNode = "Patient",
                             datasetId = "syn33715357")
  expect_true(grepl("^https://docs.google.com", mg))
})

test_that("manifest_validate passes and returns an empty list.", {
  mv <- manifest_validate_py(good_manifest_path,
                             "Patient", TRUE, "syn33715412")
  expect_equal(mv, list(list(), list()))
})

test_that("manifest_populate returns a google sheet.", {
  mp <- manifest_populate_py("Test Populate", good_manifest_path, "Patient")
  expect_true(grepl("^https://docs.google.com", mp))
})

test_that("model submit returns a synID upon successful upload", {
  ms <- model_submit_py(schema_generator, good_manifest_path, "syn33715357", "table", FALSE)
  expect_true(grepl("^syn", ms))
})

test_that("synapse_user_profile returns anonymous if not logged in", {
  user <- synapse_user_profile()$userName
  expect_equal("anonymous", user)
})

test_that("get_component_requires works", {
  gcr <- get_component_requirements_py("Biospecimen", as_graph=FALSE)
  expect_equal(gcr, c("Patient", "Biospecimen"))
})
