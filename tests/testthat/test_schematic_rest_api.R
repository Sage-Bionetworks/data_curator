context("test schematic rest api wrappers")

### Test that schematic server is online. Make sure schematic_url matches the actual
### schematic server URL https://github.com/Sage-Bionetworks/schematic/tree/develop/api
### If not available, skip these tests.

schematic_url <- "https://schematic-dev.api.sagebionetworks.org"
ping <- try(httr::GET(schematic_url), silent = TRUE)
skip_it <- function(skip=ping) {
  if (inherits(ping, "try-error")) skip(sprintf("schematic server URL unavailable (%s).", schematic_url)) #nolint
}

schema_url <- "https://raw.githubusercontent.com/Sage-Bionetworks/data-models/main/example.model.jsonld"
pass_csv <- system.file("testdata", "HTAN-Biospecimen-Tier-1-2-pass.csv",
                        package = "datacurator")
fail_csv <- system.file("testdata", "HTAN-Biospecimen-Tier-1-2-fail.csv",
                        package = "datacurator")

test_that("manifest_generate returns expected output", {
  skip_it()
  
  url <- file.path(schematic_url, "v1/manifest/generate")
  
  # output_format = google_sheet - expect url
  url <- manifest_generate(
    url = url,
    schema_url = schema_url, 
    access_token = Sys.getenv("SYNAPSE_PAT"),
    title = "Test biospecimen", 
    data_type = "Biospecimen",
    use_annotations = FALSE,
    dataset_id = "syn33715357", 
    asset_view = "syn33715412",
    output_format = "google_sheet")
  
  expect_true(grepl("^https://docs.google", url))
  
  # output_format = excel - expect not to error
  # TODO: Would need to update what the api function returns to check specifically for excel sheet
  expect_no_error(
    manifest_generate(
    url=url,
    schema_url = schema_url, 
    access_token = Sys.getenv("SYNAPSE_PAT"),
    title = "Test biospecimen", 
    data_type = "Biospecimen",
    use_annotations = FALSE,
    dataset_id = "syn33715357", 
    asset_view = "syn33715412",
    output_format = "excel")
    )
})

test_that("manifest_generate errors as expected", {
  skip_it()
  
  url <- file.path(schematic_url, "v1/manifest/generate")
  
  # expect dataframe to error out
  # not supported at this time
  expect_error(
    manifest_generate(
      url = url,
      schema_url = schema_url,
      access_token = Sys.getenv("SYNAPSE_PAT"),
      title = "Test biospecimen",
      data_type = "Biospecimen",
      use_annotations = FALSE,
      dataset_id = "syn33715357",
      asset_view = "syn33715412",
      output_format = "dataframe")
  )
  
  # bad pat
  expect_error(
    manifest_generate(
      url = url,
      schema_url = schema_url,
      access_token = "bad pat",
      title = "Test biospecimen",
      data_type = "Biospecimen",
      use_annotations = FALSE,
      dataset_id = "syn33715357",
      asset_view = "syn33715412",
      output_format = "google_sheet")
  )
  
  expect_error(
    manifest_generate(
      url = paste0(url, "typo"),
      schema_url = schema_url,
      access_token = Sys.getenv("SYNAPSE_PAT"),
      title = "Test biospecimen",
      data_type = "Biospecimen",
      use_annotations = FALSE,
      dataset_id = "syn33715357",
      asset_view = "syn33715412",
      output_format = "google_sheet")
  )
  
  expect_error(
    manifest_generate(
      url = url,
      schema_url = paste0(schema_url, "typo"),
      access_token = Sys.getenv("SYNAPSE_PAT"),
      title = "Test biospecimen",
      data_type = "Biospecimen",
      use_annotations = FALSE,
      dataset_id = "syn33715357",
      asset_view = "syn33715412",
      output_format = "google_sheet")
  )
  
})

# TODO: Is there a way to check that the google sheet is populated?
test_that("manifest_populate returns a google sheet link", {
  skip_it()
  req <- manifest_populate(
    url = file.path(schematic_url, "v1/manifest/populate"),
    schema_url = schema_url,
    data_type = "Biospecimen", 
    title = "Example",
    csv_file = pass_csv)
  
  # expect a google doc url
  expect_true(grepl("^https://docs.google", httr::content(req)))
  
})
  
test_that("manifest_validate passes and fails correctly", {
  skip_it()
  
  # expect empth list
  pass <- manifest_validate(url=file.path(schematic_url, "v1/model/validate"),
                            data_type="Biospecimen", file_name=fail_csv,
                            access_token = Sys.getenv("SYNAPSE_PAT"),
                            schema_url = schema_url)
  expect_identical(pass, list(errors = list(), warnings = list()))
  
  # expect list with content
  fail <- manifest_validate(url=file.path(schematic_url, "v1/model/validate"),
                            data_type="Biospecimen", file_name=pass_csv,
                            access_token = Sys.getenv("SYNAPSE_PAT"),
                            schema_url = schema_url)
  expect_true(length(unlist(fail)) > 0L)
})

test_that("model_submit successfully uploads to synapse", {
  skip_it()
  
  submit <- model_submit(url=file.path(schematic_url,"v1/model/submit"),
                         schema_url = schema_url,
                         data_type=NULL, dataset_id="syn20977135",
                         restrict_rules = FALSE, access_token=Sys.getenv("SYNAPSE_PAT"),
                         asset_view="syn33715412", file_name=pass_csv,
                         manifest_record_type="file_only",
                         table_manipulation="replace"
                      )
  expect_true(grepl("^syn", submit))
})

test_that("storage_project_datasets returns a list of datasets", {
  skip_it()
  
  url <- file.path(schematic_url, "v1/storage/project/datasets")
  
  # expect list with projects
  datasets <- storage_project_datasets(
    url = url,
    asset_view="syn23643253",
    project_id="syn26251192",
    access_token=Sys.getenv("SYNAPSE_PAT"))
  
  expect_true(is.list(datasets))
  expect_true(length(datasets) > 0)
  
  # typo in project id expect empty list
  datasets <- storage_project_datasets(
    url = url,
    asset_view="syn23643253",
    project_id="typo",
    access_token=Sys.getenv("SYNAPSE_PAT"))
  
  expect_true(is.list(datasets))
  expect_true(length(datasets) == 0)
})

test_that("storage_project_datasets errors correctly", {
  skip_it()
  
  # bad url
  expect_error(
    storage_project_datasets(
      url = file.path(schematic_url, "bad url"),
      asset_view = "syn23643253",
      project_id = "syn26251192",
      access_token = Sys.getenv("SYNAPSE_PAT")
      )
    )
  
  # typo in asset_view
  expect_error(
    storage_project_datasets(
      url = url,
      asset_view = "typo",
      project_id = "syn26251192",
      access_token = Sys.getenv("SYNAPSE_PAT")
    )
  )
  
  # typo in synapse pat
  expect_error(
    storage_project_datasets(
      url = url,
      asset_view = "typo",
      project_id = "syn26251192",
      access_token = "bad pat"
    )
  )
})

test_that("storage_projects returns a list of projects", {
  skip_it()
  
  # successful call
  projects <- storage_projects(
    url = file.path(schematic_url, "v1/storage/projects"),
    asset_view = "syn23643253",
    access_token = Sys.getenv("SYNAPSE_PAT"))
  
  expect_true(is.list(projects))
  expect_true(length(projects) > 0)
  
})

test_that("storage_projects errors as expected", {
  skip_it()
  
  url <- file.path(schematic_url, "v1/storage/projects")
  
  # typo in url
  expect_error(
    projects <- storage_projects(
      url = file.path(url, "typo"),
      asset_view = "syn23643253",
      access_token = Sys.getenv("SYNAPSE_PAT"))
  )
  
  # typo in asset_view
  expect_error(
    projects <- storage_projects(
      url = url,
      asset_view = "typo",
      access_token = Sys.getenv("SYNAPSE_PAT"))
  )
  
  # invalid access_token
  expect_error(
    projects <- storage_projects(
      url = url,
      asset_view = "syn23643253",
      access_token = "bad pat")
  )
})

test_that("storage_dataset_files returns a list of files", {
  skip_it()
  
  # success
  files <- storage_dataset_files(
    url = file.path(schematic_url, "v1/storage/dataset/files"),
    asset_view = "syn23643253",
    dataset_id = "syn23643250",
    access_token = Sys.getenv("SYNAPSE_PAT"))
  
  expect_true(is.list(files))
  expect_true(length(files) > 0)
})

test_that("storage_dataset_files errors as expected", {
  skip_it()
  
  url <- file.path(schematic_url, "v1/storage/dataset/files")
  
  # bad file path
  expect_error(
    storage_dataset_files(
      url = paste0(url, "typo"),
      asset_view = "syn23643253",
      dataset_id = "syn23643250",
      access_token = Sys.getenv("SYNAPSE_PAT"))
  )
  
  # bad asset_view
  expect_error(
    storage_dataset_files(
      url = url,
      asset_view = "syn23643253333",
      dataset_id = "syn23643250",
      access_token = Sys.getenv("SYNAPSE_PAT"))
  )
  
  # bad dataset_id
  expect_error(
    storage_dataset_files(
      url = url,
      asset_view = "syn23643253",
      dataset_id = "syn2364325000",
      access_token = Sys.getenv("SYNAPSE_PAT"))
  )
  
  # bad PAT
  expect_error(
    storage_dataset_files(
      url = url,
      asset_view = "syn23643253",
      dataset_id = "syn23643250",
      access_token = paste0(Sys.getenv("SYNAPSE_PAT"), "1234"))
  )
})

test_that("model_component_requirements returns list of required components", {
  skip_it()
  
  # Successful call
  reqs <- model_component_requirements(
    url = file.path(schematic_url, "v1/model/component-requirements"),
    schema_url = "https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld",
    source_component = "Patient",
    as_graph = FALSE)
  
  expect_equal(length(reqs), 8L)
  
  # Filepath for url is errant
  # returns an empty list()
  reqs <- model_component_requirements(
    url = file.path(schematic_url, "v1/model/component-requirements-badurl"),
    schema_url = "https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld",
    source_component = "Patient",
    as_graph = FALSE)
  
  expect_equal(length(reqs), 0L)
  
  reqs <- model_component_requirements(
    url = file.path(schematic_url, "v1/model/component-requirements"),
    schema_url = "https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld",
    source_component = "Patient",
    as_graph = TRUE)
  
  #FIXME: Does this output make sense?
  expect_equal(length(reqs), 7L)
})

test_that("model_component_requirements errors as expected", {
  
  # bad schema url
  expect_error(
    model_component_requirements(
      url=file.path(schematic_url, "v1/model/component-requirements"),
      schema_url="https://aaaabad.url.jsonld",
      source_component="Patient",
      as_graph = FALSE)
    )
  
  # FIXME
  # if source component is errant, function seems to run endlessly?
  
  # reqs <- model_component_requirements(
  #   url = file.path(schematic_url, "v1/model/component-requirements"),
  #   schema_url = "https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld",
  #   source_component = "xxx",
  #   as_graph = FALSE)
})

test_that("manifest_download returns a manifest as a dataframe", {
  skip_it()
  
 manifest <- manifest_download(
   url = file.path(schematic_url, "v1/manifest/download"),
   manifest_id = "syn51078535",
   access_token = Sys.getenv("SYNAPSE_PAT")
   )
 
  exp <- setNames(c("BulkRNA-seqAssay", "CSV/TSV", "Sample_A", "GRCm38", NA, 2022L, "syn28278954"),
    c("Component", "File Format", "Filename", "Genome Build", "Genome FASTA", "Sample ID", "entityId"))
  
  expect_equal(unlist(manifest), exp)
  expect_true(is.data.frame(manifest))
  expect_true(nrow(manifest) == 1)
})

test_that("manifest_download errors as expected", {
  skip_it()
  
  # errant URL DOESNT ERROR
  # outputs a list including status and detail message
  error <- manifest_download(
    url = file.path(schematic_url, "v1/manifest/download/TYPO"),
    manifest_id = "syn51078535",
    access_token = Sys.getenv("SYNAPSE_PAT")
  )
  
  expect_true(is.list(error))
  expect_true(error$status == 404)
  
  # bad manifest id
  expect_error(
    manifest_download(
      url = file.path(schematic_url, "v1/manifest/download"),
      manifest_id = "syn51078535xx",
      access_token = Sys.getenv("SYNAPSE_PAT")
    )
  )
  
  # bad PAT
  expect_error(
    manifest_download(
      url = file.path(schematic_url, "v1/manifest/download"),
      manifest_id = "syn51078535",
      access_token = "bad PAT"
    )
  )
})

test_that("get_asset_view_table returns output as expected", {
  skip_it()
  
  # returns manifest from csv
  # FIXME: THIS FAILS (no csv is actually returned)
  # TODO: fix or remove csv return as an option
  # get_asset_view_table(
  #   url = file.path(schematic_url, "v1/storage/assets/tables"),
  #   access_token = Sys.getenv("SYNAPSE_PAT"),
  #   asset_view ="syn23643253",
  #   return_type = "csv"
  #   )
  
  # returns manifest from json
  asset_view_table <- get_asset_view_table(
    url = file.path(schematic_url, "v1/storage/assets/tables"),
    access_token = Sys.getenv("SYNAPSE_PAT"),
    asset_view ="syn23643253",
    return_type = "json"
    )
  
  expect_true(
    nrow(asset_view_table) > 0 & is.data.frame(asset_view_table)
    )
})

test_that("graph_by_edge_type returns a list", {
  skip_it()
  
  out <- graph_by_edge_type(
    schema_url = schema_url,
    relationship = "requiresDependency",
    data_model_labels = "class_label"
    )
  
  expect_true(is.list(out))
  expect_true(length(out) > 0)
})
