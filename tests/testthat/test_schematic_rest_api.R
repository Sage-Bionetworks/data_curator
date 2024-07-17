context("test schematic rest api wrappers")

### Test that schematic server is online. Make sure schematic_url matches the actual
### schematic server URL https://github.com/Sage-Bionetworks/schematic/tree/develop/api
### If not available, skip these tests.

schematic_url <- "https://schematic-dev.api.sagebionetworks.org"
ping <- try(httr::GET(schematic_url), silent = TRUE)
skip_it <- function(skip=ping) {
  if (inherits(ping, "try-error")) skip(sprintf("schematic server URL unavailable (%s). Is it running locally?", schematic_url)) #nolint
}

schema_url <- "https://raw.githubusercontent.com/Sage-Bionetworks/data-models/main/example.model.jsonld"
pass_csv <- system.file("testdata", "HTAN-Biospecimen-Tier-1-2-pass.csv",
                        package = "datacurator")
fail_csv <- system.file("testdata", "HTAN-Biospecimen-Tier-1-2-fail.csv",
                        package = "datacurator")

test_that("manifest_generate returns a URL if sucessful", {
  skip_it()
  
  url <- manifest_generate(url=file.path(schematic_url, "v1/manifest/generate"),
    schema_url = schema_url, access_token = Sys.getenv("SYNAPSE_PAT"),
    title="Test biospecimen", data_type="Biospecimen",
    use_annotations = FALSE,
    dataset_id="syn33715357", asset_view="syn33715412",
    output_format = "google_sheet")
  expect_true(grepl("^https://docs.google", url))
})

# test_that("manifest_generate returns an xlsx", {
#   skip_it()
#   
#   xlsx <- manifest_generate(url=file.path(schematic_url, "v1/manifest/generate"),
#                     title="Test biospecimen", data_type="Biospecimen",
#                     asset_view="syn33715412", output_format="excel")
#   
# })

# test_that("manifest_populate returns a google sheet link with records filled", {
#   skip_it()
#   req <- manifest_populate(data_type="Biospecimen", title="Example",
#                            csv_file = pass_csv)
# })
  
test_that("manifest_validate passes and fails correctly", {
  skip_it()
  
  pass <- manifest_validate(url=file.path(schematic_url, "v1/model/validate"),
                            data_type="Biospecimen", file_name=fail_csv,
                            access_token = Sys.getenv("SYNAPSE_PAT"),
                            schema_url = schema_url)
  expect_identical(pass, list(errors = list(), warnings = list()))
  
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

test_that("storage_project_datasets returns available datasets", {
  skip_it()
  storage_project_datasets(url=file.path(schematic_url, "v1/storage/project/datasets"),
                           asset_view="syn23643253",
                           project_id="syn26251192",
                           access_token=Sys.getenv("SYNAPSE_PAT"))
})

test_that("storage_projects returns available projects", {
  skip_it()
  storage_projects(url=file.path(schematic_url, "v1/storage/projects"),
                   asset_view="syn23643253",
                   access_token=Sys.getenv("SYNAPSE_PAT"))
})

test_that("storage_dataset_files returns files", {
  skip_it()
  storage_dataset_files(url=file.path(schematic_url, "v1/storage/dataset/files"),
                        asset_view = "syn23643253",
                        dataset_id = "syn23643250",
                        access_token=Sys.getenv("SYNAPSE_PAT"))
})

test_that("model_component_requirements returns list of required components", {
  skip_it()
  good <- model_component_requirements(url=file.path(schematic_url, "v1/model/component-requirements"),
                                           schema_url="https://raw.githubusercontent.com/ncihtan/data-models/main/HTAN.model.jsonld",
                                           source_component="Patient",
                                           as_graph = FALSE)
  expect_equal(length(good), 8L)
  
  expect_error(model_component_requirements(url=file.path(schematic_url, "v1/model/component-requirements"),
                                       schema_url="https://aaaabad.url.jsonld",
                                       source_component="Patient",
                                       as_graph = FALSE))
  
})

# test_that("manifest_download returns a csv.", {
#   skip_it()
#  csv <- manifest_download(url=file.path(schematic_url, "v1/manifest/download"),
#                           manifest_id="syn51078535",
#                           access_token=Sys.getenv("SYNAPSE_PAT"))
#   exp <- setNames(c("BulkRNA-seqAssay", "CSV/TSV", "Sample_A", "GRCm38", NA, 2022L, "syn28278954"),
#     c("Component", "File Format", "Filename", "Genome Build", "Genome FASTA", "Sample ID", "entityId"))
#   expect_equal(unlist(csv), exp)
# })

test_that("get_asset_view_table returns asset view table", {
  skip_it()
  av <- get_asset_view_table(url=file.path(schematic_url, "v1/storage/assets/tables"),
                             access_token = Sys.getenv("SYNAPSE_PAT"),
                       asset_view="syn23643253")
  storage_tbl <- subset(av, av$name == "synapse_storage_manifest.csv")
  expect_true(inherits(av, "data.frame"), "name" %in% names(av))
})

test_that("asset_tables returns a data.frame", {
  skip_it()
  tst <- get_asset_view_table(url=file.path(schematic_url, "v1/storage/assets/tables"),
                       asset_view = "syn28559058",
                       access_token = Sys.getenv("SYNAPSE_PAT"),
                       return_type="json")
  expect_identical(nrow(tst), 4L)
  
  expect_error(get_asset_view_table(url=file.path(schematic_url, "v1/storage/assets/tables"),
                                  asset_view = "syn28559058",
                                  access_token = Sys.getenv("SYNAPSE_PAT"),
                                  return_type = "csv")
               )
})
