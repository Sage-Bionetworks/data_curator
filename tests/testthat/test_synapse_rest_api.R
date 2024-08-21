context("test synapse rest api wrappers")

test_that("synapse_user_profile returns list with successful auth", {
  req <- synapse_user_profile(auth=Sys.getenv("SYNAPSE_PAT"))
  expect_true(all(c("ownerId", "userName") %in% names(req)))
})

test_that("synapse_user_profile bad auth token returns message", {
  req <- synapse_user_profile(auth="bad token")
  expect_identical(req$reason, "Invalid access token")
})

test_that("synapse_user_profile returns list with NULL auth", {
  req <- synapse_user_profile(auth=NULL)
  req_name <- req$userName
  expect_identical(req_name, "anonymous")
})

test_that("is_certified returns TRUE or FALSE", {
  
  expect_false(synapse_is_certified(auth=Sys.getenv("SYNAPSE_PAT")))
  expect_false(synapse_is_certified(auth=NULL))
  expect_false(synapse_is_certified(auth="bad auth"))
  
})

test_that("synapse_get returns a tibble", {
  
  good_req <- synapse_get(id="syn61941085", auth=Sys.getenv("SYNAPSE_PAT"))
  expect_true(length(good_req) > 1)
})

test_that("synapse_get errors as expected", {
  
  # nonexistant id
  expect_error(synapse_get(id="bad", auth=Sys.getenv("SYNAPSE_PAT")))
  
  # NULL id
  expect_error(synapse_get(id=NULL, auth=Sys.getenv("SYNAPSE_PAT")))
  
  # nonexistant id and auth
  expect_error(synapse_get(id="bad", auth="bad"))
  
})

test_that("synapse_access returns TRUE/FALSE", {
  
  # has DOWNLOAD access
  expect_true(
    synapse_access(id="syn62147982", access = "DOWNLOAD", auth=Sys.getenv("SYNAPSE_PAT"))
    )
  
  # doesn not have DOWNLOAD access
  expect_false(
    synapse_access(id="syn23643255", access = "DOWNLOAD", auth=Sys.getenv("SYNAPSE_PAT"))
  )
  
  # Bad PAT
  expect_false(
    synapse_access(id="syn23643255", access = "DOWNLOAD", auth=Sys.getenv("ABCD"))
  )
})

test_that("synapse_access returns errors as expected", {
  
  # non existent access argument
  expect_error(
    synapse_access(id="syn23643255", access = "TYPO", auth=Sys.getenv("SYNAPSE_PAT"))
  )
  
  # non existent id argument
  expect_error(
    synapse_access(id="not an id", access = "DOWNLOAD", auth=Sys.getenv("SYNAPSE_PAT"))
  )
  
  # non existant auth orgument 
  expect_error(
    synapse_access(id="syn23643255", access = "DOWNLOAD", auth="Not an auth")
  )
})

test_that("synapse_entity_children returns a tibble", {
  
  # all arguments valid
  req <- synapse_entity_children(
    parentId="syn35187716", includeTypes = list("file", "folder"), auth=Sys.getenv("SYNAPSE_PAT")
    )
  
  expect_true(
    all(c("tbl_df", "data.frame") %in% class(req))
  )
  
  # no children to return
  req_no_children <- synapse_entity_children(
    parentId="syn35187716", includeTypes = "projects", auth=Sys.getenv("SYNAPSE_PAT")
    )
  
  expect_true(
    all(c("tbl_df", "data.frame") %in% class(req_no_children))
  )
  
  # typo in parentId
  req_parentId_typo <- synapse_entity_children(
    parentId="not an id", includeTypes = "projects", auth=Sys.getenv("SYNAPSE_PAT")
  )
  
  expect_true(
    all(c("tbl_df", "data.frame") %in% class(req_parentId_typo))
  )
  
  # typo in auth
  req_auth_typo <- synapse_entity_children(
    parentId="syn35187716", includeTypes = "projects", auth="not an auth"
  )
  
  expect_true(
    all(c("tbl_df", "data.frame") %in% class(req_auth_typo))
  )
})

test_that("synapse_projects_user returns expected tibble", {
  req <- synapse_projects_user(auth=Sys.getenv("SYNAPSE_PAT"))
  expect_identical(names(req), c("name", "id", "lastActivity", "modifiedOn", "modifiedBy"))
})

test_that("synapse_projects_user errors when auth token is not valid", {
  expect_error(
    req <- synapse_projects_user(auth="ABC")
  )
})

test_that("synapse_table_query", {
  
  # Return token
  id <- "syn61941085"
  query <- sprintf("select id from %s limit 1", id)
  token <- synapse_table_query(id = id, auth = Sys.getenv("SYNAPSE_PAT"), query = query, partMask = 0x10)

  expect_identical(names(token), "token")
  
  # not a table (returns error message but does not error)
  id <- "syn52578158"
  query <- sprintf("select id from %s limit 1", id)
  project_resp <- synapse_table_query(id = id, query = query, partMask = 0x10, auth = Sys.getenv("SYNAPSE_PAT"))
  
  expect_identical(project_resp$reason, "syn52578158 is not a table or view")
  
  # invalid access token (returns error message but does not error)
  id <- "syn52578158"
  query <- sprintf("select id from %s limit 1", id)
  invalid_auth_resp <- synapse_table_query(id = id, query = query, partMask = 0x10, auth = "auth")
  
  expect_identical(invalid_auth_resp$reason, "Invalid access token")
})

# TODO: Add tests for functions used in DCA dashboard modules
# test_that("synapse_table_get success", {
# 
# })
# 
# test_that("get_synapse_table_names success", {
#
# })

# test_that("synapse_storage_projects success", {
# 
# })
# 
# test_that("synapse_download_file_handle success", {
#   
# })
# 
# test_that("synapse_get_manifests_in_asset_view success", {
#   
# })
