context("test synapse rest api wrappers")

test_that("synapse_user_profile returns list with successful auth", {
  req <- synapse_user_profile(auth=Sys.getenv("SYNAPSE_PAT"))
  expect_true(all(c("ownerId", "userName") %in% names(req)))
})

test_that("synapse_user_profile bad auth token returns message", {
  req <- synapse_user_profile(auth="bad token")
  expect_identical(req, list(reason="Invalid access token"))
})

test_that("synapse_user_profile returns list with NULL auth", {
  req <- synapse_user_profile(auth=NULL)
  req_name <- req$userName
  expect_identical(req_name, "anonymous")
})

test_that("is_certified returns TRUE or FALSE", {
  
  expect_true(synapse_is_certified(auth=Sys.getenv("SYNAPSE_PAT")))
  expect_true(synapse_is_certified(auth=NULL))
  expect_false(synapse_is_certified(auth="bad auth"))
  
})

test_that("get returns a tibble or error", {
  
  good_req <- synapse_get(id="syn23643255", auth=Sys.getenv("SYNAPSE_PAT"))
  expect_true(nrow(good_req) == 1)
  
  expect_error(synapse_get(id="bad", auth=Sys.getenv("SYNAPSE_PAT")))
  expect_error(synapse_get(id=NULL, auth=Sys.getenv("SYNAPSE_PAT")))
  expect_error(synapse_get(id="bad", auth="bad"))
  
})
