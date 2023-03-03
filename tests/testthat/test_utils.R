context("Test utils")

testthat("parse_env_var handles empty string", {
  expect_error(parse_env_var(""), "delimiter not in")
  expect_error(parse_env_var(Sys.getenv(".fake_env")), "delimiter not in")
})
  