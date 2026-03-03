
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("cbm4_virtualenv_create", {

  is_CI <- as.logical(Sys.getenv("CI", "false"))
  testthat::skip_if(!is_CI)

  cbm4_virtualenv_create("r-CBM4")

  testthat::expect_true(reticulate::virtualenv_exists("r-CBM4"))

})

