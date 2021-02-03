context("validation environment table")

test_that("works on valtools (no Depends)", {
  this_pkg_path <- gsub(here(), pattern = "tests", replacement = "valtools")
  testthat::expect_error(desc(file.path(this_pkg_path, "DESCRIPTION"))$get_field("Depends"))

  validation_env <- vt_scrape_val_env(this_pkg_path)
  testthat::expect_true(!all(is.na(validation_env)))


})
