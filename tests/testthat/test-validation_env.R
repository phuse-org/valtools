context("validation environment table")

# Note: when running tests via a CRAN-like environment the testing
#       folder structure is valtools.Rcheck/tests w/o DESCRIPTION,
#       which causes package_file to fail.

test_that("works on valtools (no Depends)", {


  skip_if(grepl(dirname(rprojroot::is_testthat$find_file()), pattern = "\\.Rcheck"))

  testthat::expect_false(all(grepl(readLines(rprojroot::is_r_package$find_file("DESCRIPTION")), pattern = "Depends")))

  validation_env <- vt_scrape_val_env()
  testthat::expect_true(!all(is.na(validation_env)))


})


test_that("temp", {

  withr::with_tempdir({

  capture_output <- capture.output({usethis::create_package(path = ".", open = FALSE,
                                            rstudio = TRUE)})

  # bare
  validation_env0 <- vt_scrape_val_env()
  testthat::expect_equal(c("OS", "R"),
                         validation_env0[validation_env0$type != "session", "resource"])

  # Only Suggests
  fields <- usethis::use_description_defaults()
  suggests_field <- "covr, testthat, here"
  fields$Suggests <- suggests_field
  desc_contents <- desc::desc(text = glue::glue("{names(fields)}: {fields}"))
  writeLines(con = normalizePath(file.path(".", "DESCRIPTION")),
             text = desc_contents$str(by_field = TRUE, normalize = FALSE, mode = "file"))
  validation_env1 <- vt_scrape_val_env()
  testthat::expect_equal(0, nrow(validation_env1[validation_env1$type == "package_req",]))
  testthat::expect_identical(sort(strsplit(suggests_field, split = ", ")[[1]]),
                             validation_env1[validation_env1$type == "extended_req", "resource"])
  # Suggest + Depends
  depends_field <- "tidyverse"
  if(depends_field %in% row.names(installed.packages())){
    fields$Depends <- depends_field
    desc_contents2 <- desc::desc(text = glue::glue("{names(fields)}: {fields}"))
    writeLines(con = normalizePath(file.path(".", "DESCRIPTION")),
             text = desc_contents2$str(by_field = TRUE, normalize = FALSE, mode = "file"))
    validation_env2 <- vt_scrape_val_env()
    testthat::expect_equal(3, nrow(validation_env2[validation_env2$type == "extended_req",]))
    testthat::expect_equal(depends_field, validation_env2[validation_env2$type == "package_req", "resource"])
  }

  # all
  imports_field <- "magrittr, plotly"
  if(all(strsplit(imports_field, split = ", ")[[1]] %in% row.names(installed.packages()))){
    fields$Imports <- imports_field
    desc_contents3 <- desc::desc(text = glue::glue("{names(fields)}: {fields}"))
    writeLines(con = normalizePath(file.path(".", "DESCRIPTION")),
             text = desc_contents3$str(by_field = TRUE, normalize = FALSE, mode = "file"))
    validation_env3 <- vt_scrape_val_env()
    testthat::expect_equal(3, nrow(validation_env3[validation_env3$type == "extended_req",]))
    testthat::expect_equal(c(strsplit(imports_field, split = ", ")[[1]], depends_field),
                         validation_env3[validation_env3$type == "package_req", "resource"])
  }

  # package that doesn't exist
  fake_pkg <- "myFakePackage"
  fields$Imports <- fake_pkg
  desc_contents4 <- desc::desc(text = glue::glue("{names(fields)}: {fields}"))
  writeLines(con = normalizePath(file.path(".", "DESCRIPTION")),
             text = desc_contents4$str(by_field = TRUE, normalize = FALSE, mode = "file"))
  testthat::expect_error(vt_scrape_val_env(),
                         "there is no package called 'myFakePackage'", perl = TRUE)

})})

