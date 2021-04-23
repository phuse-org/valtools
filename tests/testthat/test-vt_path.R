test_that("vt_path returns expected path", {
  withr::with_tempdir({

    ## setup
    vt_use_validation()
    actual_dir <- file.path(getwd(),"validation")

    path_check <- vt_path()
    setwd("validation/")
    path_check2 <- vt_path()




    expect_equal(
      normalizePath(path_check,winslash = "/"),
      normalizePath(actual_dir,winslash = "/")
    )

    expect_equal(
      normalizePath(path_check2,winslash = "/"),
      normalizePath(actual_dir,winslash = "/")
    )

  })

})
