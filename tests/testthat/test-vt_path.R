test_that("vt_path returns expected path", {
  withr::with_tempdir({

    ## setup
    make_vt_test_package()
    actual_dir <- file.path(getwd(), "vignettes", "validation")

    path_check <- vt_path()
    setwd(file.path("vignettes", "validation"))
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
