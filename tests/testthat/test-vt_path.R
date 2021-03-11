test_that("vt_path returns expected path", {
  withr::with_tempdir({

    ## setup
    actual_dir <- file.path(getwd(),"vignettes/validation")
    vt_use_validation_config()
    vt_use_validation()

    path_check <- vt_path()
    setwd("vignettes/")
    path_check2 <- vt_path()

    expect_equal(
      path_check,
      actual_dir
    )

    expect_equal(
      path_check2,
      actual_dir
    )

  })

})
