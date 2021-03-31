test_that("vt_path returns expected path", {
  withr::with_tempdir({

    ## setup
    vt_use_validation()
    actual_dir <- file.path(getwd(),"validation")

    path_check <- vt_path()
    setwd("validation/")
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
