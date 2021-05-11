test_that("Able to add validation structure to package", {
  withr::with_tempdir({

    quite <- capture.output({
      usethis::create_package(".", open = FALSE)
    })

    vt_use_validation(".")

    expect_true(
      dir.exists("vignettes/validation")
    )

  })
})


test_that("Able to add validation structure outside package", {
  withr::with_tempdir({

    vt_use_validation("rlang_validation", package = "rlang")

    expect_true(
      dir.exists("rlang_validation/validation")
    )

  })
})


