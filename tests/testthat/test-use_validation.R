test_that("Able to add validation structure to package", {
  withr::with_tempdir({

    quite <- capture.output({
      usethis::create_package(".", open = FALSE)
    })

    vt_use_validation_config(".")

    vt_use_validation(".")

    expect_true(
      dir.exists("vignettes/validation")
    )

  })
})

