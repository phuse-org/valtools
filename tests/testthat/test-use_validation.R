test_that("Able to add validation structure to package", {
  withr::with_tempdir({

    quite <- capture.output({
      usethis::create_package("temp.package")
    })

    vt_use_validation("temp.package")

    expect_true(
      dir.exists("temp.package/vignettes/validation")
    )

  })
})

