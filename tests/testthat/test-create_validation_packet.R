test_that("Able to create packet with validated project basics", {
  withr::with_tempdir({
    quiet <- capture.output({
      vt_create_packet("example_packet", target = "example.package", open = FALSE)
    })


    withr::with_dir(new = "example_packet", {
      expect_error(devtools::as.package("."))

      expect_true(dir.exists("validation"))

      expect_true(file.exists("validation/validation.yml"))

      expect_true(file.exists("validation/validation.yml"))
    })

  })
})

test_that("throws error when trying to add to existing packageunable to create the package", {
  withr::with_tempdir({

    quiet <- capture.output({
      usethis::create_package(".")
      })

    expect_error(
      vt_create_packet(".", target = "fake.package"),
      paste0("`vt_create_packet()` is not intended to add validation infrastructure",
      " to an existing package. Use `vt_use_validation()` instead."),
      fixed=TRUE
    )
  })
})
