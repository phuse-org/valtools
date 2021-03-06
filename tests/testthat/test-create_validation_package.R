test_that("Able to create package with validated package basics", {
  withr::with_tempdir({

    quite <- capture.output({
     vt_create_package(".", open = FALSE)
    })

    expect_true(
      devtools::is.package(devtools::as.package("."))
    )

    expect_true(
      dir.exists("vignettes/validation")
    )

    expect_equal(
      desc::desc_get_deps(file = "DESCRIPTION"),
      data.frame(
        type = "Imports",
        package = "valtools",
        version = "*",
        stringsAsFactors = FALSE
      )
    )

  })
})

test_that("throws standard error when unable to create the package", {
  withr::with_tempdir({
    expect_error(
      vt_create_package("temp_package"),
      "Failed to create package. Error"
      )
  })
})

