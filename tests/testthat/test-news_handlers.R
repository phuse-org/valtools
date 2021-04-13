test_that("create change log from template", {
  withr::with_tempdir({

    captured_output <- capture.output(vt_create_package("myTestPkg", open = FALSE))
    setwd("myTestPkg")

    vt_use_change_log(date = "2021-01-01")

    expect_true(file.exists("vignettes/validation/change_log.md"))
    expect_equal(readLines("vignettes/validation/change_log.md")[1],
                 "# 0.0.0.9000 2021-01-01" )

  })

  withr::with_tempdir({
    captured_output <- capture.output(vt_create_package("myTestPkg", open = FALSE))
    setwd("myTestPkg")
    vt_use_change_log()
    expect_true(file.exists("vignettes/validation/change_log.md"))
    expect_equal(readLines("vignettes/validation/change_log.md")[1],
                 paste("# 0.0.0.9000",format(Sys.Date(), "%Y-%m-%d")))

  })
})

test_that("change log not in a package", {

  withr::with_tempdir({

    file.create(".here")
    vt_use_validation()

    vt_use_change_log(date = "2021-01-01", version = "0.0.0.9000")

    expect_equal(
      vt_scrape_change_log(),
      data.frame(version = "0.0.0.9000",
                 effective_date = "2021-01-01",
                 description = "Validation release notes for version 0.0.0.9000")
      )
  })

  withr::with_tempdir({

    file.create(".here")
    vt_use_validation()

    vt_use_change_log()
    expect_equal(
      vt_scrape_change_log(),
      data.frame(version = "1.0",
                 effective_date = format(Sys.Date(), "%Y-%m-%d"),
                 description = "Validation release notes for version 1.0")
    )
  })
})

