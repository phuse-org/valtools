test_that("create change log from template", {
  withr::with_tempdir({

    make_vt_test_package()
    vt_use_change_log(date = "2021-01-01", open = FALSE)

    expect_true(file.exists("vignettes/validation/change_log.md"))
    expect_equal(readLines("vignettes/validation/change_log.md")[1],
                 "# 0.0.0.9000 2021-01-01" )

  })

  withr::with_tempdir({
    make_vt_test_package()
    vt_use_change_log(open = FALSE)
    expect_true(file.exists("vignettes/validation/change_log.md"))
    expect_equal(readLines("vignettes/validation/change_log.md")[1],
                 paste("# 0.0.0.9000",format(Sys.Date(), "%Y-%m-%d")))

  })
})

test_that("change log not in a package", {

  withr::with_tempdir({

    make_vt_test_package()

    vt_use_change_log(date = "2021-01-01", version = "0.0.0.9000", open = FALSE)

    expect_equal(
      vt_scrape_change_log(),
      data.frame(version = "0.0.0.9000",
                 effective_date = "2021-01-01",
                 description = "Validation release notes for version 0.0.0.9000",
                 stringsAsFactors = FALSE)
      )
  })

  withr::with_tempdir({

    make_vt_test_package()
    vt_use_change_log(open = FALSE, version = "1.0")
    expect_equal(
      vt_scrape_change_log(),
      data.frame(version = "1.0",
                 effective_date = format(Sys.Date(), "%Y-%m-%d"),
                 description = "Validation release notes for version 1.0",
                 stringsAsFactors = FALSE)
    )
  })
})


test_that("Throw informative error when change log does not exist", {

  withr::with_tempdir({

    make_vt_test_package()

    expect_error(
      vt_scrape_change_log(),
      "A change log does not exist in the validation folder.\nRun `valtools::vt_use_change_log()` to create a change_log.md file.",
      fixed = TRUE
    )
  })

})
