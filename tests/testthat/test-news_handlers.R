test_that("create NEWS from template", {
  withr::with_tempdir({
    captured_output <- capture.output(vt_create_package("myTestPkg", open = FALSE))
    setwd("myTestPkg")
    vt_use_news_md(date = "2021-01-01")
    expect_true(file.exists("NEWS.md"))
    expect_equal(readLines("NEWS.md")[1],
                 "# myTestPkg 0.0.0.9000 (2021-01-01)" )

  })

  withr::with_tempdir({
    captured_output <- capture.output(vt_create_package("myTestPkg", open = FALSE))
    setwd("myTestPkg")
    vt_use_news_md()
    expect_true(file.exists("NEWS.md"))
    expect_equal(readLines("NEWS.md")[1],
                 "# myTestPkg 0.0.0.9000 " )

  })
})


test_that("NEWs with date field", {
  withr::with_tempdir({
    captured_output <- capture.output(vt_create_package("myTestPkg", open = FALSE))
    setwd("myTestPkg")
    vt_use_news_md(date = "2021-01-01")
    setwd("..")
    with_temp_libpaths({
      install.packages("./myTestPkg", type = "source", repo = NULL, quiet = TRUE)
      expect_equal(vt_scrape_news("myTestPkg"),
                   data.frame(version = "0.0.0.9000",
                              effective_date = "2021-01-01",
                              description = "Validation release notes for version 0.0.0.9000"))

      unloadNamespace("myTestPkg")
    })




  })
})


test_that("NEWS not in a package", {

  withr::with_tempdir({

    file.create(".here")

    vt_use_news_md(date = "2021-01-01", version = "0.0.0.9000")

    expect_equal(
      vt_scrape_news(pkg = "."),
      data.frame(version = "0.0.0.9000",
                 effective_date = "2021-01-01",
                 description = "Validation release notes for version 0.0.0.9000")
      )
  })

  withr::with_tempdir({

    file.create(".here")

    vt_use_news_md()
    expect_equal(
      vt_scrape_news(pkg = "."),
      data.frame(version = "1.0",
                 effective_date = "",
                 description = "Validation release notes for version 1.0")
    )
  })

})


test_that("NEWS without date, installed", {
  withr::with_tempdir({
    captured_output <- capture.output(vt_create_package("myTestPkg", open = FALSE))
    # vt_use_news_md looks up the file directory structure but not down to designate root
    setwd("myTestPkg")
    vt_use_news_md( )




    with_temp_libpaths({
      install.packages(".", type = "source", repos = NULL, lib = .libPaths()[1],
                       quiet = TRUE)
# browser()
      expect_equal(vt_scrape_news("."),
                   data.frame(version = "0.0.0.9000",
                              effective_date = "",
                              description = "Validation release notes for version 0.0.0.9000"))
      unloadNamespace("myTestPkg")
    })
})
})


test_that("NEWS with date, installed", {
  withr::with_tempdir({
    captured_output <- capture.output(vt_create_package("myTestPkg", open = FALSE,
                                                        fields = list(version = "0.1")))
    # vt_use_news_md looks up the file directory structure but not down to designate root
    setwd("myTestPkg")
    vt_use_news_md( )
    usethis::proj_set(force = TRUE)

  cat(file = "NEWS.md", append = TRUE,
      "* [version date] 2021-02-02\n")
  cat(file = "NEWS.md", append = TRUE,
      "* [validation] some other notes\n")


  with_temp_libpaths({
    install.packages(".", type = "source", repos = NULL, lib = .libPaths()[1],
                     quiet = TRUE)
    expect_equal(vt_scrape_news("myTestPkg"),
                 data.frame(version = "0.0.0.9000",
                            effective_date = "2021-02-02",
                            description = c("Validation release notes for version 0.0.0.9000",
                                            "some other notes")))
    unloadNamespace("myTestPkg")
    })
  })
})
