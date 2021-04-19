test_that("ensure the correct config is selected when not interactive", {
  withr::with_tempdir({

    quiet <- capture.output({
      valtools::vt_create_package()
    })

    dir.create("inst/validation/",recursive = TRUE)
    file.copy("vignettes/validation/validation.yml",to = "inst/validation")

    withr::with_dir("vignettes", {
      expect_equal(
        config_selector(normalizePath(
          find_file("validation.yml", ref = "..", full_names = TRUE),
          winslash = "/"
        ),
        interactive = FALSE),
        normalizePath(file.path(getwd(), "validation/validation.yml"),
                      winslash = "/")
      )

    })


    ## config_selector changes behavior when not interactive to select whichever validation.yml is in the same path & closest
    withr::with_dir("inst", {
      expect_equal(
        config_selector(normalizePath(
          find_file("validation.yml", ref = "..", full_names = TRUE),
          winslash = "/"
        ),
        interactive = FALSE),
        normalizePath(file.path(getwd(), "validation/validation.yml"),
                      winslash = "/")
      )

    })

    dir.create("inst/validation/nested")
    file.copy("inst/validation/validation.yml",to = "inst/validation/nested/validation.yml")

    ## config_selector changes behavior when not interactive to select whichever validation.yml is in the same path & closest
    withr::with_dir("inst", {
      expect_true(all(
        find_file("validation.yml", ref = ".", full_names = TRUE) %in%
          c(
            "./validation/validation.yml",
            "./validation/nested/validation.yml"
          )
      ))

      expect_equal(
        config_selector(normalizePath(
          find_file("validation.yml", ref = "..", full_names = TRUE),
          winslash = "/"
        ),
        interactive = FALSE),
        normalizePath(file.path(getwd(), "validation/validation.yml"),
                      winslash = "/")
      )

    })

  })

})
