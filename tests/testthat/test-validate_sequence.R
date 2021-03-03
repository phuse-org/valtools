test_that("test running validation.Rmd from source", {
  withr::with_tempdir({

    ## create blank package
    quiet <- capture.output({
      usethis::create_package("example.package")
    })

    setwd("example.package")

    ## create config file
    writeLines(text = c(
      "working_dir: vignettes",
      "output_dir: inst",
      "usernames:",
      "  NewUser:",
      "    name: New User",
      "    title: new",
      "    role: user"),
      con = ".validation")

    ## make vignette
    dir.create("vignettes")
    writeLines( text = c(
        "---",
        "title: \"Validation Report\"",
        "output: rmarkdown::pdf_document",
        "vignette: >",
        "  %\\VignetteIndexEntry{validation}",
        "  %\\VignetteEngine{knitr::rmarkdown}",
        "  %\\VignetteEncoding{UTF-8}",
        "---",
        "",
        "```{r}",
        "",
        "Sys.Date()",
        "",
        "```",
        ""),
      con = "vignettes/validation.Rmd"
    )

    ## validate source.
    quiet <- capture.output({
    validation_report_output <- vt_validate_source(pkg = ".", open = FALSE)
    })

    expect_equal(
      validation_report_output,
      file.path(
        getwd(),
        paste0("inst/validation/Validation_Report_example.package_v0.0.0.9000_",format(Sys.Date(),"%Y%m%d.pdf"))
      )
    )

    expect_true(
      file.exists(validation_report_output)
    )

    validation_report_output_rendered <-
      strsplit(split = "\r\n",
               pdftools::pdf_text(validation_report_output))[[1]]

    expect_equal(
      trimws(validation_report_output_rendered),
      c(
      "Validation Report",
      "Sys.Date()",
      paste0("## [1] \"",Sys.Date(),"\""),
      "1"
      )
    )

})})

test_that("test running validation.Rmd from source for failure", {
  withr::with_tempdir({

    ## create blank package
    quiet <- capture.output({
      usethis::create_package("example.package")
    })

    setwd("example.package")

    ## create config file
    writeLines(text = c(
      "working_dir: vignettes",
      "output_dir: inst",
      "usernames:",
      "  NewUser:",
      "    name: New User",
      "    title: new",
      "    role: user"),
      con = ".validation")

    ## make vignette
    dir.create("vignettes")
    writeLines( text = c(
      "---",
      "title: \"Validation Report\"",
      "output: rmarkdown::pdf_document",
      "vignette: >",
      "  %\\VignetteIndexEntry{validation}",
      "  %\\VignetteEngine{knitr::rmarkdown}",
      "  %\\VignetteEncoding{UTF-8}",
      "---",
      "",
      "```{r}",
      "",
      "stop(\"hammer time\")",
      "",
      "```",
      ""),
      con = "vignettes/validation.Rmd"
    )

    ## validate source.
    quiet <- capture.output({
      expect_error(
        vt_validate_source(pkg = ".", open = FALSE),
        "Error during validation of package. Error: ",
        fixed = TRUE
      )
    })

  })})

test_that("test building a validated bundle from source", {
  withr::with_tempdir({

    ## create blank package
    quiet <- capture.output({
      usethis::create_package("example.package")
    })

    setwd("example.package")

    ## create config file
    writeLines(text = c(
      "working_dir: vignettes",
      "output_dir: inst",
      "usernames:",
      "  NewUser:",
      "    name: New User",
      "    title: new",
      "    role: user"),
      con = ".validation")

    ## make vignette
    dir.create("vignettes")
    dir.create("vignettes/validation")

    writeLines( text = c(
      "---",
      "title: \"Validation Report\"",
      "output: rmarkdown::pdf_document",
      "vignette: >",
      "  %\\VignetteIndexEntry{validation}",
      "  %\\VignetteEngine{knitr::rmarkdown}",
      "  %\\VignetteEncoding{UTF-8}",
      "---",
      "",
      "```{r}",
      "",
      "Sys.Date()",
      "",
      "```",
      ""),
      con = "vignettes/validation.Rmd"
    )

    valtools::vt_use_req("sample_req.md", username = "NewUser", open = FALSE)
    valtools::vt_use_test_case("sample_test_case.md", username = "NewUser", open = FALSE)
    valtools::vt_use_test_code("sample_test_code.R", username = "NewUser", open = FALSE)

    ## validate source & create bundle.
    suppressMessages({
    quiet <- capture.output({
    validated_bundle <- vt_validate_build(pkg = ".")
    })})

    ## check bundle
    expect_equal(
      validated_bundle,
      file.path(
        dirname(getwd()),
        "example.package_0.0.0.9000.tar.gz"
      )
    )
    expect_true(
      file.exists(validated_bundle)
    )

    ## check outputs were generated as expected

    validation_report_output <- list.files("inst/validation", pattern = ".pdf", full.names = TRUE)
    validation_docs <- setdiff(
      list.files("inst/validation", full.names = FALSE,recursive = TRUE),
      basename(validation_report_output)
    )

    ## validation report rendered properly
    validation_report_output_rendered <-
      strsplit(split = "\r\n",
               pdftools::pdf_text(validation_report_output))[[1]]

    expect_equal(
      trimws(validation_report_output_rendered),
      c(
        "Validation Report",
        "Sys.Date()",
        paste0("## [1] \"",Sys.Date(),"\""),
        "1"
      )
    )

    ## validation content were copied properly
    expect_true(
      all(validation_docs %in% c(
        "requirements/sample_req.md",
        "test_cases/sample_test_case.md",
        "test_code/sample_test_code.R",
        "validation.Rmd"
      ))
    )

  })})

test_that("test installing a validated bundle from source and rerunning report", {
  withr::with_temp_libpaths({
  withr::with_tempdir({

    pkg_name <- paste0("example.package",sample(10:20, 1))

    ## create blank package
    quiet <- capture.output({
      usethis::create_package(pkg_name)
    })

    setwd(pkg_name)

    ## create config file
    writeLines(text = c(
      "working_dir: vignettes",
      "output_dir: inst",
      "usernames:",
      "  NewUser:",
      "    name: New User",
      "    title: new",
      "    role: user"),
      con = ".validation")

    ## make vignette
    dir.create("vignettes")
    dir.create("vignettes/validation")

    writeLines( text = c(
      "---",
      "title: \"Validation Report\"",
      "output: rmarkdown::pdf_document",
      "vignette: >",
      "  %\\VignetteIndexEntry{validation}",
      "  %\\VignetteEngine{knitr::rmarkdown}",
      "  %\\VignetteEncoding{UTF-8}",
      "---",
      "",
      "```{r}",
      "",
      "Sys.Date()",
      "",
      "```",
      ""),
      con = "vignettes/validation.Rmd"
    )

    valtools::vt_use_req("sample_req.md", username = "NewUser", open = FALSE)
    valtools::vt_use_test_case("sample_test_case.md", username = "NewUser", open = FALSE)
    valtools::vt_use_test_code("sample_test_code.R", username = "NewUser", open = FALSE)

    old_pkgs <- rownames(installed.packages(lib.loc = .libPaths()[1]))

    ## validate source & create bundle.
    suppressMessages({
      quiet <- capture.output({
    vt_validate_install(pkg = ".", install_verbose = FALSE)
    })})

    new_pkg <- rownames(installed.packages(lib.loc = .libPaths()[1]))

    expect_true(
      pkg_name %in% new_pkg && is.null(old_pkgs)

    )

    pkg_dir <- system.file(package = new_pkg)

    pkg_dir_val <- file.path(pkg_dir, "validation")

    ## check outputs were generated as expected
    validation_report_output <- list.files(pkg_dir_val, pattern = ".pdf", full.names = TRUE)
    validation_docs <- setdiff(
      list.files(pkg_dir_val, full.names = FALSE,recursive = TRUE),
      basename(validation_report_output)
    )

    ## validation report rendered properly
    validation_report_output_rendered <-
      strsplit(split = "\r\n",
               pdftools::pdf_text(validation_report_output))[[1]]

    expect_equal(
      trimws(validation_report_output_rendered),
      c(
        "Validation Report",
        "Sys.Date()",
        paste0("## [1] \"",Sys.Date(),"\""),
        "1"
      )
    )

    ## validation content were copied properly
    expect_true(
      all(validation_docs %in% c(
        "requirements/sample_req.md",
        "test_cases/sample_test_case.md",
        "test_code/sample_test_code.R",
        "validation.Rmd"
      ))
    )

    ### rerun validation report
    new_output_dir <- tempdir()

    new_validation_report_output <- vt_validate_installed_package(
      package = pkg_name,
      output_dir = new_output_dir,
      open = FALSE)

    new_validation_report_output_rendered <-
      strsplit(split = "\r\n",
               pdftools::pdf_text(new_validation_report_output))[[1]]

    expect_equal(
      trimws(new_validation_report_output_rendered),
      c(
        "Validation Report",
        "Sys.Date()",
        paste0("## [1] \"",Sys.Date(),"\""),
        "1"
      )
    )

  })})})

test_that("Attempting rerunning report for package not built for validation throws error", {

  expect_error(
    vt_validate_installed_package(
        package = "utils",
        open = FALSE),
    "Package utils was not built with `vt_validated_build()",
    fixed = TRUE
  )

})

