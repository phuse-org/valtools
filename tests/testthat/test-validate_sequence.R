test_that("test running validation.Rmd from source", {
  skip_if(!"valtools" %in% rownames(installed.packages()))
  withr::with_tempdir({

    ## create blank package
    quiet <- capture.output({
      usethis::create_package("example.package")
    })

    setwd("example.package")

    ## make vignette and validation dir
    dir.create("vignettes")
    dir.create("vignettes/validation")

    ## create config file
    writeLines(text = c(
      "package: example.package",
      "working_dir: vignettes",
      "output_dir: inst",
      "report_rmd_name: validation.Rmd",
      "usernames:",
      "  NewUser:",
      "    name: New User",
      "    title: new",
      "    role: user"),
    con = "vignettes/validation/validation.yml")


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
      normalizePath(validation_report_output,winslash = "/"),
      normalizePath(file.path(
        getwd(),
        paste0("inst/validation/Validation_Report_example.package_v0.0.0.9000_",format(Sys.Date(),"%Y%m%d.pdf"))),
        winslash = "/")
    )

    expect_true(
      file.exists(validation_report_output)
    )

    validation_report_output_rendered <-
      strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
           pdftools::pdf_text(validation_report_output)))[[1]]

    expect_equal(
      trimws(validation_report_output_rendered),
      c(
      "Validation Report",
      "Sys.Date()",
      paste0("## [1] \"",Sys.Date(),"\""),
      "1"
      )
    )

})
})

test_that("test running validation.Rmd from source for failure", {
  skip_if(!"valtools" %in% rownames(installed.packages()))
  withr::with_tempdir({

    ## create blank package
    quiet <- capture.output({
      usethis::create_package("example.package", open = FALSE)
    })

    setwd("example.package")

    dir.create("vignettes/validation",recursive = TRUE, showWarnings = FALSE)

    ## create config file
    writeLines(text = c(
      "package: example.package",
      "working_dir: vignettes",
      "output_dir: inst",
      "report_rmd_name: validation.Rmd",
      "usernames:",
      "  NewUser:",
      "    name: New User",
      "    title: new",
      "    role: user"),
    con = "vignettes/validation/validation.yml")

    ## make vignette
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
  skip_if(!"valtools" %in% rownames(installed.packages()))
  withr::with_tempdir({

    ## create blank package
    quiet <- capture.output({
      usethis::create_package("example.package")
    })

    setwd("example.package")

    dir.create("vignettes/validation",recursive = TRUE, showWarnings = FALSE)

    ## create config file
    writeLines(text = c(
      "package: example.package",
      "working_dir: vignettes",
      "output_dir: inst",
      "report_rmd_name: validation.Rmd",
      "usernames:",
      "  NewUser:",
      "    name: New User",
      "    title: new",
      "    role: user"),
    con = "vignettes/validation/validation.yml")

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

    vt_use_req("sample_req.md", username = "NewUser", open = FALSE)
    vt_use_test_case("sample_test_case.md", username = "NewUser", open = FALSE)
    vt_use_test_code("sample_test_code.R", username = "NewUser", open = FALSE)

    writeLines(c(
      "#' @title Hello World",
      "#' @editor Sample Name",
      "#' @editDate 1900-01-01",
      "#' @param name name to say hello to",
      "#' @returns string of 'Hello, ' pasted to name",
      "hello_world <- function(name){",
      "  paste('Hello, ', name)",
      "}"),
      con = "R/hello_world.R")

    ## validate source & create bundle.
    suppressMessages({
    quiet <- capture.output({
    validated_bundle <- vt_validate_build(pkg = ".")
    })})

    ## check bundle
    expect_equal(
      normalizePath(validated_bundle,winslash = "/"),
      normalizePath(file.path(
        dirname(getwd()),
        "example.package_0.0.0.9000.tar.gz"),
        winslash = "/")
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
      strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
               pdftools::pdf_text(validation_report_output)))[[1]]

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
    expect_true(all(
      c(
        "R/Function_Roxygen_Blocks.R",
        "requirements/sample_req.md",
        "test_cases/sample_test_case.md",
        "test_code/sample_test_code.R",
        "validation.Rmd",
        "validation.yml"
      ) %in%
        validation_docs
    ))

    ## check roxygen blocks
    expect_equal(
      readLines("inst/validation/R/Function_Roxygen_Blocks.R"),
      c("#' @title Hello World",
        "#' @editor Sample Name",
        "#' @editDate 1900-01-01",
        "#' @param name name to say hello to",
        "#' @returns string of 'Hello, ' pasted to name",
        "hello_world <- function(){}",
        "")
    )

  })})

test_that("test installing a validated bundle from source and rerunning report", {
  skip_if(!"valtools" %in% rownames(installed.packages()))
  withr::with_temp_libpaths({
  withr::with_tempdir({

    pkg_name <- paste0("example.package",sample(10:20, 1))

    ## create blank package
    quiet <- capture.output({
      usethis::create_package(pkg_name)
    })

    setwd(pkg_name)

    ## make vignette and validation dir
    dir.create("vignettes")
    dir.create("vignettes/validation")

    ## create config file
    writeLines(text = c(
      "package: example.package",
      "working_dir: vignettes",
      "output_dir: inst",
      "report_rmd_name: validation.Rmd",
      "usernames:",
      "  NewUser:",
      "    name: New User",
      "    title: new",
      "    role: user"),
      con = "vignettes/validation/validation.yml")



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

    writeLines(c(
      "#' @title Hello World",
      "#' @editor Sample Name",
      "#' @editDate 1900-01-01",
      "#' @param name name to say hello to",
      "#' @returns string of 'Hello, ' pasted to name",
      "hello_world <- function(name){",
      "  paste('Hello, ', name)",
      "}"),
      con = "R/hello_world.R")

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
      strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
               pdftools::pdf_text(validation_report_output)))[[1]]

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
    expect_true(all(
      c(
        "R/Function_Roxygen_Blocks.R",
        "requirements/sample_req.md",
        "test_cases/sample_test_case.md",
        "test_code/sample_test_code.R",
        "validation.Rmd",
        "validation.yml"

      ) %in% validation_docs
    ))

    ## check roxygen blocks
    expect_equal(
      readLines("inst/validation/R/Function_Roxygen_Blocks.R"),
      c("#' @title Hello World",
        "#' @editor Sample Name",
        "#' @editDate 1900-01-01",
        "#' @param name name to say hello to",
        "#' @returns string of 'Hello, ' pasted to name",
        "hello_world <- function(){}",
        "")
    )

    ### rerun validation report
    new_output_dir <- tempdir()

    new_validation_report_output <- vt_validate_installed_package(
      package = pkg_name,
      output_dir = new_output_dir,
      open = FALSE)

    new_validation_report_output_rendered <-
      strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
            pdftools::pdf_text(new_validation_report_output)))[[1]]

    expect_equal(
      trimws(new_validation_report_output_rendered),
      c(
        "Validation Report",
        "Sys.Date()",
        paste0("## [1] \"",Sys.Date(),"\""),
        "1"
      )
    )

  })
  })
})

test_that("Attempting rerunning report for package not built for validation throws error", {

  expect_error(
    vt_validate_installed_package(
        package = "utils",
        open = FALSE),
    "Package utils was not built with `vt_validated_build()",
    fixed = TRUE
  )

})

test_that("test validating external package", {

  withr::with_tempdir({

    dir.create("rlang_validation")

    ## create validation folder
    quiet <- capture.output({
      vt_use_validation(pkg = "rlang_validation",
                        working_dir = ".",
                        package = "rlang")
    })

    setwd("rlang_validation")

    vt_add_user_to_config(
      username = "userA",
      name = "User A",
      title = "Programmer",
      role = "Validation Lead, Specifier, Test Case Writer"
    )

    vt_add_user_to_config(
      username = "userB",
      name = "User B",
      title = "Programmer",
      role = "Tester"
    )

    vt_use_req("req_1a", username = "User A", open = FALSE)
    vt_use_test_case("test_case_1a", username = "User A", open = FALSE)
    vt_use_test_code("test_code_1a", username = "User B", open = FALSE)

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
        con = "validation.Rmd"
      )

    ## run validation report from CLI, uses version provided
    suppressMessages({
      quiet <- capture.output({
        report_file_providedv <- vt_validate_report(version = "1234",open = FALSE)
      })
    })

    ## check version was assigned as expected when provided
    expect_equal(
      basename(report_file_providedv),
      paste0("Validation_Report_rlang_v1234_",format(Sys.Date(),"%Y%m%d"),".pdf")
    )

     ## run validation report from CLI, uses package version of rlang when version is not provided
      suppressMessages({
        quiet <- capture.output({
          report_file_pkgv <- vt_validate_report(open = FALSE)
        })
      })

      ## check version was assigned as expected when not provided and is an installed package
      expect_equal(
        basename(report_file_pkgv),
        paste0("Validation_Report_rlang_v",packageVersion("rlang"),"_",format(Sys.Date(),"%Y%m%d"),".pdf")
      )

      vt_use_change_log(open = FALSE)

      ## run validation report from CLI, uses change log version when version is not provided
      suppressMessages({
        quiet <- capture.output({
          report_file_changelogv <- vt_validate_report(open = FALSE)
        })
      })

      ## check version was assigned as expected when not provided and is an installed package
      expect_equal(
        basename(report_file_changelogv),
        paste0("Validation_Report_rlang_v1.0_",format(Sys.Date(),"%Y%m%d"),".pdf")
      )


      ## validation report rendered properly
      validation_report_output_rendered_providedv <-
        strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
                                     pdftools::pdf_text(report_file_providedv)))[[1]]

      expect_equal(
        trimws(validation_report_output_rendered_providedv),
        c(
          "Validation Report",
          "Sys.Date()",
          paste0("## [1] \"",Sys.Date(),"\""),
          "1"
        )
      )

      ## validation report rendered properly
      validation_report_output_rendered_pkgv <-
        strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
                                     pdftools::pdf_text(report_file_pkgv)))[[1]]

      expect_equal(
        trimws(validation_report_output_rendered_pkgv),
        c(
          "Validation Report",
          "Sys.Date()",
          paste0("## [1] \"",Sys.Date(),"\""),
          "1"
        )
      )

      ## validation report rendered properly
      validation_report_output_rendered_changelogv <-
        strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
                                     pdftools::pdf_text(report_file_changelogv)))[[1]]

      expect_equal(
        trimws(validation_report_output_rendered_changelogv),
        c(
          "Validation Report",
          "Sys.Date()",
          paste0("## [1] \"",Sys.Date(),"\""),
          "1"
        )
      )

    })
})

test_that("test validating external - not package", {

  withr::with_tempdir({

    dir.create("R_Environment_validation")

    ## create validation folder
    quiet <- capture.output({
      vt_use_validation(pkg = "R_Environment_validation",
                        working_dir = ".",
                        package = "R Environment")
    })

    setwd("R_Environment_validation")

    vt_add_user_to_config(
      username = "userA",
      name = "User A",
      title = "Programmer",
      role = "Validation Lead, Specifier, Test Case Writer"
    )

    vt_add_user_to_config(
      username = "userB",
      name = "User B",
      title = "Programmer",
      role = "Tester"
    )

    vt_use_req("req_1a", username = "User A", open = FALSE)
    vt_use_test_case("test_case_1a", username = "User A", open = FALSE)
    vt_use_test_code("test_code_1a", username = "User B", open = FALSE)

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
      con = "validation.Rmd"
    )

    ## run validation report from CLI, uses version provided
    suppressMessages({
      quiet <- capture.output({
        report_file_providedv <- vt_validate_report(version = "1234",open = FALSE)
      })
    })

    ## check version was assigned as expected when provided
    expect_equal(
      basename(report_file_providedv),
      paste0("Validation_Report_R.Environment_v1234_",format(Sys.Date(),"%Y%m%d"),".pdf")
    )

    ## run validation report from CLI, should error since not a package & no version provided or change log exists
    expect_error(
        vt_validate_report(open = FALSE),
        "Provide validation report version number or create a change log via `vt_use_change_log()`",
        fixed = TRUE
    )

    vt_use_change_log(open = FALSE)

    ## run validation report from CLI, uses change log version when version is not provided
    suppressMessages({
      quiet <- capture.output({
        report_file_changelogv <- vt_validate_report(open = FALSE)
      })
    })

    ## check version was assigned as expected when not provided and is an installed package
    expect_equal(
      basename(report_file_changelogv),
      paste0("Validation_Report_R.Environment_v1.0_",format(Sys.Date(),"%Y%m%d"),".pdf")
    )


    ## validation report rendered properly
    validation_report_output_rendered_providedv <-
      strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
                                   pdftools::pdf_text(report_file_providedv)))[[1]]

    expect_equal(
      trimws(validation_report_output_rendered_providedv),
      c(
        "Validation Report",
        "Sys.Date()",
        paste0("## [1] \"",Sys.Date(),"\""),
        "1"
      )
    )

    ## validation report rendered properly
    validation_report_output_rendered_changelogv <-
      strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
                                   pdftools::pdf_text(report_file_changelogv)))[[1]]

    expect_equal(
      trimws(validation_report_output_rendered_changelogv),
      c(
        "Validation Report",
        "Sys.Date()",
        paste0("## [1] \"",Sys.Date(),"\""),
        "1"
      )
    )

  })
})


test_that("test validating external - report failure", {

  withr::with_tempdir({

    dir.create("R_Environment_validation")

    ## create validation folder
    quiet <- capture.output({
      vt_use_validation(pkg = "R_Environment_validation",
                        working_dir = ".",
                        package = "R Environment")
    })

    setwd("R_Environment_validation")

    vt_add_user_to_config(
      username = "userA",
      name = "User A",
      title = "Programmer",
      role = "Validation Lead, Specifier, Test Case Writer"
    )

    vt_add_user_to_config(
      username = "userB",
      name = "User B",
      title = "Programmer",
      role = "Tester"
    )

    vt_use_req("req_1a", username = "User A", open = FALSE)
    vt_use_test_case("test_case_1a", username = "User A", open = FALSE)
    vt_use_test_code("test_code_1a", username = "User B", open = FALSE)

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
      "print(a)",
      "",
      "```",
      ""),
      con = "validation.Rmd"
    )

    ## run validation report from CLI, uses version provided
    expect_error(
      vt_validate_report(version = "1234",open = FALSE),
      "Error during rendering of validation report[.] Error[:]"
    )

  })
})
