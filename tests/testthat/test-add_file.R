test_that("validation file path finds files within the validation infrastructure, throws error otherwise", {
  withr::with_tempdir({

    vt_use_validation()
    vt_use_req("example_req.md",username = "sample")

    fp <- validation_file_path(
      file = "example_req.md",
      ref = vt_path()
    )

    expect_equal(
      as.character(fp),
      "requirements/example_req.md"
    )

    expect_equal(
      class(fp),
      c("md","validation_file_path")
    )

    expect_error(
      validation_file_path("FAKE_FILE"),
      "File `FAKE_FILE` not found."
    )
  })
})

test_that("add_file finds file and adds a reading section to the current report", {
  withr::with_tempdir({

    vt_use_validation()
    vt_use_req("example_req.md",username = "sample")

    file.create("report.Rmd")
    file.create("report_dynamic.Rmd")
    file.create("report2.Rmd")
    file.create("report_dynamic2.Rmd")

    add_file.default("requirements/example_req.md",report = "report.Rmd")
    add_file.default("requirements/example_req.md",report = "report_dynamic.Rmd", dynamic_referencing = TRUE)
    add_file("requirements/example_req.md",report = "report2.Rmd")
    add_file("requirements/example_req.md",report = "report_dynamic2.Rmd", dynamic_referencing = TRUE)

    report_text <- readLines("report.Rmd")
    report_dynamic_text <- readLines("report_dynamic.Rmd")
    report_text2 <- readLines("report2.Rmd")
    report_dynamic_text2 <- readLines("report_dynamic2.Rmd")

    expect_equal(
      report_text,
      c(
        "",
        "```{r example-req, echo = FALSE, results = 'asis'}",
        "vt_file(file=vt_path(\"requirements/example_req.md\"))",
        "```"
        )
      )

    expect_equal(
      report_dynamic_text,
      c(
        "",
        "```{r example-req, echo = FALSE, results = 'asis'}",
        "vt_file(file=vt_path(\"requirements/example_req.md\"), dynamic_referencing = TRUE)",
        "```"
      )
    )

    expect_equal(
      report_text2,
      c(
        "",
        "```{r example-req, echo = FALSE, results = 'asis'}",
        "vt_file(file=vt_path(\"requirements/example_req.md\"))",
        "```"
      )
    )

    expect_equal(
      report_dynamic_text2,
      c(
        "",
        "```{r example-req, echo = FALSE, results = 'asis'}",
        "vt_file(file=vt_path(\"requirements/example_req.md\"), dynamic_referencing = TRUE)",
        "```"
      )
    )
  })
})

test_that("add_file finds test files and adds an evaluation section to the current report", {
  withr::with_tempdir({

    vt_use_validation()
    vt_use_test_code("example_code.r",username = "sample")

    file.create("report.Rmd")
    file.create("report_dynamic.Rmd")
    file.create("report2.Rmd")
    file.create("report_dynamic2.Rmd")

    test_code <- validation_file_path(
      file = "example_code.r",
      ref = vt_path()
    )

    add_file.r_test_code(test_code,report = "report.Rmd")
    add_file.r_test_code(test_code,report = "report_dynamic.Rmd", dynamic_referencing = TRUE)
    add_file(test_code,  report = "report2.Rmd")
    add_file(test_code,report = "report_dynamic2.Rmd", dynamic_referencing = TRUE)

    report_text <- readLines("report.Rmd")
    report_dynamic_text <- readLines("report_dynamic.Rmd")
    report_text2 <- readLines("report2.Rmd")
    report_dynamic_text2 <- readLines("report_dynamic2.Rmd")

    expect_equal(
      report_text,
      c(
        "",
        "```{r example-code, echo = FALSE, results = 'asis'}",
        "results <- vt_run_test_code_file(file=\"example_code.r\")",
        "vt_kable_test_code(results)",
        "```"
      )
    )

    expect_equal(
      report_dynamic_text,
      c(
        "",
        "```{r example-code, echo = FALSE, results = 'asis'}",
        "results <- vt_run_test_code_file(file=\"example_code.r\")",
        "results <- dynamic_reference_rendering(results)",
        "vt_kable_test_code(results)",
        "```"
      )
    )

    expect_equal(
      report_text2,
      c(
        "",
        "```{r example-code, echo = FALSE, results = 'asis'}",
        "results <- vt_run_test_code_file(file=\"example_code.r\")",
        "vt_kable_test_code(results)",
        "```"
      )
    )

    expect_equal(
      report_dynamic_text2,
      c(
        "",
        "```{r example-code, echo = FALSE, results = 'asis'}",
        "results <- vt_run_test_code_file(file=\"example_code.r\")",
        "results <- dynamic_reference_rendering(results)",
        "vt_kable_test_code(results)",
        "```"
      )
    )
  })
})

test_that("add_file_to_report is a simple wrapper around add_file", {
  withr::with_tempdir({

    vt_use_validation()
    vt_use_req("example_req.md",username = "sample")
    vt_use_test_code("example_code.r",username = "sample")


    file.create("report.Rmd")
    file.create("report_dynamic.Rmd")

    #standard
    add_file_to_report(
      file = "example_req.md",
      report = "report.Rmd")
    add_file_to_report(
      file = "example_code.r",
      report = "report.Rmd")

    #dynamic
    add_file_to_report(
      file = "example_req.md",
      report = "report_dynamic.Rmd",
      dynamic_referencing = TRUE)
    add_file_to_report(
      file = "example_code.r",
      report = "report_dynamic.Rmd",
      dynamic_referencing = TRUE)


    report_text <- readLines("report.Rmd")
    report_dynamic_text <- readLines("report_dynamic.Rmd")

    expect_equal(
      report_text,
      c("",
        "```{r example-req, echo = FALSE, results = 'asis'}",
        "vt_file(file=vt_path(\"requirements/example_req.md\"))",
        "```",
        "",
        "```{r example-code, echo = FALSE, results = 'asis'}",
        "results <- vt_run_test_code_file(file=\"example_code.r\")",
        "vt_kable_test_code(results)",
        "```"
      )
    )

    expect_equal(
      report_dynamic_text,
      c("",
        "```{r example-req, echo = FALSE, results = 'asis'}",
        "vt_file(file=vt_path(\"requirements/example_req.md\"), dynamic_referencing = TRUE)",
        "```",
        "",
        "```{r example-code, echo = FALSE, results = 'asis'}",
        "results <- vt_run_test_code_file(file=\"example_code.r\")",
        "results <- dynamic_reference_rendering(results)",
        "vt_kable_test_code(results)",
        "```"
      )
    )

  })
})

test_that("added files and then rendered report is as expected", {
  withr::with_tempdir({

    vt_use_validation()
    vt_use_req("example_req.md",username = "sample")
    vt_use_test_case("example_test_case.md",username = "sample")

    writeLines(c(
      "## header",
      "Content",
      "",
      "  - bullet 1",
      "  - bullet 2",
      "```{r}",
      "print('hello')",
      "```"
    ),
    con = vt_path("rando_file.Rmd"))

    writeLines(
      c("---",
        "title: test report",
        "output: pdf_document",
        "---"),
      con = "report.Rmd"
    )

    #standard
    add_file_to_report(
      file = "rando_file.Rmd",
      report = "report.Rmd")
    add_file_to_report(
      file = "example_req.md",
      report = "report.Rmd")
    add_file_to_report(
      file = "example_test_case.md",
      report = "report.Rmd")

    report_text <- readLines("report.Rmd")

    expect_equal(
      report_text,
      c("---",
        "title: test report",
        "output: pdf_document",
        "---",
        "",
        "```{r rando-file, echo = FALSE, results = 'asis'}",
        "vt_file(file=vt_path(\"rando_file.Rmd\"))",
        "```",
        "",
        "```{r example-req, echo = FALSE, results = 'asis'}",
        "vt_file(file=vt_path(\"requirements/example_req.md\"))",
        "```",
        "",
        "```{r example-test-case, echo = FALSE, results = 'asis'}",
        "vt_file(file=vt_path(\"test_cases/example_test_case.md\"))",
        "```"
      )
    )

    ## test rendering report
    quiet <- capture_warnings({
      quiet <- capture.output({
        rmarkdown::render("report.Rmd")
    })})

    test_report_rendered <-
      trimws(strsplit(split = "\r\n", gsub("((\r)|(\n))+","\r\n",
                                    pdftools::pdf_text("report.pdf")))[[1]])



    expect_equal(
      test_report_rendered,
      c(
        "test report",
        "header",
        "Content",
        "• bullet 1",
        "• bullet 2",
        "print(’hello’)" ,
        "## [1] \"hello\"" ,
        "• Start documenting requirements here!",
        "• Test Case",
        "– Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
        "– Start documenting test case here!",
        "1"
      )
    )

  })
})
