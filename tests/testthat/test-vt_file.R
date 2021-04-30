test_that("evaluting markdown files works", {
  withr::with_tempdir({

    writeLines(c(
      "#' @title sample title",
      "#' @editor sample editor",
      "#' @editDate 1900-01-01",
      "## Header",
      "+ Content",
      "  + more content",
      "+ Content 2"),
      con = "sample.md"
    )

    writeLines(c(
      "#' @title sample title",
      "#' @editor sample editor",
      "#' @editDate 1900-01-01",
      "## Header",
      "+ ##req:Dynamic_ref.1 Reference",
      "  + more content",
      "+ Content 2"),
      con = "sample2.md"
    )

    knitr::opts_knit$set("output.dir"= ".")

    referencer <- vt_dynamic_referencer$new()

    sample_output <- capture.output({
      cat(file_parse.md(file = "sample.md"))
    })

    sample_output2 <- capture.output({
      cat(file_parse.md(
        file = "sample2.md",
        reference = referencer,
        dynamic_referencing = TRUE))
    })

    sample_output3 <- capture.output({
      vt_file(file = "sample.md")
    })

    sample_output4 <- capture.output({
      vt_file(
        file = "sample2.md",
        reference = referencer,
        dynamic_referencing = TRUE)
    })

    expect_equal(
      sample_output,
      c("","## Header", "+ Content", "  + more content", "+ Content 2")
    )

    expect_equal(
      sample_output2,
      c("","## Header", "+ 1.1 Reference", "  + more content", "+ Content 2")
    )

    expect_equal(
      sample_output3,
      c("","## Header", "+ Content", "  + more content", "+ Content 2")
    )

    expect_equal(
      sample_output4,
      c("","## Header", "+ 1.1 Reference", "  + more content", "+ Content 2")
    )

  })
})

test_that("evaluating Rmarkdown files works", {
  withr::with_tempdir({
    writeLines(c(
      "---",
      "#' @title sample title",
      "#' @editor sample editor",
      "#' @editDate 1900-01-01",
      "---",
      "## Header",
      "+ Content",
      "  + more content",
      "+ Content 2",

      "```{r}",
      "print(\"hello\")",
      "```"),
      con = "sample.Rmd"
    )

    writeLines(c(
      "---",
      "title: sample title",
      "#' @title sample title",
      "#' @editor sample editor",
      "#' @editDate 1900-01-01",
      "---",
      "## Header",
      "+ ##req:Dynamic_ref.1 Reference",
      "  + more content",
      "+ Content 2",
      "```{r}",
      "print(\"hello\")",
      "```"),
      con = "sample2.Rmd"
    )

    knitr::opts_knit$set("output.dir"= ".")
    referencer <- vt_dynamic_referencer$new()
    curr_env <- environment()

    sample_output <- capture.output({
      file_parse.rmd(
        file = "sample.Rmd",
        envir = curr_env)
    })

    sample_output2 <- capture.output({
      file_parse.rmd(
        file = "sample2.Rmd",
        reference = referencer,
        dynamic_referencing = TRUE,
        envir = curr_env)
    })

    sample_output3 <- capture.output({
      vt_file(file = "sample.Rmd",
              envir = curr_env)
    })

    sample_output4 <- capture.output({
      vt_file(
        file = "sample2.Rmd",
        reference = referencer,
        dynamic_referencing = TRUE,
        envir = curr_env)
    })

    expect_equal(
      sample_output,
      c("","","",
        "## Header",
        "+ Content",
        "  + more content",
        "+ Content 2",
        "print(\"hello\")## [1] \"hello\"")
    )

    expect_equal(
      sample_output2,
      c("","","","",
        "## Header",
        "+ 1.1 Reference",
        "  + more content",
        "+ Content 2",
        "print(\"hello\")## [1] \"hello\"")
    )

    expect_equal(
      sample_output3,
      c("","","",
        "## Header",
        "+ Content",
        "  + more content",
        "+ Content 2",
        "print(\"hello\")## [1] \"hello\"")
    )

    expect_equal(
      sample_output4,
      c("","","","",
        "## Header",
        "+ 1.1 Reference",
        "  + more content",
        "+ Content 2",
        "print(\"hello\")## [1] \"hello\"")
    )

  })
})

test_that("evaluating default files works", {
  withr::with_tempdir({

    writeLines(c(
      c(
        "<table style=\"width:100%\">",
        "<tr>",
        "<th>Firstname</th>",
        "<th>Lastname</th>",
        "<th>Age</th>",
        "</tr>",
        "<tr>",
        "<td>Jill</td>",
        "<td>Smith</td>",
        "<td>50</td>",
        "</tr>",
        "<tr>",
        "<td>Eve</td>",
        "<td>Jackson</td>",
        "<td>94</td>",
        "</tr>",
        "</table>"
      )
    ),
      con = "sample.html"
    )

    writeLines(c(
      c(
        "<table style=\"width:100%\">",
        "<tr>",
        "<th>Index</th>",
        "<th>Lastname</th>",
        "<th>Age</th>",
        "</tr>",
        "<tr>",
        "<td>##req:req_this</td>",
        "<td>Smith</td>",
        "<td>50</td>",
        "</tr>",
        "<tr>",
        "<td>##req:req_that</td>",
        "<td>Jackson</td>",
        "<td>94</td>",
        "</tr>",
        "</table>"
      )
    ),
    con = "sample_dynamic.html"
    )

    writeLines(c(
      "\\begin{center}",
      "\\begin{tabular}{ c c c }",
      "cell1 & cell2 & cell3 \\\\",
      "cell4 & cell5 & cell6 \\\\",
      "cell7 & cell8 & cell9",
      "\\end{tabular}",
      "\\end{center}"
    ),
      con = "sample.tex"
    )

    writeLines(c(
      "\\begin{center}",
      "\\begin{tabular}{ c c c }",
      "##req:req_this & cell2 & cell3 \\\\",
      "##req:req_that & cell5 & cell6 \\\\",
      "cell7 & cell8 & cell9",
      "\\end{tabular}",
      "\\end{center}"
    ),
    con = "sample_dynamic.tex"
    )

    referencer <- vt_dynamic_referencer$new()


    sample_output <- capture.output({
      cat(file_parse.default(file = "sample.html"))
    })

    sample_output2 <- capture.output({
      cat(file_parse.default(file = "sample.tex"))
    })

    sample_output3 <- capture.output({
      vt_file(file = "sample.html")
    })

    sample_output4 <- capture.output({
      vt_file(file = "sample.tex")
    })

    sample_output_dynamic <- capture.output({
      cat(file_parse.default(
        file = "sample_dynamic.html",
        reference = referencer,
        dynamic_referencing = TRUE))
    })

    sample_output2_dynamic <- capture.output({
      cat(file_parse.default(
        file = "sample_dynamic.tex",
        reference = referencer,
        dynamic_referencing = TRUE))
    })

    sample_output3_dynamic <- capture.output({
      vt_file(
        file = "sample_dynamic.html",
        reference = referencer,
        dynamic_referencing = TRUE)
    })

    sample_output4_dynamic <- capture.output({
      vt_file(
        file = "sample_dynamic.tex",
        reference = referencer,
        dynamic_referencing = TRUE)
    })

    expect_equal(
      sample_output,
      c("<table style=\"width:100%\">",
        "<tr>",
        "<th>Firstname</th>",
        "<th>Lastname</th>",
        "<th>Age</th>",
        "</tr>",
        "<tr>",
        "<td>Jill</td>",
        "<td>Smith</td>",
        "<td>50</td>",
        "</tr>",
        "<tr>",
        "<td>Eve</td>",
        "<td>Jackson</td>",
        "<td>94</td>",
        "</tr>",
        "</table>")
    )

    expect_equal(
      sample_output2,
      c("\\begin{center}",
        "\\begin{tabular}{ c c c }",
        "cell1 & cell2 & cell3 \\\\",
        "cell4 & cell5 & cell6 \\\\",
        "cell7 & cell8 & cell9",
        "\\end{tabular}",
        "\\end{center}")
    )

    expect_equal(
      sample_output3,
      c("<table style=\"width:100%\">",
        "<tr>",
        "<th>Firstname</th>",
        "<th>Lastname</th>",
        "<th>Age</th>",
        "</tr>",
        "<tr>",
        "<td>Jill</td>",
        "<td>Smith</td>",
        "<td>50</td>",
        "</tr>",
        "<tr>",
        "<td>Eve</td>",
        "<td>Jackson</td>",
        "<td>94</td>",
        "</tr>",
        "</table>")
    )

    expect_equal(
      sample_output4,
      c("\\begin{center}",
        "\\begin{tabular}{ c c c }",
        "cell1 & cell2 & cell3 \\\\",
        "cell4 & cell5 & cell6 \\\\",
        "cell7 & cell8 & cell9",
        "\\end{tabular}",
        "\\end{center}")
    )

    expect_equal(
      sample_output_dynamic,
      c("<table style=\"width:100%\">",
        "<tr>",
        "<th>Index</th>",
        "<th>Lastname</th>",
        "<th>Age</th>",
        "</tr>",
        "<tr>",
        "<td>1</td>",
        "<td>Smith</td>",
        "<td>50</td>",
        "</tr>",
        "<tr>",
        "<td>2</td>",
        "<td>Jackson</td>",
        "<td>94</td>",
        "</tr>",
        "</table>")
    )

    expect_equal(
      sample_output2_dynamic,
      c("\\begin{center}",
        "\\begin{tabular}{ c c c }",
        "1 & cell2 & cell3 \\\\",
        "2 & cell5 & cell6 \\\\",
        "cell7 & cell8 & cell9",
        "\\end{tabular}",
        "\\end{center}")
    )

    expect_equal(
      sample_output3_dynamic,
      c("<table style=\"width:100%\">",
        "<tr>",
        "<th>Index</th>",
        "<th>Lastname</th>",
        "<th>Age</th>",
        "</tr>",
        "<tr>",
        "<td>1</td>",
        "<td>Smith</td>",
        "<td>50</td>",
        "</tr>",
        "<tr>",
        "<td>2</td>",
        "<td>Jackson</td>",
        "<td>94</td>",
        "</tr>",
        "</table>")
    )

    expect_equal(
      sample_output4_dynamic,
      c("\\begin{center}",
        "\\begin{tabular}{ c c c }",
        "1 & cell2 & cell3 \\\\",
        "2 & cell5 & cell6 \\\\",
        "cell7 & cell8 & cell9",
        "\\end{tabular}",
        "\\end{center}")
    )

  })
})

test_that("rendered report is as expected using vt_file", {
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
      "print(\"hello\")",
      "```"
    ),
    con = vt_path("rando_file.Rmd"))

    writeLines(
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
      ), con = "report.Rmd"
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
        "print(\"hello\")",
        "## [1] \"hello\"" ,
        "• Start documenting requirements here!",
        "• Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
        "• Start documenting test case here!",
        "1"
      )
    )

  })
})

test_that("rendered report works using file.path inside vt_file", {
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
      "print(\"hello\")",
      "```"
    ),
    con = vt_path("rando_file.Rmd"))

    writeLines(
      c("---",
        "title: test report",
        "output: pdf_document",
        "---",
        "",
        "```{r rando-file, echo = FALSE, results = 'asis'}",
        "vt_file(file=file.path(\"validation\",\"rando_file.Rmd\"))",
        "```",
        "",
        "```{r example-req, echo = FALSE, results = 'asis'}",
        "vt_file(file=file.path(\"validation\",\"requirements\",\"example_req.md\"))",
        "```",
        "",
        "```{r example-test-case, echo = FALSE, results = 'asis'}",
        "vt_file(file=file.path(\"validation\",\"test_cases\",\"example_test_case.md\"))",
        "```"
      ), con = "report.Rmd"
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
        "print(\"hello\")",
        "## [1] \"hello\"" ,
        "• Start documenting requirements here!",
        "• Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
        "• Start documenting test case here!",
        "1"
      )
    )

  })
})

test_that("rendered report works using file.path inside vt_file - vectorized", {
  withr::with_tempdir({

    vt_use_validation()
    vt_use_req("example_req.md",username = "sample")
    vt_use_test_case("example_test_case.md",username = "sample")
    vt_use_test_code("example_test_code.r",username = "sample")

    writeLines(c(
      "## header",
      "Content",
      "",
      "  - bullet 1",
      "  - bullet 2",
      "```{r}",
      "print(\"hello\")",
      "```"
    ),
    con = vt_path("rando_file.Rmd"))

    writeLines(c(
      "## header",
      "test_that(\"1.1\",{",
      " expect_equal(1,1)",
      "})"
    ),
    con = vt_path("test_code","example_test_code.r"))

    writeLines(
      c("---",
        "title: test report",
        "output: pdf_document",
        "header-includes:",
        "  - \\usepackage{float}",
        "  - \\usepackage{array}",
        "  - \\usepackage{multirow}",
        "  - \\usepackage{longtable}",
        "---",
        "",
        "```{r rando-file, echo = FALSE, results = 'asis'}",
        "vt_file(file=c(vt_path(\"rando_file.Rmd\"),
                        vt_path(\"requirements\",\"example_req.md\"),
                        vt_path(\"test_cases\",\"example_test_case.md\"),
                        vt_path(\"test_code\",\"example_test_code.r\")))",
        "```"
      ), con = "report.Rmd"
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
        "print(\"hello\")",
        "## [1] \"hello\"" ,
        "• Start documenting requirements here!",
        "• Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
        "• Start documenting test case here!",
        "Test    Results          Pass/Fail",
        "1.1     As expected      Pass",
        "1"
      )
    )

  })
})

