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

    sample_output <- capture.output({
      cat(file_parse.md(file = "sample.md"))
    })

    sample_output2 <- capture.output({
      cat(file_parse.md(file = "sample2.md",
              dynamic_referencing = TRUE))
    })

    sample_output3 <- capture.output({
      cat(vt_file(file = "sample.md"))
    })

    sample_output4 <- capture.output({
      cat(vt_file(file = "sample2.md", dynamic_referencing = TRUE))
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
      "print('hello')",
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
      "print('hello')",
      "```"),
      con = "sample2.Rmd"
    )

    knitr::opts_knit$set("output.dir"= ".")

    sample_output <- capture.output({
      cat(file_parse.rmd(file = "sample.Rmd"))
    })

    sample_output2 <- capture.output({
      cat(file_parse.rmd(file = "sample2.Rmd",
                        dynamic_referencing = TRUE))
    })

    sample_output3 <- capture.output({
      cat(vt_file(file = "sample.Rmd"))
    })

    sample_output4 <- capture.output({
      cat(vt_file(file = "sample2.Rmd", dynamic_referencing = TRUE))
    })

    expect_equal(
      sample_output,
      c("","","",
        "## Header",
        "+ Content",
        "  + more content",
        "+ Content 2",
        "print('hello')## [1] \"hello\"")
    )

    expect_equal(
      sample_output2,
      c("","","","",
        "## Header",
        "+ 1.1 Reference",
        "  + more content",
        "+ Content 2",
        "print('hello')## [1] \"hello\"")
    )

    expect_equal(
      sample_output3,
      c("","","",
        "## Header",
        "+ Content",
        "  + more content",
        "+ Content 2",
        "print('hello')## [1] \"hello\"")
    )

    expect_equal(
      sample_output4,
      c("","","","",
        "## Header",
        "+ 1.1 Reference",
        "  + more content",
        "+ Content 2",
        "print('hello')## [1] \"hello\"")
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

    sample_output <- capture.output({
      cat(file_parse.default(file = "sample.html"))
    })

    sample_output2 <- capture.output({
      cat(file_parse.default(file = "sample.tex"))
    })

    sample_output3 <- capture.output({
      cat(vt_file(file = "sample.html"))
    })

    sample_output4 <- capture.output({
      cat(vt_file(file = "sample.tex", dynamic_referencing = TRUE))
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

  })
})
