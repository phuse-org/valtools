test_that("Copying roxygen comments works", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c("#' @title Test",
        "#' @param name name to say hello to",
        "#' @editor Test Person",
        "#' @editDate 1900-01-01",
        "hello_world <- function(name){",
        "  print('hello,',name)",
        "}"),
      con = "R/hello.R"
    )

    roxygen_copy(
      from = "R",
      to = "function_roxygen.R"
    )

    function_roxygen_copy <- readLines("function_roxygen.R")

    expect_equal(
      function_roxygen_copy,
      c(
      "#' @title Test",
      "#' @param name name to say hello to",
      "#' @editor Test Person",
      "#' @editDate 1900-01-01",
      "hello_world <- function(){}",
      ""
      )
    )
  })
})

test_that("Copying roxygen comments works", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c("#' @title Deprecated_Function",
        "#' @editor Test Person",
        "#' @editDate 1900-01-01",
        "NULL"),
      con = "R/deprecated.R"
    )

    writeLines(
      c("#' @title Deprecated_Function",
        "#' @editor Test Person",
        "#' @editDate 1900-01-01",
        "#' @deprecate Deprecated in v1.2",
        "NULL"),
      con = "R/deprecated2.R"
    )

    roxygen_copy(
      from = "R",
      to = "function_roxygen.R"
    )

    function_roxygen_copy <- readLines("function_roxygen.R")

    expect_equal(
      function_roxygen_copy,
      c(
        "#' @title Deprecated_Function",
        "#' @editor Test Person",
        "#' @editDate 1900-01-01",
        "NULL",
        "",
        "#' @title Deprecated_Function",
        "#' @editor Test Person",
        "#' @editDate 1900-01-01",
        "#' @deprecate Deprecated in v1.2",
        "NULL",
        ""
      )
    )
  })
})

test_that("Copying roxygen and error gets returned", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c("#' @name Deprecated_Function",
        "#' @editor Test Person",
        "#' @editDate 1900-01-01",
        "NULL"),
      con = "R/deprecated.R"
    )

    expect_error(
      roxygen_copy(from = "R",
                   to = "function_roxygen.R"),
      "Error in copying function roxygen comments:\n",
      fixed = TRUE
    )

  })
})

