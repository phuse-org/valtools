test_that("parsing R function files as expected", {
  withr::with_tempdir({
    ## create sample md file
    fil <- tempfile(fileext = ".r")
    cat(
      c(
        "#' @title sample title",
        "#' @editor Sample Editor",
        "#' @editDate 1900-01-01",
        "#' @export",
        "hello_world <- function(name){",
        "  print(\"hello,\",name)",
        "}",
        ""
      ),
      sep = "\n",
      file = fil
    )

    block_list <- scrape_roxygen(fil)

    expect_equal(roxygen2::block_get_tag(block_list[[1]], "editor")$val,
                 "Sample Editor")

    expect_equal(roxygen2::block_get_tag(block_list[[1]], "editDate")$val,
                 "1900-01-01")
  })
})

test_that("parsing R test files as expected", {
  withr::with_tempdir({
    ## create sample md file
    fil <- tempfile(fileext = ".r")
    cat(
      c(
        "#' @editor Sample Editor",
        "#' @editDate 1900-01-01",
        "#' @export",
        "test_that(\"Test Name\",{",
        "  expect_equal(2+2,4)",
        "})",
        ""
      ),
      sep = "\n",
      file = fil
    )

    block_list <- scrape_roxygen(fil,type = "r_test_code")

    expect_equal(roxygen2::block_get_tag(block_list[[1]], "editor")$val,
                 "Sample Editor")

    expect_equal(roxygen2::block_get_tag(block_list[[1]], "editDate")$val,
                 "1900-01-01")
  })
})

test_that("parsing md files as expected", {
  withr::with_tempdir({
    ## create sample md file
    fil <- tempfile(fileext = ".md")
    cat(
      c(
        "#' @title sample title",
        "#' @editor Sample Editor",
        "#' @editDate 1900-01-01",
        "",
        "_Markdown Content_!",
        ""
      ),
      sep = "\n",
      file = fil
    )

    block_list <- scrape_roxygen(fil)

    expect_equal(roxygen2::block_get_tag(block_list[[1]], "editor")$val,
                 "Sample Editor")

    expect_equal(roxygen2::block_get_tag(block_list[[1]], "editDate")$val,
                 "1900-01-01")
  })
})

