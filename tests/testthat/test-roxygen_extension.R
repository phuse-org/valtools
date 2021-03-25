test_that("Roxygen can read in new tags - YYYY-MM-DD", {

  simple_test_function_text <- "
  #' @title simple title
  #' @description sample function
  #' @param pkg path to package
  #' @editor Sample Person
  #' @editDate 1900-01-01
  #' @coverage
  #' test1: req1, req2, req3
  #' @export
  #' @aliases testing
  hello_world <- function(name = \"\"){
    paste(\"Hello,\",name)
  }
  "

  roxygen_block <- roxygen2::parse_text(simple_test_function_text)[[1]]

  expect_true(
    roxygen2::block_has_tags(roxygen_block, tags = c("editor"))
  )

  expect_true(
    roxygen2::block_has_tags(roxygen_block, tags = c("editDate"))
  )

  expect_true(
    roxygen2::block_has_tags(roxygen_block, tags = c("coverage"))
  )

  expect_equal(
    roxygen2::block_get_tag_value(roxygen_block,"editor"),
    "Sample Person"
  )

  expect_equal(
    roxygen2::block_get_tag_value(roxygen_block,"editDate"),
    "1900-01-01"
  )

  expect_equal(
    roxygen2::block_get_tag_value(roxygen_block,"coverage"),
    "test1: req1, req2, req3"
  )

})

test_that("Roxygen can read in new tags - MM-DD-YYYY", {

  simple_test_function_text <- "
  #' @title simple title
  #' @description sample function
  #' @param pkg path to package
  #' @editor Sample Person
  #' @editDate 01-02-2010
  #' @export
  #' @aliases testing
  hello_world <- function(name = \"\"){
    paste(\"Hello,\",name)
  }
  "

  roxygen_block <- roxygen2::parse_text(simple_test_function_text)[[1]]

  expect_true(
    roxygen2::block_has_tags(roxygen_block, tags = c("editor","editDate"))
  )

  expect_equal(
    roxygen2::block_get_tag_value(roxygen_block,"editor"),
    "Sample Person"
  )

  expect_equal(
    roxygen2::block_get_tag_value(roxygen_block,"editDate"),
    "2010-01-02"
  )

})

test_that("Roxygen can read in new tags - DD-MMMM-YYYY", {

  simple_test_function_text <- "
  #' @title simple title
  #' @description sample function
  #' @param pkg path to package
  #' @editor Sample Person
  #' @editDate 10-Jan-2021
  #' @export
  #' @aliases testing
  hello_world <- function(name = \"\"){
    paste(\"Hello,\",name)
  }
  "

  roxygen_block <- roxygen2::parse_text(simple_test_function_text)[[1]]

  expect_true(
    roxygen2::block_has_tags(roxygen_block, tags = c("editor","editDate"))
  )

  expect_equal(
    roxygen2::block_get_tag_value(roxygen_block,"editor"),
    "Sample Person"
  )

  expect_equal(
    roxygen2::block_get_tag_value(roxygen_block,"editDate"),
    "2021-01-10"
  )

})


test_that("formatting coverage section parses correctly", {
    # one test case
    cov_tag1 <- roxygen2::roxy_tag(tag = "coverage",
                                raw = "1.1: 1.1, 1.2")
    expect_s3_class(class = "roxy_tag_coverage",
                 format_coverage_text(cov_tag1))

    # multiple test cases
    cov_tag2 <- roxygen2::roxy_tag(tag = "coverage",
                                       raw = "\n1.1: 1.1, 1.2\n1.2: 1.2, 1.3\n1.3: 1.1, 1.3, 1.4",
                                       line = 4)
    expect_s3_class(class = "roxy_tag_coverage",
                    format_coverage_text(cov_tag2))


})
