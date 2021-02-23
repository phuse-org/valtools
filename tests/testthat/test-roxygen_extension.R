test_that("Roxygen can read in new tags - YYYY-MM-DD", {

  simple_test_function_text <- "
  #' @title simple title
  #' @description sample function
  #' @param pkg path to package
  #' @editor Sample Person
  #' @editDate 1900-01-01
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
    "1900-01-01"
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
