test_that("parsing R function files as expected", {

  file_content <- structure("
    #' @title sample title
    #' @editor Sample Editor
    #' @editDate 1900-01-01
    #' @param name name printed
    #' @export
    hello_world <- function(name){
      print(\"hello,\",name)
    }
    ",
    class = "r")

  block_list <- parse_roxygen(file_content)

  expect_equal(
    roxygen2::block_get_tag(block_list[[1]],"editor")$val,
    "Sample Editor"
  )

  expect_equal(
    roxygen2::block_get_tag(block_list[[1]],"editDate")$val,
    "1900-01-01"
  )

})

test_that("parsing R 'defunct' function files as expected", {

  file_content <- roxy_text(
    "
    #' some_function
    #' @editor Sample Editor
    #' @editDate 1900-01-01
    #' @keywords internal
    #' @note Defunct
    #' @noRd
    NULL
    ",
    class = "r"
  )

  file_content_2 <- roxy_text(
    "
    #' @editor Sample Editor
    #' @editDate 1900-01-01
    #' @keywords internal
    #' @note Defunct
    #' @noRd
    NULL
    ",
    file = "defunct.R",
    class = "r"
  )

  block_list <- parse_roxygen(file_content)

  expect_equal(
    roxygen2::block_get_tag(block_list[[1]],"editor")$val,
    "Sample Editor"
  )

  expect_equal(
    roxygen2::block_get_tag(block_list[[1]],"editDate")$val,
    "1900-01-01"
  )

  expect_equal(
    block_list[[1]]$object$alias,
    "some_function"
  )

  expect_equal(
    block_list[[1]]$object$topic,
    "some_function"
  )

  expect_error(
    parse_roxygen(file_content_2),
    paste0("NULL/deprecated functions must have a title.\n",
           "Review file `defunct.R`, line 7")
  )

})

test_that("parsing R function files with old nomenclature as expected", {

  file_content <- structure("
    #' @title sample title
    #' @section last updated by:
    #' Sample Editor
    #' @section last update date:
    #' 1900-01-01
    #' @param name name printed
    #' @export
    #' @rdname test
    hello_world <- function(name){
      print(\"hello,\",name)
    }
    ",
    class = "r")

  warnings <- capture_warnings({
    block_list <- parse_roxygen(file_content)
  })

  expect_equal(
    roxygen2::block_get_tag(block_list[[1]],"editor")$val,
    "Sample Editor"
  )

  expect_equal(
    roxygen2::block_get_tag(block_list[[1]],"editDate")$val,
    "1900-01-01"
  )

  expect_equal(
    warnings,
    c(
    "`@section last updated by:`is superseded.\nUse `@editor Sample Editor` instead.",
    "`@section last update date:`is superseded.\nUse `@editDate 1900-01-01` instead."
    )
  )
})

test_that("parsing R test code files as expected", {

  fil <- tempfile(fileext = ".md")
  cat(
    c(
      "",
      "#' @editor Sample Editor",
      "#' @editDate 1900-01-01",
      "test_that(\"Test Name\",{",
      "  expect_equal(2+2,4)",
      "})",
      ""
    ),
    sep = "\n",
    file = fil
  )

  file_content <- roxy_text(readLines(fil),
                            file = fil,
                            class = "r_test_code")

  file_content_2 <- structure(
    "
    #' @editor Sample Editor
    #' @editDate 1900-01-01
    test_that(\"Test Name\",{
      expect_equal(2+2,4)
    })

    #' @editor Sample Editor 2
    #' @editDate 1900-01-02
    test_that(\"Test Name 2\",{
      expect_equal(2+3,5)
    })

    ",
    class = "r_test_code"
  )

  block_list <- parse_roxygen(file_content)
  block_list_2 <- parse_roxygen(file_content_2)

  expect_equal(roxygen2::block_get_tag(block_list[[1]], "editor")$val,
               "Sample Editor")
  expect_equal(sapply(block_list_2,roxygen2::block_get_tag_value, "editor"),
               c("Sample Editor","Sample Editor 2"))
  expect_equal(roxygen2::block_get_tag(block_list[[1]], "editDate")$val,
               "1900-01-01")
  expect_equal(sapply(block_list_2,roxygen2::block_get_tag_value, "editDate"),
               c("1900-01-01","1900-01-02"))
  expect_equal(block_list[[1]]$object$alias,"Test Name")
  expect_equal(sapply(block_list_2,function(block)block$object$alias),
               c("Test Name","Test Name 2"))
  expect_equal(block_list[[1]]$object$topic,
               "Test Name")
  expect_equal(sapply(block_list_2,function(block)block$object$topic),
               c("Test Name","Test Name 2"))
})

test_that("parsing R test code files with the same test name throws an error", {

  file_content <- structure(
    "
    #' @editor Sample Editor
    #' @editDate 1900-01-01
    test_that(\"Test Name\",{
      expect_equal(2+2,4)
    })

    #' @editor Sample Editor
    #' @editDate 1900-01-01
    test_that(\"Test Name\",{ ## should have a different test name!
      expect_equal(2+3,5)
    })

    ",
    file = "test_file_001.R",
    class = "r_test_code"
  )

  expect_error(
    parse_roxygen(file_content),
    paste0("Duplicated Test Code names in `test_file_001.R`.\n",
    "Each test must have a distinct name:\n\n",
    " Test.Name Lines\n",
    " Test Name  4,10"),
    fixed = TRUE
  )

})

test_that("parsing md function files as expected", {

  withr::with_tempdir({

  ## create sample md file
  fil <- tempfile(fileext = ".md")
  cat(c(
    "#' @title sample title",
    "#' @editor Sample Editor",
    "#' @editDate 1900-01-01",
    "",
    "_Markdown Content_!",
    ""),
    sep = "\n",
    file = fil)
  file_content <- roxy_text(
    readLines(fil),
    file = fil,
    class = "md")

  fil2 <- tempfile(fileext = ".md")
  cat(c(
    "#' @editor Sample Editor",
    "#' @editDate 1900-01-01",
    "",
    "_Markdown Content_!",
    ""),
    sep = "\n",
    file = fil2)
  file_content_2 <- roxy_text(
    readLines(fil2),
    file = "bad.md",
    class = "md")

  block_list <- parse_roxygen(file_content)

  expect_equal(
    roxygen2::block_get_tag(block_list[[1]],"editor")$val,
    "Sample Editor"
  )

  expect_equal(
    roxygen2::block_get_tag(block_list[[1]],"editDate")$val,
    "1900-01-01"
  )

  expect_error(
    parse_roxygen(file_content_2),
    paste0("All markdown roxygen headers must have a title.\n",
           "Review file `bad.md`")
  )


  })

})

test_that("parsing Rmd function files as expected", {

  withr::with_tempdir({

    ## create sample md file
    fil <- tempfile(fileext = ".Rmd")
    cat(c(
      "---",
      "#' @title sample title",
      "#' @editor Sample Editor",
      "#' @editDate 1900-01-01",
      "---",
      "",
      "_Markdown Content_!",
      "```{r}",
      "print(\"Hello World\")",
      "```",
      ""),
      sep = "\n",
      file = fil)

    file_content <- roxy_text(
      readLines(fil),
      file = fil,
      class = "rmd")

    fil2 <- tempfile(fileext = ".md")
    cat(c(
      "---",
      "#' @editor Sample Editor",
      "#' @editDate 1900-01-01",
      "---",
      "",
      "_Markdown Content_!",
      "```{r}",
      "print(\"Hello World\")",
      "```",
      ""),
      sep = "\n",
      file = fil2)

    file_content_2 <- roxy_text(
      readLines(fil2),
      file = "bad.rmd",
      class = "rmd")

    block_list <- parse_roxygen(file_content)

    expect_equal(
      roxygen2::block_get_tag(block_list[[1]],"editor")$val,
      "Sample Editor"
    )

    expect_equal(
      roxygen2::block_get_tag(block_list[[1]],"editDate")$val,
      "1900-01-01"
    )

    expect_error(
      parse_roxygen(file_content_2),
      paste0("All markdown roxygen headers must have a title.\n",
             "Review file `bad.rmd`")
    )


  })
})

test_that("writing the block list can subset to keep blocks with specific tags", {

  withr::with_tempdir({


  writeLines(c("",
    "#' @title sample title",
    "#' @editor Sample Editor",
    "#' @editDate 1900-01-01",
    "#' @param name name printed",
    "#' @export",
    "hello_world <- function(name){",
    "  print(\"hello,\",name)",
    "}",
    "",
    "#' @title sample title2",
    "#' @param name name printed",
    "#' @export",
    "hello_world2 <- function(name){",
    "  print(\"hello,\",name,\", my friend\")",
    "}",
    ""),
    con = "hello.r")

  block_list <- vt_scrape_roxygen("hello.r")

  subset_list <- subset_blocks(
    block_list,
    tags = c("editor","editDate")
  )

  expect_equal(
    length(subset_list),
    1
  )

  write_roxygen_block(
    roxy_list = subset_list,
    path = "roxy_copy.R",
    append = FALSE)

  write_roxygen_block(
    roxy_list = block_list,
    path = "roxy_copy2.R",
    only_with_tags = c("editor","editDate"),
    append = FALSE)

  expect_equal(
    readLines("roxy_copy.R"),
    c(
      "#' @title sample title",
      "#' @editor Sample Editor",
      "#' @editDate 1900-01-01",
      "#' @param name name printed",
      "#' @export ",
      "hello_world <- function(){}",
      ""
    )
  )

  expect_equal(
    readLines("roxy_copy2.R"),
    c(
      "#' @title sample title",
      "#' @editor Sample Editor",
      "#' @editDate 1900-01-01",
      "#' @param name name printed",
      "#' @export ",
      "hello_world <- function(){}",
      ""
    )
  )


  })

})

