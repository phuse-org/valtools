context("function author table")

test_that("works on valtools locally", {
 skip_if(grepl(dirname(here::here()), pattern = "\\.Rcheck"))
 author_table <- vt_scrape_function_authors()
 # valtools includes .R files with NULL, author info, missing author info, @section in example
 testthat::expect_identical(author_table$title, c("vt_scrape_function_authors",
                                                    "vt_scrape_section",
                                                    "vt_scrape_val_env"))
})


test_that("dummy case in temp dir", {
  temp_path <- tempdir()

  capture_output <- usethis::create_package(path = temp_path, open = FALSE,
                                            rstudio = TRUE)
  this_file1 <- file.path(temp_path, "R/hello_world.R")
  fs::file_create(path = this_file1)
  cat(file = this_file1, append = FALSE,
      "#' @title Hello World!\n#' @description A description\n#' @param Param1",
      "definition\n#' @return string greeting the argument\n#'",
      "@export\nhello_world <- function(Param1){\n  # A function",
      "with no authorship\n  paste('Hello', Param1)\n}")

  # use case: multiple functions in single file
  # export tag is ignored
  # ignores extra spaces
  # accommodates minor variation in date format
  # accommodates minor variation in formatting of last update date
  this_file2 <- file.path(temp_path, "R/second_func.R")
  fs::file_create(path = this_file2)
  cat(file = this_file2, append = FALSE,
      "#' @title Second Function\n#' @description A description\n#' @param Param1",
      "definition\n#' @param Param2 definiion\n#' @section Last updated by:\n#' Author",
      "Name\n#' @section Last updated date:\n#' 2021-01-01\n#'",
      "@export\nsecond_func <- function(Param1, Param2){\n  return('Something')\n}",
      "\n\n",
      "\n#' @title Third Function\n#' @description A description\n#' @param Param1",
      "definition\n#' @param Param2 definiion\n#' @section last updated by:\n#'   Author2",
      "Name\n#' @section Last Update Date:\n#' 12/01/2021\nthird_func",
      "<- function(Param1, Param2){\n  return('Something')\n}")

  # pulls function authorship from roxygen2 block, even if no function included
  # ignores stray comments
  this_file3 <- file.path(temp_path, "R/null_doc.R")
  fs::file_create(path = this_file3)
  cat(file = this_file3, append = FALSE,
      "#' @name Fourth_Function\n#' @description A description\n#' @param Param1",
      "definition\n#' @param Param2 definiion\n#' @section Last updated by:\n#' Author",
      "Name\n#' @section Last updated date:\n#' 2021/12/01\nNULL\n\n#'",
      "@name Fifth Function\n#' @description A help page that won't be flagged for",
      "function authorship\n#' @param Param1",
      "definition\n#' @param Param2 definiion\nNULL\n\n# some stray comment")

  func_info <- vt_scrape_function_authors(temp_path)
  testthat::expect_equal(func_info,
                             data.frame(title = c("Fourth_Function", "second_func", "third_func"),
                                        author = c("Author Name", "Author Name", "Author2 Name"),
                                        last_updated_date = lubridate::ymd(c("2021-12-01", "2021-01-01", "2021-12-01"))))
  unlink(temp_path,
         recursive = TRUE,
         force = TRUE
  )

})
