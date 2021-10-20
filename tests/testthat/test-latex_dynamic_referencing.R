test_that("latex Number Referencing across rmarkdown chunks", {

  skip_if_not_installed("valtools")
  withr::with_tempdir({

  ## Create test files
  test_req1 <- tempfile(fileext = ".tex", tmpdir = getwd())
  test_req2 <- tempfile(fileext = ".tex", tmpdir = getwd())
  test_report <- tempfile(fileext = ".Rmd", tmpdir = getwd())
  test_output <- tempfile(fileext = ".pdf", tmpdir = getwd())

  cat(
    file = test_req1,
    sep = "\n",
    c("---",
      "#' @editor An Author",
      "#' @editDate 2021-02-15",
      "---",
      "\\rtask{req1} A section", # title
      "",
      "+ _Requirements_",
      "  + S\\ref{req1}.1 User is able to reference numbers dynamically",
      "     + S\\ref{req1}.1.1 Numbers will automatically update on rendering",
      ""))

  cat(
    file = test_req2,
    sep = "\n",
    c("---",
      "#' @editor Another Author",
      "#' @editDate 2021-02-20",
      "---",
      "\\rtask{req2} Another Section", #title
      "",
      "+ _Requirements_",
      "   + S\\ref{req2}.1 User is able to reference numbers dynamically",
      "     + S\\ref{req2}.1.1 Numbers will automatically update on rendering",
      ""))

  cat(
    file = test_report,
    sep = "\n",
    c(
      '---',
      'title: "Package Validation Report"',
      'author: "Authors"',
      'date: "2021-03-01"',
      'output: pdf_document',
      'vignette: >',
      '  %\\VignetteIndexEntry{scharpbama Package Validation Report}',
      '  \\usepackage[utf8]{inputenc}',
      '  %\\VignetteEngine{knitr::rmarkdown_notangle}',
      '---',
      '\n\n',
      '\\newcounter{rtaskno}',
      '\\DeclareRobustCommand{\\rtask}[1]{%',
      '  \\refstepcounter{rtaskno}%',
      '  \\thertaskno\\label{#1}}',
      '\n\n',
      paste0('```{r require-1, child="', gsub(pattern = "\\\\", replacement = "\\\\\\\\", test_req1), '"}'),
      '```',
      '\n\n',
      paste0('```{r require-2, child="', gsub(pattern = "\\\\", replacement = "\\\\\\\\", test_req2), '"}'),
      '```'

    ))


  suppressWarnings({
  quiet <- capture.output({
    test_output <- rmarkdown::render(input = test_report, clean = FALSE, quiet = TRUE)
  })})


  test_output_rendered <-trimws(strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
       pdftools::pdf_text(test_output)))[[1]])

  expect_equal("1", substr(test_output_rendered[4], 1,1))
  expect_equal("S1.1", substr(test_output_rendered[6], 3, 6))
  expect_equal("S1.1.1", substr(trimws(test_output_rendered[7]), 3, 8))

  expect_equal("2", substr(test_output_rendered[8], 1, 1))
  expect_equal("S2.1", substr(trimws(test_output_rendered[10]), 3, 6))
  expect_equal("S2.1.1", substr(trimws(test_output_rendered[11]), 3, 8))

 })
})


test_that("latex Number Referencing across rmarkdown chunks", {
  ## this test demonstrates how to use native latex + bookdown::render_book for dynamic labeling
  ## does not depend on valtools dynamic labeling
 skip_if_not_installed("valtools")
 skip_if_not(rmarkdown::pandoc_version() == numeric_version("2.7.3"))
 
 withr::with_tempdir({

  ## Create test files
  test_req1 <- tempfile(fileext = ".Rmd", tmpdir = getwd())
  test_req2 <- tempfile(fileext = ".Rmd", tmpdir = getwd())
  yml_contents <- file.path(dirname(test_req1), "_bookdown.yml")

  ## these files will not change with release
  yml_output <- file.path(dirname(test_req1), "_output.yml")
  test_report <- tempfile(fileext = ".Rmd", tmpdir = getwd())


  cat(
    file = test_req1,
    sep = "\n",
    c("---",
      "#' @editor An Author",
      "#' @editDate 2021-02-15",
      "---",
      "\\rtask{req1} A section", # title
      "",
      "+ _Requirements_",
      "  + S\\ref{req1}.1 User is able to reference numbers dynamically",
      "     + S\\ref{req1}.1.1 Numbers will automatically update on rendering",
      ""))

  cat(
    file = test_req2,
    sep = "\n",
    c("---",
      "#' @editor Another Author",
      "#' @editDate 2021-02-20",
      "---",
      "\\rtask{req2} Another Section", #title
      "",
      "+ _Requirements_",
      "   + S\\ref{req2}.1 User is able to reference numbers dynamically",
      "     + S\\ref{req2}.1.1 Numbers will automatically update on rendering",
      ""))
  cat(
    file = test_report,
    sep = "\n",
    c(   '\\newcounter{rtaskno}',
         '\\DeclareRobustCommand{\\rtask}[1]{%',
         '  \\refstepcounter{rtaskno}%',
         '  \\thertaskno\\label{#1}}',
         '\n\n'))

  cat(
    file = yml_contents,
    sep = "\n",
    c(
      'book_filename: "Validation_report_V1.0"',
      paste0('rmd_files: [','"', basename(test_report), '", "', basename(test_req1), '", "',
      basename(test_req2), '"',  ']'),
      'output_dir: "docs"'

    ))

  cat(
    file = yml_output,
    sep = "\n",
    c(
      'bookdown::pdf_book:',
        'keep_tex: yes'

    ))

  setwd(dirname(test_report))

  suppressWarnings({
  quiet <- capture_output({
  bookdown::render_book(basename(test_report), quiet = TRUE,)
  })})

  pdf_report_name <- file.path(dirname(test_report),
                               "docs",
                               "Validation_report_V1.0.pdf")

  test_output_rendered <-
    trimws(strsplit(split = "\r\n", gsub("((\r)|(\n))+","\r\n",
                                 pdftools::pdf_text(pdf_report_name)))[[1]])

  expect_equal("1", substr(test_output_rendered[2], 1,1))
  expect_equal("S1.1", substr(trimws(test_output_rendered[4]), 3, 6))
  expect_equal("S1.1.1", substr(trimws(test_output_rendered[5]), 3, 8))

  expect_equal("2", substr(test_output_rendered[6], 1, 1))
  expect_equal("S2.1", substr(trimws(test_output_rendered[8]), 3, 6))
  expect_equal("S2.1.1", substr(trimws(test_output_rendered[9]), 3, 8))

})
})
