test_that("latex Number Referencing across rmarkdown chunks", {

  withr::with_tempdir({

  ## Create test files
  test_req1 <- tempfile(fileext = ".tex")
  test_req2 <- tempfile(fileext = ".tex")
  test_report <- tempfile(fileext = ".Rmd")
  test_output <- tempfile(fileext = ".pdf")

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

  quiet <- capture.output({
    test_output <- rmarkdown::render(input = test_report, clean = FALSE)
  })

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


# test_that("latex Number Referencing across rmarkdown chunks", {
#
#   # need to alter test such that does not need `valtools` installed to test
#   withr::with_tempdir({
#
#   ## Create test files
#   test_req1 <- tempfile(fileext = ".Rmd")
#   test_req2 <- tempfile(fileext = ".Rmd")
#   yml_contents <- file.path(dirname(test_req1), "_bookdown.yml")
#
#
#   ## these files will not change with release
#   yml_output <- file.path("_output.yml")
#   test_report <- file.path("index.Rmd")
#
#
#   cat(
#     file = test_req1,
#     sep = "\n",
#     c("---",
#       "#' @editor An Author",
#       "#' @editDate 2021-02-15",
#       "---",
#       "",
#       "\\rtask{req1} A section", # title
#       "",
#       "+ _Requirements_",
#       "  + S\\ref{req1}.1 User is able to reference numbers dynamically",
#       "     + S\\ref{req1}.1.1 Numbers will automatically update on rendering",
#       ""))
#
#   cat(
#     file = test_req2,
#     sep = "\n",
#     c("---",
#       "#' @editor Another Author",
#       "#' @editDate 2021-02-20",
#       "---",
#       "",
#       "\\rtask{req2} Another Section", #title
#       "",
#       "+ _Requirements_",
#       "   + S\\ref{req2}.1 User is able to reference numbers dynamically",
#       "     + S\\ref{req2}.1.1 Numbers will automatically update on rendering",
#       ""))
#
#   cat(
#     file = test_report,
#     sep = "\n",
#     c('---',
#       'title: "Validation report"',
#       'description: "This is a minimal example of a report with dynamic referencing"',
#       'output:',
#       '  bookdown::pdf_book',
#       '---',
#       '',
#       '\\newcounter{rtaskno}',
#       '\\DeclareRobustCommand{\\rtask}[1]{%',
#       '  \\refstepcounter{rtaskno}%',
#       '  \\thertaskno\\label{#1}}',
#       '\n\n'))
#
#   cat(
#     file = yml_contents,
#     sep = "\n",
#     c(
#       'book_filename: "Validation_report_V1.0"',
#       'rmd_files: [',
#       paste0(basename(test_report), ","),
#       paste0(basename(test_req1), ","),
#       paste0(basename(test_req2), ","),
#       ']',
#       'output_dir: "docs"'
#
#     ))
#
#   cat(
#     file = yml_output,
#     sep = "\n",
#     c(
#       "bookdown::pdf_book:",
#       "  keep_tex: yes"
#     ))
#
#   quiet <- capture.output({
#     output <- bookdown::render_book(
#       input = "index.Rmd",
#       output_format = "bookdown::pdf_book"
#     )
#   })
#
#   browser()
#
#   test_output_rendered <- trimws(strsplit(split = "\r\n",gsub("((\r)|(\n))+","\r\n",
#              pdftools::pdf_text(
#                output
#                )))[[1]])
#
#   expect_equal("1", substr(test_output_rendered[2], 1,1))
#   expect_equal("S1.1", substr(trimws(test_output_rendered[4]), 3, 6))
#   expect_equal("S1.1.1", substr(trimws(test_output_rendered[5]), 3, 8))
#
#   expect_equal("2", substr(test_output_rendered[6], 1, 1))
#   expect_equal("S2.1", substr(trimws(test_output_rendered[8]), 3, 6))
#   expect_equal("S2.1.1", substr(trimws(test_output_rendered[9]), 3, 8))
#
#  })
#
# })
