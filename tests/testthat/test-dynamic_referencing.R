## Test rendering of dynamic numbering

test_that("Dynamic Number Referencer can initialize and identify patterns ", {

  references <- vt_dynamic_referencer$new()
  references$initialize(reference_indicator = "##")

  ## simple single matches
  simple <- c("test ##req:first_match")
  references$scrape_references(simple)

  expect_equal(
    names(references$list_references()),
    "req:first_match"
  )

  ## string replacement
  expect_equal(
    references$reference_insertion(simple),
    "test 1"
  )

  ## Add new simple matching
  simple_new <- c("test ##req:second_match")

  references$scrape_references(simple_new)

  expect_equal(
    names(references$list_references()),
    c("req:first_match","req:second_match")
  )

  ## string replacement
  expect_equal(
    references$reference_insertion(simple_new),
    "test 2"
  )

  ## Add new simple matching, alphanumeric with dashes
  simple_alphanum <- c("test ##req:3rd-match")

  references$scrape_references(simple_alphanum)

  expect_equal(
    names(references$list_references()),
    c("req:first_match","req:second_match","req:3rd-match")
  )

  ## string replacement
  expect_equal(
    references$reference_insertion(simple_alphanum),
    "test 3"
  )

  ## Add new multi matching
  multi <- c("test ##req:fourth_match some text ##req:fifth_match")

  references$scrape_references(multi)

  expect_equal(
    names(references$list_references()),
    c(
      "req:first_match",
      "req:second_match",
      "req:3rd-match",
      "req:fourth_match",
      "req:fifth_match"
    )
  )

  ## string replacement
  expect_equal(
    references$reference_insertion(multi),
    "test 4 some text 5"
  )
})

test_that("Dynamic Number Referencer can initialize and identify patterns in multi-line text", {

  references <- vt_dynamic_referencer$new()
  references$initialize(reference_indicator = "##")

  ## Add new multi string
  multi_line <- c("test ##req:string1","text ##req:string2")

  references$scrape_references(multi_line)

  expect_equal(
    names(references$list_references()),
    c("req:string1","req:string2")
  )

  ## string replacement
  expect_equal(
    references$reference_insertion(multi_line),
    c("test 1","text 2")
  )

})

test_that("Dynamic Number Referencer can use any reference identifier including specials", {

  references <- vt_dynamic_referencer$new()
  references$initialize(reference_indicator = "^^")

  sample_text <- "test ^^req:string1"

  references$scrape_references(sample_text)

  expect_equal(
    names(references$list_references()),
    c("req:string1")
  )
  ## string replacement
  expect_equal(
    references$reference_insertion(sample_text),
    c("test 1")
  )

  references2 <- vt_dynamic_referencer$new()
  references2$initialize(reference_indicator = "\\%%&&")

  sample_text <- "test \\%%&&req:string1"

  references2$scrape_references(sample_text)

  expect_equal(
    names(references2$list_references()),
    c("req:string1")
  )
  ## string replacement
  expect_equal(
    references2$reference_insertion(sample_text),
    c("test 1")
  )

  references3 <- vt_dynamic_referencer$new()
  references3$initialize(reference_indicator = "...")

  sample_text <- "test ...req:string1"

  references3$scrape_references(sample_text)

  expect_equal(
    names(references3$list_references()),
    c("req:string1")
  )
  ## string replacement
  expect_equal(
    references3$reference_insertion(sample_text),
    c("test 1")
  )

})

test_that("Dynamic Number Referencing Works on files", {

  test_referencer <- vt_dynamic_referencer$new(
    reference_indicator = "##"
  )

  ## Create test files
  test_spec <- tempfile(fileext = ".md")
  test_test_case <- tempfile(fileext = ".md")
  test_test_code <- tempfile(fileext = ".R")

  cat(
    file = test_spec,
    sep = "\n",
    c(
    "#' @title Spec ##req:dynamic_numbering",
    "#' @section Last updated by:",
    "#' User One",
    "#' @section Last update date:",
    "#' 2021-02-15",
    "",
    "#' + _Specifications_",
    "#'   + S##req:dynamic_numbering.1 User is able to reference numbers dynamically",
    "#'     + S##req:dynamic_numbering.1.1 Numbers will automatically update on rendering",
    ""))

  cat(
    file = test_test_case,
    sep = "\n",
    c(
      "#' @title Test Case ##tc:dynamic_numbering_testcase",
      "#' @section Last updated by:",
      "#' User One",
      "#' @section Last update date:",
      "#' 2021-02-15",
      "#' @section Specification coverage:",
      "#' ##tc:dynamic_numbering_testcase.1: ##req:dynamic_numbering.1, ##req:dynamic_numbering.2",
      "",
      "#' + _Test Cases_",
      "#'   + T##tc:dynamic_numbering_testcase.1 Create a sample spec with a unique reference number. Ensure the output matches 1 on rendering",
      ""))

  cat(
    file = test_test_code,
    sep = "\n",
    c(
      "#' @section Last updated by:",
      "#' User Two",
      "#' @section Last update date:",
      "#' 2021-02-15",
      "",
      "test_that(\"T##tc:dynamic_numbering_testcase.1\", {",
      "   + expect_true(TRUE)",
      "})",
      ""))

  test_spec_rendered <- dynamic_reference_rendering(
    file = test_spec,
    reference = test_referencer
  )

  test_test_case_rendered <- dynamic_reference_rendering(
    file = test_test_case,
    reference = test_referencer
  )

  test_test_code_rendered <- dynamic_reference_rendering(
    file = test_test_code,
    reference = test_referencer
  )

  expect_equal(
    test_spec_rendered,
    c(
      "#' @title Spec 1",
      "#' @section Last updated by:",
      "#' User One",
      "#' @section Last update date:",
      "#' 2021-02-15",
      "",
      "#' + _Specifications_",
      "#'   + S1.1 User is able to reference numbers dynamically",
      "#'     + S1.1.1 Numbers will automatically update on rendering",
      ""))

  expect_equal(
    test_test_case_rendered,
    c(
      "#' @title Test Case 1",
      "#' @section Last updated by:",
      "#' User One",
      "#' @section Last update date:",
      "#' 2021-02-15",
      "#' @section Specification coverage:",
      "#' 1.1: 1.1, 1.2",
      "",
      "#' + _Test Cases_",
      "#'   + T1.1 Create a sample spec with a unique reference number. Ensure the output matches 1 on rendering",
      ""))

  expect_equal(
    test_test_code_rendered,
    c(
      "#' @section Last updated by:",
      "#' User Two",
      "#' @section Last update date:",
      "#' 2021-02-15",
      "",
      "test_that(\"T1.1\", {",
      "   + expect_true(TRUE)",
      "})",
      ""))

})


test_that("Dynamic Number Referencing across rmarkdown chunks", {
  # need to alter test such that does not need `valtools` installed to test
  # if(FALSE){

  ## Create test files
  test_req1 <- tempfile(fileext = ".md")
  test_req2 <- tempfile(fileext = ".md")
  test_report <- tempfile(fileext = ".Rmd")
  test_output <- tempfile(fileext = ".pdf")


  cat(
    file = test_req1,
    sep = "\n",
    c(
      "#' @title Test Requirement ##req:Req_1",
      "#' @editor An Author",
      "#' @editDate 2021-02-15",
      "",
      "+ _Requirements_",
      "  + S##req:Req_1.1 User is able to reference numbers dynamically",
      "    + S##req:Req_1.1.1 Numbers will automatically update on rendering",
      ""))

  cat(
    file = test_req2,
    sep = "\n",
    c(
      "#' @title Test Requirement ##req:Req_2",
      "#' @editor Another Author",
      "#' @editDate 2021-02-20",
      "",
      "+ _Requirements_",
      "  + S##req:Req_2.1 User is able to reference numbers dynamically",
      "     + S##req:Req_2.1.1 Numbers will automatically update on rendering",
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
      '```{r setup}',
      '#library(valtools)',
      'test_referencer <- vt_dynamic_referencer$new(reference_indicator = "##")',
      '```',
      '\n\n',
      '```{r require-1, echo=FALSE}',
      'dynamic_reference_rendering(',
      paste0('  file = "', gsub(pattern = "\\\\",
                                replacement = "\\\\\\\\",
                                normalizePath(test_req1)), '",'),
      '  reference = test_referencer',
      ')',
      '```',
      '\n\n',
      '```{r require-2, echo=FALSE}',
      'dynamic_reference_rendering(',
      paste0('  file = "', gsub(pattern = "\\\\",
                                replacement = "\\\\\\\\",
                                normalizePath(test_req2)), '",'),
      '  reference = test_referencer',
      ')',
      '```'


      ))

  quiet <- capture.output({
  rmarkdown::render(input = test_report, clean = FALSE)
  })

  test_output_rendered <-
    readLines(gsub(test_report, pattern = ".Rmd", replacement = ".knit.md"))

  expect_equal(test_output_rendered, c(
    "---",
    "title: \"Package Validation Report\"",
    "author: \"Authors\"",
    "date: \"2021-03-01\"",
    "output: pdf_document",
    "vignette: >",
    "  %\\VignetteIndexEntry{scharpbama Package Validation Report}",
    "  \\usepackage[utf8]{inputenc}",
    "  %\\VignetteEngine{knitr::rmarkdown_notangle}",
    "---",
    "",
    "",
    "",
    "",
    "```r",
    "#library(valtools)",
    "test_referencer <- vt_dynamic_referencer$new(reference_indicator = \"##\")",
    "```",
    "",
    "",
    "",
    "",
    "```",
    "## [1] \"#' @title Test Requirement 1\"                               ",
    "## [2] \"#' @editor An Author\"                                       ",
    "## [3] \"#' @editDate 2021-02-15\"                                    ",
    "## [4] \"\"                                                           ",
    "## [5] \"+ _Requirements_\"                                           ",
    "## [6] \"  + S1.1 User is able to reference numbers dynamically\"     ",
    "## [7] \"    + S1.1.1 Numbers will automatically update on rendering\"",
    "## [8] \"\"",
    "```",
    "",
    "",
    "",
    "",
    "```",
    "## [1] \"#' @title Test Requirement 2\"                                ",
    "## [2] \"#' @editor Another Author\"                                   ",
    "## [3] \"#' @editDate 2021-02-20\"                                     ",
    "## [4] \"\"                                                            ",
    "## [5] \"+ _Requirements_\"                                            ",
    "## [6] \"  + S2.1 User is able to reference numbers dynamically\"      ",
    "## [7] \"     + S2.1.1 Numbers will automatically update on rendering\"",
    "## [8] \"\"",
    "```"
  ))
# }
})
