## Test rendering of dynamic numbering

test_that("Dynamic Number Referencer can initialize and identify patterns ", {
  
  references <- vt_dynamic_referencer$new()
  references$initialize(reference_indicator = "@@")
  
  ## simple single matches
  simple <- c("test @@first_match")
  references$scrape_references(simple)
  
  expect_equal(
    names(references$list_references()),
    "first_match"
  )
  
  ## string replacement
  expect_equal(
    references$reference_insertion(simple),
    "test 1"
  )
  
  ## Add new simple matching
  simple_new <- c("test @@second_match")
  
  references$scrape_references(simple_new)
  
  expect_equal(
    names(references$list_references()),
    c("first_match","second_match")
  )
  
  ## string replacement
  expect_equal(
    references$reference_insertion(simple_new),
    "test 2"
  )
  
  ## Add new simple matching, alphanumeric with dashes
  simple_alphanum <- c("test @@3rd-match")
  
  references$scrape_references(simple_alphanum)
  
  expect_equal(
    names(references$list_references()),
    c("first_match","second_match","3rd-match")
  )
  
  ## string replacement
  expect_equal(
    references$reference_insertion(simple_alphanum),
    "test 3"
  )
  
  ## Add new multi matching
  multi <- c("test @@fourth_match some text @@fifth_match")
  
  references$scrape_references(multi)
  
  expect_equal(
    names(references$list_references()),
    c("first_match","second_match","3rd-match","fourth_match","fifth_match")
  )
  
  ## string replacement
  expect_equal(
    references$reference_insertion(multi),
    "test 4 some text 5"
  )
})

test_that("Dynamic Number Referencer can initialize and identify patterns in multi-line text", {
  
  references <- vt_dynamic_referencer$new()
  references$initialize(reference_indicator = "@@")
  
  ## Add new multi string
  multi_line <- c("test @@string1","text @@string2")
  
  references$scrape_references(multi_line)
  
  expect_equal(
    names(references$list_references()),
    c("string1","string2")
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
  
  sample_text <- "test ^^string1"
  
  references$scrape_references(sample_text)
  
  expect_equal(
    names(references$list_references()),
    c("string1")
  )
  ## string replacement
  expect_equal(
    references$reference_insertion(sample_text),
    c("test 1")
  )
  
  references2 <- vt_dynamic_referencer$new()
  references2$initialize(reference_indicator = "\\%%&&")
  
  sample_text <- "test \\%%&&string1"
  
  references2$scrape_references(sample_text)
  
  expect_equal(
    names(references2$list_references()),
    c("string1")
  )
  ## string replacement
  expect_equal(
    references2$reference_insertion(sample_text),
    c("test 1")
  )
  
  references3 <- vt_dynamic_referencer$new()
  references3$initialize(reference_indicator = "...")
  
  sample_text <- "test ...string1"
  
  references3$scrape_references(sample_text)
  
  expect_equal(
    names(references3$list_references()),
    c("string1")
  )
  ## string replacement
  expect_equal(
    references3$reference_insertion(sample_text),
    c("test 1")
  )
  
})

test_that("Dynamic Number Referencing Works on files", {
  
  test_referencer <- vt_dynamic_referencer$new(
    reference_indicator = "@@"
  )
  
  ## Create test files
  test_spec <- tempfile(fileext = ".md")
  test_test_case <- tempfile(fileext = ".md")
  test_test_code <- tempfile(fileext = ".R")
  
  cat(
    file = test_spec,
    sep = "\n",
    c(
    "#' @title Test Spec @@dynamic_numbering",
    "#' @section Last updated by:",
    "#' User One",
    "#' @section Last update date:",
    "#' 2021-02-15",
    "",
    "#' + _Specifications_",
    "#'   + S@@dynamic_numbering.1 User is able to reference numbers dynamically",
    "#'     + S@@dynamic_numbering.1.1 Numbers will automatically update on rendering",
    ""))
  
  cat(
    file = test_test_case,
    sep = "\n",
    c(
      "#' @title Test Case @@dynamic_numbering_testcase",
      "#' @section Last updated by:",
      "#' User One",
      "#' @section Last update date:",
      "#' 2021-02-15",
      "#' @section Specification coverage:",
      "#' @@dynamic_numbering_testcase.1: @@dynamic_numbering.1, @@dynamic_numbering.2",
      "",
      "#' + _Test Cases_",
      "#'   + T@@dynamic_numbering_testcase.1 Create a sample spec with a unique reference number. Ensure the output matches 1 on rendering",
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
      "test_that(\"T@@dynamic_numbering_testcase.1\", {",
      "   + expect_true(TRUE)",
      "})",
      ""))
  
  test_spec_rendered <- dynamic_reference_rendering(
    file = test_spec,
    type = "spec",
    reference = test_referencer
  )
  
  test_test_case_rendered <- dynamic_reference_rendering(
    file = test_test_case,
    type = "test_case",
    reference = test_referencer
  )
  
  test_test_code_rendered <- dynamic_reference_rendering(
    file = test_test_code,
    type = "test_code",
    reference = test_referencer
  )
  
  expect_equal(
    test_spec_rendered,
    c(
      "#' @title Test Spec 1",
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
