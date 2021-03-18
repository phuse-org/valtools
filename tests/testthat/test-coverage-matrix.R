test_that("coverage matrix, one file", {
  withr::with_tempdir({
    vt_create_package(open = FALSE)
    vt_use_test_case("testcase1", username = "a user", open = FALSE)
    vt_use_test_case("testcase2", username = "a user", open = FALSE)
    vt_use_test_case("testcase3", username = "a user", open = FALSE)

    config_wd <- get_config_working_dir()
    cat(
      file = file.path(config_wd, "validation", "test_cases", "testcase1.md"),
      sep = "\n",
      c(
        "#' @title Test Case ##tc:dynamic_numbering_testcase1",
        "#' @editor User One",
        "#' @editDate 2021-03-17",
        "#' @coverage",
        "#' ##tc:dynamic_numbering_testcase1.1: ##req:dynamic_numbering1.1, ##req:dynamic_numbering1.2",
        "#' ##tc:dynamic_numbering_testcase1.2: ##req:dynamic_numbering1.2, ##req:dynamic_numbering1.3",
        "#' ##tc:dynamic_numbering_testcase1.3: ##req:dynamic_numbering1.1, ##req:dynamic_numbering1.3, ##req:dynamic_numbering1.4",
        "",
        "+ _Test Cases_",
        "  + T##tc:dynamic_numbering_testcase1.1 Create a sample spec with a unique reference number. Matches requirements: ##req:dynamic_numbering1.1 and  ##req:dynamic_numbering1.2",
        "  + T##tc:dynamic_numbering_testcase1.2 Another test case. Matches requirements: ##req:dynamic_numbering1.2 and ##req:dynamic_numbering1.3",
        "  + T##tc:dynamic_numbering_testcase1.3 More testing. Matches requirements: ##req:dynamic_numbering1.1, ##req:dynamic_numbering1.3, and ##req:dynamic_numbering1.4",
        ""))
    cat(
      file = file.path(config_wd, "validation", "test_cases", "testcase2.md"),
      sep = "\n",
      c(
        "#' @title Test Case ##tc:dynamic_numbering_testcase2",
        "#' @editor User One",
        "#' @editDate 2021-03-17",
        "#' @coverage",
        "#' ##tc:dynamic_numbering_testcase2.2: ##req:dynamic_numbering2.1, ##req:dynamic_numbering2.2",
        "#' ##tc:dynamic_numbering_testcase2.1: ##req:dynamic_numbering2.2, ##req:dynamic_numbering2.3",
        "#' ##tc:dynamic_numbering_testcase2.3: ##req:dynamic_numbering2.1, ##req:dynamic_numbering2.3, ##req:dynamic_numbering2.4",
        "",
        "+ _Test Cases_",
        "  + T##tc:dynamic_numbering_testcase2.1 Create a sample spec with a unique reference number. Matches requirements: ##req:dynamic_numbering2.1 and  ##req:dynamic_numbering2.2",
        "  + T##tc:dynamic_numbering_testcase2.2 Another test case. Matches requirements: ##req:dynamic_numbering2.2 and ##req:dynamic_numbering2.3",
        "  + T##tc:dynamic_numbering_testcase2.3 More testing. Matches requirements: ##req:dynamic_numbering2.1, ##req:dynamic_numbering2.3, and ##req:dynamic_numbering2.4",
        ""))

    cat(
      file = file.path(config_wd, "validation", "test_cases", "testcase3.md"),
      sep = "\n",
      c(
        "#' @title Test Case ##tc:dynamic_numbering_testcase3",
        "#' @editor User One",
        "#' @editDate 2021-03-17",
        "#' @coverage",
        "#' ##tc:dynamic_numbering_testcase3.1: ##req:dynamic_numbering3.1, ##req:dynamic_numbering3.2",
        "#' ##tc:dynamic_numbering_testcase3.2: ##req:dynamic_numbering3.2, ##req:dynamic_numbering3.3",
        "#' ##tc:dynamic_numbering_testcase3.3: ##req:dynamic_numbering3.1, ##req:dynamic_numbering3.3, ##req:dynamic_numbering3.4",
        "",
        "+ _Test Cases_",
        "  + T##tc:dynamic_numbering_testcase3.3 Create a sample spec with a unique reference number. Matches requirements: ##req:dynamic_numbering3.1 and  ##req:dynamic_numbering3.2",
        "  + T##tc:dynamic_numbering_testcase3.2 Another test case. Matches requirements: ##req:dynamic_numbering3.2 and ##req:dynamic_numbering3.3",
        "  + T##tc:dynamic_numbering_testcase3.1 More testing. Matches requirements: ##req:dynamic_numbering3.1, ##req:dynamic_numbering3.3, and ##req:dynamic_numbering3.4",
        ""))

    vt_add_file_to_config(c("testcase1.md", "testcase2.md", "testcase3.md"))
    cov_matrix <- vt_scrape_coverage_matrix()
    expect_matrix <- data.frame(req_id = as.vector(sapply(1:3, paste,  1:3, sep = ".")),
                                tc_id = c("1.1, 1.2", "1.2, 1.3", "1.1, 1.3, 1.4",
                                          "2.2, 2.3", "2.1, 2.2", "2.1, 2.3, 2.4",
                                          "3.1, 3.2", "3.2, 3.3", "3.1, 3.3, 3.4"))
    expect_equal(cov_matrix,
                 expect_matrix)


  })
})



