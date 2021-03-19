test_that("coverage matrix from dynam num", {
  withr::with_tempdir({
    vt_create_package(open = FALSE)
    vt_use_test_case("testcase1", username = "a user", open = FALSE)
    vt_use_test_case("testcase2", username = "a user", open = FALSE)
    vt_use_test_case("testcase3", username = "a user", open = FALSE)
    vt_use_req("req1", username = "a user", open = FALSE)
    vt_use_req("req2", username = "a user", open = FALSE)
    vt_use_req("req3", username = "a user", open = FALSE)

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

    vt_add_file_to_config(c("req1.md", "testcase1.md", "req2.md", "testcase2.md", "req3.md", "testcase3.md"))
    cov_matrix <- vt_scrape_coverage_matrix()
    expect_matrix <- data.frame(req_id = as.vector(sapply(1:3, paste,  1:3, sep = ".")),
                                tc_id = c("1.1, 1.2", "1.2, 1.3", "1.1, 1.3, 1.4",
                                          "2.2, 2.3", "2.1, 2.2", "2.1, 2.3, 2.4",
                                          "3.1, 3.2", "3.2, 3.3", "3.1, 3.3, 3.4"))
    expect_equal(cov_matrix,
                 expect_matrix)

    cov_matrix2 <- vt_scrape_coverage_matrix(type = "wide")
    expect_matrix2 <- data.frame(req_id = c(rep("1.1", 2),
                                            rep("1.2", 2),
                                            rep("1.3", 3),
                                            rep("2.1", 2),
                                            rep("2.2", 2),
                                            rep("2.3", 3),
                                            rep("3.1", 2),
                                            rep("3.2", 2),
                                            rep("3.3", 3)),
                                 `1.1` = c("x", rep("", 3), "x", rep("", 16)),
                                 `1.2` = c("", rep("x", 2), rep("", 18)),
                                 `1.3` = c(rep("", 3), "x", "", "x", rep("", 15)),
                                 `1.4` = c(rep("", 6), "x", rep("", 14)),
                                 `2.1` = c(rep("", 9), "x", "", "x", rep("", 9)),
                                 `2.2` = c(rep("", 7), "x", rep("", 2), "x", rep("", 10)),
                                 `2.3` = c(rep("", 8), "x", rep("", 3), "x", rep("", 8)),
                                 `2.4` = c(rep("", 13), "x", rep("", 7)),
                                 `3.1` = c(rep("", 14), "x", rep("", 3), "x", rep("", 2)),
                                 `3.2` = c(rep("", 15), rep("x", 2), rep("", 4)),
                                 `3.3` = c(rep("", 17), "x", "", "x", ""),
                                 `3.4` = c(rep("", 20), "x"),
                                 check.names = FALSE)

    expect_equal(cov_matrix2,
                 expect_matrix2)


  })
})

test_that("coverage matrix no dynam num", {
  withr::with_tempdir({
    vt_create_package(open = FALSE)
    vt_use_test_case("testcase1", username = "a user", open = FALSE)
    vt_use_test_case("testcase2", username = "a user", open = FALSE)
    vt_use_test_case("testcase3", username = "a user", open = FALSE)
    vt_use_req("req1", username = "a user", open = FALSE)
    vt_use_req("req2", username = "a user", open = FALSE)
    vt_use_req("req3", username = "a user", open = FALSE)

    config_wd <- get_config_working_dir()
    cat(
      file = file.path(config_wd, "validation", "test_cases", "testcase1.md"),
      sep = "\n",
      c(
        "#' @title Test Case 1",
        "#' @editor User One",
        "#' @editDate 2021-03-17",
        "#' @coverage",
        "#' 1.1: 1.1, 1.2",
        "#' 1.2: 1.2, 1.3",
        "#' 1.3: 1.1, 1.3, 1.4",
        "",
        "+ _Test Cases_",
        "  + T1.1 Create a sample spec with a unique reference number. Matches requirements: 1.1 and 1.2",
        "  + T1.2 Another test case. Matches requirements: 1.2 and 1.3",
        "  + T1.3 More testing. Matches requirements: 1.1, 1.3, and 1.4",
        ""))
    cat(
      file = file.path(config_wd, "validation", "test_cases", "testcase2.md"),
      sep = "\n",
      c(
        "#' @title Test Case 2",
        "#' @editor User One",
        "#' @editDate 2021-03-17",
        "#' @coverage",
        "#' 2.2: 2.1, 2.2",
        "#' 2.1: 2.2, 2.3",
        "#' 2.3: 2.1, 2.3, 2.4",
        "",
        "+ _Test Cases_",
        "  + T2.1 Create a sample spec with a unique reference number. Matches requirements: 2.1 and 2.2",
        "  + T2.2 Another test case. Matches requirements: 2.2 and 2.3",
        "  + T2.3 More testing. Matches requirements: 2.1, 2.3, and 2.4",
        ""))

    cat(
      file = file.path(config_wd, "validation", "test_cases", "testcase3.md"),
      sep = "\n",
      c(
        "#' @title Test Case 3",
        "#' @editor User One",
        "#' @editDate 2021-03-17",
        "#' @coverage",
        "#' 3.1: 3.1, 3.2",
        "#' 3.2: 3.2, 3.3",
        "#' 3.3: 3.1, 3.3, 3.4",
        "",
        "+ _Test Cases_",
        "  + T3.3 Create a sample spec with a unique reference number. Matches requirements: 3.1 and 3.2",
        "  + T3.2 Another test case. Matches requirements: 3.2 and 3.3",
        "  + T3.1 More testing. Matches requirements: 3.1, 3.3, and 3.4",
        ""))

    vt_add_file_to_config(c("req1.md", "testcase1.md", "req2.md", "testcase2.md", "req3.md", "testcase3.md"))
    cov_matrix <- vt_scrape_coverage_matrix()
    expect_matrix <- data.frame(req_id = as.vector(sapply(1:3, paste,  1:3, sep = ".")),
                                tc_id = c("1.1, 1.2", "1.2, 1.3", "1.1, 1.3, 1.4",
                                          "2.2, 2.3", "2.1, 2.2", "2.1, 2.3, 2.4",
                                          "3.1, 3.2", "3.2, 3.3", "3.1, 3.3, 3.4"))
    expect_equal(cov_matrix,
                 expect_matrix)

    cov_matrix2 <- vt_scrape_coverage_matrix(type = "wide")
    expect_matrix2 <- data.frame(req_id = c(rep("1.1", 2),
                                            rep("1.2", 2),
                                            rep("1.3", 3),
                                            rep("2.1", 2),
                                            rep("2.2", 2),
                                            rep("2.3", 3),
                                            rep("3.1", 2),
                                            rep("3.2", 2),
                                            rep("3.3", 3)),
                                 `1.1` = c("x", rep("", 3), "x", rep("", 16)),
                                 `1.2` = c("", rep("x", 2), rep("", 18)),
                                 `1.3` = c(rep("", 3), "x", "", "x", rep("", 15)),
                                 `1.4` = c(rep("", 6), "x", rep("", 14)),
                                 `2.1` = c(rep("", 9), "x", "", "x", rep("", 9)),
                                 `2.2` = c(rep("", 7), "x", rep("", 2), "x", rep("", 10)),
                                 `2.3` = c(rep("", 8), "x", rep("", 3), "x", rep("", 8)),
                                 `2.4` = c(rep("", 13), "x", rep("", 7)),
                                 `3.1` = c(rep("", 14), "x", rep("", 3), "x", rep("", 2)),
                                 `3.2` = c(rep("", 15), rep("x", 2), rep("", 4)),
                                 `3.3` = c(rep("", 17), "x", "", "x", ""),
                                 `3.4` = c(rep("", 20), "x"),
                                 check.names = FALSE)

    expect_equal(cov_matrix2,
                 expect_matrix2)


  })
})
