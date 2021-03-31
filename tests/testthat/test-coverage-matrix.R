test_that("coverage matrix from dynam num", {
  withr::with_tempdir({
    capture_output <- capture.output({vt_create_package(open = FALSE)})
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
    expect_matrix <- data.frame(req_title = rep(paste("Requirement", 1:3), each = 7),
                                req_id = paste(rep(1:3, each = 7), rep(c(1, 1, 2, 2, 3, 3, 4), 3), sep = "."),
                                tc_title = rep(paste("Test Case", 1:3), each = 7),
                                tc_id = c(paste(1, c(1, 3, 1, 2, 2, 3, 3), sep = "."),
                                          paste(2, c(2, 3, 2, 1, 1, 3, 3), sep = "."),
                                          paste(3, c(1, 3, 1, 2, 2, 3, 3), sep = ".")))
    attr(expect_matrix, "table_type") <- "long"
    expect_equal(cov_matrix,
                 expect_matrix)

    cov_matrix_tex_file <- tempfile(fileext = ".tex", tmpdir = getwd())
    writeLines(
      c("---",
          "title: validation report",
          "output: pdf_document",
          "header-includes:",
          "  - \\usepackage{array}",
          "  - \\usepackage{longtable}",
          "  - \\usepackage{multirow}",
          "classoption: dvipsnames",
          "---",
          "\n\n",
          vt_kable_coverage_matrix(cov_matrix)),
          con = cov_matrix_tex_file)

    capture_output <- capture.output({
      rmarkdown::render(cov_matrix_tex_file, output_format = "pdf_document")
    })
    rendered_cov_matrix_pdf <- trimws(strsplit(split = "\r\n", gsub("((\r)|(\n))+","\r\n",
                                                      pdftools::pdf_text(gsub(cov_matrix_tex_file, pattern = ".tex",
                                                                replacement = ".pdf"))))[[1]])
    expect_equal(rendered_cov_matrix_pdf[2:23],
                 c("Requirement Name  Requirement ID Test Case Name Test Cases",
                   "1.1                           1.1",
                   "1.1                           1.3",
                   "1.2                           1.1",
                   "Requirement 1     1.2            Test Case 1    1.2",
                   "1.3                           1.2",
                   "1.3                           1.3",
                   "1.4                           1.3",
                   "2.1                           2.2",
                   "2.1                           2.3",
                   "2.2                           2.2",
                   "Requirement 2     2.2            Test Case 2    2.1",
                   "2.3                           2.1",
                   "2.3                           2.3",
                   "2.4                           2.3",
                   "3.1                           3.1",
                   "3.1                           3.3",
                   "3.2                           3.1",
                   "Requirement 3     3.2            Test Case 3    3.2",
                   "3.3                           3.2",
                   "3.3                           3.3",
                   "3.4                           3.3" ) )

    cov_matrix_rmd_file <- tempfile(fileext = ".Rmd", tmpdir = getwd())
    writeLines(
      c("---",
        "title: validation report",
        "output: html_document",
        "---",
        "\n\n",
        vt_kable_coverage_matrix(cov_matrix, format = "html")),
      con = cov_matrix_rmd_file)

    capture_output <- capture.output({
      rmarkdown::render(cov_matrix_rmd_file)
    })
    this_test <- xml2::read_html(gsub(cov_matrix_rmd_file, pattern = ".Rmd", replacement = ".html"))
    rendered_cov_matrix_html <- as.data.frame(rvest::html_table(rvest::html_nodes(this_test, "table")[1], fill = TRUE)[[1]])
    expect_equal(rendered_cov_matrix_html,
                 data.frame(`Requirement Name` = rep(paste("Requirement", 1:3), each = 7),
                            `Requirement ID` = as.double(paste(rep(1:3, each = 7), rep(c(1, 1, 2, 2, 3, 3, 4), 3), sep = ".")),
                            `Test Case Name` = rep(paste("Test Case", 1:3), each = 7),
                            `Test Cases` = as.double(c(paste(1, c(1, 3, 1, 2, 2, 3, 3), sep = "."),
                                      paste(2, c(2, 3, 2, 1, 1, 3, 3), sep = "."),
                                      paste(3, c(1, 3, 1, 2, 2, 3, 3), sep = "."))),
                            check.names = FALSE))

    cov_matrix2 <- vt_scrape_coverage_matrix(type = "wide")
    expect_matrix2 <- data.frame(req_title = rep(paste("Requirement", 1:3), each = 7),
                                 req_id = c(paste(1, c(1, 2, 2, 3, 1, 3, 4), sep = "."),
                                            paste(2, c(1, 2, 2, 3, 1, 3, 4), sep = "."),
                                            paste(3, c(1, 2, 2, 3, 1, 3, 4), sep = ".")),
                                 `1.1` = c(rep("x", 2), rep("", 19)),
                                 `1.2` = c(rep("", 2), rep("x", 2), rep("", 17)),
                                 `1.3` = c(rep("", 4), rep("x", 3), rep("", 14)),
                                 `2.1` = c(rep("", 9), rep("x", 2), rep("", 10)),
                                 `2.2` = c(rep("", 7), rep("x", 2), rep("", 12)),
                                 `2.3` = c(rep("", 11 ), rep("x", 3), rep("", 7)),
                                 `3.1` = c(rep("", 14), rep("x", 2), rep("", 5)),
                                 `3.2` = c(rep("", 16), rep("x", 2), rep("", 3)),
                                 `3.3` = c(rep("", 18), rep("x", 3)),
                                 check.names = FALSE)
    attr(expect_matrix2, "table_type") <- "wide"
    attr(expect_matrix2, "tc_title") <- data.frame(tc_id = as.vector(sapply(1:3, FUN = function(x){paste(x, 1:3, sep = ".")})),
                                                   tc_title = rep(paste("Test Case", 1:3), each = 3))

    expect_equal(cov_matrix2,
                 expect_matrix2)

    cov_matrix2_rmd_file <-  tempfile(fileext = ".Rmd", tmpdir = getwd())
    writeLines(
      c("---",
        "title: validation report",
        "output: html_document",
        "---",
        "\n\n",
        vt_kable_coverage_matrix(cov_matrix2, format = "html")),
      con = cov_matrix2_rmd_file)

    capture_output <- capture.output({
      rmarkdown::render(cov_matrix2_rmd_file)
    })

    this_test2 <- xml2::read_html(gsub(cov_matrix2_rmd_file, pattern = ".Rmd", replacement = ".html"))

    rendered_cov_matrix2_html <- as.data.frame(rvest::html_table(rvest::html_nodes(this_test2, "table")[1], fill = TRUE)[[1]])
    expected_cov_matrix2_html <- data.frame(reqs = c("", rep(paste("Requirement", 1:3), each = 7)),
                                            req_id = as.double(c(NA, paste(1, c(1, 2, 2, 3, 1, 3, 4), sep = "."),
                                                                 paste(2, c(1, 2, 2, 3, 1, 3, 4), sep = "."),
                                                                 paste(3, c(1, 2, 2, 3, 1, 3, 4), sep = "."))),
                                            `Test Case 1` = c("1.1", rep("x", 2), rep("", 19)),
                                            `Test Case 1` = c(1.2, rep("", 2), rep("x", 2), rep("", 17)),
                                            `Test Case 1` = c(1.3, rep("", 4), rep("x", 3), rep("", 14)),
                                            `Test Case 2` = c(2.1, rep("", 9), rep("x", 2), rep("", 10)),
                                            `Test Case 2` = c(2.2, rep("", 7), rep("x", 2), rep("", 12)),
                                            `Test Case 2` = c(2.3, rep("", 11 ), rep("x", 3), rep("", 7)),
                                            `Test Case 3` = c(3.1, rep("", 14), rep("x", 2), rep("", 5)),
                                            `Test Case 3` = c(3.2, rep("", 16), rep("x", 2), rep("", 3)),
                                            `Test Case 3` = c(3.3,rep("", 18), rep("x", 3)),
                                            check.names = FALSE)
    names(expected_cov_matrix2_html)[1:2] <- c("", "")
    expect_equal(rendered_cov_matrix2_html,
                 expected_cov_matrix2_html )

    skip_on_os("mac")
    cov_matrix2_tex_file <- tempfile(fileext = ".Rmd", tmpdir = getwd())

    writeLines(
      c("---",
        "title: validation report",
        "output: pdf_document",
        "header-includes:",
        "  - \\usepackage{array}",
        "  - \\usepackage{longtable}",
        "  - \\usepackage{multirow}",
        "  - \\usepackage{float}",
        "  - \\usepackage{booktabs}",
        "classoption: dvipsnames",
        "---",
        "\n\n",
        vt_kable_coverage_matrix(cov_matrix2, format = "latex")),
      con = cov_matrix2_tex_file)

    capture_output <- capture.output({
      rmarkdown::render(cov_matrix2_tex_file, output_format = "pdf_document")
    })

    expect_true(file.exists(gsub(cov_matrix2_tex_file, pattern = ".Rmd", replacement = '.pdf')))

    rendered_cov_matrix2_pdf <- trimws(strsplit(split = "\r\n", gsub("((\r)|(\n))+","\r\n",
                 pdftools::pdf_text(gsub(cov_matrix2_tex_file, pattern = ".Rmd", replacement = '.pdf'))))[[1]])

    expect_equal(rendered_cov_matrix2_pdf[2:24],
                 c( "Test Case 1     Test Case 2     Test Case 3",
                    "1.1   1.2   1.3 2.1   2.2   2.3 3.1   3.2   3.3",
                    "1.1   x",
                    "1.2   x",
                    "1.2         x",
                    "1.3         x",
                    "Requirement 1 1.1               x",
                    "1.3               x",
                    "1.4               x",
                    "2.1                         x",
                    "2.2                         x",
                    "2.2                   x",
                    "2.3                   x",
                    "Requirement 2 2.1                               x",
                    "2.3                               x",
                    "2.4                               x",
                    "3.1                                   x",
                    "3.2                                   x",
                    "3.2                                         x",
                    "3.3                                         x",
                    "Requirement 3 3.1                                               x",
                    "3.3                                               x",
                    "3.4                                               x"  ))




  })
})

test_that("coverage matrix no dynam num", {
  withr::with_tempdir({
    capture_output <- capture.output({
      vt_create_package(open = FALSE)
    })
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
    expect_matrix <- data.frame(req_title = rep(paste("Requirement", 1:3), each = 7),
                                req_id = paste(rep(1:3, each = 7), rep(c(1, 1, 2, 2, 3, 3, 4), 3), sep = "."),
                                tc_title = rep(paste("Test Case", 1:3), each = 7),
                                tc_id = c(paste(1, c(1, 3, 1, 2, 2, 3, 3), sep = "."),
                                          paste(2, c(2, 3, 2, 1, 1, 3, 3), sep = "."),
                                          paste(3, c(1, 3, 1, 2, 2, 3, 3), sep = ".")))
    attr(expect_matrix, "table_type") <- "long"
    expect_equal(cov_matrix,
                 expect_matrix)

    cov_matrix2 <- vt_scrape_coverage_matrix(type = "wide")
    expect_matrix2 <- data.frame(req_title = rep(paste("Requirement", 1:3), each = 7),
                                 req_id = c(paste(1, c(1, 2, 2, 3, 1, 3, 4), sep = "."),
                                            paste(2, c(1, 2, 2, 3, 1, 3, 4), sep = "."),
                                            paste(3, c(1, 2, 2, 3, 1, 3, 4), sep = ".")),
                                 `1.1` = c(rep("x", 2), rep("", 19)),
                                 `1.2` = c(rep("", 2), rep("x", 2), rep("", 17)),
                                 `1.3` = c(rep("", 4), rep("x", 3), rep("", 14)),
                                 `2.1` = c(rep("", 9), rep("x", 2), rep("", 10)),
                                 `2.2` = c(rep("", 7), rep("x", 2), rep("", 12)),
                                 `2.3` = c(rep("", 11 ), rep("x", 3), rep("", 7)),
                                 `3.1` = c(rep("", 14), rep("x", 2), rep("", 5)),
                                 `3.2` = c(rep("", 16), rep("x", 2), rep("", 3)),
                                 `3.3` = c(rep("", 18), rep("x", 3)),
                                 check.names = FALSE)
    attr(expect_matrix2, "table_type") <- "wide"
    attr(expect_matrix2, "tc_title") <- data.frame(tc_id = as.vector(sapply(1:3, FUN = function(x){paste(x, 1:3, sep = ".")})),
                                                   tc_title = rep(paste("Test Case", 1:3), each = 3))

    expect_equal(cov_matrix2,
                 expect_matrix2)


  })
})

test_that("existing reference obj", {
  withr::with_tempdir({

    capture_output <- capture.output({
      vt_create_package(open = FALSE)
    })
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

    cat(
      file = file.path(config_wd, "validation", "requirements", "req1.md"),
      sep = "\n",
      c(
        "#' @title req##req:dynamic_numbering4",
        "#' @editor a user",
        "#' @editDate 2021-03-19",
        "",
        "+ Start documenting requirements here!",
        ""))
    cat(
      file = file.path(config_wd, "validation", "requirements", "req2.md"),
      sep = "\n",
      c(
        "#' @title req##req:dynamic_numbering5",
        "#' @editor a user",
        "#' @editDate 2021-03-19",
        "",
        "+ Start documenting requirements here!",
        ""))


    vt_add_file_to_config(c("req1.md", "testcase1.md", "req2.md", "testcase2.md", "req3.md", "testcase3.md"))

    references <- vt_dynamic_referencer$new()
    expect_equal(references$list_references(), list())

    references$scrape_references(
      do.call("rbind", scrape_tags_from(type = "requirements", tags = "title")))

    expect_equal(references$list_references(),
                 list(`req:dynamic_numbering4` = 1,
                      `req:dynamic_numbering5` = 2))

    cov_matrix <- vt_scrape_coverage_matrix(reference = references)

    expect_equal(references$list_references(),
                 list(`req:dynamic_numbering4` = 1,
                      `req:dynamic_numbering5` = 2,
                      `tc:dynamic_numbering_testcase1` = 1,
                      `tc:dynamic_numbering_testcase2` = 2,
                      `tc:dynamic_numbering_testcase3` = 3,
                      `req:dynamic_numbering1` = 3,
                      `req:dynamic_numbering2` = 4,
                      `req:dynamic_numbering3` = 5))
    expect_matrix <- data.frame(req_title = rep(paste("Requirement", 3:5), each = 7),
                                req_id = c(paste(3, c(1, 1, 2, 2, 3, 3, 4), sep = "."),
                                           paste(4, c(1, 1, 2, 2, 3, 3, 4), sep = "."),
                                           paste(5, c(1, 1, 2, 2, 3, 3, 4), sep = ".")),
                                tc_title = rep(paste("Test Case", 1:3), each = 7),
                                tc_id = c(paste(1, c(1, 3, 1, 2, 2, 3, 3), sep = "."),
                                          paste(2, c(2, 3, 2, 1, 1, 3, 3), sep = "."),
                                          paste(3, c(1, 3, 1, 2, 2, 3, 3), sep = ".")))
    attr(expect_matrix, "table_type") <- "long"
    expect_equal(cov_matrix,
                 expect_matrix)

    cov_matrix2 <- vt_scrape_coverage_matrix(reference = references, type = "wide")
    expect_matrix2 <- data.frame(req_title = rep(paste("Requirement", 3:5), each = 7),
                                 req_id = c(paste(3, c(1, 2, 2, 3, 1, 3, 4), sep = "."),
                                            paste(4, c(1, 2, 2, 3, 1, 3, 4), sep = "."),
                                            paste(5, c(1, 2, 2, 3, 1, 3, 4), sep = ".")),
                                 `1.1` = c(rep("x", 2), rep("", 19)),
                                 `1.2` = c(rep("", 2), rep("x", 2), rep("", 17)),
                                 `1.3` = c(rep("", 4), rep("x", 3), rep("", 14)),
                                 `2.1` = c(rep("", 9), rep("x", 2), rep("", 10)),
                                 `2.2` = c(rep("", 7), rep("x", 2), rep("", 12)),
                                 `2.3` = c(rep("", 11 ), rep("x", 3), rep("", 7)),
                                 `3.1` = c(rep("", 14), rep("x", 2), rep("", 5)),
                                 `3.2` = c(rep("", 16), rep("x", 2), rep("", 3)),
                                 `3.3` = c(rep("", 18), rep("x", 3)),
                                 check.names = FALSE)
    attr(expect_matrix2, "table_type") <- "wide"
    attr(expect_matrix2, "tc_title") <- data.frame(tc_id = as.vector(sapply(1:3, FUN = function(x){paste(x, 1:3, sep = ".")})),
                                                   tc_title = rep(paste("Test Case", 1:3), each = 3))
    expect_equal(cov_matrix2,
                 expect_matrix2)


  })
})
