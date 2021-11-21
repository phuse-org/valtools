test_that("Evaluate test code works", {
  withr::with_tempdir({

    writeLines(c(
      "test_that(\"test_example\", {",
      "  expect_equal(2 * 2, 4)",
      "})"
    ), con = "testfile.R")

    writeLines(c(
      "test_that(\"test_example\", {",
      "  expect_equal(2 * 2, 5)",
      "})"
    ), con = "testfile2.R")

    results <- eval_test_code(path = "testfile.R")
    results2 <- eval_test_code(path = "testfile2.R")

    expect_equal(
      results,
      data.frame(
        Test = "test_example.1",
        Results = "As expected",
        Pass_Fail = "Pass",
        stringsAsFactors = FALSE
      )
    )

    expect_equal(
      results2,
      data.frame(
        Test = "test_example.1",
        Results = "2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1",
        Pass_Fail = "Fail",
        stringsAsFactors = FALSE
      )
    )

  })
})

test_that("Evaluate multiple tests within test_that works", {
  withr::with_tempdir({

    writeLines(c(
      "test_that(\"test_example\", {",
      "  expect_equal(2 * 2, 4)",
      "  expect_equal(2 * 2, 4)",
      "})"
    ), con = "testfile.R")

    writeLines(c(
      "test_that(\"test_example\", {",
      "  expect_equal(2 * 2, 4)",
      "  expect_equal(2 * 2, 5)",
      "})"
    ), con = "testfile2.R")

    results <- eval_test_code(path = "testfile.R")
    results2 <- eval_test_code(path = "testfile2.R")

    expect_equal(
      results,
      data.frame(
        Test = c("test_example.1","test_example.2"),
        Results = c("As expected","As expected"),
        Pass_Fail = c("Pass","Pass"),
        stringsAsFactors = FALSE
      )
    )

    expect_equal(
      results2,
      data.frame(
        Test = c("test_example.1","test_example.2"),
        Results = c("As expected","2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1"),
        Pass_Fail = c("Pass","Fail"),
        stringsAsFactors = FALSE
      )
    )

  })
})

test_that("Evaluate multiple test_thats within file works", {
  withr::with_tempdir({

    writeLines(c(
      "test_that(\"test_example1\", {",
      "  expect_equal(2 * 2, 4)",
      "  expect_equal(2 * 2, 4)",
      "})",
      "",
      "test_that(\"test_example2\", {",
      "  expect_equal(2 * 2, 4)",
      "  expect_equal(2 * 2, 5)",
      "})"
    ), con = "testfile.R")

    results <- eval_test_code(path = "testfile.R")

    expect_equal(
      results,

      data.frame(
        Test = c(
          "test_example1.1",
          "test_example1.2",
          "test_example2.1",
          "test_example2.2"
        ),
        Results = c(
          "As expected",
          "As expected",
          "As expected",
          "2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1"
        ),
        Pass_Fail = c(
          "Pass",
          "Pass",
          "Pass",
          "Fail"),
        stringsAsFactors = FALSE
      )
    )

  })
})

test_that("Empty test_that returns skip", {
  withr::with_tempdir({

    writeLines(c(
      "test_that(\"test_example\", {",
      "})"
    ), con = "testfile.R")

    results <- eval_test_code(path = "testfile.R")

    expect_equal(
      results,
      data.frame(
        Test = "test_example.1",
        Results = "Reason: empty test",
        Pass_Fail = "Skip",
        stringsAsFactors = FALSE
      )
    )
  })
})

test_that("Empty file returns 0 row data.frame", {
  withr::with_tempdir({

    writeLines(c(
      ""
    ), con = "testfile.R")

    warn_val <- capture_warnings({
      results <- eval_test_code(path = "testfile.R")
    })

    expect_equal(
      warn_val,
      "File `testfile.R` did not have any tests included."
    )

    expect_equal(
      results,
      data.frame(
        Test = character(),
        Results = character(),
        Pass_Fail = character(),
        stringsAsFactors = FALSE
      )
    )
  })
})

test_that("Can run tests with vt_run_test_code_file()", {

  withr::with_tempdir({

    vt_use_validation(
      package = "rlang",
      username_list = list(
        vt_user(
          username = "user1",
          name = "Test User",
          role = "sample",
          title = "Req Writer"
        ),
        vt_user(
          username = "user2",
          name = "Test User 2",
          role = "sample",
          title = "Req Writer"
        )
      ))

    vt_use_test_code("test_001",username = "Test User")
    text <- gsub("TESTNUMBER","1",readLines("validation/test_code/test_001.R"))
    text[grepl("#TEST CODE HERE", text)] <- "  expect_equal(2 * 2, 4)\n  expect_equal(2 * 2, 4)"
    writeLines(text,"validation/test_code/test_001.R")

    vt_use_test_code("test_002",username = "Test User")
    text <- gsub("TESTNUMBER","2",readLines("validation/test_code/test_002.R"))
    text[grepl("#TEST CODE HERE", text)] <- "  expect_equal(2 * 2, 4)\n  expect_equal(2 * 2, 5)"
    writeLines(text,"validation/test_code/test_002.R")

    results <- vt_run_test_code_file(file = "test_001.R", ref = "validation/")
    results2 <- vt_run_test_code_file(file = "test_002.R", ref = "validation/")

    expect_equal(
      results,
      data.frame(
        Test = c("1.1","1.2"),
        Results = c("As expected","As expected"),
        Pass_Fail = c("Pass","Pass"),
        stringsAsFactors = FALSE
      )
    )

    expect_equal(
      results2,
      data.frame(
        Test = c("2.1","2.2"),
        Results = c("As expected","2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1"),
        Pass_Fail = c("Pass","Fail"),
        stringsAsFactors = FALSE
      )
    )

  })
})

test_that("Can run tests with vt_run_test_code_file()", {

  withr::with_tempdir({

    vt_use_validation(
      package = "rlang",
      username_list = list(
        vt_user(
          username = "user1",
          name = "Test User",
          role = "sample",
          title = "Req Writer"
        ),
        vt_user(
          username = "user2",
          name = "Test User 2",
          role = "sample",
          title = "Req Writer"
        )
      ))

    vt_use_test_code("test_001",username = "Test User")
    text <- gsub("TESTNUMBER","1",readLines("validation/test_code/test_001.R"))
    text[grepl("#TEST CODE HERE", text)] <- "  expect_equal(2 * 2, 4)\n  expect_equal(2 * 2, 4)"
    writeLines(text,"validation/test_code/test_001.R")

    vt_use_test_code("test_002",username = "Test User")
    text <- gsub("TESTNUMBER","2",readLines("validation/test_code/test_002.R"))
    text[grepl("#TEST CODE HERE", text)] <- "  expect_equal(2 * 2, 4)\n  expect_equal(2 * 2, 5)"
    writeLines(text,"validation/test_code/test_002.R")

    results <- vt_run_test_code_file(file = "test_001.R", ref = "validation/")
    results2 <- vt_run_test_code_file(file = "test_002.R", ref = "validation/")

    expect_equal(
      results,
      data.frame(
        Test = c("1.1","1.2"),
        Results = c("As expected","As expected"),
        Pass_Fail = c("Pass","Pass"),
        stringsAsFactors = FALSE
      )
    )

    expect_equal(
      results2,
      data.frame(
        Test = c("2.1","2.2"),
        Results = c("As expected","2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1"),
        Pass_Fail = c("Pass","Fail"),
        stringsAsFactors = FALSE
      )
    )

  })
})


test_that("vt_kable_test_code_results returns formatted kable object",{

  pass_ex <- data.frame(
    Test = "test_example",
    Results = "As expected ",
    Pass_Fail = "Pass",
    stringsAsFactors = FALSE
  )

  output_pass <- vt_kable_test_code_results(pass_ex)

  expect_equivalent(
    output_pass,
    kable_styling(
      kable_styling(
        column_spec(
      column_spec(
        kable(
          data.frame(
            Test = "test_example",
            Results = "As expected ",
            Pass_Fail = "Pass",
            stringsAsFactors = FALSE
          ),
          escape = FALSE,
          col.names = c("Test", "Results", "Pass/Fail")
        ),2:3, width = "10em"),
        3, color = "#006400"),
    position = "center"), latex_options = "hold_position")
  )

  output_fail <- vt_kable_test_code_results(data.frame(
    Test = "test_example",
    Results = "Failure Reasons ",
    Pass_Fail = "Fail",
    stringsAsFactors = FALSE
  ))

  expect_equal(
    output_fail,
    kable_styling(kable_styling(
      column_spec(
      column_spec(
        kable(
          data.frame(
            Test = "test_example",
            Results = "Failure Reasons ",
            Pass_Fail = "Fail",
            stringsAsFactors = FALSE
          ),
          escape = FALSE,
          col.names = c("Test", "Results", "Pass/Fail")
        ),2:3, width = "10em"),
      3, color = "#FF0000"
      ), latex_options = "hold_position"), position = "center")
  )

  output_skip <- vt_kable_test_code_results(data.frame(
    Test = "test_example",
    Results = "Skipped test for reasons",
    Pass_Fail = "Skip",
    stringsAsFactors = FALSE
  ))

  expect_equal(
    output_skip,
    kable_styling(kable_styling(
      column_spec(
        column_spec(
        kable(
          data.frame(
            Test = "test_example",
            Results = "Skipped test for reasons",
            Pass_Fail = "Skip",
            stringsAsFactors = FALSE
          ),
          escape = FALSE,
          col.names = c("Test", "Results", "Pass/Fail")
        ),2:3, width = "10em"),
        3, color = "#FFC800"
      ),position = "center"), latex_options = "hold_position")
  )

  output_empty <- vt_kable_test_code_results(data.frame(
    Test = character(),
    Results = character(),
    Pass_Fail = character(),
    stringsAsFactors = FALSE
  ))

  expect_equal(
    output_empty,
    kable_styling(
    kable_styling(
        kable(
          data.frame(
            Test = character(),
            Results = character(),
            Pass_Fail = character(),
            stringsAsFactors = FALSE
          ),
          escape = FALSE,
          col.names = c("Test", "Results", "Pass/Fail")
      ),position = "center"), latex_options = "hold_position")
  )


  skip_on_cran()
  withr::with_tempfile(
    "tf", fileext = ".Rmd", {
      cat("---",
          "output: ",
          "  pdf_document:",
          "    fig_crop: false",
          "header-includes:",
          "  - \\usepackage{array}",
          "  - \\usepackage{multirow}",
          "---",
          "\n\n",
          "```{r}",
          "library(knitr)",
          "library(kableExtra)",
          "pass_ex <- data.frame(",
          "Test = \"test\\\\_example\",",
          "Results = \"As expected \",",
          "Pass_Fail = \"Pass\",",
          "stringsAsFactors = FALSE",
          ")",
          "```",
          "\n\n",
          "```{r results=\"asis\"}",
          "vt_kable_test_code_results(pass_ex, format = \"latex\")",
          "```",
          "\n\n", file = tf, sep = "\n")

      quiet <- capture.output({
        rmarkdown::render(tf)
      })

      testthat::expect_true(file.exists(gsub(tf, pattern = '.Rmd$', replacement = ".pdf")))


    })
})

test_that("vt_kable_test_code_results returns error when incorrect data are entered",{

  expect_error(
    vt_kable_test_code_results(data.frame(bad_entry = 1)),
    "Results data must contain the fields `Test`, `Results`, and `Pass_Fail`",
    fixed = TRUE
  )

})
