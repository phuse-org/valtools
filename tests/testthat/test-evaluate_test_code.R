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
        Test = "test_example",
        Results = "As expected ",
        Pass_Fail = "Pass",
        stringsAsFactors = FALSE
      )
    )

    expect_equal(
      results2,
      data.frame(
        Test = "test_example",
        Results = "2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1 ",
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
        Results = c("As expected ","As expected "),
        Pass_Fail = c("Pass","Pass"),
        stringsAsFactors = FALSE
      )
    )

    expect_equal(
      results2,
      data.frame(
        Test = c("test_example.1","test_example.2"),
        Results = c("As expected ","2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1 "),
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
          "As expected ",
          "As expected ",
          "As expected ",
          "2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1 "
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
        Test = "test_example",
        Results = "Reason: empty test ",
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

    vt_use_validation_config(
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

    vt_use_validation()

    vt_use_test_code("test_001",username = "Test User")
    text <- gsub("TESTNUMBER","1",readLines("vignettes/validation/test_code/test_001.R"))
    text[grepl("#TEST CODE HERE", text)] <- "  expect_equal(2 * 2, 4)\n  expect_equal(2 * 2, 4)"
    writeLines(text,"vignettes/validation/test_code/test_001.R")

    vt_use_test_code("test_002",username = "Test User")
    text <- gsub("TESTNUMBER","2",readLines("vignettes/validation/test_code/test_002.R"))
    text[grepl("#TEST CODE HERE", text)] <- "  expect_equal(2 * 2, 4)\n  expect_equal(2 * 2, 5)"
    writeLines(text,"vignettes/validation/test_code/test_002.R")

    results <- vt_run_test_code_file(file = "test_001.R", ref = "vignettes/validation/")
    results2 <- vt_run_test_code_file(file = "test_002.R", ref = "vignettes/validation/")

    expect_equal(
      results,
      data.frame(
        Test = c("1.1","1.2"),
        Results = c("As expected ","As expected "),
        Pass_Fail = c("Pass","Pass"),
        stringsAsFactors = FALSE
      )
    )

    expect_equal(
      results2,
      data.frame(
        Test = c("2.1","2.2"),
        Results = c("As expected ","2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1 "),
        Pass_Fail = c("Pass","Fail"),
        stringsAsFactors = FALSE
      )
    )

  })
})

test_that("Can run tests with vt_run_test_code_file()", {

  withr::with_tempdir({

    vt_use_validation_config(
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

    vt_use_validation()

    vt_use_test_code("test_001",username = "Test User")
    text <- gsub("TESTNUMBER","1",readLines("vignettes/validation/test_code/test_001.R"))
    text[grepl("#TEST CODE HERE", text)] <- "  expect_equal(2 * 2, 4)\n  expect_equal(2 * 2, 4)"
    writeLines(text,"vignettes/validation/test_code/test_001.R")

    vt_use_test_code("test_002",username = "Test User")
    text <- gsub("TESTNUMBER","2",readLines("vignettes/validation/test_code/test_002.R"))
    text[grepl("#TEST CODE HERE", text)] <- "  expect_equal(2 * 2, 4)\n  expect_equal(2 * 2, 5)"
    writeLines(text,"vignettes/validation/test_code/test_002.R")

    results <- vt_run_test_code_file(file = "test_001.R", ref = "vignettes/validation/")
    results2 <- vt_run_test_code_file(file = "test_002.R", ref = "vignettes/validation/")

    expect_equal(
      results,
      data.frame(
        Test = c("1.1","1.2"),
        Results = c("As expected ","As expected "),
        Pass_Fail = c("Pass","Pass"),
        stringsAsFactors = FALSE
      )
    )

    expect_equal(
      results2,
      data.frame(
        Test = c("2.1","2.2"),
        Results = c("As expected ","2 * 2 not equal to 5.\n1/1 mismatches\n[1] 4 - 5 == -1 "),
        Pass_Fail = c("Pass","Fail"),
        stringsAsFactors = FALSE
      )
    )

  })
})


test_that("vt_kable_test_code returns formatted kable object",{

  output_pass <- vt_kable_test_code(data.frame(
    Test = "test_example",
    Results = "As expected ",
    Pass_Fail = "Pass",
    stringsAsFactors = FALSE
    ))

  expect_equal(
    capture.output(output_pass),
    c(
      "\\begin{table}[H]",
      "\\centering",
      "\\begin{tabular}{l|>{\\raggedright\\arraybackslash}p{10em}|>{\\raggedright\\arraybackslash}p{10em}}",
      "\\hline",
      "Test & Results & Pass/Fail\\\\",
      "\\hline",
      "test_example & As expected & \\shortstack{\\textcolor{OliveGreen}{Pass}}\\\\",
      "\\hline",
      "\\end{tabular}",
      "\\end{table}"
    )
  )

  output_fail <- vt_kable_test_code(data.frame(
    Test = "test_example",
    Results = "Failure Reasons ",
    Pass_Fail = "Fail",
    stringsAsFactors = FALSE
  ))

  expect_equal(
    capture.output(output_fail),
    c(
      "\\begin{table}[H]",
      "\\centering",
      "\\begin{tabular}{l|>{\\raggedright\\arraybackslash}p{10em}|>{\\raggedright\\arraybackslash}p{10em}}",
      "\\hline",
      "Test & Results & Pass/Fail\\\\",
      "\\hline",
      "test_example & Failure Reasons & \\shortstack{\\textcolor{red}{Fail}}\\\\",
      "\\hline",
      "\\end{tabular}",
      "\\end{table}"
    )
  )

  output_skip <- vt_kable_test_code(data.frame(
    Test = "test_example",
    Results = "Skipped test for reasons",
    Pass_Fail = "Skip",
    stringsAsFactors = FALSE
  ))

  expect_equal(
    capture.output(output_skip),
    c(
      "\\begin{table}[H]",
      "\\centering",
      "\\begin{tabular}{l|>{\\raggedright\\arraybackslash}p{10em}|>{\\raggedright\\arraybackslash}p{10em}}",
      "\\hline",
      "Test & Results & Pass/Fail\\\\",
      "\\hline",
      "test_example & Skipped test for reasons & \\shortstack{\\textcolor{YellowOrange}{Skip}}\\\\",
      "\\hline",
      "\\end{tabular}",
      "\\end{table}"
    )
  )

  output_empty <- vt_kable_test_code(data.frame(
    Test = character(),
    Results = character(),
    Pass_Fail = character(),
    stringsAsFactors = FALSE
  ))

  expect_equal(
    capture.output(output_empty),
    c(
      "\\begin{table}[H]",
      "\\centering",
      "\\begin{tabular}{l|l|l}",
      "\\hline",
      "Test & Results & Pass/Fail\\\\",
      "",
      "",
      "\\hline",
      "\\end{tabular}",
      "\\end{table}"
    )
  )


})

test_that("vt_kable_test_code returns error when incorrect data are entered",{

  expect_error(
    vt_kable_test_code(data.frame(bad_entry = 1)),
    "Results data must contain the fields `Test`, `Results`, and `Pass_Fail`",
    fixed = TRUE
  )

})
