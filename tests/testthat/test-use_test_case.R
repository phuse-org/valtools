test_that("Creating test cases and set user", {

  withr::with_tempdir({

    writeLines(c(
      "working_dir: vignettes",
      "usernames:",
      "  New User:",
      "    name: New User",
      "    title: new",
      "    role: user"),
      "validation.yml")
    dir.create("vignettes/validation", recursive = TRUE)

    test_case_path <- vt_use_test_case(
      name = "testcase001.md",
      username = "New User",
      pkg = ".",
      open = FALSE
      )

    expect_true(
      file.exists(test_case_path)
    )

    content <- readLines(test_case_path)

    expect_equal(
      content,
      c("#' @title testcase001",
        "#' @editor New User",
        paste ("#' @editDate",as.character(Sys.Date())),
        "",
        "+ _Test Case_",
        "    + Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
        "",
        "    + Start documenting test case here!",
        "",
        "",
        ""
      )
    )

  })

})

test_that("Creating test cases and not setting user takes username", {

  withr::with_tempdir({

    writeLines(c(
      "working_dir: vignettes",
      "usernames:",
      paste0("  ",username(fallback = ""),":"),
      "    name: New User",
      "    title: new",
      "    role: user"),
      "validation.yml")
    dir.create("vignettes/validation", recursive = TRUE)

    test_case_path <- vt_use_test_case(
      name = "testcase001.md",
      pkg = ".",
      open = FALSE
    )

    expect_true(
      file.exists(test_case_path)
    )

    content <- readLines(test_case_path)

    expect_equal(
      content,
      c("#' @title testcase001",
        "#' @editor New User",
        paste ("#' @editDate",as.character(Sys.Date())),
        "",
        "+ _Test Case_",
        "    + Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
        "",
        "    + Start documenting test case here!",
        "",
        "",
        ""
      )
    )
  })

})

test_that("Creating test cases adds correct extension", {

  withr::with_tempdir({

    writeLines(c(
      "working_dir: vignettes",
      "usernames:",
      paste0("  ",username(fallback = ""),":"),
      "    name: New User",
      "    title: new",
      "    role: user"),
      "validation.yml")
    dir.create("vignettes/validation", recursive = TRUE)

    test_case_path <- vt_use_test_case(
      name = "testcase001",
      pkg = ".",
      open = FALSE
    )

    test_case_path2 <- vt_use_test_case(
      name = "testcase001.badext",
      pkg = ".",
      open = FALSE
    )

    expect_equal(
      tools::file_ext(test_case_path),
      "md"
    )

    expect_equal(
      tools::file_ext(test_case_path2),
      "md"
    )

  })

})
