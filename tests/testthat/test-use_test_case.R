test_that("Creating test cases and set user", {

  withr::with_tempdir({

    dir.create("validation", recursive = TRUE)
    writeLines(c(
      "working_dir: vignettes",
      "usernames:",
      "  New User:",
      "    name: New User",
      "    title: new",
      "    role: user"),
      "validation/validation.yml")
    file.create(".here")


    test_case_path <- vt_use_test_case(
      name = "testcase001.md",
      username = "New User",
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

    dir.create("validation", recursive = TRUE)
    writeLines(c(
      "working_dir: vignettes",
      "usernames:",
      paste0("  ",username(fallback = ""),":"),
      "    name: New User",
      "    title: new",
      "    role: user"),
      "validation/validation.yml")
    file.create(".here")

    test_case_path <- vt_use_test_case(
      name = "testcase001.md",
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

    dir.create("validation", recursive = TRUE)
    writeLines(c(
      "working_dir: vignettes",
      "usernames:",
      paste0("  ",username(fallback = ""),":"),
      "    name: New User",
      "    title: new",
      "    role: user"),
      "validation/validation.yml")
    file.create(".here")

    test_case_path <- vt_use_test_case(
      name = "testcase001",
      open = FALSE
    )

    test_case_path2 <- vt_use_test_case(
      name = "testcase001.badext",
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

test_that("Cases are added to the config file", {
  withr::with_tempdir({
    vt_create_package("example.package", open = FALSE)
    setwd("example.package")
    vt_add_user_to_config(
      username = whoami::username(),
      name = "Sample Name",
      title = "Sample",
      role = "example"
    )

    vt_use_test_case("case1", open = FALSE)

    expect_equal(
      tail(readLines("validation.yml"), 2),
      c(
        "validation_files:",
        "- case1.md"
      )
    )

    vt_use_test_case("case2", open = FALSE)

    expect_equal(
      tail(readLines("validation.yml"), 3),
      c(
        "validation_files:",
        "- case1.md",
        "- case2.md"
      )
    )

    vt_use_test_case("case1a", add_after = "case1.md", open = FALSE)

    expect_equal(
      tail(readLines("validation.yml"), 4),
      c(
        "validation_files:",
        "- case1.md",
        "- case1a.md",
        "- case2.md"
      )
    )
  })
})

