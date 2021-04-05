test_that("Creating test code and set user", {

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


    test_code_path <- vt_use_test_code(
      name = "testcode001.md",
      username = "New User",
      open = FALSE
      )

    expect_true(
      file.exists(test_code_path)
    )

    content <- readLines(test_code_path)

    expect_equal(
      content,
      c("",
        "# Test setup",
        "",
        "",
        "#' @editor New User",
        paste ("#' @editDate",as.character(Sys.Date())),
        "test_that(\"TESTNUMBER\", {",
        "  #TEST CODE HERE",
        "",
        "})",
        "",
        "",
        ""
      )
    )

  })

})

test_that("Creating test code and not setting user takes username", {

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

    test_code_path <- vt_use_test_code(
      name = "testcode001.md",
      open = FALSE
    )

    expect_true(
      file.exists(test_code_path)
    )

    content <- readLines(test_code_path)

    expect_equal(
      content,
      c("",
        "# Test setup",
        "",
        "",
        paste("#' @editor",vt_username()),
        paste ("#' @editDate",as.character(Sys.Date())),
        "test_that(\"TESTNUMBER\", {",
        "  #TEST CODE HERE",
        "",
        "})",
        "",
        "",
        ""
      )
    )
  })

})

test_that("Creating test codes adds correct extension", {

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

    test_code_path <- vt_use_test_code(
      name = "testcode001",
      open = FALSE
    )

    test_code_path2 <- vt_use_test_code(
      name = "testcode001.badext",
      open = FALSE
    )

    expect_equal(
      tools::file_ext(test_code_path),
      "R"
    )

    expect_equal(
      tools::file_ext(test_code_path2),
      "R"
    )

  })

})
