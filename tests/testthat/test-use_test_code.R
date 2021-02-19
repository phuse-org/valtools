test_that("Creating test code and set user", {

  withr::with_tempdir({

    dir.create("vignettes/validation", recursive = TRUE)

    test_code_path <- vt_use_test_code(
      name = "testcode001.md",
      username = "New User",
      pkg = ".",
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
        "#' @section Last Updated By:",
        "#' New User",
        "#' @section Last Update Date:",
        paste("#'",as.character(Sys.Date())),
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

    dir.create("vignettes/validation", recursive = TRUE)

    test_code_path <- vt_use_test_code(
      name = "testcode001.md",
      pkg = ".",
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
        "#' @section Last Updated By:",
        paste("#'",vt_username()),
        "#' @section Last Update Date:",
        paste ("#'",as.character(Sys.Date())),
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

    dir.create("vignettes/validation", recursive = TRUE)

    test_code_path <- vt_use_test_code(
      name = "testcode001",
      pkg = ".",
      open = FALSE
    )

    test_code_path2 <- vt_use_test_code(
      name = "testcode001.badext",
      pkg = ".",
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
