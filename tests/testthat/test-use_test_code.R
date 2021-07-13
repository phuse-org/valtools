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
      name = "testcode002.badext",
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

test_that("Test Codes are added to the config file", {
  withr::with_tempdir({
    quiet <- capture.output({
      vt_create_package("example.package", open = FALSE)
    })
    setwd("example.package")
    vt_add_user_to_config(
      username = whoami::username(),
      name = "Sample Name",
      title = "Sample",
      role = "example"
    )

    vt_use_test_code("code1", open = FALSE)

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 2),
      c(
        "validation_files:",
        "- code1.R"
      )
    )

    vt_use_test_code("code2", open = FALSE)

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 3),
      c(
        "validation_files:",
        "- code1.R",
        "- code2.R"
      )
    )

    vt_use_test_code("code1a", add_after = "code1.R", open = FALSE)

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 4),
      c(
        "validation_files:",
        "- code1.R",
        "- code1a.R",
        "- code2.R"
      )
    )
  })
})
