test_that("Creating requirements and set user/title", {

  withr::with_tempdir({

    dir.create("validation", recursive = TRUE)
    writeLines(c(
      "working_dir: vignettes",
      "usernames:",
      "  NewUser:",
      "    name: New User",
      "    title: new",
      "    role: user"),
      "validation/validation.yml")
    file.create(".here")

    req_path <- vt_use_req(
      name = "req001.md",
      username = "New User",
      title = "Requirement 001",
      open = FALSE
      )

    expect_true(
      file.exists(req_path)
    )

    content <- readLines(req_path)

    expect_equal(
      content,
      c(
        "#' @title Requirement 001",
        "#' @editor New User",
        paste ("#' @editDate",as.character(Sys.Date())),
        "",
        "+ Start documenting requirements here!",
        ""
      )
    )

  })

})

test_that("Creating requirements and not setting user takes username", {

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


    req_path <- vt_use_req(
      name = "req001.md",
      open = FALSE
    )

    expect_true(
      file.exists(req_path)
    )

    content <- readLines(req_path)

    expect_equal(
      content,
      c(
        "#' @title req001",
        "#' @editor New User",
        paste ("#' @editDate",as.character(Sys.Date())),
        "",
        "+ Start documenting requirements here!",
        ""
      )
    )
  })

})


test_that("Creating requirements adds correct extension", {

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

    req_path <- vt_use_req(
      name = "req001",
      open = FALSE
    )

    req_path2 <- vt_use_req(
      name = "req002.badext",
      open = FALSE
    )

    expect_equal(
      tools::file_ext(req_path),
      "md"
    )

    expect_equal(
      tools::file_ext(req_path2),
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

    vt_use_req("req1", open = FALSE)

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 2),
      c(
        "validation_files:",
        "- req1.md"
      )
    )

    vt_use_test_case("req2", open = FALSE)

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 3),
      c(
        "validation_files:",
        "- req1.md",
        "- req2.md"
      )
    )

    vt_use_test_case("req1a", add_after = "req1.md", open = FALSE)

    expect_equal(
      tail(readLines("vignettes/validation/validation.yml"), 4),
      c(
        "validation_files:",
        "- req1.md",
        "- req1a.md",
        "- req2.md"
      )
    )
  })
})
