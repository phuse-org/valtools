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
      name = "req001.badext",
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
