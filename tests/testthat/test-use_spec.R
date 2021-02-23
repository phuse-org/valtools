test_that("Creating specifications and set user/title", {

  withr::with_tempdir({

    dir.create("vignettes/validation", recursive = TRUE)

    spec_path <- vt_use_spec(
      name = "spec001.md",
      username = "New User",
      title = "Specification 001",
      pkg = ".",
      open = FALSE
      )

    expect_true(
      file.exists(spec_path)
    )

    content <- readLines(spec_path)

    expect_equal(
      content,
      c(
        "#' @title Specification 001",
        "#' @editor New User",
        paste ("#' @editDate",as.character(Sys.Date())),
        "",
        "+ Start documenting requirements here!",
        ""
      )
    )

  })

})

test_that("Creating specifications and not setting user takes username", {

  withr::with_tempdir({

    dir.create("vignettes/validation", recursive = TRUE)

    spec_path <- vt_use_spec(
      name = "spec001.md",
      pkg = ".",
      open = FALSE
    )

    expect_true(
      file.exists(spec_path)
    )

    content <- readLines(spec_path)

    expect_equal(
      content,
      c(
        "#' @title spec001",
        paste("#' @editor",vt_username()),
        paste ("#' @editDate",as.character(Sys.Date())),
        "",
        "+ Start documenting requirements here!",
        ""
      )
    )
  })

})


test_that("Creating specifications adds correct extension", {

  withr::with_tempdir({

    dir.create("vignettes/validation", recursive = TRUE)

    spec_path <- vt_use_spec(
      name = "spec001",
      pkg = ".",
      open = FALSE
    )

    spec_path2 <- vt_use_spec(
      name = "spec001.badext",
      pkg = ".",
      open = FALSE
    )

    expect_equal(
      tools::file_ext(spec_path),
      "md"
    )

    expect_equal(
      tools::file_ext(spec_path2),
      "md"
    )

  })

})
