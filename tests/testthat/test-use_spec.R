test_that("Creating specifications and set user", {

  withr::with_tempdir({

    dir.create("vignettes/validation", recursive = TRUE)

    spec_path <- vt_use_spec(
      name = "spec001.md",
      username = "New User",
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
        "#' @section Last Updated By:",
        "#' New User",
        "#' @section Last Update Date:",
        paste("#'",as.character(Sys.Date())),
        "",
        "+ _Specifications_",
        "    + Start documenting specifications here!",
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
        "#' @section Last Updated By:",
        paste("#'",vt_username()),
        "#' @section Last Update Date:",
        paste ("#'",as.character(Sys.Date())),
        "",
        "+ _Specifications_",
        "    + Start documenting specifications here!",
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
