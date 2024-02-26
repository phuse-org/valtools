test_that("validation file path finds files within the validation infrastructure, throws error otherwise", {
  withr::with_tempdir({

    make_vt_test_package()
    vt_use_req("example_req.md", username = "sample", open = FALSE)

    fp <- validation_file_path(
      file = "example_req.md",
      ref = vt_path()
    )

    expect_equal(
      as.character(fp),
      "requirements/example_req.md"
    )

    expect_equal(
      class(fp),
      c("md","validation_file_path")
    )

    expect_error(
      validation_file_path("FAKE_FILE"),
      "File `FAKE_FILE` not found."
    )
  })
})
