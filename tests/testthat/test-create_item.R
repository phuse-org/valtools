

test_that("simple item creation", {
  withr::with_tempdir({
    # set up "validation" infrastructure
    dir.create("vignettes/validation", recursive = TRUE)

    spec_path <- create_item(
      item_name = "new_specification",
      type = "specification",
      pkg = ".")

    test_case_path <- create_item(
      item_name = "new_test_case",
      type = "test_case",
      pkg = ".")

    test_code_path <- create_item(
      item_name = "new_test_code",
      type = "test_code",
      pkg = ".")

    expect_true(
      file.exists("vignettes/validation/specification/new_specification")
    )
    expect_true(
      file.exists("vignettes/validation/test_case/new_test_case")
    )
    expect_true(
      file.exists("vignettes/validation/test_code/new_test_code")
    )
  })
})

test_that("simple item creation and adds a directory if missing", {
  withr::with_tempdir({
    # set up "validation" infrastructure
    dir.create("vignettes/validation", recursive = TRUE)

    spec_path <- create_item(
      item_name = "new_dir/new_specification",
      type = "specification",
      pkg = ".")

    test_case_path <- create_item(
      item_name = "new_dir/new_test_case",
      type = "test_case",
      pkg = ".")

    test_code_path <- create_item(
      item_name = "new_dir/new_test_code",
      type = "test_code",
      pkg = ".")

    expect_true(
      file.exists("vignettes/validation/specification/new_dir/new_specification")
    )
    expect_true(
      file.exists("vignettes/validation/test_case/new_dir/new_test_case")
    )
    expect_true(
      file.exists("vignettes/validation/test_code/new_dir/new_test_code")
    )
  })

})

test_that("Throw an error if the validation directory has not been set up", {
  withr::with_tempdir({
    # set up "validation" infrastructure
   expect_error(
     create_item(
      item_name = "new_dir/new_specification",
      type = "specification",
      pkg = "."),
     "No validation structure found."
   )
  })
})
