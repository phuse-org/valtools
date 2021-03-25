

test_that("simple item creation", {
  withr::with_tempdir({
    # set up "validation" infrastructure
    writeLines(c("working_dir: vignettes"),"validation.yml")
    dir.create("vignettes/validation", recursive = TRUE)

    spec_path <- create_item(
      item_name = "new_specification",
      type = "requirements",
      pkg = ".")

    test_case_path <- create_item(
      item_name = "new_test_case",
      type = "test_cases",
      pkg = ".")

    test_code_path <- create_item(
      item_name = "new_test_code",
      type = "test_code",
      pkg = ".")

    expect_true(
      file.exists("vignettes/validation/requirements/new_specification")
    )
    expect_true(
      file.exists("vignettes/validation/test_cases/new_test_case")
    )
    expect_true(
      file.exists("vignettes/validation/test_code/new_test_code")
    )
  })
})

test_that("simple item creation and adds a directory if missing", {
  withr::with_tempdir({
    # set up "validation" infrastructure
    writeLines(c("working_dir: vignettes"),"validation.yml")
    dir.create("vignettes/validation", recursive = TRUE)

    spec_path <- create_item(
      item_name = "new_dir/new_specification",
      type = "requirements",
      pkg = ".")

    test_case_path <- create_item(
      item_name = "new_dir/new_test_case",
      type = "test_cases",
      pkg = ".")

    test_code_path <- create_item(
      item_name = "new_dir/new_test_code",
      type = "test_code",
      pkg = ".")

    expect_true(
      file.exists("vignettes/validation/requirements/new_dir/new_specification")
    )
    expect_true(
      file.exists("vignettes/validation/test_cases/new_dir/new_test_case")
    )
    expect_true(
      file.exists("vignettes/validation/test_code/new_dir/new_test_code")
    )
  })

})

test_that("vt_use_ family works nicely with simple item creation", {
  withr::with_tempdir({

    # set up validation infrastructure
    vt_use_validation_config()
    vt_use_validation()

    ## create items
    spec_path <- create_item(
      item_name = "new_dir/new_specification",
      type = "requirements",
      pkg = ".")

    test_case_path <- create_item(
      item_name = "new_dir/new_test_case",
      type = "test_cases",
      pkg = ".")

    test_code_path <- create_item(
      item_name = "new_dir/new_test_code",
      type = "test_code",
      pkg = ".")

    expect_true(
      file.exists("vignettes/validation/requirements/new_dir/new_specification")
    )
    expect_true(
      file.exists("vignettes/validation/test_cases/new_dir/new_test_case")
    )
    expect_true(
      file.exists("vignettes/validation/test_code/new_dir/new_test_code")
    )
  })

})

test_that("Throw an error if the validation directory has not been set up", {
  withr::with_tempdir({
    # set up "validation" infrastructure
    writeLines(c("working_dir: vignettes"),"validation.yml")

   expect_error(
     create_item(
      item_name = "new_dir/new_specification",
      type = "requirements",
      pkg = "."),
     "No validation structure found."
   )
  })
})
