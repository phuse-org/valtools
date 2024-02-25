

test_that("simple item creation", {
  withr::with_tempdir({
    # set up "validation" infrastructure
    dir.create("vignettes/validation", recursive = TRUE)
    writeLines(c("working_dir: vignettes"),"vignettes/validation/validation.yml")
    file.create(".here")


    spec_path <- create_item(
      item_name = "new_specification",
      type = "requirements")

    test_case_path <- create_item(
      item_name = "new_test_case",
      type = "test_cases")

    test_code_path <- create_item(
      item_name = "new_test_code",
      type = "test_code")

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
    dir.create("vignettes/validation", recursive = TRUE)
    writeLines(c("working_dir: vignettes"),"vignettes/validation/validation.yml")
    file.create(".here")

    spec_path <- create_item(
      item_name = "new_dir/new_specification",
      type = "requirements")

    test_case_path <- create_item(
      item_name = "new_dir/new_test_case",
      type = "test_cases")

    test_code_path <- create_item(
      item_name = "new_dir/new_test_code",
      type = "test_code")

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
    make_vt_test_package()

    ## create items
    spec_path <- create_item(
      item_name = "new_dir/new_specification",
      type = "requirements")

    test_case_path <- create_item(
      item_name = "new_dir/new_test_case",
      type = "test_cases")

    test_code_path <- create_item(
      item_name = "new_dir/new_test_code",
      type = "test_code")

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
    file.create(".here")

    expect_error(
      create_item(item_name = "new_dir/new_specification",
                  type = "requirements"),
      "A validation structure does not exist."
    )
  })
})

test_that("simple item creation does not overwrite file", {
  withr::with_tempdir({
    # set up "validation" infrastructure
    dir.create("vignettes/validation", recursive = TRUE)
    writeLines(c("working_dir: vignettes"),"vignettes/validation/validation.yml")
    file.create(".here")
    
    
    spec_path <- create_item(
      item_name = "new_specification",
      type = "requirements")
    
    spec_path_file_info <- file.info(spec_path)

    
    spec_path <- create_item(
      item_name = "new_specification",
      type = "requirements")
    
    spec_path_file_info_2 <- file.info(spec_path)
    
    expect_equal(
      spec_path_file_info[,c("mtime","ctime")],
      spec_path_file_info_2[,c("mtime","ctime")]
    )
  })
})

