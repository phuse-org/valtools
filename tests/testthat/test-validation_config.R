test_that("Test creation of the config file", {
  withr::with_tempdir({

    vt_use_validation(
      package = "test.package",
      username_list = list(
                               vt_user(
                               name = "test",
                               title = "test",
                               role = "tester",
                               username = "test"
                             )),
                             validation_files = list("req1.Rmd",
                                                     "test_case1.Rmd",
                                                     "test_code1.R"))

    validation_config <- readLines("validation/validation.yml")

    expect_equal(
      validation_config,
      c("package: test.package",
        "working_dir: '.'",
        "output_dir: '.'",
        "report_rmd_name: validation.Rmd",
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test",
        "    role: tester",
        "validation_files:",
        "- req1.Rmd",
        "- test_case1.Rmd",
        "- test_code1.R"
      )
    )

  })

})

test_that("Test creation of the config file with invalid username list", {
  withr::with_tempdir({

    expect_error(
    vt_use_validation(
      package = "test.package",
      username_list = list(vt_user(
        username = "test",
        name = "test",
        title = "test",
        role = "tester"
      ),
      "bad Entry")),
    "1 invalid entry in username_list: \nArgument 2 is not created by `vt_user()`.",
    fixed = TRUE
    )
  })

})

test_that("Test creation of the config file without passed values in a non-interative environment", {
  withr::with_tempdir({

    vt_use_validation(package = "test.package")

    validation_config <- readLines("validation/validation.yml")


    expect_equal(
      validation_config,
      c("package: test.package",
        "working_dir: '.'",
        "output_dir: '.'",
        "report_rmd_name: validation.Rmd",
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames: []",
        "validation_files: []"
      )
    )

  })

})

test_that("Test creation of the config file without passed values in a non-interative environment and adding a user", {
  withr::with_tempdir({

    vt_use_validation(package = "test.package")

    validation_config <- readLines("validation/validation.yml")

    expect_equal(
      validation_config,
      c("package: test.package",
        "working_dir: '.'",
        "output_dir: '.'" ,
        "report_rmd_name: validation.Rmd",
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames: []",
        "validation_files: []"
      )
    )

    add_user_message <- capture_messages(
      vt_add_user_to_config(username = "test", name = "test", title = "test", role = "tester")
    )

    validation_config2 <- readLines("validation/validation.yml")

    expect_equal(
      add_user_message,
      "User `test` added to validation config file.",
    )

    expect_equal(
      validation_config2,
      c("package: test.package",
        "working_dir: '.'",
        "output_dir: '.'" ,
        "report_rmd_name: validation.Rmd",
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test" ,
        "    role: tester",
        "validation_files: []"
      )
    )

    add_user_message2 <- capture_messages(
      vt_add_user_to_config(username = "test2", name = "test2", role = "tester2", title = "tester2")
    )

    validation_config3 <- readLines("validation/validation.yml")

    expect_equal(
      add_user_message2,
      "User `test2` added to validation config file.",
    )

    expect_equal(
      validation_config3,
      c("package: test.package",
        "working_dir: '.'",
        "output_dir: '.'" ,
        "report_rmd_name: validation.Rmd",
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test" ,
        "    role: tester" ,
        "  test2:",
        "    name: test2",
        "    title: tester2" ,
        "    role: tester2",
        "validation_files: []"
      )
    )

  })

})

test_that("Test creation of the config file without passed values in a non-interative environment and overriding a user", {
  withr::with_tempdir({

    vt_use_validation(
      package = "test.package",
      username_list = list(
        vt_user(
          name = "test",
          title = "test",
          role = "tester",
          username = "test"
        )
      ),
      validation_files = list("req1.Rmd",
                              "test_case1.Rmd",
                              "test_code1.R")
    )

    validation_config <- readLines("validation/validation.yml")


    expect_equal(
      validation_config,
      c("package: test.package",
        "working_dir: '.'",
        "output_dir: '.'" ,
        "report_rmd_name: validation.Rmd",
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test",
        "    role: tester",
        "validation_files:",
        "- req1.Rmd",
        "- test_case1.Rmd",
        "- test_code1.R"
      )
    )

    add_user_message <- capture_messages(
      vt_add_user_to_config(username = "test", name = "test", role = "tester2", title = "test2")
    )

    validation_config2 <- readLines("validation/validation.yml")

    expect_equal(
      add_user_message,
      "User `test` information updated in the validation config file.",
    )

    expect_equal(
      validation_config2,
      c("package: test.package",
        "working_dir: '.'",
        "output_dir: '.'" ,
        "report_rmd_name: validation.Rmd",
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test2" ,
        "    role: tester2",
        "validation_files:",
        "- req1.Rmd",
        "- test_case1.Rmd",
        "- test_code1.R"
      )
    )

  })

})

test_that("Test overwriting of the config file", {
  withr::with_tempdir({

    vt_use_validation(
      package = "test.package",
      username_list = list(vt_user(
                               name = "test",
                               title = "test",
                               role = "tester",
                               username = "test"
                             )),
                             validation_files = list("req1.Rmd",
                                                     "test_case1.Rmd",
                                                     "test_code1.R"))

    validation_config<- readLines("validation/validation.yml")

    expect_error(
      vt_use_config(
        package = "test.package",
        pkg = "."),
      paste0(
        "Validation config file already exists.\n",
        "To overwrite, set `overwrite` to `TRUE` in `vt_use_config()`"
      ),
      fixed = TRUE
    )

    vt_use_config(pkg = ".", package = "test.package", overwrite = TRUE)

    validation_config_new <- readLines("validation/validation.yml")

    expect_equal(
      validation_config_new,
      c("package: test.package",
        "working_dir: '.'",
        "output_dir: '.'",
        "report_rmd_name: validation.Rmd",
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames: []",
        "validation_files: []"
      )
    )

    expect_false(
      identical(
        validation_config, validation_config_new
      )
    )

  })

})

test_that("Test config creation without validation infrastructure throws error", {
  withr::with_tempdir({

    quiet <- capture.output(
      usethis::create_package(path = ".")
    )

    expect_error(
      vt_use_config(
        package = "test.package",
        pkg = "."),
      "No validation structure found. Run `valtools::vt_use_validation().",
      fixed = TRUE
    )

  })

})

test_that("Test removal of individual from config file", {

  withr::with_tempdir({

    vt_use_validation(
      package = "test.package",
      username_list = list(vt_user(
                               name = "test",
                               title = "test",
                               role = "tester",
                               username = "test"
                             ),vt_user(
                               name = "test2",
                               title = "test2",
                               role = "tester",
                               username = "test2"
                             )),
                             validation_files = list("req1.Rmd",
                                                     "test_case1.Rmd",
                                                     "test_code1.R"))


    expect_equal(
      get_config_user_name("test2"),
      "test2"
    )

    vt_drop_user_from_config("test2")
    expect_equal(get_config_validation_files(),
                 c("req1.Rmd", "test_case1.Rmd", "test_code1.R"))

    expect_error(
      get_config_user_name("test2")
    )

    expect_warning(
      vt_drop_user_from_config("test2"),
      "User `test2` does not exist in the validation config file.",
      fixed = TRUE
    )

  })

})

test_that("retrieve all users from config", {
  withr::with_tempdir({

    vt_use_validation(package = "test.package")

    expect_equal(vt_get_all_users(), list())

    user_a <- c(username = "auser", name = "A user", title = "title",
                role = "role")
    user_b <- c(username = "buser", name = "Another user", title = "this title",
                role = "this role")
    do.call(vt_add_user_to_config, as.list(user_a))
    do.call(vt_add_user_to_config, as.list(user_b))

    expect_user <- list(as.list(user_a[-1]), as.list(user_b[-1]))
    names(expect_user) <- c(user_a["username"], user_b["username"])
    expect_equal(vt_get_all_users(),
                 expect_user)
  })
})

test_that("Attempting to read when a config file does not exist is informative", {
  withr::with_tempdir({

    file.create(".here")
    dir.create("validation")

    expect_error(
    read_validation_config(),
    "A validation structure does not exist.\nRun `valtools::vt_use_validation()`.",
    fixed=TRUE)

  })

})

test_that("ask_user_name_title_role only requests when missing information",{

  res <- ask_user_name_title_role(
    username = "test",
    name = "test",
    title = "test",
    role = "test"
  )

  expect_equal(
    res,
    vt_user(
      username = "test",
      name = "test",
      title = "test",
      role = "test"
      )
  )

})

test_that("adding and removing validation files from list", {
  withr::with_tempdir({

    vt_use_validation(
      package = "test.package",
      username_list = list(vt_user(
                               name = "test",
                               title = "test2",
                               role = "tester2",
                               username = "test"
                             )))

    validation_config <- readLines("validation/validation.yml")
    expect_equal(validation_config,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames:",
                   "  test:",
                   "    name: test",
                   "    title: test2" ,
                   "    role: tester2",
                   "validation_files: []"
                 ))

    expect_message(vt_add_file_to_config(filename = "req1.Rmd"),
                   "Filename(s): req1.Rmd added to validation config file.", fixed = TRUE)

    validation_config2 <- readLines("validation/validation.yml")
    expect_equal(validation_config2,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames:",
                   "  test:",
                   "    name: test",
                   "    title: test2" ,
                   "    role: tester2",
                   "validation_files:",
                   "- req1.Rmd"
                 ))

    expect_message(vt_add_file_to_config(filename = c("test_case1.Rmd", "test_code1.R")),
                   "Filename(s): test_case1.Rmd, test_code1.R added to validation config file.",
                   fixed = TRUE)
    validation_config3 <- readLines("validation/validation.yml")
    expect_equal(validation_config3,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames:",
                   "  test:",
                   "    name: test",
                   "    title: test2" ,
                   "    role: tester2",
                   "validation_files:",
                   "- req1.Rmd",
                   "- test_case1.Rmd",
                   "- test_code1.R"
                 ))
    expect_error(
      vt_add_file_to_config(filename = "test_case1.Rmd"),
      "File already exists in validation config file. Run `valtools::vt_drop_file_from_config(c(\"test_case1.Rmd\"))` first!",
      fixed = TRUE)


    expect_message(vt_drop_file_from_config("test_case1.Rmd"),
                   "Filename(s): test_case1.Rmd removed from validation config file.",
                   fixed = TRUE)
    validation_config4 <- readLines("validation/validation.yml")
    expect_equal(validation_config4,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames:",
                   "  test:",
                   "    name: test",
                   "    title: test2" ,
                   "    role: tester2",
                   "validation_files:",
                   "- req1.Rmd",
                   "- test_code1.R"
                 ))
    expect_error(vt_drop_file_from_config("test_case1.Rmd"),
                   "File does not exist in validation config file. Run `valtools::vt_add_file_to_config(c(\"test_case1.Rmd\"))` first!",
                   fixed = TRUE)

    # make sure that playing with user entries doesn't break files list

    vt_drop_user_from_config("test")
    validation_config5 <- readLines("validation/validation.yml")
    expect_equal(validation_config5,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames: {}",
                   "validation_files:",
                   "- req1.Rmd",
                   "- test_code1.R"
                 ))

    vt_add_user_to_config(username = "test2", name = "a person", role = "a role", title = "title")
    validation_config6 <- readLines("validation/validation.yml")
    expect_equal(validation_config6,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames:",
                   "  test2:",
                   "    name: a person",
                   "    title: title",
                   "    role: a role",
                   "validation_files:",
                   "- req1.Rmd",
                   "- test_code1.R"
                 ))
  })
})

test_that("inserting validation file at diff location", {
  withr::with_tempdir({
    # default placement at end of filename list captured via:
    # "adding and removing validation files from list"

    # various permutation of position locator
    vt_use_validation(
      package = "test.package",
      username_list = list(vt_user(
                               name = "test",
                               title = "test2",
                               role = "tester2",
                               username = "test"
                             )),
                             validation_files = list("req1.Rmd",
                                                     "test_case1.Rmd",
                                                     "test_code1.R"))
    validation_config <- readLines("validation/validation.yml")
    expect_equal(validation_config,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames:",
                   "  test:",
                   "    name: test",
                   "    title: test2" ,
                   "    role: tester2",
                   "validation_files:",
                   "- req1.Rmd",
                   "- test_case1.Rmd",
                   "- test_code1.R"))

    expect_message(vt_add_file_to_config("report_content_from_template.Rmd", before = "req1.Rmd"),
                   "Filename(s): report_content_from_template.Rmd added to validation config file.",
                   fixed = TRUE)
    validation_config2 <- readLines("validation/validation.yml")
    expect_equal(validation_config2,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames:",
                   "  test:",
                   "    name: test",
                   "    title: test2" ,
                   "    role: tester2",
                   "validation_files:",
                   "- report_content_from_template.Rmd",
                   "- req1.Rmd",
                   "- test_case1.Rmd",
                   "- test_code1.R"))

    vt_add_file_to_config("another_file.Rmd", after = "report_content_from_template.Rmd")
    validation_config3 <- readLines("validation/validation.yml")
    expect_equal(validation_config3,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames:",
                   "  test:",
                   "    name: test",
                   "    title: test2" ,
                   "    role: tester2",
                   "validation_files:",
                   "- report_content_from_template.Rmd",
                   "- another_file.Rmd",
                   "- req1.Rmd",
                   "- test_case1.Rmd",
                   "- test_code1.R"))

    # using tidyselect
    vt_drop_file_from_config("report_content_from_template.Rmd")
    vt_drop_file_from_config("another_file.Rmd")
    vt_add_file_to_config("another_file.Rmd", after = last_col())
    vt_add_file_to_config("report_content_from_template.Rmd", before = starts_with("test"))
    validation_config4 <- readLines("validation/validation.yml")
    expect_equal(validation_config4,
                 c("package: test.package",
                   "working_dir: '.'",
                   "output_dir: '.'" ,
                   "report_rmd_name: validation.Rmd",
                   "report_naming_format: Validation_Report_{package}_v{version}_{date}",
                   "usernames:",
                   "  test:",
                   "    name: test",
                   "    title: test2" ,
                   "    role: tester2",
                   "validation_files:",
                   "- req1.Rmd",
                   "- report_content_from_template.Rmd",
                   "- test_case1.Rmd",
                   "- test_code1.R",
                   "- another_file.Rmd"))

  # position locator doesn't exist in file list
  expect_error(vt_add_file_to_config("another_file2.Rmd", before = "req2.Rmd"),
               class = "vt.validation_config_file_not_listed")
  expect_error(vt_add_file_to_config("another_file2.Rmd", after = "req2.Rmd"),
               class = "vt.validation_config_file_not_listed")


  # both options for locator used
  expect_error(vt_add_file_to_config("a_bad_file.R", before = "req1.Rmd",
                                     after = "report_content_from_template.Rmd"),
               "Must supply only one of `before` and `after`", fixed = TRUE)

  # remove list of files at once
  vt_drop_file_from_config(read_validation_config()$validation_files )
  expect_equal(read_validation_config()$validation_files, list())

  # position locator doesn't exist empty list
  expect_error(vt_add_file_to_config("a_new_file.md", before = "req1.Rmd"),
               class =  "vt.validation_config_file_not_listed")
  expect_error(vt_add_file_to_config("a_new_file.md", after = "req1.Rmd"),
               class = "vt.validation_config_file_not_listed")
  })



})

test_that("when creating validation config yaml throws an error, error propagates up", {
  withr::with_tempdir({


    err <- suppressWarnings(capture_error({
      write_validation_config (
        package = "test.package",
        path = "Invalid/file/path__"
      )
    })
    )

    expect_true(
      grepl(
        "Error during creation of validation.yml config file. Error:",
        x = as.character(err),
        fixed = TRUE
        )
    )

  })

})

