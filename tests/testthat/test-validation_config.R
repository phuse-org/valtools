test_that("Test creation of the config file", {
  withr::with_tempdir({

    vt_use_validation_config(pkg = ".",
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

    validation_config <- readLines("validation.yml")

    expect_equal(
      validation_config,
      c(
        "working_dir: vignettes",
        "output_dir: inst",
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
    vt_use_validation_config(pkg = ".",
                             username_list = list(
                               vt_user(
                                 username = "test",
                                 name = "test",
                                 title = "test",
                                 role = "tester"),
                               "bad Entry"
                             )),
    "1 invalid entry in username_list: \nArgument 2 is not created by `vt_user()`.",
    fixed = TRUE
    )
  })

})

test_that("Test creation of the config file without passed values in a non-interative environment", {
  withr::with_tempdir({

    vt_use_validation_config(pkg = ".")

    validation_config <- readLines("validation.yml")


    expect_equal(
      validation_config,
      c(
        "working_dir: vignettes",
        "output_dir: inst",
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames: []",
        "validation_files: []"
      )
    )

  })

})

test_that("Test creation of the config file without passed values in a non-interative environment and adding a user", {
  withr::with_tempdir({

    vt_use_validation_config(pkg = ".")

    validation_config <- readLines("validation.yml")

    expect_equal(
      validation_config,
      c(
        "working_dir: vignettes",
        "output_dir: inst" ,
        "report_naming_format: Validation_Report_{package}_v{version}_{date}",
        "usernames: []",
        "validation_files: []"
      )
    )

    add_user_message <- capture_messages(
      vt_add_user_to_config(username = "test", name = "test", title = "test", role = "tester")
    )

    validation_config2 <- readLines("validation.yml")

    expect_equal(
      add_user_message,
      "User `test` added to validation config file.\n",
    )

    expect_equal(
      validation_config2,
      c(
        "working_dir: vignettes",
        "output_dir: inst" ,
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

    validation_config3 <- readLines("validation.yml")

    expect_equal(
      add_user_message2,
      "User `test2` added to validation config file.\n",
    )

    expect_equal(
      validation_config3,
      c(
        "working_dir: vignettes",
        "output_dir: inst" ,
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

    vt_add_file_to_config(filename = "req1.Rmd")
    validation_config4 <- readLines("validation.yml")
    expect_equal(
      validation_config4,
      c(
        "working_dir: vignettes",
        "output_dir: inst" ,
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
        "validation_files:",
        "- req1.Rmd"
      )
    )

    vt_add_file_to_config(filename = list("test_case1.Rmd", "test_code1.R"))
    validation_config5 <- readLines("validation.yml")
    expect_equal(
      validation_config5,
      c(
        "working_dir: vignettes",
        "output_dir: inst",
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

test_that("Test creation of the config file without passed values in a non-interative environment and overriding a user", {
  withr::with_tempdir({

    vt_use_validation_config(pkg = ".",
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

    validation_config <- readLines("validation.yml")


    expect_equal(
      validation_config,
      c(
        "working_dir: vignettes",
        "output_dir: inst" ,
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

    validation_config2 <- readLines("validation.yml")

    expect_equal(
      add_user_message,
      "User `test` information updated in the validation config file.\n",
    )

    expect_equal(
      validation_config2,
      c(
        "working_dir: vignettes",
        "output_dir: inst" ,
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

    vt_use_validation_config(pkg = ".",
                             username_list = list(vt_user(
                               name = "test",
                               title = "test",
                               role = "tester",
                               username = "test"
                             )),
                             validation_files = list("req1.Rmd",
                                                     "test_case1.Rmd",
                                                     "test_code1.R"))

    validation_config<- readLines("validation.yml")

    expect_error(
      vt_use_validation_config(pkg = "."),
      paste0(
        "Validation config file already exists.\n",
        "To overwrite, set `overwrite` to `TRUE` in `vt_use_validation_config()`"
      ),
      fixed = TRUE
    )

    vt_use_validation_config(pkg = ".", overwrite = TRUE)

    validation_config_new <- readLines("validation.yml")

    expect_equal(
      validation_config_new,
      c(
        "working_dir: vignettes",
        "output_dir: inst",
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

test_that("Test when in a package validation.yml is added to .Rbuildignore of the config file", {

  withr::with_tempdir({

    capture_output <- capture.output({usethis::create_package(path = ".", open = FALSE,
                                                              rstudio = TRUE,)})


    vt_use_validation_config(pkg = ".",
                             username_list = list(vt_user(
                               name = "test",
                               title = "test",
                               role = "tester",
                               username = "test"
                             )),
                             validation_files = list("req1.Rmd",
                                                     "test_case1.Rmd",
                                                     "test_code1.R"))



    r_build_ignore<- readLines(".Rbuildignore")

    expect_true(
      "^\\validation.yml$" %in% r_build_ignore
    )

  })

})

test_that("Test removal of individual from config file", {

  withr::with_tempdir({

    vt_use_validation_config(pkg = ".",
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

test_that("Attempting to read when a config file does not exist is informative", {
  withr::with_tempdir({

    expect_error(
    read_validation_config(pkg = "."),
    "A validation config file does not exist.\nRun `valtools::vt_use_validation_config()` to create a validation config file.",
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

test_that("add one or more validation files to list",{
  withr::with_tempdir({

  })
})
