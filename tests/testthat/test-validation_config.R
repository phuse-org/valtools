test_that("Test creation of the config file", {
  withr::with_tempdir({
    vt_use_validation_config(pkg = ".",
                             username_list = list(test = list(
                               name = "test",
                               title = "test",
                               role = "tester",
                               username = "test"
                             )))

    validation_config <- readLines(".validation")


    expect_equal(
      validation_config,
      c(
        "validation_directory: vignettes/validation",
        "validation_output_directory: validation",
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test",
        "    role: tester",
        "    username: test"
      )
    )

  })

})

test_that("Test creation of the config file with invalid username list", {
  withr::with_tempdir({

    expect_error(
    vt_use_validation_config(pkg = ".",
                             username_list = list(test = list(
                               name = "test",
                               username = "test",
                               role = "tester"
                             ))),
    "Entry for username `test` is missing entries for `title` in the username list."
    )

    expect_error(
      vt_use_validation_config(pkg = ".",
                               username_list = list(test = c(
                                 name = "test",
                                 title = "test",
                                 role = "tester"
                               ))),
      "Entry for username `test` is not a list."
    )

    expect_error(
      vt_use_validation_config(pkg = ".",
                               username_list = list(test2 = list(
                                 name = "test",
                                 title = "test",
                                 role = "tester",
                                 username = "test"
                               ))),
      "Entry for username `test2` does not match the username of the content: `test`."
    )
  })

})

test_that("Test creation of the config file without passed values in a non-interative environment", {
  withr::with_tempdir({

    vt_use_validation_config(pkg = ".")

    validation_config <- readLines(".validation")


    expect_equal(
      validation_config,
      c(
        "validation_directory: vignettes/validation",
        "validation_output_directory: validation",
        "usernames: []"
      )
    )

  })

})

test_that("Test creation of the config file without passed values in a non-interative environment and adding a user", {
  withr::with_tempdir({

    vt_use_validation_config(pkg = ".")

    validation_config <- readLines(".validation")

    expect_equal(
      validation_config,
      c(
        "validation_directory: vignettes/validation",
        "validation_output_directory: validation" ,
        "usernames: []"
      )
    )

    add_user_message <- capture_messages(
      vt_add_user_to_config(username = "test", name = "test", title = "test", role = "tester")
    )

    validation_config2 <- readLines(".validation")

    expect_equal(
      add_user_message,
      "User `test` added to validation config file.\n",
    )

    expect_equal(
      validation_config2,
      c(
        "validation_directory: vignettes/validation",
        "validation_output_directory: validation" ,
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test" ,
        "    role: tester" ,
        "    username: test"
      )
    )

    add_user_message2 <- capture_messages(
      vt_add_user_to_config(username = "test2", name = "test2", role = "tester2", title = "tester2")
    )

    validation_config3 <- readLines(".validation")

    expect_equal(
      add_user_message2,
      "User `test2` added to validation config file.\n",
    )

    expect_equal(
      validation_config3,
      c(
        "validation_directory: vignettes/validation",
        "validation_output_directory: validation" ,
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test" ,
        "    role: tester" ,
        "    username: test",
        "  test2:",
        "    name: test2",
        "    title: tester2" ,
        "    role: tester2" ,
        "    username: test2"
      )
    )

  })

})

test_that("Test creation of the config file without passed values in a non-interative environment and overriding a user", {
  withr::with_tempdir({

    vt_use_validation_config(pkg = ".",
                             username_list = list(test = list(
                               name = "test",
                               title = "test",
                               role = "tester",
                               username = "test"
                             )))

    validation_config <- readLines(".validation")


    expect_equal(
      validation_config,
      c(
        "validation_directory: vignettes/validation",
        "validation_output_directory: validation" ,
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test",
        "    role: tester",
        "    username: test"
      )
    )

    add_user_message <- capture_messages(
      vt_add_user_to_config(username = "test", name = "test", role = "tester2", title = "test2")
    )

    validation_config2 <- readLines(".validation")

    expect_equal(
      add_user_message,
      "User `test` information updated in the validation config file.\n",
    )

    expect_equal(
      validation_config2,
      c(
        "validation_directory: vignettes/validation",
        "validation_output_directory: validation" ,
        "usernames:",
        "  test:",
        "    name: test",
        "    title: test2" ,
        "    role: tester2" ,
        "    username: test"
      )
    )

  })

})

test_that("Test overwriting of the config file", {
  withr::with_tempdir({

    vt_use_validation_config(pkg = ".",
                             username_list = list(test = list(
                               name = "test",
                               title = "test",
                               role = "tester",
                               username = "test"
                             )))

    validation_config<- readLines(".validation")

    expect_error(
      vt_use_validation_config(pkg = "."),
      paste0(
        "Validation config file already exists.\n",
        "To overwrite, set `overwrite` to `TRUE` in `vt_use_validation_config()`"
      ),
      fixed = TRUE
    )

    vt_use_validation_config(pkg = ".", overwrite = TRUE)

    validation_config_new <- readLines(".validation")

    expect_equal(
      validation_config_new,
      c(
        "validation_directory: vignettes/validation",
        "validation_output_directory: validation",
        "usernames: []"
      )
    )

    expect_false(
      identical(
        validation_config, validation_config_new
      )
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
    list(
      test = list(
        name = "test",
        title = "test",
        role = "test",
        username = "test"
      )
    )
  )



})
