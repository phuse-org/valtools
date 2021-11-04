test_that("Accessing config dirs works", {

    withr::with_tempdir({

      vt_use_validation(pkg = ".", package = "test.package")

      expect_equal(
        get_config_working_dir(),
        "."
      )

      expect_equal(
        get_config_output_dir(),
        "."
      )

      expect_equal(
        get_config_report_rmd_name(),
        "validation.Rmd"
      )

      expect_equal(
        get_config_report_naming_format(),
        "Validation_Report_{package}_v{version}_{date}"
      )

    })

  withr::with_tempdir({

    vt_use_validation(
      package = "test.package",
      working_dir = "new/dir",
      output_dir = "test",
      report_rmd_name = "test_validation.Rmd",
      report_naming_format = "{package}_v{version}_Validation_report"
    )

    expect_equal(
      get_config_working_dir(),
      "new/dir"
    )

    expect_equal(
      get_config_output_dir(),
      "test"
    )

    expect_equal(
      get_config_report_rmd_name(),
      "test_validation.Rmd"
    )

  })

})

test_that("Accessing config user info works", {

  withr::with_tempdir({

    vt_use_validation(
      package = "test.package",
      username_list = list(
                               vt_user(
                                 name = "test-name",
                                 title = "test-title",
                                 role = "test-role",
                                 username = "test-username"
                               )
                             ))

    expect_equal(
      get_config_user_name("test-username"),
      "test-name"
    )
    expect_equal(
      get_config_user_title("test-username"),
      "test-title"
    )
    expect_equal(
      get_config_user_role("test-username"),
      "test-role"
    )

    expect_equal(
      vt_get_user_info(username = "test-username", type = c("name","title","role")),
      c(
        name = "test-name",
        title = "test-title",
        role = "test-role"
      )
    )

  })

})

test_that("Accessing config user info works even with multiple users", {

  withr::with_tempdir({

    vt_use_validation(
      package = "test.package",
      username_list = list(
                               vt_user(
                                 name = "test-name",
                                 title = "test-title",
                                 role = "test-role",
                                 username = "test-username"
                               ),
                               vt_user(
                                 name = "test-name2",
                                 title = "test-title2",
                                 role = "test-role2",
                                 username = "test-username2"
                               ),
                               vt_user(
                                 name = "test-name3",
                                 title = "test-title3",
                                 role = "test-role3",
                                 username = "test-username3"
                               )
                             ))

    expect_equal(
      get_config_user_name("test-username2"),
      "test-name2"
    )
    expect_equal(
      get_config_user_title("test-username2"),
      "test-title2"
    )
    expect_equal(
      get_config_user_role("test-username2"),
      "test-role2"
    )

    expect_equal(
      vt_get_user_info(username = "test-username2", type = c("name","title","role")),
      c(
        name = "test-name2",
        title = "test-title2",
        role = "test-role2"
      )
    )

  })


})

test_that("Accessing config user info that does not exist throws informative error", {

  withr::with_tempdir({

    vt_use_validation(
      package = "test.package",
                             username_list = list(
                               vt_user(
                                 name = "test-name",
                                 title = "test-title",
                                 role = "test-role",
                                 username = "test-username"
                               )
                             ))

    expect_error(
      get_config_user("test-username2"),
      paste0(
        "User `test-username2` does not exist in the config file.\n",
        "Add `test-username2` to the config file with `vt_add_user_to_config(\"test-username2\")`."
      ),
      fixed = TRUE
    )

    expect_error(
      vt_get_user_info(username = "test-username2", type = c("name","title","role")),
      paste0(
        "User `test-username2` does not exist in the config file.\n",
        "Add `test-username2` to the config file with `vt_add_user_to_config(\"test-username2\")`."
      ),
      fixed = TRUE
    )

  })

})



test_that("Test getting package name from config file", {

  withr::with_tempdir({

    vt_use_validation(package = "test.package")

    expect_equal(
      get_config_package(),
      "test.package"
    )
  })

  withr::with_tempdir({

    quiet <- capture.output({
      vt_create_package(pkg = "my.package", open = FALSE)
    })

    setwd("my.package")

    expect_equal(
      get_config_package(),
      "my.package"
    )
  })

})

test_that("Accessing config output dirs returns working dir when it is missing", {

  withr::with_tempdir({

    vt_use_validation(
      pkg = ".",
      package = "test.package"
    )

    ## remove output_dir from config
    write_yaml(
        x = list(
          package = "test.package",
          working_dir = "working_dir"
        ),
        file = file.path("validation", "validation.yml")
    )

    expect_equal(
      get_config_output_dir(),
      "working_dir"
    )

  })
})
