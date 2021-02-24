test_that("Accessing config dirs works", {

    withr::with_tempdir({

      vt_use_validation_config(pkg = ".")

      expect_equal(
        get_config_val_dir(),
        "vignettes/validation"
      )

      expect_equal(
        get_config_val_dir_o(),
        "validation"
      )

    })

  withr::with_tempdir({

    vt_use_validation_config(pkg = ".",val_dir = "new/dir",val_dir_o = "test")

    expect_equal(
      get_config_val_dir(),
      "new/dir"
    )

    expect_equal(
      get_config_val_dir_o(),
      "test"
    )

  })

})

test_that("Accessing config user info works", {

  withr::with_tempdir({

    vt_use_validation_config(pkg = ".",
                             username_list = list(
                               "test-username" = list(
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

    vt_use_validation_config(pkg = ".",
                             username_list = list(
                               "test-username" = list(
                                 name = "test-name",
                                 title = "test-title",
                                 role = "test-role",
                                 username = "test-username"
                               ),
                               "test-username2" = list(
                                 name = "test-name2",
                                 title = "test-title2",
                                 role = "test-role2",
                                 username = "test-username2"
                               ),
                               "test-username3" = list(
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

    vt_use_validation_config(pkg = ".",
                             username_list = list(
                               "test-username" = list(
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

