test_that("user can set default username", {
  withr::with_tempdir({
  withr::with_envvar(
    new = list(
      "USER" = "SampleUser"
    ),
    {
      writeLines(c(
        "working_dir: vignettes",
        "usernames:",
        paste0("  ",username(fallback = ""),":"),
        "    name: Sample User",
        "    title: new",
        "    role: user"),
        ".validation")

      expect_equal(
        vt_username(),
        "Sample User")
    }
  )})
})


test_that("When a username is not set, it will throw an error that the user does not exist", {
  withr::with_tempdir({
  withr::with_options(
    new = list(
      "vt_username" = NULL
    ),
    {
      writeLines(c(
        "working_dir: vignettes",
        "usernames:",
        paste0("  sampleUser:"),
        "    name: Sample User",
        "    title: new",
        "    role: user"),
        ".validation")


      expect_error(
        vt_username(),
        "User `.+` does not exist in the config file[.]"
        )
    }
  )})
})
