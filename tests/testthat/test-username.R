test_that("user can set default username", {
  withr::with_tempdir({
  withr::with_envvar(
    new = list(
      "USER" = "SampleUser"
    ),
    {
      dir.create("validation", recursive = TRUE)
      writeLines(c(
        "working_dir: '.'",
        "usernames:",
        paste0("  ",username(fallback = ""),":"),
        "    name: Sample User",
        "    title: new",
        "    role: user"),
        "validation/validation.yml")
      file.create(".here")

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

      dir.create("validation")
      writeLines(c(
        "working_dir: '.'",
        "usernames:",
        paste0("  sampleUser:"),
        "    name: Sample User",
        "    title: new",
        "    role: user"),
        "validation/validation.yml")
      file.create(".here")


      expect_equal(
        vt_username(),
        ""
        )
    }
  )})
})
