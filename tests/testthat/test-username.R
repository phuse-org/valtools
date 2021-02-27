test_that("user can set default username", {
  withr::with_options(
    new = list(
      "vt_username" = "Sample User"
    ),
    {
      expect_equal(
        vt_username(),
        "Sample User")
    }
  )
})


test_that("When a username is not set, it will default to username", {
  withr::with_options(
    new = list(
      "vt_username" = NULL
    ),
    {
      expect_equal(
        vt_username(),
        whoami::username(fallback = "")
        )
    }
  )
})
