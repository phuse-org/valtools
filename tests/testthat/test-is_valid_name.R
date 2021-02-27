test_that("valid names all pass", {
  expect_equal(
    capture.output(is_valid_name("test")),
    character()
  )

  expect_equal(
    capture.output(is_valid_name("test123")),
    character()
  )

  expect_equal(
    capture.output(is_valid_name("test-123")),
    character()
  )

  expect_equal(
    capture.output(is_valid_name("test_123")),
    character()
  )

  expect_equal(
    capture.output(is_valid_name("test/path")),
    character()
  )


  expect_equal(
    capture.output(is_valid_name("test\\path")),
    character()
  )

})

test_that("invalid names invoke error", {

  expect_error(
    is_valid_name("test fail"),
    "is not a valid file name.",
    fixed = TRUE
  )

  expect_error(
    is_valid_name("test!"),
    "is not a valid file name.",
    fixed = TRUE
  )

})

test_that("not strings invoke error", {

  expect_error(
    is_valid_name(12345),
    "Name must be a single string",
    fixed = TRUE
  )

  expect_error(
    is_valid_name(c("test", "testtwo")),
    "Name must be a single string",
    fixed = TRUE
  )

})

