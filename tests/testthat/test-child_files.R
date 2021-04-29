test_that("child files in pkg", {
  withr::with_tempdir({
    capture_output <- capture.output({vt_create_package(open = FALSE)})

    vt_use_req("req1", username = "a user", open = FALSE)
    vt_use_test_case("testcase1", username = "a user", open = FALSE)
    vt_use_test_code("testcode1", username = "another user", open = FALSE)

    vt_use_req("req2", username = "a user", open = FALSE)
    vt_use_test_case("testcase2", username = "a user", open = FALSE)
    vt_use_test_code("testcode2", username = "another user", open = FALSE)

    vt_use_req("req3", username = "a user", open = FALSE)
    vt_use_test_case("testcase3", username = "a user", open = FALSE)
    vt_use_test_code("testcode3", username = "another user", open = FALSE)
    testthat::expect_equal(
      vt_get_child_files(validation_order = c("test_cases", "test_code", "requirements")),
                           c("testcase1.md",
                             "testcase2.md",
                             "testcase3.md",
                             "testcode1.R",
                             "testcode2.R",
                             "testcode3.R",
                             "req1.md",
                             "req2.md",
                             "req3.md"))
    testthat::expect_equal(
      vt_get_child_files(loc = "yml"),
      c("req1.md",
        "testcase1.md",
        "testcode1.R",
        "req2.md",
        "testcase2.md",
        "testcode2.R",
        "req3.md",
        "testcase3.md",
        "testcode3.R"  )
    )
  })
})


test_that("child files outside pkg", {
  withr::with_tempdir({
     vt_use_validation()
     vt_use_test_case("testcase1", username = "a user", open = FALSE)
     vt_use_req("req1", username = "a user", open = FALSE)
     vt_use_test_code("testcode1", username = "another user", open = FALSE)

     # as listed in validation.yml validation_files
     testthat::expect_equal(
       vt_get_child_files(loc = "yml"),
       c("testcase1.md","req1.md","testcode1.R"))

     # as ordered in validation subfolders
     testthat::expect_equal(
       vt_get_child_files(loc = "folder", validation_order = c("requirements", "test_cases", "test_code")),
       c("req1.md","testcase1.md","testcode1.R" ))

   })
})


test_that("incomplete set", {
  withr::with_tempdir({
    vt_use_validation()
    vt_use_test_case("testcase1", username = "a user", open = FALSE)
    vt_use_test_case("testcase2", username = "a user", open = FALSE)
    vt_use_req("req1", username = "a user", open = FALSE)
    vt_use_req("req2", username = "a user", open = FALSE)

    # as listed in validation.yml validation_files
    testthat::expect_equal(
      vt_get_child_files(loc = "yml"),
      c("testcase1.md", "testcase2.md",
        "req1.md","req2.md"))

    # as ordered in validation subfolders
    testthat::expect_equal(
      vt_get_child_files(loc = "folder", validation_order = c("requirements", "test_cases", "test_code")),
      c("req1.md","req2.md", "testcase1.md", "testcase2.md"
        ))

  })
})
