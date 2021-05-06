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
                           c("test_cases/testcase1.md",
                             "test_cases/testcase2.md",
                             "test_cases/testcase3.md",
                             "test_code/testcode1.R",
                             "test_code/testcode2.R",
                             "test_code/testcode3.R",
                             "requirements/req1.md",
                             "requirements/req2.md",
                             "requirements/req3.md"))
    testthat::expect_equal(
      vt_get_child_files(loc = "yml"),
      c("requirements/req1.md",
        "test_cases/testcase1.md",
        "test_code/testcode1.R",
        "requirements/req2.md",
        "test_cases/testcase2.md",
        "test_code/testcode2.R",
        "requirements/req3.md",
        "test_cases/testcase3.md",
        "test_code/testcode3.R"  )
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
       c("test_cases/testcase1.md","requirements/req1.md","test_code/testcode1.R"))

     # as ordered in validation subfolders
     testthat::expect_equal(
       vt_get_child_files(loc = "folder", validation_order = c("requirements", "test_cases", "test_code")),
       c("requirements/req1.md","test_cases/testcase1.md","test_code/testcode1.R" ))

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
      c("test_cases/testcase1.md", "test_cases/testcase2.md",
        "requirements/req1.md","requirements/req2.md"))

    # as ordered in validation subfolders
    testthat::expect_equal(
      vt_get_child_files(loc = "folder", validation_order = c("requirements", "test_cases", "test_code")),
      c("requirements/req1.md","requirements/req2.md", "test_cases/testcase1.md", "test_cases/testcase2.md")
        )

  })
})


test_that("compatibility between vt_get_child_files and vt_files", {
  withr::with_tempdir({
    captured_output <- capture.output({vt_create_package(open = FALSE)})
    vt_use_test_case("testcase1", username = "a user", open = FALSE)
    vt_use_test_case("testcase2", username = "a user", open = FALSE)
    vt_use_req("req1", username = "a user", open = FALSE)
    vt_use_req("req2", username = "a user", open = FALSE)

    child_files <- vt_get_child_files(loc = "yml")
    setwd(file.path("vignettes", "validation"))

    rmd_asis <- vt_file(child_files)
    testthat::expect_true(exists("rmd_asis"))

  })
})
