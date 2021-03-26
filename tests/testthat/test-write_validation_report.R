test_that("validation report in package",{

  withr::with_tempdir({
    # using the default .validation Validation Lead user
    test_user <- whoami::username(fallback = "")
    report_name <- paste0("Validation_Report_", basename(getwd()),
                          "_v0.0.0.9000_", format(Sys.Date(), "%Y%m%d"), ".Rmd")
    vt_create_package(open = FALSE)
    vt_use_report()
    report_code <- readLines(file.path(getwd(), "inst", report_name))

    expect_true(file.exists(file.path(getwd(),"inst", report_name)))
    # lines in rmd template that are updated via vt_use_report calls
    expect_equal(report_code[2], "title: Validation Report")
    expect_equal(report_code[3], paste0("author: ", test_user))
    expect_equal(report_code[9], "  %\\VignetteIndexEntry{ Validation Report }")
    expect_equal(report_code[21], paste0("library(", basename(getwd()), ")"))


  })
})

test_that("validation report in package",{

  withr::with_tempdir({
    # using the default .validation Validation Lead user

    report_name <- paste0("Validation_Report_", basename(getwd()),
                          "_v0.0.0.9000_", format(Sys.Date(), "%Y%m%d"), ".Rmd")
    captured_output <- capture.output({vt_create_package(open = FALSE)})
    vt_add_user_to_config(username = "aperson",
                          name = "An author",
                          title = "Programmer",
                          role = "Validation Lead")
    vt_use_report()
    report_code <- readLines(file.path(getwd(), "inst", report_name))

    expect_true(file.exists(file.path(getwd(),"inst", report_name)))
    # lines in rmd template that are updated via vt_use_report calls
    expect_equal(report_code[3], paste0("author: An author"))


  })
})


test_that("multiple authors",{

  withr::with_tempdir({
    # using the default .validation Validation Lead user

    report_name <- paste0("Validation_Report_", basename(getwd()),
                          "_v0.0.0.9000_", format(Sys.Date(), "%Y%m%d"), ".Rmd")
    captured_output <- capture.output(vt_create_package(open = FALSE))
    vt_add_user_to_config(username = "aperson",
                          name = "An author",
                          title = "Programmer",
                          role = "Validation Lead")

    vt_add_user_to_config(username = "bperson",
                          name = "Another author",
                          title = "Something",
                          role = "Validation Lead Designee")

    vt_add_user_to_config(username = "cperson",
                          name = "Not the validation lead",
                          title = "Manager",
                          role = "Manager")
    vt_use_report()
    report_code <- readLines(file.path(getwd(), "inst", report_name))

    expect_true(file.exists(file.path(getwd(),"inst", report_name)))
    # lines in rmd template that are updated via vt_use_report calls
    expect_equal(report_code[3], paste0("author: An author, Another author"))


  })
})




test_that("multiple authors",{

  withr::with_tempdir({
    # using the default Validation Lead user
    test_user <- whoami::username(fallback = "")
    report_name <- paste0("Validation_Report_", basename(getwd()),
                          "_v0.0.0.9000_", format(Sys.Date(), "%Y%m%d"), ".Rmd")
    vt_create_package(open = FALSE)

    vt_use_req("requirement1.md", username = "author1", open = FALSE)
    vt_use_test_case("testcase1.md", username = "author1", open = FALSE)

    vt_use_report()
    report_code <- readLines(file.path(getwd(), "inst", report_name))


  })
})

