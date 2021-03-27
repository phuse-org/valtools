test_that("integration test for CRAN", {
  withr::with_tempdir({
    # using the default .validation Validation Lead user
    test_user <- whoami::username(fallback = "")
    report_name <- paste0("Validation_Report_", basename(getwd()),
                          "_v0.0.0.9000_", format(Sys.Date(), "%Y%m%d"), ".Rmd")
    captured_output <- capture.output(vt_create_package(open = FALSE))
    vt_add_user_to_config(username = "auser", name = "A user", title = "staff",
                          role = "Project Lead")
    vt_add_user_to_config(username = "buser", name = "B user", title = "staff",
                          role = "Write Requirements")
    vt_add_user_to_config(username = "cuser", name = "C user", title = "staff",
                          role = "Write Test Cases, Tester")
    vt_add_user_to_config(username = "duser", name = "D user", title = "staff",
                          role = "Write Code")
    vt_use_news_md(date = "2021-01-01")
    vt_use_req("req1.md", username = "B user", title = "##req:req1", open = FALSE)
    vt_use_test_case("test_case1.md", username = "C User", title = "##tc:tc1")
    writeLines(con = file.path("vignettes", "validation", "test_cases", "test_case1.md"),
               c(
               "#' @title ##tc:tc1",
               "#' @editor C User",
               "#' @editDate 2021-03-26",
               "#' @coverage",
               "#' ##tc1.1: ##req1.1, ##req1.2",
               "#' ##tc1.2: ##req1.3",
               "#' ##tc1.3: ##req1.4",
               "",
               "+ _Test Case_",
               "+ Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
               "",
               "+ Start documenting test case here!"))
    file.create(file.path("R", "hello_world.R"))
    writeLines(con = file.path("R", "hello_world.R"),
               c("#' dummy function",
                 "#' @param name person name",
                 "#' @return greeting",
                 "#' @editor An author",
                 "#' @editDate 2021-01-01",
                 "#' @export",
                 " hello_world <- function(name){",
                 "   paste('Hello', name, '!')",
                 " }",
                 "\n\n",
                 "#' dummy function2",
                 "#' @param name person name",
                 "#' @return greeting with date",
                 "#' @editor Another author",
                 "#' @editDate 2021-01-01",
                 "hello_world2 <- function(name){",
                 "  paste0('Hello ', name, ', today is: ', Sys.Date(), '!')",
                 "}"
               ))
    vt_use_report()
    report_code <- readLines(file.path(getwd(), "vignettes", report_name))

    expect_true(file.exists(file.path(getwd(),"vignettes", report_name)))
    # lines in rmd template that are updated via vt_use_report calls
    expect_equal(report_code[2], "title: Validation Report")
    expect_equal(report_code[3], paste0("author: ", test_user))
    expect_equal(report_code[9], "  %\\VignetteIndexEntry{ Validation Report }")
    expect_equal(report_code[24], paste0("  library(", basename(getwd()), ")"))


  })
})

test_that("validation report in package",{

  withr::with_tempdir({
    # using the default .validation Validation Lead user
    test_user <- whoami::username(fallback = "")
    report_name <- paste0("Validation_Report_", basename(getwd()),
                          "_v0.0.0.9000_", format(Sys.Date(), "%Y%m%d"), ".Rmd")
    captured_output <- capture.output(vt_create_package(open = FALSE))
    vt_use_report()
    report_code <- readLines(file.path(getwd(), "vignettes", report_name))

    expect_true(file.exists(file.path(getwd(),"vignettes", report_name)))
    # lines in rmd template that are updated via vt_use_report calls
    expect_equal(report_code[2], "title: Validation Report")
    expect_equal(report_code[3], paste0("author: ", test_user))
    expect_equal(report_code[9], "  %\\VignetteIndexEntry{ Validation Report }")
    expect_equal(report_code[24], paste0("  library(", basename(getwd()), ")"))

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
    report_code <- readLines(file.path(getwd(), "vignettes", report_name))

    expect_true(file.exists(file.path(getwd(),"vignettes", report_name)))
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
    report_code <- readLines(file.path(getwd(), "vignettes", report_name))

    expect_true(file.exists(file.path(getwd(),"vignettes", report_name)))
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

