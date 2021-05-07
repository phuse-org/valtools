test_that("integration test for CRAN", {
  withr::with_tempdir({
    # using the default .validation Validation Lead user
    test_user <- whoami::username(fallback = "runner")
    report_name <- "validation_report.Rmd"
    captured_output <- capture.output(vt_create_package(open = FALSE))
    vt_add_user_to_config(username = "auser", name = "A user", title = "staff",
                          role = "Project Lead")
    vt_add_user_to_config(username = "buser", name = "B user", title = "staff",
                          role = "Write Requirements")
    vt_add_user_to_config(username = "cuser", name = "C user", title = "staff",
                          role = "Write Test Cases, Tester")
    vt_add_user_to_config(username = "duser", name = "D user", title = "staff",
                          role = "Write Code")
    vt_use_change_log(date = "2021-01-01", open = FALSE)
    vt_use_req("req1.md", username = "B user", title = "##req:req1", open = FALSE)
    writeLines(con = file.path("vignettes", "validation", "requirements", "req1.md"),
               c(
                 "#' @title ##req:req1",
                 "#' @editor B user",
                 "#' @editDate 2021-04-27",
                 "#' @riskAssessment",
                 "#' REQUIREMENT: ASSESSMENT",
                 "\n",
                 "## Requirement ##req:req1",
                 "\n",
                 "+ Requirement text",
                 "  - More text",
                 "\n")
    )
    vt_use_req("req2.md", username = "B user", title = "##req:req2", open = FALSE)
    writeLines(con = file.path("vignettes", "validation", "requirements", "req2.md"),
               c(
                 "#' @title ##req:req2",
                 "#' @editor B user",
                 "#' @editDate 2021-04-27",
                 "#' @riskAssessment",
                 "#' REQUIREMENT: ASSESSMENT",
                 "\n",
                 "## Requirement ##req:req2",
                 "\n",
                 "+ Requirement text",
                 "  - More text",
                 "\n")
    )
    vt_use_test_case("test_case1.md", username = "C User", title = "##tc:tc1", open = FALSE)
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
               "##  Test Case ##tc:tc1",
               "+ Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
               "",
               "+ Start documenting test case here!"))
    vt_use_test_code("test_code1.R", username = "another user", open = FALSE)
    writeLines(con = file.path("vignettes", "validation", "test_code", "test_code1.R"),
               c(
                 "# Test setup",
                 "\n",
                 "#' @editor another user",
                 "#' @editDate 2021-04-28",
                 "test_that(\"1.1\", {",
                 "  testthat::expect_true(TRUE)",
                 "})"
               ))

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


    vt_use_report(dynamic_referencing = TRUE, open = FALSE)
    report_code <- readLines(file.path(getwd(), "vignettes", report_name))

    withr::with_temp_libpaths({

      install.packages(getwd(), type = "source", repos = NULL, quiet = TRUE)
      rmarkdown::render(file.path(getwd(), "vignettes", report_name), quiet = TRUE)

    })

    expect_true(file.exists(file.path(getwd(),"vignettes", report_name)))
    # lines in rmd template that are updated via vt_use_report calls
    expect_equal(report_code[2], paste("title: Validation Report for",basename(getwd())))
    expect_equal(report_code[3], paste0("author: ", test_user))
    expect_equal(report_code[28], paste0("  library(", basename(getwd()), ")"))
    expect_equal(report_code[154], "vt_file(vt_path(child_files),dynamic_referencing = TRUE)")

  })
})

test_that("validation report in package",{
  skip_on_cran()
  withr::with_tempdir({
    # using the default .validation Validation Lead user
    test_user <- whoami::username(fallback = "")
    report_name <- "validation_report.Rmd"
    captured_output <- capture.output(vt_create_package(open = FALSE))
    vt_use_report()
    report_code <- readLines(file.path(getwd(), "vignettes", report_name))

    expect_true(file.exists(file.path(getwd(),"vignettes", report_name)))
    # lines in rmd template that are updated via vt_use_report calls
    expect_equal(report_code[2], paste("title: Validation Report for",basename(getwd())))
    expect_equal(report_code[3], paste0("author: ", test_user))
    expect_equal(report_code[28], paste0("  library(", basename(getwd()), ")"))
    expect_equal(report_code[154], "vt_file(vt_path(child_files),dynamic_referencing = FALSE)")

  })
})

test_that("validation report in package",{
  skip_on_cran()
  withr::with_tempdir({
    # using the default .validation Validation Lead user
    report_name <- "validation_report.Rmd"
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
  skip_on_cran()
  withr::with_tempdir({
    # using the default .validation Validation Lead user
    report_name <- "validation_report.Rmd"
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



test_that("define author on report generation",{
  skip_on_cran()
  withr::with_tempdir({
    # using the default .validation Validation Lead user

    report_name <- "validation_report.Rmd"
    captured_output <- capture.output(vt_create_package(open = FALSE))
    vt_add_user_to_config(username = "a_person",
                          name = "Andy Person",
                          title = "Programmer",
                          role = "Specifier")

    vt_add_user_to_config(username = "b_person",
                          name = "Brandy Person",
                          title = "Something",
                          role = "Progammer")

    vt_add_user_to_config(username = "c_person",
                          name = "Ager Man",
                          title = "Manager",
                          role = "Manager")

    withr::with_envvar(
      new = list(LOGNAME = "a_person"),{
        vt_use_report()
    })

    report_code <- readLines(file.path(getwd(), "vignettes", report_name))

    expect_true(file.exists(file.path(getwd(),"vignettes", report_name)))
    # lines in rmd template that are updated via vt_use_report calls
    expect_equal(report_code[3], paste0("author: Andy Person"))


  })
})

test_that("Validation outside a package - integration test for CRAN", {
  withr::with_tempdir({

    dir.create("rlang_validation")
    setwd("rlang_validation")

    # using the default .validation Validation Lead user
    test_user <- whoami::username(fallback = "runner")
    report_name <- "validation_report.Rmd"
    captured_output <- capture.output(
      vt_use_validation(package = "rlang",open = FALSE)
    )


    vt_add_user_to_config(username = "auser", name = "A user", title = "staff",
                          role = "Project Lead")
    vt_add_user_to_config(username = "buser", name = "B user", title = "staff",
                          role = "Write Requirements")
    vt_add_user_to_config(username = "cuser", name = "C user", title = "staff",
                          role = "Write Test Cases, Tester")
    vt_add_user_to_config(username = "duser", name = "D user", title = "staff",
                          role = "Write Code")

    vt_use_change_log(date = "2021-01-01", open = FALSE)

    vt_use_req("req1.md", username = "B user", title = "1", open = FALSE)

    writeLines(con = vt_path("requirements", "req1.md"),
               c(
                 "#' @title 1",
                 "#' @editor B user",
                 "#' @editDate 2021-04-27",
                 "#' @riskAssessment",
                 "#' REQUIREMENT: ASSESSMENT",
                 "\n",
                 "## Requirement 1",
                 "\n",
                 "+ Requirement text",
                 "  - More text",
                 "\n")
    )

    vt_use_req("req2.md", username = "B user", title = "2", open = FALSE)
    writeLines(con = vt_path("requirements", "req2.md"),
               c(
                 "#' @title 2",
                 "#' @editor B user",
                 "#' @editDate 2021-04-27",
                 "#' @riskAssessment",
                 "#' REQUIREMENT: ASSESSMENT",
                 "\n",
                 "## Requirement 2",
                 "\n",
                 "+ Requirement text",
                 "  - More text",
                 "\n")
    )

    vt_use_test_case("test_case1.md", username = "C User", title = "##tc:tc1", open = FALSE)
    writeLines(con = vt_path("test_cases", "test_case1.md"),
               c(
                 "#' @title ##tc:tc1",
                 "#' @editor C User",
                 "#' @editDate 2021-03-26",
                 "#' @coverage",
                 "#' ##tc1.1: ##req1.1, ##req1.2",
                 "#' ##tc1.2: ##req1.3",
                 "#' ##tc1.3: ##req1.4",
                 "",
                 "##  Test Case ##tc:tc1",
                 "+ Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
                 "",
                 "+ Start documenting test case here!"))

    vt_use_test_code("test_code1.R", username = "another user", open = FALSE)
    writeLines(con = vt_path("test_code", "test_code1.R"),
               c(
                 "# Test setup",
                 "\n",
                 "#' @editor another user",
                 "#' @editDate 2021-04-28",
                 "test_that(\"1.1\", {",
                 "  testthat::expect_true(TRUE)",
                 "})"
               ))


    vt_use_report()
    report_code <- readLines(report_name)

    ## remove section to get function authors from package - not applicable
    # writeLines(report_code[-c(94:108)], report_name)

    output_report <- rmarkdown::render(report_name, quiet = TRUE)

    expect_true(file.exists(report_name))
    expect_true(file.exists(gsub("[.]Rmd$",".pdf",report_name)))
    # lines in rmd template that are updated via vt_use_report calls
    expect_equal(report_code[2], "title: Validation Report for rlang")
    expect_equal(report_code[3], paste0("author: ", test_user))
    expect_equal(report_code[10], "  %\\VignetteIndexEntry{Validation Report}")
    expect_equal(report_code[27], "  library(rlang)")

  })
})


