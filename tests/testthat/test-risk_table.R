test_that("Able to scrape risk assessments into a simple table works", {
  withr::with_tempdir({

    quiet <- capture.output({
      vt_create_package("example.package", open=FALSE)
    })

    setwd("example.package")

    vt_use_req("requirement_001.md",username = "example",title = "Req 001", open = FALSE)
    writeLines(c("#' @title Req 001",
                 "#' @editor example",
                 "#' @editDate 2021-04-26",
                 "#' @riskAssessment",
                 "#' 1.1: 4 Low probablity, Low impact",
                 "#' 1.2: 10 Low probablity, High impact",
                 "",
                 "+ 1.1 Say Hello",
                 "+ 1.2 Say Name",
                 ""),
               con = vt_path("requirements","requirement_001.md")
               )

    risk_table <- vt_scrape_risk_assessment()

    expect_equal(
      risk_table,
      data.frame(
        Title = c("Req 001"),
        Requirement = c("1.1", "1.2"),
        Assessment = c("4 Low probablity, Low impact","10 Low probablity, High impact"),
        stringsAsFactors = FALSE
      )
    )

  })
})

test_that("Able to scrape multiple risk assessments into a simple table works", {
  withr::with_tempdir({

    quiet <- capture.output({
      vt_create_package("example.package", open=FALSE)
    })

    setwd("example.package")

    vt_use_req("requirement_001.md",username = "example",title = "Req 001", open = FALSE)
    vt_use_req("requirement_002.md",username = "example",title = "Req 002", open = FALSE)
    writeLines(c("#' @title Req 001",
                 "#' @editor example",
                 "#' @editDate 2021-04-26",
                 "#' @riskAssessment",
                 "#' 1.1: 4 Low probablity, Low impact",
                 "#' 1.2: 10 Low probablity, High impact",
                 "",
                 "+ 1.1 Say Hello",
                 "+ 1.2 Say Name",
                 ""),
               con = vt_path("requirements","requirement_001.md")
    )
    writeLines(c("#' @title Req 002",
                 "#' @editor example",
                 "#' @editDate 2021-04-26",
                 "#' @riskAssessment",
                 "#' 2.1: 1 Low probablity, really Low impact",
                 "",
                 "+ 2.1 Print values Hello",
                 ""),
               con = vt_path("requirements","requirement_002.md")
    )
    risk_table <- vt_scrape_risk_assessment()

    expect_equal(
      risk_table,
      data.frame(
        Title = c("Req 001","Req 001","Req 002"),
        Requirement = c("1.1", "1.2","2.1"),
        Assessment = c("4 Low probablity, Low impact","10 Low probablity, High impact","1 Low probablity, really Low impact"),
        stringsAsFactors = FALSE
      )
    )

  })
})

test_that("Able to scrape risk assessments into a simple table with dynamic referencing works", {
  withr::with_tempdir({

    quiet <- capture.output({
      vt_create_package("example.package", open=FALSE)
    })

    setwd("example.package")

    vt_use_req("requirement_001.md",username = "example",title = "Req 001", open = FALSE)
    writeLines(c("#' @title Req 001",
                 "#' @editor example",
                 "#' @editDate 2021-04-26",
                 "#' @riskAssessment",
                 "#' ##req:first_req.1: 4 Low probablity, Low impact",
                 "#' ##req:first_req.2: 10 Low probablity, High impact",
                 "",
                 "+ ##req:first_req.1 Say Hello",
                 "+ ##req:first_req.2 Say Name",
                 ""),
               con = vt_path("requirements","requirement_001.md")
    )

    risk_test_referencer <- vt_dynamic_referencer$new()

    risk_table <- vt_scrape_risk_assessment(reference = risk_test_referencer)

    expect_equal(
      risk_table,
      data.frame(
        Title = c("Req 001"),
        Requirement = c("1.1", "1.2"),
        Assessment = c("4 Low probablity, Low impact","10 Low probablity, High impact"),
        stringsAsFactors = FALSE
      )
    )

  })
})

