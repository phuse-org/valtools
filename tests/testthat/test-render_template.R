test_that("explicit template testing - success", {
  withr::with_tempdir({

    template_success <- render_template(template = "requirements",
                    output = "req_template_output.md",
                    data = list(
                      title = "TEST TITLE",
                      username = "USERNAME",
                      editDate = "DATE"
                    ))

    expect_equal(
      readLines("req_template_output.md"),
      c("#' @title TEST TITLE",
        "#' @editor USERNAME",
        "#' @editDate DATE",
        "#' @riskAssessment",
        "#' REQUIREMENT: ASSESSMENT",
        "",
        "+ Start documenting requirements here!",
        "")
    )

  })

})

test_that("explicit template testing - warning&error - bad path", {

  withr::with_tempdir({
    expect_warning(
    expect_error(
      render_template(
        template = "requirements",
        output = "new/file/path/req_template_output.md",
        data = list(title = "TEST TITLE",
                    username = "USERNAME",
                    date = "DATE")
      ),
      "Error during creation of template `requirements`. Error:")
    )
  })
})

test_that("explicit template testing - failure - template does not exist", {

  withr::with_tempdir({

    expect_error(
      render_template(
        template = "TEMPLATE DNE",
        output = "req_template_output.md"
      ),
      "Template `TEMPLATE DNE` does not exist[.]"
    )
  })

})
