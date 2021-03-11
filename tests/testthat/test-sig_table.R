test_that("vt_generate_sig_table generates the expected dataframe", {

  withr::with_tempdir({

    vt_create_package(".", open = FALSE)

    vt_add_user_to_config(username = "em", name = "Eli Miller", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "mv", name = "Marie", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "eh", name = "Ellis H", role = "dev", title = "Developer")

    expect_equal(
      vt_generate_sig_table(),
      data.frame(
        role = c("dev", "dev", "dev"),
        name_and_title = c("Eli Miller, Developer",
                           "Marie, Developer",
                           "Ellis H, Developer"),
        signature = NA,
        date = NA
      )
    )

  })
})

test_that("`vt_render_sig_table` returns the expected RMD text", {
  withr::with_tempdir({

    vt_create_package(".", open = FALSE)

    vt_add_user_to_config(username = "em", name = "Eli Miller", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "mv", name = "Marie", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "eh", name = "Ellis H", role = "dev", title = "Developer")

    vt_render_sig_table(
      template = "sig_table.Rmd",
      output = "rendered_table"
    )

    expect_equal(readLines("rendered_table.Rmd"),
                 c("# Signatures",
                   "",
                   "```{r, sig_table, echo = FALSE}",
                   "vt_generate_sig_table() %>%",
                   "  vt_kable_sig_table()",
                   "```"
                 ))

  })

})

test_that("`vt_kable_sig_table` returns expected value", {

  withr::with_tempdir({

    vt_create_package(".", open = FALSE)

    vt_add_user_to_config(username = "em", name = "Eli Miller", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "mv", name = "Marie", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "eh", name = "Ellis H", role = "dev", title = "Developer")

    kable_output <- vt_generate_sig_table() %>%
      vt_kable_sig_table()

    expect_s3_class(kable_output, c("kableExtra", "knitr_kable"))

  })
})

test_that("`vt_kable_sig_table` errors when not given expected columns", {

  expect_error({
    vt_kable_sig_table(
      data.frame(
        role = "aRole",
        name = "Eli"
        )
    )
  }, c("people table must have variables: role, Name and Title, Signature, and Date "))

})
