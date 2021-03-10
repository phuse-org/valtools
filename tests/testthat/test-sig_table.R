test_that("vt_generate_sig_table generates the expected dataframe", {
  expect_equal(
    vt_generate_sig_table(
      data.frame(
        name = c("Eli Miller", "Marie", "Ellis H"),
        role = c("dev", "dev", "dev"),
        title = c("Developer", "Developer", "Developer")
      )
    ),
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

test_that("`vt_render_sig_table` returns the expected RMD text", {
  withr::with_tempdir({
    vt_render_sig_table(
      data.frame(
        name = c("Eli Miller", "Marie", "Ellis H"),
        role = c("dev", "dev", "dev"),
        title = c("Developer", "Developer", "Developer")
      ),
      template = "sig_table.Rmd",
      output = "rendered_table"
    )

    expect_equal(readLines("rendered_table"),
                 c("# Signitures",
                   "",
                   "```{r, sig_table, echo = FALSE}",
                   "structure(list(name = c(&quot;Eli Miller&quot;, &quot;Marie&quot;, &quot;Ellis H&quot;), role = c(&quot;dev&quot;, &quot;dev&quot;, &quot;dev&quot;), title = c(&quot;Developer&quot;, &quot;Developer&quot;, &quot;Developer&quot;)), class = &quot;data.frame&quot;, row.names = c(NA, -3L)) %>%",
                   "  vt_generate_sig_table() %>%",
                   "  kable(col.names = c(\"Role\", \"Name and Title\", \"Signature\", \"Date\"),",
                   "        escape = FALSE, booktabs = FALSE) %>%",
                   "  kable_styling(full_width = FALSE, position = \"left\") %>%",
                   "  row_spec(0, background = rgb(184, 204, 228, maxColorValue = 255)) %>%",
                   "  column_spec(1, width = \"9em\", border_left = TRUE) %>%",
                   "  column_spec(2, width = \"11em\") %>%", "  column_spec(3, width = \"15em\") %>%",
                   "  column_spec(4, width = \"8em\", border_right = TRUE)",
                   "```"
                 ))

  })

})

test_that("`vt_generate_sig_table` errors when not given expected columns", {

  expect_error({
    vt_generate_sig_table(
      data.frame(
        role = "aRole",
        name = "Eli"
        )
    )
  }, c("people table must have variables: role, name, and title. Contains: role, name"))

})
