test_that("vt_generate_sig_table generates the expected dataframe", {

  withr::with_tempdir({

    capture_output <- capture.output({
      vt_create_package(".", open = FALSE)
    })

    vt_add_user_to_config(username = "em", name = "Eli Miller", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "mv", name = "Marie", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "eh", name = "Ellis H", role = "dev", title = "Developer")

    expect_equal(
      vt_scrape_sig_table(),
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



test_that("render returns the expected RMD text", {
  withr::with_tempdir({

    capture_output <- capture.output({
      vt_create_package(pkg = ".", open = FALSE)
    })

    vt_add_user_to_config(username = "em", name = "Eli Miller", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "mv", name = "Marie", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "eh", name = "Ellis H", role = "dev", title = "Developer")

    people <- vt_scrape_sig_table()
    sig_kable <- vt_kable_sig_table(people)
    # using internal function that will be called when validation report is built
    valtools:::render_template(
      template = "sig_table.Rmd",
      output = file.path(valtools:::get_config_working_dir(), "sig_table.Rmd"),
      data = list(pkg = "here()", format = "latex")
    )

    sig_table_rmd <- readLines(file.path("vignettes", "sig_table.Rmd"))

    expect_equal(sig_table_rmd,
                 c( "# Signatures",
                    "",
                    "```{r, sig_table, echo = FALSE}",
                    "vt_scrape_sig_table() %>%",
                    "  vt_kable_sig_table(format = \"latex\")",
                    "```"
                 ))

    writeLines(
      c("---",
        "output:",
        "  pdf_document:",
        "    fig_crop: false",
        "header-includes:",
        "  - \\usepackage{array}",
        "---",
        "\n\n",
        "```{r, setup, echo = FALSE, warning = FALSE, message = FALSE}",
        "suppressWarnings({",
        "library(magrittr)",
        "library(here)",
        "library(usethis)",
        "})",
        "```",
        "",
        "\n\n",
        sig_table_rmd
        ),
      con = file.path("vignettes", "validation", "stand_alone_sig_table.Rmd")
    )

    suppressWarnings({
    capture_output <- capture.output(rmarkdown::render(file.path("vignettes", "validation",
                                "stand_alone_sig_table.Rmd")))
    })

    pdf_report_name <- file.path("vignettes", "validation",
                                 "stand_alone_sig_table.pdf")
    test_output_rendered <-
      trimws(strsplit(split = "\r\n", gsub("((\r)|(\n))+","\r\n",
                                           pdftools::pdf_text(pdf_report_name)))[[1]])
    expect_equal(test_output_rendered[1:5],
                 c("Signatures",
                   "Role      Name and Title        Signature Date",
                   "dev       Eli Miller, Developer NA        NA",
                   "dev       Marie, Developer      NA        NA",
                   "dev       Ellis H, Developer    NA        NA"
                   ))

  })

})

test_that("render returns the expected html", {

    withr::with_tempdir({

      capture_output <- capture.output({
        vt_create_package(pkg = ".", open = FALSE)
      })

      vt_add_user_to_config(username = "em", name = "Eli Miller", role = "dev", title = "Developer")
      vt_add_user_to_config(username = "mv", name = "Marie", role = "dev", title = "Developer")
      vt_add_user_to_config(username = "eh", name = "Ellis H", role = "dev", title = "Developer")

      # using internal function that will be called when validation report is built
      valtools:::render_template(
        template = "sig_table.Rmd",
        output = file.path(valtools:::get_config_working_dir(), "sig_table.Rmd"),
        data = list(pkg = "here()", format = "html")
      )

      sig_table_rmd <- readLines(file.path("vignettes", "sig_table.Rmd"))

      expect_equal(sig_table_rmd,
                   c( "# Signatures",
                      "",
                      "```{r, sig_table, echo = FALSE}",
                      "vt_scrape_sig_table() %>%",
                      "  vt_kable_sig_table(format = \"html\")",
                      "```"
                   ))

      writeLines(
        c("---",
          "title: A report",
          "output:",
          "  html_document:",
          "    fig_crop: false",
          "---",
          "\n\n",
          "```{r, setup, echo = FALSE, warning = FALSE, message = FALSE}",
          "suppressWarnings({",
          # "library(valtools)",
          "library(magrittr)",
          "library(here)",
          "library(usethis)",
          "proj_set(force = TRUE)",
          "})",
          "```",
          "\n\n",
          sig_table_rmd),
        con = file.path("vignettes", "validation", "stand_alone_sig_table.Rmd")
      )

      capture_output <- capture.output({
        rmarkdown::render(file.path("vignettes", "validation",
                                  "stand_alone_sig_table.Rmd"))
      })
      html_report_name <- file.path("vignettes", "validation",
                                   "stand_alone_sig_table.html")

      this_test <- xml2::read_html(html_report_name)
      test_output_rendered <- rvest::html_table(rvest::html_nodes(this_test, "table")[1], fill = TRUE)[[1]]
      expect_equal(as.data.frame(test_output_rendered),
                   data.frame(
                     Role = rep("dev", 3),
                     `Name and Title` = c( "Eli Miller, Developer", "Marie, Developer", "Ellis H, Developer"),
                     Signature = NA,
                     Date = NA,
                     check.names = FALSE,
                     stringsAsFactors = FALSE
                   ))



  })
})

test_that("`vt_scrape_sig_table` throws errors", {
  people <- list(username = "auser", name = "A user", title = "title", role = "role")
  expect_error(vt_scrape_sig_table(people),
               "Usernames must be list of vt_users. Run `list(valtools::vt_user(...))`.",
               fixed = TRUE)
  people <- list(vt_user(username = "auser",
                         name = "A user",
                         title = "A title",
                         role = "A role"),
                 vt_user(username = "buser",
                         name = "Another user",
                         title = "Their Title",
                         role = "Their Role"))

  sig_table_raw <- vt_scrape_sig_table(people)
  expect_equal(sig_table_raw,
               data.frame(
                 role = c("A role", "Their Role"),
                 name_and_title = c("A user, A title", "Another user, Their Title"),
                 signature = NA,
                 date = NA
               ))
})

test_that("`vt_kable_sig_table` returns expected value", {

  withr::with_tempdir({
    capture_output <- capture.output({
      vt_create_package(".", open = FALSE)
    })

    vt_add_user_to_config(username = "em", name = "Eli Miller", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "mv", name = "Marie", role = "dev", title = "Developer")
    vt_add_user_to_config(username = "eh", name = "Ellis H", role = "dev", title = "Developer")

    sig_table_raw <- vt_scrape_sig_table()
    kable_output_latex <- vt_kable_sig_table(sig_table_raw, format = "latex")

    expect_s3_class(kable_output_latex, c("kableExtra", "knitr_kable"))
    kable_output_html <- vt_kable_sig_table(sig_table_raw, format = "html")
    expect_s3_class(kable_output_html, c("kableExtra", "knitr_kable"))

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
