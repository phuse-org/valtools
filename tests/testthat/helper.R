#' Create vt test package silently and non-interactively
make_vt_test_package <- function(){
  withr::with_options(
    list(
      usethis.quiet = TRUE
    ),
    {
      vt_create_package(rstudio = FALSE, open = FALSE)
    })
}

#' Private wrapper for usethis::create_package() for use in unit tests
#'
#' usethis <= 3.0.0 creates an invalid ORCID placeholder in package DESCRIPTION
#' by default. In R >= 4.5, this throws a warning (actually, 122 of them in the
#' valtools unit tests!). The `fields` argument in this wrapper removes that
#' invalid ORCID so that this doesn't happen. This has been fixed upstream, so
#' once usethis > 3.0.0 is released, the custom `fields` argument below should
#' be able to be removed. At that time, if desired, this wrapper function could
#' also just be removed and replaced with usethis::create_package() in the unit
#' tests. See also another instance in vt_create_package().
#'
#' @param ... Passed to usethis::create_package()
#'
#' @return See ?usethis::create_package()
safe_usethis_create_package <- function(...){
  usethis::create_package(
    ...,
    open = FALSE,
    fields = list(
      `Authors@R` = paste0(
        "person(\"First\", \"Last\", email = \"first.last",
        "@example.com\", role = c(\"aut\", \"cre\"))"
      )
  )
  )
}