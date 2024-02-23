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
