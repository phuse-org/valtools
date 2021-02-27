#' @importFrom usethis edit_file
NULL
.onLoad <- function(libname, pkgname) {
  options("vt.validation_directory" = "vignettes/validation")
  options("vt.validation_output_directory" = "inst/validation")
  options("vt.validate_install_directory" = "validation")
}
