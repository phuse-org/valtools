#' Validate a package source
#'
#' @param pkg Top-level directory of the package to validate
#' @param ... Additional argument passed to `devtools::build()`
#'
#' @importFrom devtools build
#' @importFrom rmarkdown render
#'
#' @export
vt_validated_build <- function(pkg = ".", ...) {

  tryCatch({
    file.copy(from = paste0(c(pkg, "inst", "validation"),sep = .Platform$file.sep),
              to = paste0(c(pkg, "vignettes"), sep = .Platform$file.sep), recursive = TRUE)


    render(paste0(c(pkg, "inst", "validation", "validation.Rmd"), sep = .Platform$file.sep),
           output_dir = paste0(c(pkg, "inst"), sep = .Platform$file.sep))

    inform("Validation Complete", class = "vt.validation")

  }, error = function(e) {
    abort(paste0(c("Error during validation of package. Error: ",
                   e, sep = "\n")),
          class = "vt.validationFail")
  })

  tryCatch({
    build(pkg, ...)
    inform("Validated package built", class = "vt.build")

  },
  error = function(e) {
    abort(paste0(c("Error in validated build", e), sep = .Platform$file.sep),
          class = "vt.buildFail")
  })
}

#' Validate an installed package
#'
#' @param pkg Name of installed package
#' @param output_directory Location of directory to output validation report
#'
#' @export
vt_validate_installed_package <- function(pkg, output_directory = ".") {

  val_dir <- system.file("validation", package = pkg)

  if(val_dir == ""){
    abort(paste0(c("Package ", pkg, " was not built with `vt_validated_build()")),
          class = "vt.packageMissingValidation")
  }

  render(paste0(c(val_dir, "validation.Rmd"), sep = .Platform$file.sep),
         output_dir  = output_directory)
}
