#' provide a nice wrapper to set states around render
#'
#' This package is not intended for use by the end users. This is to be used within
#' valtools packages.
#'
#' @noRd
#'
#' @param report_path path to the validation report rmarkdown
#' @param output_dir path to directory to output rendered report. defaults to same folder
#' @param ... arguments passed to \link[rmarkdown]{render}
#' @param render_time type of rendering of validation to run, "build" or "installed".
#' @param package the report  type of rendering of validation to run, "build" or "installed".
#'
#' @importFrom withr with_envvar

render_validation_report <- function(report_path, output_dir = dirname(report_path), ..., render_time = c("build","installed"), package = ""){

  render_time <- match.arg(render_time)

  tryCatch({
    withr::with_envvar(
      new = list("vt_validation_state" = render_time,
                 "vt_validation_package" = package),
      render(input = report_path, output_dir = output_dir, ...)
    )
  },
  error = function(e) {
    abort(paste0("Error in rendering validation report ", e),
          class = "vt.render_report_fail")
  })
}


#' Use to set dynamic file paths in a validation.
#' @param ... `[character]`\cr
#'   Path components below the validation folder, can be empty.
#'   Each argument should be a string containing one or more
#'   path components separated by a forward slash `"/"`.
#'
#' @export
#'
#' @importFrom here here
#'
#' @examples
#' vt_path()
#' \dontrun{
#' vt_path("some", "specs", "spec01.md")
#' vt_path("some/specs/spec01.md")
#' }
#'
#'
vt_path <- function(...){

  state <- Sys.getenv("vt_validation_state")
  package <- Sys.getenv("vt_validation_package")

  ## default and if building (state == "build") uses the same path
  path <- switch (state,
                  "installed" = file.path(system.file(package = package), getOption("vt.validate_install_directory", default = "validation")),
                  here(getOption("vt.validation_directory",default = "vignettes/validation"))
  )

  file.path(path, ...)

}

