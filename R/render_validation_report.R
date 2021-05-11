#' provide a nice wrapper to set states around render
#'
#' This package is not intended for use by the end users. This is to be used within
#' valtools packages.
#'
#' @param report_path path to the validation report rmarkdown
#' @param output_dir path to directory to output rendered report. defaults to same folder
#' @param output_file expected output filename sans extension
#' @param ... arguments passed to \link[rmarkdown]{render}
#' @param render_time type of rendering of validation to run, "build" or "installed".
#' @param package the report  type of rendering of validation to run, "build" or "installed".
#'
#' @importFrom withr with_envvar
#' @importFrom rmarkdown render
#'
#' @export
vt_render_validation_report <- function(report_path, output_dir = dirname(report_path), output_file = NULL, ..., render_time = c("build","installed"), package = ""){

  render_time <- match.arg(render_time)

  tryCatch({
    with_envvar(
      new = list("vt_validation_state" = render_time,
                 "vt_validation_package" = package),

      render(input = report_path,
             output_dir = output_dir,
             output_file = output_file,
             ...)
    )
  },
  error = function(e) {
    abort(paste0("Error in rendering validation report ", e),
          class = "vt.render_report_fail")
  })
}




#' evaluate validation report output name
#'
#' @param pkg base path to use as reference
#' @param package package name
#' @param version package version
#'
#' @importFrom desc desc_get_field
#' @importFrom glue glue
#'
#' @noRd
#'
evaluate_filename <- function(pkg = ".", package, version){

  filename_format <- get_config_report_naming_format()



  if(is.null(filename_format)){
    filename_format <- "Validation_Report_{package}_v{version}_{date}"
  }

  if(missing(package)){
    package <- make.names(get_config_package())
  }

  if(missing(version) && grepl("{version}", filename_format,fixed = TRUE)){
    if(is_package(pkg = pkg)){
      version <- desc_get_field("Version",file = pkg)
    }else{
      version <- packageVersion(package)
    }
  }

  date <- format(Sys.Date(),"%Y%m%d")

  glue(filename_format)

}
