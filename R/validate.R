#' Validate a package source
#'
#' @param pkg Top-level directory of the package to validate
#' @param ... Additional argument passed to `devtools::build()`
#'
#' @importFrom devtools build install
#' @importFrom withr with_temp_libpaths
#' @importFrom callr r
#'
#' @export
vt_validated_build <- function(pkg = ".",...) {

  validation_directory <- getOption("vt.validation_directory",default = "vignettes/validation")
  validation_output_directory <- getOption("vt.validation_output_directory",default = "inst/validation")

  tryCatch({

    with_temp_libpaths(
      r( function(pkg, validation_directory){

        ## install R package to temporary libpath
        install(
          pkg = pkg,
          quick = TRUE,
          build = FALSE,
          upgrade = "never",
          force = TRUE,
          )

        ## render validation report
        render_validation_report(
          report_path = file.path(pkg, "vignettes", "validation.Rmd"),
          output_dir = file.path(pkg, validation_directory),
          render_time = "build",
          package = ""
        )

      },args = list(pkg = pkg, validation_directory = validation_directory)))

    inform("Validation Report Generated", class = "vt.validation")

  }, error = function(e) {
    abort(paste0(c("Error during validation of package. Error: ",
                   e, sep = "\n")),
          class = "vt.validationFail")
  })

  tryCatch({

    if(!dir.exists(file.path(pkg, validation_output_directory))){
      dir.create(file.path(pkg, validation_output_directory),recursive = TRUE)
    }

    ## copy validation contents to validation output dir
    vt_directory_copy(
      from = file.path(pkg, validation_directory),
      to = file.path(pkg, validation_output_directory),
      recursive = TRUE,
      overwrite = TRUE)

    ## copy validation Rmd
    file.copy(
      from = file.path(pkg, "vignettes", "validation.Rmd"),
      to = file.path(pkg, validation_output_directory),
      overwrite = TRUE
    )


    ## TODO: copy code documentation to validation output dir
    # vt_roxygen_copy(
    #   from = file.path(pkg, "R"),
    #   to = file.path(pkg, validation_output_directory),
    #   recursive = TRUE,
    #   overwrite = TRUE)

    ## build package
    build_path <- build(pkg, ...)
    inform("Validated package built", class = "vt.build")

  },
  error = function(e) {
    abort(paste0(c("Error in validated build", e), sep = .Platform$file.sep),
          class = "vt.buildFail")
  })

}

#' Validate an installed package
#'
#' Rerun the validation report that is contained within a package that was built
#' by `valtools::vt_validated_built()`. Saves the new report in the current directory.
#'
#' @param pkg Name of installed package
#' @param output_directory Location of directory to output validation report
#' @param open open the validation report after the rendering is completed
#'
#' @returns Returns the path of the regenerated validation report
#'
#' @importFrom callr r
#'
#' @export
vt_validate_installed_package <- function(pkg, output_directory = ".", open = interactive()) {

  validation_directory <- system.file("validation", package = pkg)

  output_directory <- file.path(getwd(),output_directory)

  if(validation_directory == ""){
    abort(paste0(c("Package ", pkg, " was not built with `vt_validated_build()")),
          class = "vt.packageMissingValidation")
  }

  tryCatch({


    validation_report_path <- r( function(pkg, validation_directory, output_directory){

        ## render validation report
        render_validation_report(
          report_path = file.path(validation_directory, "validation.Rmd"),
          output_dir = output_directory,
          render_time = "installed",
          package = pkg
        )

      },args = list(
        pkg = pkg,
        validation_directory = validation_directory,
        output_directory = output_directory
      ))

    inform("Validation Report Generated", class = "vt.installed_validation")

  }, error = function(e) {
    abort(paste0(c("Error during validation of package. Error: ",
                   e, sep = "\n")),
          class = "vt.vt.installed_validationFail")
  })

  if(open){
    file.show(validation_report_path)
  }

  invisible(validation_report_path)

}


#' helper function to copy folder contents from one folder to another
#' and maintain directory structure
#'
#' @noRd
#'
#' @param from directory from which to copy files from
#' @param to directory to copy files to
#' @param overwrite should contents in to be overwritten if a file with the same name exists
#' @param recursive should the function copy all the subdirectories too
#'
#' @returns boolean as to whether the copying completed successfully
#'
vt_directory_copy <- function(from, to, overwrite = FALSE, recursive = TRUE){

  ## list all files to copy
  list_files_from <- list.files(
    path = from,
    all.files = TRUE,
    recursive = recursive
  )

  dir_structure <- setdiff(unique(dirname(list_files_from)),".")

  for(new_dir in file.path(to,dir_structure)[order(nchar(dir_structure))]){
    if(!dir.exists(new_dir)){
      dir.create(new_dir,recursive = TRUE)
    }
  }

  all(file.copy(
    from = file.path(from,list_files_from),
    to = file.path(to,list_files_from),
    overwrite = overwrite
  ))
}


