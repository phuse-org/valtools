#' Validate a package
#'
#' `vt_validate_source` runs the validation on the current source, temporarily
#' installing the source to properly evaluate the report. `vt_validate_build()`
#' runs the same step, then compiles a bundle that includes th the validation
#' report and any other contents that are required for validation. Finally,
#' `vt_validate_installed_package` run rerun the validation report for packages
#' that were built and then installed using the`vt_validate_build()`.
#'
#' @param pkg Top-level directory of the package to validate
#' @param package installed package name
#' @param ... Additional argument passed to `devtools::build()`
#' @param open should the validation report be opened after it is built?
#' @param install_verbose should the installation be verbose?
#'
#'
#' @return path to either the validation report or the bundled package
#'
#' @importFrom devtools install
#' @importFrom withr with_temp_libpaths
#' @importFrom callr r
#'
#' @export
#'
#' @rdname validate
#'
vt_validate_source <- function(pkg = ".", open = interactive()){

  tryCatch({

    with_temp_libpaths(
      validation_report_path <- r( function(pkg, working_dir, output_dir, output_file){

        # nocov start

        ## install R package to temporary libpath
        devtools::install(
          pkg = pkg,
          quick = TRUE,
          build = FALSE,
          upgrade = "never",
          force = TRUE,
        )

        ## render validation report
        valtools::vt_render_validation_report(
          report_path = file.path(pkg,working_dir, "validation.Rmd"),
          output_dir = file.path(pkg,output_dir),
          output_file = output_file,
          render_time = "build",
          package = ""
        )

        # nocov end

      },args = list(
        pkg = pkg,
        working_dir = get_config_working_dir(pkg),
        output_dir = file.path(get_config_output_dir(pkg = "."),"validation"),
        output_file = evaluate_filename(pkg = pkg)
      )
      ))

    inform("Validation Report Generated", class = "vt.validation")

  }, error = function(e) {
    abort(paste0(c("Error during validation of package. Error: ",
                   e, sep = "\n")),
          class = "vt.validationFail")
  })

  if(open){
    file.show(validation_report_path) # nocov
  }

  return(validation_report_path)
}

#' Validate a package source an Build package
#'
#' @importFrom devtools build
#' @importFrom callr r
#'
#' @export
#'
#' @rdname validate
#'
vt_validate_build <- function(pkg = ".",...) {

  validation_directory <- file.path(get_config_working_dir(pkg = "."), "validation")
  validation_output_directory <- file.path(get_config_output_dir(pkg = "."),"validation")

  vt_validate_source(pkg = ".", open = FALSE)

  tryCatch({

    if(!dir.exists(file.path(pkg, validation_output_directory))){
      dir.create(file.path(pkg, validation_output_directory),recursive = TRUE)
    }

    ## copy validation contents to validation output dir
    directory_copy(
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
    # roxygen_copy(
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

  return(build_path)

}



#' Validate and install package and Build package
#'
#' @importFrom rlang inform
#' @importFrom utils install.packages
#' @export
#'
#' @rdname validate
#'
vt_validate_install <- function(pkg = ".", ..., install_verbose = TRUE){
  bundle <- vt_validate_build(pkg = ".", ...)
  on.exit({unlink(bundle)})
  install.packages(bundle, type = "source", repos = NULL, verbose = install_verbose, quiet = !install_verbose)
  inform("validated package installed")
  return(TRUE)
}

#' Validate an installed package
#'
#' @param output_directory Location of directory to output validation report
#'
#' @importFrom callr r
#'
#' @export
#'
#' @rdname validate
#'
vt_validate_installed_package <- function(package, output_directory = ".", open = interactive()) {

  validation_directory <- system.file("validation", package = package)

  if(validation_directory == ""){
    abort(paste0("Package ", package, " was not built with `vt_validated_build()"),
          class = "vt.packageMissingValidation")
  }

  tryCatch({

    validation_report_path <- r( function(package, validation_directory, output_directory){
      # nocov start
        ## render validation report
        valtools::vt_render_validation_report(
          report_path = file.path(validation_directory, "validation.Rmd"),
          output_dir = output_directory,
          render_time = "installed",
          package = package
        )
      # nocov end
      },args = list(
        package = package,
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
    file.show(validation_report_path) # nocov
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
directory_copy <- function(from, to, overwrite = FALSE, recursive = TRUE){

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
