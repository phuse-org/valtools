#' Validate a package
#'
#' `vt_validate_source` runs the validation on the current source, temporarily
#' installing the source to properly evaluate the report. `vt_validate_build()`
#' runs the same step, then compiles a bundle that includes th the validation
#' report and any other contents that are required for validation. Finally,
#' `vt_validate_installed_package` run rerun the validation report for packages
#' that were built and then installed using the`vt_validate_build()`.
#'
#' @param src location of the source code. Assumed to be the same location as "pkg"
#' @param package installed package name
#' @param ... Additional argument passed to `devtools::build()`
#' @param open should the validation report be opened after it is built?
#' @param install_verbose should the installation be verbose?
#' @param install_tests should the installation include installation of package-specific tests (if any)?
#' @param reload Should package be reloaded after install? defaults to TRUE
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
vt_validate_source <- function(src = ".", open = interactive()){

  root <- find_root(is_rstudio_project | is_r_package | is_vcs_root)

  tryCatch({

    with_temp_libpaths(
      validation_report_path <- r( function(root, src, working_dir, validation_rmd, output_dir, output_file){

        # nocov start

        ## install R package to temporary libpath
        devtools::install(
          pkg = src,
          quick = TRUE,
          build = FALSE,
          upgrade = "never",
          force = TRUE,
        )

        ## render validation report
        valtools::vt_render_validation_report(
          report_path = file.path(root,working_dir, validation_rmd),
          output_dir = file.path(root,output_dir),
          output_file = output_file,
          render_time = "build",
          package = ""
        )

        # nocov end

      },args = list(
        root = root,
        src = src,
        working_dir = get_config_working_dir(),
        validation_rmd = get_config_report_rmd_name(),
        output_dir = file.path(get_config_output_dir(),"validation"),
        output_file = evaluate_filename(pkg = root)
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
vt_validate_build <- function(src = ".", ...) {

  root <- find_root(is_rstudio_project | is_r_package | is_vcs_root)

  vt_validate_source(src = src, open = FALSE)

  copy_validation_content(pkg = root, src = src)

  ## build package
  build_path <- build(src, ...)
  inform("Validated package built", class = "vt.build")

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
vt_validate_install <- function(src = ".", ..., install_verbose = TRUE, install_tests = TRUE, reload = TRUE){

  pkg_src <- as.package(src)$package

  bundle <- vt_validate_build(src = src, ...)
  on.exit({unlink(bundle)})

  INSTALL_opts <- character()
  if(install_tests){
    INSTALL_opts <- c("--install-tests")
  }

  was_loaded <- pkg_src %in% loadedNamespaces()
  if ( was_loaded ) {
    try(unloadNamespace(pkg_src), silent = TRUE)
  }

  install.packages(
    bundle,
    type = "source",
    repos = NULL,
    verbose = install_verbose,
    quiet = !install_verbose,
    INSTALL_opts = INSTALL_opts
  )

  inform("validated package installed")

  if(reload & was_loaded){
    require(pkg_src, quietly = TRUE, character.only = TRUE)
    inform("validated package loaded to namespace")
  }

  return(TRUE)
}

#' Validate an installed package
#'
#' @param output_directory Location of directory to output validation report
#'
#' @importFrom callr r
#' @importFrom yaml read_yaml
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

    validation_report_rmd_name <- read_yaml(system.file("validation/validation.yml", package = package))$report_rmd_name

    validation_report_path <- r( function(package, validation_directory, validation_report_rmd_name, output_directory){
      # nocov start
        ## render validation report
        valtools::vt_render_validation_report(
          report_path = file.path(validation_directory, validation_report_rmd_name),
          output_dir = output_directory,
          render_time = "installed",
          package = package
        )
      # nocov end
      },args = list(
        package = package,
        validation_directory = validation_directory,
        validation_report_rmd_name = validation_report_rmd_name,
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



#' Execute Validation Report Independently
#'
#' @param version version of validation report to output. If missing, it tries to use the
#'   change_log.md, if that is missing then looks at the package version if the validation package
#'   is of an R package.
#'
#' @importFrom callr r
#' @importFrom rlang abort inform
#'
#' @export
#'
#' @rdname validate
#'
vt_validate_report <- function(version, open = interactive()){

  root <- find_root(has_file(".here") | is_rstudio_project | is_r_package | is_vcs_root)

  if (missing(version)) {
    if (grepl("{version}", get_config_report_naming_format(), fixed = TRUE)) {
      ## first try to use the version from the change log
      if (file.exists(vt_path("change_log.md"))) {
        version <- read_change_log(vt_path("change_log.md"))$Version[[1]]

        ## if that does not exist, check if the "package" being validated is an
        ## installed package and use that package version
      } else if (is_installed_package(get_config_package())) {
        version <- packageVersion(get_config_package())

        ## if none are available, must be a passed argument
      } else{
        abort(
          "Provide validation report version number or create a change log via `vt_use_change_log()`"
        )
      }
    } else{
      version <- NULL
    }
  }

  tryCatch({

      validation_report_path <- r( function(root, working_dir, validation_rmd, output_dir, output_file){

        # nocov start

        ## render validation report
        valtools::vt_render_validation_report(
          report_path = file.path(root, working_dir, validation_rmd),
          output_dir = file.path(root,output_dir),
          output_file = output_file,
          render_time = "build",
          package = ""
        )

        # nocov end

      },args = list(
        root = root,
        working_dir = get_config_working_dir(),
        validation_rmd = get_config_report_rmd_name(),
        output_dir = file.path(get_config_output_dir(),"validation"),
        output_file = evaluate_filename(
          pkg = get_config_report_rmd_name(),
          version = version
          )
      )
    )

    inform("Validation Report Generated", class = "vt.validation")
  }, error = function(e) {
    abort(paste0(c("Error during rendering of validation report. Error: ",
                   e, sep = "\n")),
          class = "vt.validation_external_fail")
  })

  if(open){
    file.show(validation_report_path) # nocov
  }

  return(validation_report_path)
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

copy_validation_content <- function(pkg = ".", src = pkg){

  validation_directory <- file.path(get_config_working_dir(), "validation")
  validation_output_directory <- file.path(get_config_output_dir(),"validation")

  if(validation_directory != validation_output_directory){
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
        from = file.path(pkg, "vignettes", get_config_report_rmd_name()),
        to = file.path(pkg, validation_output_directory),
        overwrite = TRUE
      )

      # copy and strip down code documentation to validation output dir
      roxygen_copy(
        from = file.path(pkg, "R"),
        to = file.path(pkg, validation_output_directory, "R/Function_Roxygen_Blocks.R"),
        overwrite = TRUE)

    },
    error = function(e) {
      abort(paste0(c("Error in moving validated content", e), sep = .Platform$file.sep),
            class = "vt.buildFail")
    })
  }

  invisible(TRUE)
}
