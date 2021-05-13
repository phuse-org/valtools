#' Create a validation structure
#'
#' Creates a structure for validation artifacts. Validation items are stored in
#' `inst/validation`
#'
#' @param pkg Top level directory of a package
#' @param working_dir validation working directory of the project. Defaults to
#'
#' @importFrom rlang inform
#'
#' @rdname val_init
#'
#' @export
#' @importFrom usethis use_git_ignore
vt_use_validation <- function( pkg = ".", working_dir, ...) {

  if(missing(working_dir)){
    if(is_package(pkg = pkg)){
      working_dir <- "vignettes"
    }else{
      working_dir <- "."
    }
  }

  validation_directory <-
    file.path(working_dir, "validation")

  tryCatch({

    dir.create(file.path(pkg, validation_directory),recursive = TRUE)

    use_git_ignore2("!*", dir =  file.path(pkg, validation_directory))

    inform(paste("Created", file.path(validation_directory)," in package structure"),
           class = "vt.init")

    set_dir_ref(pkg = pkg)

    vt_use_config(pkg = pkg, working_dir = working_dir, ...)


  }, error = function(e) {
    abort(paste0("Failed to create validation structure. Error: ",
                 e, sep = "\n"),
          class = "vt.initFail")
  })


}

#' @param ... Additional argument passed to `vt_use_config()`
#' @inheritParams usethis::create_package
#'
#' @importFrom usethis create_package
#' @importFrom rlang inform abort
#'
#' @rdname val_init
#'
#' @export
#' @importFrom usethis create_package
vt_create_package <- function(pkg = ".", ..., fields = list(), rstudio = rstudioapi::isAvailable(),
                              roxygen = TRUE, check_name = TRUE, open = rlang::is_interactive()) {

  tryCatch({
    create_package(
      path = pkg,
      fields = fields,
      rstudio = rstudio,
      roxygen = roxygen,
      check_name = check_name,
      open = open
    )

    inform("Created package structure",
           class = "vt.initPackage")

  }, error = function(e) {
    abort(paste0("Failed to create package. Error: ",
                 e, sep = "\n"),
          class = "vt.initPackageFail")

  })


  ## set up validation structure in package & create basic config for validation
  vt_use_validation(pkg = pkg, ...)

}

#' Internal wrapper function to call vt_create_package().
#' To be used by RStudio project wizard, preventing opening the project twice.
#'
#' @param path Project directory, collected through project wizard
#'
#' @noRd
vt_create_package_wizard <- function(path, ...){
  vt_create_package(pkg= path, open= FALSE, ...)   # nocov
}



