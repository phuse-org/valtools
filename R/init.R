#' Create a validation structure
#'
#' Creates a structure for validation artifacts. Validation items are stored in
#' `inst/validation`
#'
#' @param pkg Top level directory of a package
#'
#' @importFrom rlang inform
#'
#' @rdname val_init
#'
#' @export
#' @importFrom usethis use_git_ignore
vt_use_validation <- function(pkg = ".") {

  validation_directory <-
    file.path(get_config_working_dir(pkg = pkg), "validation")

  tryCatch({

    dir.create(file.path(pkg, validation_directory),recursive = TRUE)

    use_git_ignore2("!*", dir =  file.path(pkg, validation_directory))

    inform(paste("Created", file.path(validation_directory)," in package structure"),
           class = "vt.init")

    set_dir_ref(pkg = pkg)

  }, error = function(e) {
    abort(paste0("Failed to create validation structure. Error: ",
                 e, sep = "\n"),
          class = "vt.initFail")
  })
}

#' @param ... Additional argument passed to `vt_use_validation_config()`
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
    create_package(path = pkg, ...)

    inform("Created package structure",
           class = "vt.initPackage")

  }, error = function(e) {
    abort(paste0("Failed to create package. Error: ",
                 e, sep = "\n"),
          class = "vt.initPackageFail")

  })

  ## create basic config for validation
  vt_use_validation_config(pkg = pkg, ...)

  ## set up validation structure in package
  vt_use_validation(pkg = pkg)

}

#' valtools clone of use_git_ignore to remove here dependency
#' @noRd
#' @importFrom usethis write_union
use_git_ignore2 <- function(ignores, dir = "."){
  write_union(file.path(dir, ".gitignore"), ignores)
}

#' valtools clone of use_build_ignore to remove here dependency
#' @noRd
#' @importFrom usethis write_union
use_build_ignore2 <- function(ignores, dir = "."){
  write_union(file.path(dir, ".Rbuildignore"), ignores)
}


