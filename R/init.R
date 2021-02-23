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

  validation_directory <- getOption("vt.validation_directory",default = "vignettes/validation")

  tryCatch({
    dir.create(file.path(pkg, validation_directory),recursive = TRUE)
    use_git_ignore("!*", directory =  getOption("vt.validation_directory"))

    inform(paste("Created",validation_directory," in package structure"),
           class = "vt.init")

  }, error = function(e) {
    abort(paste0("Failed to create validation structure. Error: ",
                 e, sep = "\n"),
          class = "vt.initFail")
  })
}

#' @param ... Additional argument passed to `usethis::create_package()`
#'
#' @importFrom usethis create_package
#' @importFrom rlang inform abort
#'
#' @rdname val_init
#'
#' @export
#' @importFrom usethis create_package
vt_create_package <- function(pkg = ".", ...) {

  tryCatch({
    create_package(path = pkg, ...)

    inform("Created package structure",
           class = "vt.initPackage")

  }, error = function(e) {
    abort(paste0("Failed to create package. Error: ",
                 e, sep = "\n"),
          class = "vt.initPackageFail")

  })

  vt_use_validation(pkg)
}
