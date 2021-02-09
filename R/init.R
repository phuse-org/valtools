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
vt_use_validation <- function(pkg = ".") {

  tryCatch({
    dir.create(paste0(c(pkg, "inst", "validation"),
                      sep = .Platform$file.sep,
                      collapse = ""),
               recursive = TRUE)

    inform("Created inst/validation in package structure",
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
vt_create_package <- function(pkg = ".", ...) {

  tryCatch({
    usethis::create_package(path = pkg, ...)

    inform("Created package structure",
           class = "vt.initPackage")

  }, error = function(e) {
    abort(paste0("Failed to create package. Error: ",
                 e, sep = "\n"),
          class = "vt.initPackageFail")

  })

  vt_use_validation(pkg)
}
