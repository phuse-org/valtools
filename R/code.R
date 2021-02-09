# The rest of this documentation is in case.R
#' @param code_name The name/path of the test code. Codes can be named with your
#'   file system separator and will be organized as a directory structure. Codes
#'   are located at `./inst/validation/codes/{code_name}`.
#'
#' @export
#'
#' @rdname new_item
vt_use_code <- function(code_name, pkg = ".") {

  code_path <- create_item(pkg, "codes", code_name)

  writeLines("here is your code file", con = code_path)
}
