#' Create a validation specification, test case, or test code file
#'
#' @param case_name The name/path of the test case. Cases can be named with your
#'   file system separator and will be organized as a directory structure. Cases
#'   are located at `./inst/validation/cases/{case_name}`.
#' @param pkg Top-level of package
#'
#' @export
#'
#' @rdname new_item
#'
#' @examples
#' package_dir <- tempdir()
#'
#' vt_use_validation(package_dir)
#'
#' # Create spec at the cases top level `inst/validation/cases/spec1`
#' vt_use_case("spec1", package_dir)
#' # Create spec at `inst/validation/cases/regTests/Update2/spec2`
#' vt_use_case("regTests/Update2/spec2", package_dir)
vt_use_case <- function(case_name, case_content, pkg = ".") {

  case_path <- create_item(pkg, "cases", case_name)

  # Create the content to write
  content <- paste0(c(
    "#' @section Last Updated By:",
    paste0("#' ", Sys.getenv("USER")),
    "#' @section Last Update Date",
    paste0("#' ", as.character(Sys.Date())),
    paste0("+ ", case_content, collapse = ""),
    collapse = "",
    sep = "\n"
  ))

  writeLines(content, con = case_path)
}
