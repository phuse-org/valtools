#' Create a validation specification, test case, or test code file
#'
#' @param name The name/path of the test case. Cases can be named with your
#'   file system separator and will be organized as a directory structure. Cases
#'   are located at `./inst/validation/test_case/{name}`.
#' @param username the username to insert into the specification as the author.
#' @param open should the newly make specification open for editing.
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
#' vt_use_test_case("spec1", package_dir)
#' # Create spec at `inst/validation/cases/regTests/Update2/spec2`
#' vt_use_test_case("regTests/Update2/spec2", package_dir)
vt_use_test_case <- function(name, username = vt_username(), open = interactive(), pkg = ".") {

  name <- vt_set_ext(name, ext = "md")

  case_name <- create_item(pkg, "test_case", name)

  ## if the file didnt exist before, populate with contents
  if (file.size(case_name) == 0){

    # Create the content to write
    content <- paste0(c(
      "#' @section Last Updated By:",
      paste0("#' ", username),
      "#' @section Last Update Date:",
      paste0("#' ", as.character(Sys.Date())),
      "",
      "+ _Test Case_",
      "    + Setup: DOCUMENT ANY SETUP THAT NEEDS TO BE DONE FOR TESTING",
      "",
      "    + Start documenting test case here!",
      collapse = "",
      sep = "\n"
    ))

    writeLines(content, con = case_name)

  }

  if(open){
    usethis::edit_file(case_name)
  }

  invisible(case_name)

}
