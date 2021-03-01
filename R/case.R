#' Create a validation requirement, test case, or test code file
#'
#' @param name The name/path of the validation item. These can be named with your
#'   file system separator and will be organized as a directory structure. Items
#'   are located at `./inst/validation/<ItemType>/{name}`.
#' @param username The username to insert into the validation item as the author.
#' @param open Should the newly made file be opened for editing.
#' @param pkg Top-level of package
#'
#' @return Path to the newly created validation item file, invisibly.
#'
#' @export
#'
#' @rdname new_item
#'
#' @importFrom usethis edit_file
#'
#' @examples
#' withr::with_tempdir({
#'
#' vt_create_package(package_dir)
#' # Create req at the cases top level `inst/validation/cases/case1`
#' vt_use_test_case("case1", pkg = package_dir, open = FALSE)
#' # Create req at `inst/validation/cases/regTests/Update2/case2`
#' vt_use_test_case("regTests/Update2/case2", pkg = package_dir, open = FALSE)
#'
#' })
vt_use_test_case <- function(name, username = vt_username(), open = interactive(), pkg = ".") {

  name <- vt_set_ext(name, ext = "md")

  is_valid_name(name)

  case_name <- create_item(pkg, "test_cases", name)

  ## if the file didnt exist before, populate with contents
  if (file.size(case_name) == 0){

    # Create the content to write
    content <- paste0(c(
      paste0("#' @editor ", username),
      paste0("#' @editDate ", as.character(Sys.Date())),
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
    edit_file(case_name)
  }

  invisible(case_name)

}
