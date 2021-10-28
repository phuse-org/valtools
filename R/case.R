#' Create a validation requirement, test case, or test code file
#'
#' @param name The name/path of the validation item. These can be named with your
#'   file system separator and will be organized as a directory structure. Items
#'   are located at `./inst/validation/<ItemType>/{name}`.
#' @param username The username to insert into the validation item as the author.
#' @param open Should the newly made file be opened for editing.
#' @param add_before,add_after If either parameters is supplied, the location to
#'   add the validation item to the validation configuration. If no parameter is
#'   passed the item is added at the end.
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
#' vt_create_package("example.package")
#' setwd("example.package")
#' vt_add_user_to_config(
#'   username = whoami::username(),
#'   name = "Sample Name",
#'   title = "Sample",
#'   role = "example"
#'  )
#' # Create req at the cases top level `inst/validation/cases/case1`
#' vt_use_test_case("case1", open = FALSE)
#'
#' # Create req at `inst/validation/cases/regTests/Update2/case2`
#' vt_use_test_case("regTests/Update2/case2", open = FALSE, add_before = "case1.md")
#'
#' # Create a test case using tidy select
#' vt_use_test_case("case1a", open = FALSE, add_after = tidyselect::starts_with("case1"))
#'
#' })
vt_use_test_case <- function(name, username = vt_username(), title = NULL, open = interactive(),
                             add_before = NULL, add_after = NULL) {

  name <- vt_set_ext(name, ext = c("md","rmd"))

  is_valid_name(name)

  case_name <- create_item("test_cases", name)

  ## if the file didnt exist before, populate with contents
  if (file.size(case_name) == 0){

    if(is.null(title)){
      title <- basename(file_path_sans_ext(case_name))
    }

    # Create the content to write
    render_template("test_cases", output = case_name,
                     data = list(
                       title = title,
                       username = username,
                       editDate = as.character(Sys.Date())
                     ))
    
    # Add file to validation configuration
    vt_add_file_to_config(
      filename = name,
      after = {{add_after}},
      before = {{add_before}}
      )
  }

  if(open){
    edit_file(case_name) # nocov
  }

  invisible(case_name)

}
