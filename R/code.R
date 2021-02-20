# The rest of this documentation is in case.R
#' @param name The name/path of the test code. Codes can be named with your
#'   file system separator and will be organized as a directory structure. Codes
#'   are located at `./inst/validation/test_code/{name}`.
#'
#' @export
#'
#' @rdname new_item
vt_use_test_code <- function(name, username = vt_username(), open = interactive(), pkg = ".") {

  name <- vt_set_ext(name, ext = "R")

  is_valid_name(name)

  # Create file to write in
  code_name <- create_item(pkg, "test_code", name)

  ## if the file didnt exist before, populate with contents
  if (file.size(code_name) == 0){

    # Create the content to write
    content <- paste0(c(
      "",
      "# Test setup",
      "",
      "",
      paste0("#' @editor ", username),
      paste0("#' @editDate ", as.character(Sys.Date())),
      "test_that(\"TESTNUMBER\", {",
      "  #TEST CODE HERE",
      "",
      "})",
      collapse = "",
      sep = "\n"
    ))

    writeLines(content, con = code_name)
  }

  if(open){
    usethis::edit_file(code_name)
  }

  invisible(code_name)

}
