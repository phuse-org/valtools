# The rest of this documentation is in case.R
#' @param name The name/path of the test code. Codes can be named with your
#'   file system separator and will be organized as a directory structure. Codes
#'   are located at `./inst/validation/test_code/{name}`.
#'
#' @export
#'
#' @rdname new_item
#' @importFrom usethis edit_file
vt_use_test_code <- function(name, username = vt_username(), open = interactive(), pkg = ".") {

  code_name <- vt_set_ext(name, ext = "R")

  # Create file to write in
  code_name <- create_item(pkg, "test_code", code_name)

  ## if the file didnt exist before, populate with contents
  if (file.size(code_name) == 0){

    # Create the content to write
    content <- paste0(c(
      "",
      "# Test setup",
      "",
      "",
      "#' @section Last Updated By:",
      paste0("#' ", username),
      "#' @section Last Update Date:",
      paste0("#' ", as.character(Sys.Date())),
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
    edit_file(code_name)
  }

  invisible(code_name)

}
