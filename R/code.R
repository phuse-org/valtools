# The rest of this documentation is in case.R
#' @export
#' @rdname new_item
vt_use_test_code <- function(name, username = vt_username(), open = interactive()) {

  name <- vt_set_ext(name, ext = "R")

  is_valid_name(name)

  # Create file to write in
  code_name <- create_item("test_code", name)

  ## if the file didnt exist before, populate with contents
  if (file.size(code_name) == 0){

    # Create the content to write
    render_template("test_code", output = code_name,
                    data = list(
                      username = username,
                      editDate = as.character(Sys.Date())
                    ))
  }

  if(open){
    edit_file(code_name)
  }

  invisible(code_name)

}
