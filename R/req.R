# The rest of this documentation is in case.R
#' @param title Title for the requirement defaults to be the base name passed
#'   sans file paths or extensions.
#'
#' @rdname new_item
#' @export
vt_use_req <- function(name, username = vt_username(), title = NULL, open = interactive(),
                       add_before = NULL, add_after = NULL){

  # ensure file extensions are of the acceptable type
  name <- vt_set_ext(name, ext = c("md", "rmd"))

  is_valid_name(name)

  # Create file to write in
  req_name <- create_item("requirements", name)

  ## if the file didnt exist before, populate with contents
  if (file.size(req_name) == 0){

    if(is.null(title)){
      title <- basename(file_path_sans_ext(req_name))
    }

    # Create the content to write
    render_template("requirements", output = req_name,
                    data = list(
                      username = username,
                      title = title,
                      editDate = as.character(Sys.Date())
                    ))
    
    vt_add_file_to_config(
      filename = name,
      after = {{add_after}},
      before = {{add_before}}
      )
  }


  if(open){
    edit_file(req_name) # nocov
  }

  invisible(req_name)

}


