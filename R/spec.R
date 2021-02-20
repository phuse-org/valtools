# The rest of this documentation is in case.R
#'
#' @param name The Name of the new specification. specs can be named with your
#'   file system separator and will be organized as a directory structure. Specs
#'   are located at `./inst/validation/specification/{spec}`.
#' @title title for the specification. defaults to be the base name passed sans
#'  file paths or extensions.
#'
#' @return Path to the newly created specification file, invisibly.
#'
#' @export
#'
#' @rdname new_item
vt_use_spec <- function(name, username = vt_username(), title = NULL, open = interactive(), pkg = "."){

  # ensure file extensions are of the acceptable type
  name <- vt_set_ext(name, ext = "md")

  is_valid_name(name)

  # Create file to write in
  spec_name <- create_item(pkg, "specification", name)

  ## if the file didnt exist before, populate with contents
  if (file.size(spec_name) == 0){

    if(is.null(title)){
      title <- basename(file_path_sans_ext(spec_name))
    }

    # Create the content to write
    content <- paste0(c(
      paste0("#' @title ", title),
      paste0("#' @editor ", username),
      paste0("#' @editDate ", as.character(Sys.Date())),
      "",
      "+ Start documenting requirements here!"
      ),
      collapse = "",
      sep = "\n"
    )

    writeLines(content, con = spec_name)
  }

  if(open){
    usethis::edit_file(spec_name)
  }

  invisible(spec_name)

}


