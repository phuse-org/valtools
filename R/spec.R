# The rest of this documentation is in case.R
#'
#' @param name The Name of the new specification. specs can be named with your
#'   file system separator and will be organized as a directory structure. Specs
#'   are located at `./inst/validation/specification/{spec}`.
#'
#' @return Path to the newly created specification file, invisibly.
#'
#' @export
#'
#' @rdname new_item
vt_use_spec <- function(name, username = vt_username(), open = interactive(), pkg = "."){

  # ensure file extensions are of the acceptable type
  name <- vt_set_ext(name, ext = "md")

  # Create file to write in
  spec_name <- create_item(pkg, "specification", name)

  ## if the file didnt exist before, populate with contents
  if (file.size(spec_name) == 0){

    # Create the content to write
    content <- paste0(c(
      "#' @section Last Updated By:",
      paste0("#' ", username),
      "#' @section Last Update Date:",
      paste0("#' ", as.character(Sys.Date())),
      "",
      "+ _Specifications_",
      "    + Start documenting specifications here!"
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


