# The rest of this documentation is in case.R
#' @param spec_name The name/path of the spec. specs can be named with your
#'   file system separator and will be organized as a directory structure. Specs
#'   are located at `./inst/validation/specs/{spec_name}`.
#'
#' @export
#'
#' @rdname new_item
vt_use_spec <- function(spec_name, spec_content, pkg = "."){

  # Create file to write in
  spec_path <- create_item(pkg, "specs", spec_name)

  # Create the content to write
  content <- paste0(c(
    "#' @section Last Updated By:",
    paste0("#' ", Sys.getenv("USER")),
    "#' @section Last Update Date",
    paste0("#' ", as.character(Sys.Date())),
    paste0("+ ", spec_content, collapse = ""),
    collapse = "",
    sep = "\n"
  ))

  writeLines(content, con = spec_path)
}
