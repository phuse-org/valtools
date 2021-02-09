# The rest of this documentation is in case.R
#' @param spec_name The name/path of the spec. specs can be named with your
#'   file system separator and will be organized as a directory structure. Specs
#'   are located at `./inst/validation/specs/{spec_name}`.
#'
#' @export
#'
#' @rdname new_item
vt_use_spec <- function(spec_name, pkg = "."){

  spec_path <- create_item(pkg, "specs", spec_name)

  writeLines("here is your spec file", con = spec_path)

}
