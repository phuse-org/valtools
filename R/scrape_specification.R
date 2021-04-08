#' Scrape requirement info
#' @param tags which tags to keep. defaults to editor and editDate
#' @param src path to package sources. defaults to current directory and passed to \code{\link{vt_scrape_tags_From}}
#' @param ref reference path to whre validation documentation lives. defaults to \code{\link{vt_path}} annd passed to \code{\link{vt_scrape_tags_From}}.
#' @param dynamic_ref dynamic referencer object
#' @export
vt_scrape_requirements <- function(tags = c("editor", "editDate"), src = ".", ref = vt_path(),
                                   dynamic_ref = NULL){
  out <- do.call("rbind", vt_scrape_tags_from(
    type = "requirements",
    tags = tags,
    src = src,
    ref = ref
  ))

  if(!is.null(dynamic_ref)){
    dynamic_ref$scrape_references(out)
    out <- dynamic_ref$reference_insertion(out)
  }

  out
}

#' kable code from requirement author table
#' @param requirement_details data.frame as exported from vt_scrape_requirements
#' @param format passed to \code{knitr::kable}
vt_kable_requirements <- function(requirement_details, format = "latex"){
  requirement_details$requirements = paste0("Requirement ", requirement_details$requirements)

  all_colnames <- c(requirements = "Requirement ID",
                    editor = "Editor",
                    editDate = "Edit Date")
  t <- kable(requirement_details[, names(all_colnames)],
             format = format, booktabs = FALSE,
             col.names = all_colnames)
  t <- column_spec(t, 1, border_left = TRUE)
  t <- column_spec(t, ncol(req_authors), border_right = TRUE)
  t
}
