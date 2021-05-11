#' @title Retrieve the value block of a custom section tagged via roxygen2
#' @description Looks for the value in a custom roxygen sections. Custom
#' sections are named using \code{@section <NAME>:}, where colon is use to indicate
#' end of the name, and value starts on next line.
#' @param tag name of the section, case insensitive.
#' @param block character vector that holds the documentation block. \code{#' }
#' may be present or omitted.
#' @return section value
#' @section Last Updated By:
#' Marie Vendettuoli
#' @section Last Updated Date:
#' 2021-02-18
#' @export
#' @examples
#' roxy_block1 <- c("@title Title1", "@param param1 definition",
#'     "@section Last updated date:", "2021-01-01", "@importFrom utils sessionInfo",
#'     "@export" )
#' vt_scrape_section("Last updated date:", roxy_block1)
#'
#' roxy_block2 <- paste0("#' ", roxy_block1)
#' vt_scrape_section("Last updated date:", roxy_block2)
vt_scrape_section <- function(tag, block){
  tag_no <- which(grepl(pattern = tolower(tag), tolower(block)))
  entry_value <- gsub(pattern = "[[:space:]]^|[[:space:]]$", replacement = "",
       x = block[tag_no + 1])
  gsub(pattern = "#'\\s", replacement = "", x = entry_value)
}
