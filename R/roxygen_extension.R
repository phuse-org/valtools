#' @export
#' @importFrom roxygen2 tag_markdown roxy_tag_parse
roxy_tag_parse.roxy_tag_editor <- function(x) {
  tag_markdown(x)
}

#' @export
#' @importFrom lubridate parse_date_time
#' @importFrom roxygen2  roxy_tag_parse
roxy_tag_parse.roxy_tag_editDate <- function(x) {
  x$val <- as.character(parse_date_time(x$raw, orders = c("Ymd", "mdY", "dbY")))
  x
}

#' @export
#' @importFrom roxygen2 rd_section roxy_tag_rd
roxy_tag_rd.roxy_tag_editor <- function(x, base_path, env) {
  rd_section("editor", x$val)
}

#' @export
#' @importFrom roxygen2 roxy_tag_rd rd_section
roxy_tag_rd.roxy_tag_editDate <- function(x, base_path, env) {
  rd_section("editDate", x$val)
}

#' @export
format.rd_section_editor <- function(x, ...) {
  paste0(
    "\\section{Last Edited By}{\n", x$value, "\n}\n"
  )
}

#' @export
format.rd_section_editDate <- function(x, ...) {
  paste0(
    "\\section{Last Edit Date}{\n", x$value, "\n}\n"
  )
}
