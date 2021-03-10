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
#' @importFrom lubridate parse_date_time
#' @importFrom roxygen2  roxy_tag_parse
roxy_tag_parse.roxy_tag_coverage<- function(x) {
  x <- format_coverage_text(x)
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
#' @importFrom roxygen2 roxy_tag_rd rd_section
roxy_tag_rd.roxy_tag_coverage <- function(x, base_path, env) {
  rd_section("coverage", x$val)
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

#' @export
format.rd_section_coverage <- function(x, ...) {
  paste0(
    "\\section{Test Coverage}{\n", x$value, "\n}\n"
  )
}

format_coverage_text <- function(x){

  ## capture val
  x <- tag_markdown(x)

  ## capture parsed text
  text <- trimws(x$raw)
  test_coverage <- strsplit(text, "\n")

  coverage <- lapply(
    test_coverage,
    function(tc){
      if(grepl("deprecate", tc, ignore.case = TRUE)){
        data.frame(
          test_case = "Deprecated",
          requirements = NA
        )
      }else{
        tc_rel <- gregexpr(":",tc)[[1]]
        test_case <- substr(tc, 0, tc_rel[1]-attr(tc_rel,"match.length")[1])
        reqs <- trimws(strsplit(substr(tc,  tc_rel[1]+attr(tc_rel,"match.length")[1], nchar(tc)), ",")[[1]])
        data.frame(
          test_case = test_case,
          requirements = reqs
          )
    }
  })

  class(coverage) <- "vt_test_req_coverage"

  x$coverage <- coverage

  return(x)
}
