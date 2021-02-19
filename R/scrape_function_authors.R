#' @title Retrieve authorship details for all functions
#' @description Looks for roxygen2 function documentation in /R for author details.
#' Assumes that author and date are tagged via custom sections \code{@section Last updated by:}
#' and \code{@section Last updated date:}, respectively. To exclude a roxygen
#' block from this scraping, omit these section names. \cr \cr
#' If using a dummy documentation file, looks for \code{@name} to capture
#' function name, otherwise uses the actual function call. \cr \cr
#' Exported or internal status does not affect scraping.
#' @param pkg path to package
#' @section Last Updated by:
#' Marie Vendettuoli
#' @section Last updated date:
#' 2021-02-18
#' @export
#' @importFrom devtools package_file
vt_scrape_function_authors <- function(pkg = "."){
  r_dir <- file.path(package_file(path = pkg), "R")
  do.call(rbind, lapply(normalizePath(list.files(r_dir, pattern = ".R", full.names = TRUE)),
         FUN = .scrape_one_function_file))
}

#' @title Retrieve function author for one .R file
#' @keywords internal
#' @importFrom utils getSrcref
#' @importFrom lubridate parse_date_time
.scrape_one_function_file <- function(one_file){
  refs <- getSrcref(parse(
    one_file,
    keep.source =  TRUE,
    srcfile = srcfilecopy(filename = one_file, isFile = TRUE)
  ))
  srcfile <- attr(refs[[1]], "srcfile")
  do.call(rbind, lapply(seq_along(refs), function(i) {
    first_byte <- refs[[i]][4] -5
    first_line <- ifelse(i == 1, 0, refs[[i - 1]][1])
    last_line <- refs[[i]][1] - 1
    last_byte <- refs[[i]][3] + 5
    lloc <- c(first_line, first_byte, last_line, last_byte)
    out <- as.character(srcref(srcfile, lloc))
    out_cleaned <- out[grepl(out, pattern = "#' ")]
    if (length(out_cleaned) == 0 |
        length(grep(out_cleaned, pattern = "#' @section")) == 0) {
      return(data.frame())
    } else {
      roxy_block <- gsub(pattern = "^#' ", replacement = "",
                         out_cleaned[grep(pattern = "^#' ", out_cleaned)])
      this_title <- as.character(parse(text = as.character(refs[[i]]))[[1]])[2]
      if(is.na(this_title)){
        this_title <- gsub(roxy_block[grepl(roxy_block, pattern = "@name")],
                           pattern = "@name ", replacement = "")
      }
      return(
        data.frame(
          title =  this_title,
          author = gsub(vt_scrape_section("^@section last updated* by:$", roxy_block),
                        pattern = "^\\s*", replacement = ""),
          last_updated_date = parse_date_time(
            vt_scrape_section("^@section last updated* date:$", tolower(roxy_block)),
            orders = c("ymd", "mdy")
          ),
          stringsAsFactors = FALSE
        )
      )
    }
  }))
}


