#' Scrape news from a package
#' @param pkg installed package
#' @return data.frame with variables \code{version}, \code{effective_date}, \code{description}
#' @note
#' Uses the function \code{utils::news} to extract version and date. Description
#' are news items that start with \code{[validation]}. If no date is provided, will
#' look for NEWs entries starting with \code{[version date]}
#' @export
#' @importFrom utils news
#' @importFrom rprojroot find_root has_file
vt_scrape_news <- function(pkg = rprojroot::find_root(criterion = has_file("DESCRIPTION"))){
  db <- news(package = pkg)
  all_text <- strsplit(db$Text, split = "\n ")
  all_news <- do.call("rbind", lapply(seq_along(db$Version),
    FUN = function(x){
      this_text <- all_text[[x]]
      this_date <- db$Date[x]
      if(is.na(this_date)){
        this_date <- gsub(pattern = "^-*\\s*\\[version date\\]\\s", replacement = "",
                          trimws(this_text[grepl("\\[version date\\]", this_text)]))
      }

      data.frame(version = db$Version[x],
                 effective_date = ifelse(length(this_date) == 0, "", this_date),
                 description = gsub(pattern = "^-*\\s*\\[validation\\]\\s",
                                    replacement = "",
                                    trimws(this_text[grepl("\\[validation\\]",
                                                           this_text)])))
    }))
  all_news <- all_news[order(all_news$version, decreasing = TRUE),]
  rownames(all_news) <- 1:nrow(all_news)
  all_news
}

#' Format NEWS info table for validation report
#' @param news_info data.frame as exported from \code{\link{vt_scrape_news}}
#' @param format passed to \code{knitr::kable}
#' @return a knitr_kable object
#' @export
#' @importFrom knitr kable
#' @importFrom kableExtra column_spec collapse_rows
vt_kable_news <- function(news_info, format = "latex"){
  all_news <- news_info[order(news_info$version, decreasing = TRUE),
                        c("version", "effective_date", "description")]
  rownames(all_news) <- 1:nrow(all_news)

  t <- kable(all_news,
             format = format,
             col.names = c("Version", "Effective Date", "Activity Description"))
  t <- column_spec(1, border_left = TRUE)
  t <- column_spec(3, border_right = TRUE, width = "30em")
  t <- collapse_rows(t, 1:2)
  t
}


#' Initiate a NEWS.md file
#' @param date passed to template
#' @note This is an alternative to \code{usethis::use_news_md}.
#' @export
#' @importFrom rprojroot find_root has_file
vt_use_news_md <- function(date = NULL){
  this_desc <- desc(find_root(criterion = has_file("DESCRIPTION")))
  proj_info <- c(Date = date, this_desc$get(this_desc$fields()))
  if("Date" %in% names(proj_info)){
    proj_info[["Date"]] = paste0("(", proj_info[["Date"]], ")")
  } else {
    proj_info[["Date"]] = ""
  }

  render_template("NEWS.md", data = proj_info)
}

