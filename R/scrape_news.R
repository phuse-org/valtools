#' @title Convert NEWS to data.frame for one release version
#' @param news_log vector of relevent section from NEWS, using \code{read_lines}
#' @return a data.frame with variables: Version, Comment, Date. one row for all
#' validation comments. Other comment flags ignored
#' @keywords internal
#' @author Marie Vendettuoli
#' @section Last Updated:
#' 2020-05-15
convert_news_onever <- function(news_log) {
  ver_start <- grep(news_log, pattern = "#")
  comments <- gsub(news_log[grepl(news_log, pattern = "\\* ")],
                   pattern = "\\* ", replacement = "")

  validation_comments <- gsub(comments[grep(comments, pattern = "\\[validation\\]")],
                              pattern = "\\[validation\\] ", replacement = "" )

  date_info <-
    as.POSIXct(gsub(comments[grep(comments, pattern = "\\[version date\\]")],
                    pattern = "\\[version date\\] ", replacement = ""))

  ver_info <- data.frame(
    Version = gsub(news_log[ver_start], pattern = "#+ v", replacement = ""),
    Date = date_info,
    Comment = validation_comments

  )
  return(ver_info)
}




#' @title Convert NEWS to data.frame for all release versions listed
#' @param news_log vector of NEWS, using \code{read_lines}
#' @return a data.frame with variables: Version, Comment, Date. one row for all
#' validation comments. Other comment flags ignored
#' @keywords internal
#' @author Marie Vendettuoli
#' @section Last Updated:
#' 2020-05-15
convert_allnews <- function(news_log) {
  this_ver_start <- grep(news_log, pattern = "#")

  # base_case, no more versions
  if (length(news_log) < 3) {
    return()
  } else {
    # scrape off the bottom level version
    this_news_log <-
      news_log[this_ver_start[length(this_ver_start)]:length(news_log)]
    this_ver_df <- convert_news_onever(this_news_log)

    # recursive call to convert_news_df (this function)
    out <- rbind(this_ver_df,
                 convert_allnews(news_log[1:(this_ver_start[length(this_ver_start)] - 1)]))
    return(out)

  }

}

#' @title SCrape NEWS file for version validation information
#' @param file path to NEWS markdown file
#' @return a data.frame with variables: Version, Comment, Date. one row for all
#' validation comments. Other comment flags ignored
#' @examples
#' \dontrun{
#' news_log <- read_lines(file.path(here(), "NEWS.md"))
#' scrape_news(news_log)
#' }
#' @author Marie Vendettuoli
#' @section Last Updated:
#' 2020-05-15
#' @export
#' @importFrom readr read_lines
scrape_news <- function(file){
  news_log <- read_lines(file)
  convert_allnews(news_log)
}
