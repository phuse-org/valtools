#' Scrape change log from a validation project
#' @return data.frame with variables \code{version}, \code{effective_date}, \code{description}
#' @note
#' Extracts validation version, date, and description from change log items
#' that start with \code{[validation]}.
#' @rdname change_log
#' @export
vt_scrape_change_log <- function(){


  tryCatch(
    change_log_path <- find_file("change_log.md",ref = vt_path(), full_names = TRUE),

    error = function(e){
      if(inherits(e,"vt.file_not_found")){
        abort(
          paste0(
            "A change log does not exist in the validation folder.\n",
            "Run `valtools::vt_use_change_log()` to create a change_log.md file."
          ),
          class = "vt.validation_change_log_missing"
        )
      }else{
        abort(e)
      }
    })

  db <- read_change_log(change_log_path)

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

#' Format change log info table for validation report
#' @param change_log_info data.frame as exported from \code{\link{vt_scrape_change_log}}
#' @param format passed to \code{knitr::kable}
#' @return a knitr_kable object
#' @export
#' @rdname change_log
#'
#' @importFrom knitr kable
#' @importFrom kableExtra column_spec collapse_rows
#'
#' @examples
#'
#' withr::with_tempdir({
#'  file.create(".here")
#'  vt_use_validation()
#'
#'  vt_use_change_log()
#'
#'  log_data <- vt_scrape_change_log()
#'  print(log_data)
#'
#'  vt_kable_change_log(log_data)
#'
#' })
vt_kable_change_log <- function(change_log_info, format = "latex"){
  all_news <- change_log_info[order(change_log_info$version, decreasing = TRUE),
                        c("version", "effective_date", "description")]
  rownames(all_news) <- 1:nrow(all_news)

  t <- kable(all_news,
             format = format,
             col.names = c("Version", "Effective Date", "Activity Description"))
  t <- column_spec(t, 1, border_left = TRUE)
  t <- column_spec(t, 3, border_right = TRUE, width = "30em")
  t <- collapse_rows(t, 1:2)
  t
}


#' Initiate a change_log file
#' @param date passed to template
#' @param open whether to open the file after
#' @param version version to set in news file
#' @export
#' @rdname change_log
#'
#' @importFrom rlang inform
#' @importFrom usethis edit_file
#' @importFrom rprojroot find_root has_file
#' @importFrom desc desc
#' @returns path to change log file, used for side effect of creating change_log
vt_use_change_log <- function(date = NULL, version = NULL, open = interactive()){

  cl_file <- try(find_file("change_log.md", vt_path(),full_names = TRUE),silent = TRUE)

  if(!inherits(cl_file,"try-error")){
    inform("'change_log.md' already exists.", class = "vt.change_log_exists")
    if(open){
      usethis::edit_file(cl_file)
    }
    return(cl_file)
  }


  if(is.null(version)){
    if(is_package(vt_path())){
      version <- desc(find_file("DESCRIPTION", find_root(has_file("DESCRIPTION")), full_names = TRUE))$get("Version")[[1]]
    }else{
      version <- "1.0"
    }
  }
  if(is.null(date)){
    date <- format(Sys.Date(), "%Y-%m-%d")
  }

  proj_info <- c(Date = date, Version = version)

  render_template(output = vt_path("change_log.md"), template = "change_log.md", data = proj_info)

  if(open){
    edit_file("change_log.md")
  }

  invisible(vt_path("change_log.md"))
}





## internal function for parsing change_log.md into a "db" file similar to
## utils::news does
read_change_log <- function(file){

  news_file <- readLines(file)
  n_headers <- sum(grepl("^#",news_file))
  section_headers <- c(grep("^#",news_file),length(news_file) +1)
  sections <-

  db <- list(
    Text = vector("character", length =n_headers ),
    Version = vector("character", length =n_headers ),
    Date = vector("character", length =n_headers )
  )

  for(section_idx in seq_len(n_headers)){

    section_lines <- seq(
      section_headers[[section_idx]]+1,
      section_headers[[section_idx+1]]-1
    )

    header <-
      strsplit(news_file[section_headers[[section_idx]]], split = "\\s+")[[1]]

    version <- header[2]
    date <- ifelse(length(header) == 3, format(lubridate::parse_date_time(header[3],orders = c("ymd","mdy","dmy")), "%Y-%m-%d"), "")
    body <- paste(
      trimws(substring(trimws(news_file[section_lines]),first = 2)),
        collapse = "\n ")

    db$Text[[section_idx]] <- body
    db$Version[[section_idx]] <- version
    db$Date[[section_idx]] <- date
  }

  db

}

