
#' @title Generate at data.frame from the test code roxygen documentation blocks.
#' @description May be used for .R or .r files located
#' in \code{tests/test_specs} folder.
#' @param one_file full filepath of the .R or .r file to scrape
#' @return data.frame with variables: \code{title}, \code{last_update_by}, and
#' \code{last_updated_date}.
#' @examples
#' \dontrun{
#'   list.files(here("tests", "spec_tests"), pattern = ".R", ignore.case = TRUE,
#'            full.names = TRUE)[1:4] %>%
#'     lapply(., scrape_test_code_block) %>%
#'     do.call(rbind, .)
#' }
#' @export
#' @importFrom lubridate parse_date_time
scrape_test_code_block <- function(one_file){
  lines <- readLines(one_file)
  refs <- parse(text = lines, keep.source =  TRUE,
                srcfile = srcfilecopy(one_file, lines, isFile = TRUE)) %>%
    utils::getSrcref()
  srcfile <- attr(refs[[1]], "srcfile")
  do.call(rbind, lapply(seq_along(refs)[-1], function(i) {
    first_byte <- refs[[i - 1]][4] + 1
    first_line <- refs[[i - 1]][3]
    last_line <- refs[[i]][3]
    last_byte <- refs[[i]][4]
    lloc <- c(first_line, first_byte, last_line, last_byte)
    out <- as.character(srcref(srcfile, lloc))
    out_cleaned <- out[grepl(out, pattern = "#' |test_that")]
    if (length(out_cleaned) == 0) {
      return(data.frame())
    } else {
      roxy_block <- gsub(pattern = "#' ", replacement = "",
                         out_cleaned[grep(pattern = "#' ", out_cleaned)])
      title_line <-  strsplit(split = '\\"',
                              out_cleaned[grepl(pattern = "test_that", x = out_cleaned)])[[1]][2]
      return(data.frame(title = title_line,
                        last_update_by = get_section_contents("Last updated by", roxy_block),
                        last_updated_date = lubridate::parse_date_time(get_section_contents("last update date", roxy_block), orders = c("ymd", "mdy")),
                        stringsAsFactors = FALSE))
    }
  }))
}

#' @title eval_processing_tc_code
#' @description Evaluate the procesing test code from .RMD file when the top
#' section has been identified as a code chunk (should start at first line)
#' @param locations postion in \code{lines} which correspond to start and stop
#' of code chunks
#' @param lines RMD source with previously rendered sections omitted.
#' @param envir Environment to evaluate R code chunks. Allows for later chunks
#' in single RMD to depend on earlier ones.
#' @export
eval_processing_tc_code <- function(locations, lines, envir){
  new_start <- locations[1] + 1
  new_end <- locations[2] - 1
  cat(eval(parse(text = lines[new_start:new_end]), envir = envir), sep = "\n")
  # exclude the current code chunk lines from next recursion
  new_lines <- lines[(locations[2] + 1):length(lines)]
  # recursive call
  process_single_rmd(new_lines, envir = envir)
}

# process a section of the RMD that has been identified as code
#' @title eval_processing_tc_markdown
#' @description Evaluate the procesing test code from .RMD file when the top
#' section has been identified as a markdown section.
#' @param locations postion in \code{lines} which correspond to start and stop
#' of next code chunks
#' @param lines RMD source with previously rendered sections omitted.
#' @param envir Environment to evaluate R code chunks. Allows for later chunks
#' in single RMD to depend on earlier ones.
#' @export
eval_processing_tc_markdown <- function(locations, lines, envir){
  new_start <- 1
  new_end <- locations[1] - 1
  cat(lines[new_start:new_end], sep = "\n")
  # exclude the markdown before next code chunk from next recursion
  new_lines <- lines[locations[1]:length(lines)]
  # recursive call
  process_single_rmd(new_lines, envir = envir)
}

# recursive function
#' @title process_single_rmd
#' @description Recursive function to handle processing test code RMD files.
#' @param lines RMD source with previously rendered sections omitted.
#' @param envir environment for evaluating R code. Allows for later chunks in
#' single RMD to depend on earlier ones.
#' @export
process_single_rmd <- function(lines, envir = new.env()){
  locations <- grep("```", lines)
  if (length(locations) < 2) { # base case, nothing
    invisible()
  } else {
    # is the next section code or markdown?
    code_or_markdown <- ifelse(locations[1] == 1, "code", "markdown")
    # setup for recursive call will be different if code vs. markdown
    switch(code_or_markdown,
           code = eval_processing_tc_code(locations, lines, envir = envir),
           markdown = eval_processing_tc_markdown(locations, lines, envir = envir))

  }
}

#' @name scrape_processing_rmd
#' @title Scrape a processing test code RMD file
#' @param file character vector specifying file location
#' @param envir Environment to evaluate R code chunks. Allows for later chunks
#' in single RMD to depend on earlier ones.
#' @export
scrape_spec_rmd <- function(file, envir = new.env()){
  lines <- readLines(file)
  # remove the roxy section
  clean_lines <- lines[!grepl("#' ", lines)]
  # if it detects that there are roxygen code chunks to execute,
  # process them and cat the markdown
  if (length(grep("^```\\{r.*\\}", clean_lines)) > 0) {
    process_single_rmd(clean_lines, envir = envir)
  } else {
    cat(clean_lines,sep = "\n")
  }
}

