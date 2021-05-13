#' @title Retrieve roxygen tags as a data.frame from requirements, test cases, test code and functions
#' @description Looks for roxygen2 function documentation in /R for author details.
#' Assumes that author and date are tagged via custom sections \code{@section Last updated by:}
#' and \code{@section Last updated date:}, respectively. To exclude a roxygen
#' block from this scraping, omit these section names. \cr \cr
#' If using a dummy documentation file, looks for \code{@name} to capture
#' function name, otherwise uses the actual function call. \cr \cr
#' Exported or internal status does not affect scraping.
#' @note At this time, this function does not retrieve documentation captured for functions dispatched
#' within an R6 class. Tags at the class level documentation are retrieved.
#' @param type type of scraping to be done. one of "requirements","test_cases","test_code","functions".
#'    to call functions. working directory must be an R package, or path identified in src must be an R package.
#' @param tags which tags to keep. defaults to editor and editDate
#' @param src path to package source. defaults to the current directory.
#' @param ref reference path to where validation documentation lives. defaults to vt_path()
#' @section Last Updated by:
#' Ellis Hughes
#' @section Last updated date:
#' 2021-03-05
#' @export
#' @importFrom devtools package_file
#' @importFrom roxygen2  block_get_tag_value
#' @importFrom stats setNames
#' @importFrom rlang warn
vt_scrape_tags_from <- function(type, tags = c("editor","editDate"), src = ".", ref = vt_path()){

  types <- c("requirements","test_cases","test_code")

  ## can scrape functions if:
  ### - referencing source code
  ### - running a validation report on a validated package
  if(is_package(ref) | is_package(src) | Sys.getenv("vt_validation_state") == "installed" ){
    types <- c(types, "functions")
  }

  type <- match.arg(type,choices = types)

  ## Need this so that we point to the correct folders depending on src vs pkg and running
  ## inside an installed package or not.
  dir_ref <- if(type == "functions") {
    if (Sys.getenv("vt_validation_state") != "installed") {
      file.path(package_file(path = src), "R")
    } else{
      file.path(ref, "R") # nocov
    }
  } else{
    file.path(ref, type)
  }

  dir_ref_files <- list.files(dir_ref,recursive = TRUE,full.names = TRUE)

  roxyblock_list <- do.call('c',lapply(
    dir_ref_files, function(ref_file, type){
      scrape_roxygen(
        file = ref_file,
        type = ifelse(type == "test_code", "r_test_code", tools::file_ext(ref_file))
      )
    }, type = type))

  roxyblock_list <- subset_blocks(roxyblock_list,tags)

  if(length(roxyblock_list) > 0){

    lapply(roxyblock_list,
      function(block, tags, type){

        tag_values <- lapply(tags, function(tag, block) {

          val <- block_get_tag_value(block, tag)

          if(is.null(val)){
            val <- NA
          }

          return(val)

          }, block)

        item <- block$object$topic

        do.call('data.frame', setNames(c(item, tag_values, FALSE), c(type, tags,"stringsAsFactors")))
      }, tags, type)

  }else{
    warn(paste0("No blocks with tags ", paste0("`",tags,"`",collapse = ", ")))
    return(list())
  }
}
