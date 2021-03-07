#' @title Retrieve roxygen tags as a data.frame from requirements, test cases, test code and functions
#' @description Looks for roxygen2 function documentation in /R for author details.
#' Assumes that author and date are tagged via custom sections \code{@section Last updated by:}
#' and \code{@section Last updated date:}, respectively. To exclude a roxygen
#' block from this scraping, omit these section names. \cr \cr
#' If using a dummy documentation file, looks for \code{@name} to capture
#' function name, otherwise uses the actual function call. \cr \cr
#' Exported or internal status does not affect scraping.
#' @param type type of scraping to be done. one of "requirements","test_cases","test_code","functions".
#'    to call functions. working directory must be an R package, or path identified in src must be an R package.
#' @param tags which tags to keep. defaults to editor and editDate
#' @param pkg path to package
#' @param src path to package source. defaults to the same path as pkg.
#' @param ref reference path to use. defaults to vt_path()
#' @section Last Updated by:
#' Ellis Hughes
#' @section Last updated date:
#' 2021-03-05
#' @export
#' @importFrom devtools package_file
#' @importFrom roxygen2  block_get_tag_value
#' @importFrom stats setNames
#' @importFrom rlang warn
vt_scrape_tags_from <- function(type, tags = c("editor","editDate"),pkg = ".", src = pkg, ref = vt_path()){

  types <- c("requirements","test_cases","test_code")

  ## can scrape functions if:
  ### - referencing source code
  ### - running a validation report on a validated package 
  if(is_package(pkg) | is_package(src) | Sys.getenv("vt_validation_state") == "installed" ){
    types <- c(types, "functions")
  }

  type <- match.arg(type,choices = types)

  ## Need this so that 
  dir_ref <- if(type == "functions" && Sys.getenv("vt_validation_state") != "installed"){
      file.path(package_file(path = src),"R")
    }else{
      file.path(ref, type)
    }

  dir_ref_files <- list.files(dir_ref,recursive = TRUE,full.names = TRUE)

  roxyblock_list <- do.call('c',lapply(
    dir_ref_files, function(ref_file, type){
      vt_scrape_roxygen(
        file = ref_file,
        type = ifelse(type == "test_code", "r_test_code", tools::file_ext(ref_file))
      )
    }, type = type))


  roxyblock_list <- subset_blocks(roxyblock_list,tags)

  if(length(roxyblock_list) > 0){

    lapply(roxyblock_list,
      function(block, tags, type){

        tag_values <- lapply(tags, function(tag, block) {block_get_tag_value(block, tag)}, block)

        item <- block$object$topic

        do.call('data.frame', setNames(c(item, tag_values), c(type, tags)))
      }, tags, type)

  }else{
    warn(paste0("No blocks with tags ", paste0("`",tags,"`",collapse = ", ")))
    return(list())
  }
}