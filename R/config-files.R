

#' Add validation file ordering to  validation config file
#'
#' Impose ordering of validation child files
#' @param filename character vector containing filenames in order
#' @param before,after Optional destination of new filenames, default is end of
#'   existing list. Supports <[`tidy-select`][tidyselect::language]> functions.
#'   Specifying both is error.
#' @returns Used for side effect of adding validation file ordering to
#'   validation config file. Invisibly returns TRUE on success.
#'
#' @rdname validation_config
#'
#' @importFrom tidyselect eval_select
#' @importFrom rlang abort enquo quo_is_null seq2 quo_is_call quo_get_expr
#' @examples
#' \dontrun{
#'
#' vt_use_validation()
#'
#' vt_add_file_to_config(filename = "myReqFile.Rmd")
#' }
#' @export
vt_add_file_to_config <- function(filename, before = NULL, after = NULL){

  .before <- enquo(before)
  .after <- enquo(after)
  has_before <- !quo_is_null(.before)
  not_select_call_before <- !quo_is_call(.before)
  has_after <- !quo_is_null(.after)
  not_select_call_after <- !quo_is_call(.after)

  validation_config <- read_validation_config()

  # check whether these files are already listed
  # handle whether filename value is list or vector
  validation_file_list <- validation_config$validation_files
  validation_file_list <- setNames(validation_file_list, validation_file_list)

  if(has_before & not_select_call_before){
    before_val <- quo_get_expr(.before)
    if(! before_val %in% validation_file_list){
      abort(paste0(
        "Location anchor ",
        before_val,
        " does not exist. Run `valtools::vt_add_file_to_config` first!"
      ),
      class = "vt.validation_config_file_not_listed")
    }
  }else if(has_after & not_select_call_after){
    after_val <- quo_get_expr(.after)
    if(! after_val %in% validation_file_list){
      abort(paste0(
        "Location anchor ",
        after_val,
        " does not exist. Run `valtools::vt_add_file_to_config` first!"
      ),
      class = "vt.validation_config_file_not_listed")
    }
  }

  if(length(invalid_files <- validation_file_list[validation_file_list %in% unlist(filename)]) > 0 ){
    abort(
      paste0(
        "File already exists in " ,
        "validation config file.",
        " Run ",
        "`valtools::vt_drop_file_from_config(c(",paste0("\"",invalid_files,"\"", collapse = ","),"))` first!"
      ),
      class = "vt.validation_config_add_already_exists")
  }


  if (has_before && has_after) {
    abort("Must supply only one of `before` and `after`.")
  } else if (has_before) {
    where2 <- min(unname(eval_select(.before, validation_file_list)))
    if(is.infinite(where2)){
      where2 <- 0
    }
  } else if (has_after) {
    where2 <- max(unname(eval_select(.after, validation_file_list))) + 1
    if(is.infinite(where2)){
      where2 <- length(validation_file_list) + 1
    }
  } else {
    where2 <- length(validation_file_list) + 1
  }

  lhs <- validation_file_list[seq2(1, where2 - 1)]
  validation_file_list_new <- unique(c(lhs, filename))

  if(where2-1 != length(validation_file_list) ){
    rhs <- validation_file_list[seq2(where2 , length(validation_file_list))]
    validation_file_list_new <- unique(c(validation_file_list_new, rhs))
  }


  write_validation_config(
    path = dirname(vt_find_config()),
    package = validation_config$package,
    working_dir = validation_config$working_dir,
    output_dir = validation_config$output_dir,
    report_rmd_name = validation_config$report_rmd_name,
    report_naming_format = validation_config$report_naming_format,
    username_list = validation_config$usernames,
    validation_files = validation_file_list_new
  )

  inform(paste0("Filename(s): ", paste0(filename, collapse = ", "), " added to validation config file."),
         class = "vt.validation_config_add_file")

  invisible(TRUE)
}

#' Drop validation file ordering from validation config file
#'
#' Remove validation file orderingfrom the projects validation config file
#'
#' @importFrom rlang abort inform
#'
#' @returns Used for side effect of removing file ordering information from validation config
#'     file. Invisibly returns TRUE on success.
#'
#' @rdname validation_config
#'
#' @examples
#' \dontrun{
#'
#' vt_use_validation()
#'
#' vt_add_file_to_config(filename = "myReqFile.Rmd")
#'
#' vt_drop_file_from_config(filename = "myReqFile.Rmd")
#'
#' }
#' @export
vt_drop_file_from_config <- function(filename) {
  validation_config <- read_validation_config()
  validation_file_list <- validation_config$validation_files

  ## check whether these files are already listed
  # handle whether filename value is list or vector


  if (length(invalid_files <-
             unlist(filename)[!unlist(filename) %in% validation_file_list]) > 0) {
    abort(
      paste0(
        "File does not exist in " ,
        "validation config file.",
        " Run ",
        "`valtools::vt_add_file_to_config(c(",
        paste0("\"", invalid_files, "\"", collapse = ","),
        "))` first!"
      ),
      class = "vt.validation_config_add_already_exists"
    )
  }


  write_validation_config(
    path = dirname(vt_find_config()),
    package = validation_config$package,
    working_dir = validation_config$working_dir,
    output_dir = validation_config$output_dir,
    report_rmd_name = validation_config$report_rmd_name,
    report_naming_format = validation_config$report_naming_format,
    username_list = validation_config$usernames,
    validation_files = validation_file_list[!validation_file_list %in% unlist(filename)]
  )

  inform(paste0(
    "Filename(s): ",
    paste0(filename, collapse = ", "),
    " removed from validation config file."
  ),
  class = "vt.validation_config_add_file")

  invisible(TRUE)

}


get_config_validation_files <- function(){
  files <- read_validation_config()$validation_files
}

get_config_report_naming_format <- function(){
  read_validation_config()$report_naming_format
}
