

#' Add validation file ordering to  validation config file
#'
#' Impose ordering of validation child files
#' @param filename character vector containing filenames in order
#' @param before,after Optional destination of new filenames, default is end of existing list. Specifying both is error.
#' @returns Used for side effect of adding validation file ordering to validation config
#'     file. Invisibly returns TRUE on success.
#'
#' @rdname validation_config
#'
#' @examples
#' \dontrun{
#'
#' vt_use_validation_config(pkg = ".")
#'
#' vt_add_file_to_config(filename = "myReqFile.Rmd")
#' }
#' @importFrom rlang abort
#' @export
vt_add_file_to_config <- function(filename, before = NULL, after = NULL){

  validation_config <- read_validation_config()

  # check whether these files are already listed
  # handle whether filename value is list or vector
  validation_file_list_old <- validation_config$validation_files
  if(length(validation_file_list_old[validation_file_list_old %in% unlist(filename)]) > 0 ){
    abort(
      paste0(
        "Filename(s): `",
        paste0(filename, collapse = ", "),
        "` ",
        "already exists " ,
        "validation config file. Run `valtools::vt_drop_file_from_config(filename)` first!"
      ),
      class = "vt.validation_config_add_already_exists")
  } else {
     if(!is.null(before) & !is.null(after)){
      abort(
        "Must supply only one of `before` and `after`",
        class = "vt.validation_config_before_and_after"
      )
    } else if(is.null(before) & is.null(after)){
      validation_file_list_new <- c(validation_file_list_old, as.list(filename))
    } else if(length(validation_file_list_old) == 0){

      if(!is.null(before) || !is.null(after)){
        check_location_anchor(NULL, before, after)
      }
      validation_file_list_new <- c(validation_file_list_old, as.list(filename))
    }  else if(!is.null(before)){
      list_locator <- which(validation_file_list_old %in% before)
      check_location_anchor(list_locator, before, after)
      validation_file_list_new <- insert_list_locator(validation_file_list_old, filename, list_locator)
    } else {
      list_locator <- which(validation_file_list_old %in% after) + 1
      check_location_anchor(list_locator, before, after)
      validation_file_list_new <- insert_list_locator(validation_file_list_old, filename, list_locator)
    }


    write_validation_config(
      path = dirname(vt_find_config()),
      working_dir = validation_config$working_dir,
      output_dir = validation_config$output_dir,
      report_naming_format = validation_config$report_naming_format,
      username_list = validation_config$usernames,
      validation_files = validation_file_list_new
    )

    inform(paste0("Filename(s): ", paste0(filename, collapse = ", "), " added to validation config file."),
           class = "vt.validation_config_add_file")
  }
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
#' vt_use_validation_config(pkg = ".")
#'
#' vt_add_file_to_config(filename = "myReqFile.Rmd")
#'
#' vt_drop_file_from_config(filename = "myReqFile.Rmd")
#'
#' }
#' @export
vt_drop_file_from_config <- function(filename){
  validation_config <- read_validation_config()
  validation_file_list_old <- validation_config$validation_files

  ## check whether these files are already listed
  # handle whether filename value is list or vector
  if(length(validation_file_list_old[validation_file_list_old %in% unlist(filename)]) == 0){
    abort(
      paste0(
        "Filename(s): `",
        paste0(filename, collapse = ", "),
        "` ",
        "not present " ,
        "validation config file. Run `valtools::vt_add_file_to_config(filename)` first!"
      ),
      class = "vt.validation_config_remove_doesnt_exist")
  } else {
    validation_file_list_new <- validation_file_list_old[!validation_file_list_old %in% unlist(filename)]
    write_validation_config(
      path = dirname(vt_find_config()),
      working_dir = validation_config$working_dir,
      output_dir = validation_config$output_dir,
      report_naming_format = validation_config$report_naming_format,
      username_list = validation_config$usernames,
      validation_files = validation_file_list_new

    )
    inform(paste0("Filename(s): ", paste0(filename, collapse = ", "), " removed from validation config file."),
           class = "vt.validation_config_add_file")
  }
  invisible(TRUE)
}


get_config_validation_files <- function(){
  files <- read_validation_config()$validation_files
}

get_config_report_naming_format <- function(){
  read_validation_config()$report_naming_format
}

insert_list_locator <- function(validation_file_list_old, filename, list_locator){
  if(list_locator == 1){
    return(c(as.list(filename), validation_file_list_old))
  } else {
    n_old <- length(validation_file_list_old)
    return(c(validation_file_list_old[1:(list_locator - 1)],
             as.list(filename),
             validation_file_list_old[list_locator:n_old]))
  }

}

check_location_anchor <- function(list_locator, before, after){
  if(length(list_locator) == 0){
    abort(paste0(
      "Location anchor ",
      before, after,
      " does not exist. Run `valtools::vt_add_file_to_config` first!"
    ),
    class = "vt.validation_config_file_not_listed")
  }
}
