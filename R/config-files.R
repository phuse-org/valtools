

#' Add validation file ordering to  validation config file
#'
#' Impose ordering of validation child files
#' @param filename character vector containing filenames in order
#' @param before Optional destination of new filenames, default is end of existing list
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
vt_add_file_to_config <- function(filename, before = NULL, pkg = "." ){

  validation_config <- read_validation_config(pkg = pkg)

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
    # determine ordering
    # || to short circuit
    if(is.null(before) || length(validation_file_list_old) == 0 ||
       (!is.null(before) & !(before %in% validation_file_list_old))){
      inform(
        paste0(
          "Before locator filename: ",
          before,
          " not present in existing validation filename list. Using default location."
        ),
        class = "vt.validation_config_add_missing_locator"
      )
      validation_file_list_new <- c(validation_file_list_old, as.list(filename))
    } else{
      list_locator <- which(validation_file_list_old %in% before)
      validation_file_list_new <- insert_list_locator(validation_file_list_old, filename, list_locator)
    }



    write_validation_config(
      path = pkg,
      working_dir = validation_config$working_dir,
      output_directory = validation_config$output_directory,
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
vt_drop_file_from_config <- function( filename, pkg = "."){
  validation_config <- read_validation_config(pkg = pkg)
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
      path = pkg,
      working_dir = validation_config$working_dir,
      output_directory = validation_config$output_directory,
      report_naming_format = validation_config$report_naming_format,
      username_list = validation_config$usernames,
      validation_files = validation_file_list_new

    )
    inform(paste0("Filename(s): ", paste0(filename, collapse = ", "), " removed from validation config file."),
           class = "vt.validation_config_add_file")
  }
  invisible(TRUE)
}


get_config_validation_files <- function(dir = "."){
  files <- read_validation_config(pkg = dir)$validation_files
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
