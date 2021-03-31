#' @title Validation Config
#'
#' @description Use a Validation Config File
#'     Validation configuration for working/output directories, validation report
#'     naming conventions, and tracking user information(username,name,title, role).
#'     Provides a single location for setting behaviors.
#'
#' @param pkg where to write config file
#' @param working_dir [character] which directory to be have working validation
#'  contents that are used interactively
#' @param output_dir [character] which folder should the contents for validation
#'  output to.
#'@param report_naming_format [character] a \link[glue]{glue} friendly string
#'  of the naming structure of the output validation report. use `{package}`
#'  for package name, `{version}` to record package version, and `{date}` to
#'  capture the date the report was run.
#' @param username_list list of user objects created by {make_user}. Each user
#'     contains entries for username, name, title, and role to be used for
#'     documentation.
#' @param validation_files list of validation files: requirements, test cases and test code.
#' Validation report content will be populated using this list in order.
#' @param ... These dots are for future extensions and must be empty.
#' @param overwrite `[boolean]`If a validation file exists, should it be overwritten?
#'    Defaults to FALSE.
#'
#' @importFrom rlang inform abort
#'
#' @returns Used for side effect to create validation config file. Invisibly
#'     returns TRUE on success.
#'
#' @rdname validation_config
#'
#' @examples
#'
#' vt_use_config(pkg = tempdir(),
#'               working_dir = ".",
#'               output_dir  = ".",
#'               report_naming_format = "Validation_Report_{package}_v{version}_{date}"
#'               username_list = list(
#'                  vt_user(
#'                        name = "test",
#'                        title = "test",
#'                        role = "tester",
#'                        username = "test"
#'                        )))
#'
#' @export
#'
vt_use_config <- function(pkg = ".",
                          working_dir,
                          output_dir,
                          report_naming_format = "Validation_Report_{package}_v{version}_{date}",
                          username_list = list(),
                          validation_files = list(),
                          ...,
                          overwrite = FALSE
){

  if(missing(working_dir)){
    if(is_package(pkg = pkg)){
      working_dir <- "vignettes"
    }else{
      working_dir <- "."
    }
  }

  if(missing(output_dir)){
    if(is_package(pkg = pkg)){
      output_dir <- "inst"
    }else{
      output_dir <- "."
    }
  }

  if(!dir.exists(file.path(pkg,working_dir,"validation"))){
    abort("No validation structure found. Run `valtools::vt_use_validation().`",
          class = "vt.missingStructure")
  }

  if(file.exists(file.path(pkg,working_dir,"validation","validation.yml")) & !overwrite){
    abort(
      paste0(
      "Validation config file already exists.\n",
      "To overwrite, set `overwrite` to `TRUE` in `vt_use_config()`"
      ),
      class = "vt.validation_config_exists"
    )
  }



  ## add ".here" ref if not a package
  set_dir_ref(pkg = pkg)

  ## add "validation.yml" to .Rbuildignore if is a package
  if(is_package(pkg = pkg)){
    use_build_ignore2(ignores = file.path(working_dir,"validation","validation.yml"),dir = pkg)
  }

  if(length(username_list) > 0 ){
    user_entries <- sapply(username_list, is_vt_user)
    non_users <- sum(!user_entries)
    if(non_users > 0){
      abort(
        paste0(
          non_users," invalid entr",ifelse(non_users > 1, "ies","y")," in username_list: \n",
          "Argument ",paste(which(!user_entries), collapse = ", "), " is not created by `vt_user()`."
        ),
        class = "vt.validation_config_invalid_userlist"
      )
    }
    username_list <- do.call('c', username_list)
  }

  write_validation_config(
    path = file.path(pkg,working_dir,"validation"),
    working_dir = working_dir,
    output_dir = output_dir,
    report_naming_format = report_naming_format,
    username_list = username_list,
    validation_files = validation_files,
    ...
  )

  inform(
    "validation config file created. Add user information through `vt_add_user_to_config()`",
    class = "vt.validation_config_created"
  )

  invisible(TRUE)

}



#' Create a user object
#'
#' Capture the information on a user that is going to be involved with validation
#'
#' @param username username of the user.
#' @param name full name of the user.
#' @param title title of the user.
#' @param role role of the user. Can be more than one.
#' @param ... additional information about the user to be passed into a list.
#'
#' @returns a "user" object
#'
#' @importFrom stats setNames
#' @rdname validation_config
#'
#' @examples
#'
#' vt_user(
#'     username = "ellis",
#'     name = "Ellis Hughes",
#'     title = "Statistical Programmer",
#'     role = "Programmer")
#'
#' @export
#'
vt_user <- function(username, name, title, role, ...) {
  structure(setNames(list(
    list(
      name = name,
      title = title,
      role = role,
      ...
    )
  ), username),
  class = c("vt_user","list"))
}

is_vt_user <- function(x){
  "vt_user" %in% class(x)
}



#' write validation config file
#'
#' validation configuration for directories and username-name-role key
#' @noRd
#'
#' @param ... These dots are for future extensions and must be empty.
#'
#' @importFrom yaml write_yaml
#' @importFrom rlang abort
write_validation_config <- function(path = ".",
                                    working_dir = "vignettes",
                                    output_dir = "inst",
                                    report_naming_format = "Validation_Report_{package}_v{version}_{date}",
                                    username_list = list(),
                                    validation_files = list(),
                                    ...) {
  config_contents <- list(
    working_dir = working_dir,
    output_dir = output_dir,
    report_naming_format = report_naming_format,
    usernames = username_list,
    validation_files = validation_files
  )

  tryCatch({
    write_yaml(
      x = config_contents,
      file = file.path(path, "validation.yml")
    )

  }, error = function(e) {
    abort(paste0(
      c(
        "Error during creation of validation.yml config file. Error: ",
        e,
        sep = "\n"
      )
    ),
    class = "vt.validation_config_error")
  })

}

#' @importFrom rlang abort
#' @importFrom yaml read_yaml
read_validation_config <- function(){

  config_path <- vt_find_config()

  read_yaml(file = config_path)

}

### Accessor functions for internal use

get_config_working_dir <- function(){
  read_validation_config()$working_dir
}

get_config_output_dir <- function(){
  config <- read_validation_config()
  if("output_dir" %in% names(config)){
    return(config$output_dir)
  }else{
    return(config$working_dir)
  }
}

get_config_report_naming_format <- function(){
  read_validation_config()$report_naming_format
}
