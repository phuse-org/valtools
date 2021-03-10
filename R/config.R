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
#' \dontrun{
#'
#' vt_use_validation_config(pkg = ".",
#'                          working_dir = "vignettes",
#'                          output_dir  = "inst",
#'                          report_naming_format = "Validation_Report_{package}_v{version}_{date}"
#'                          username_list = list(
#'                            vt_user(
#'                              name = "test",
#'                              title = "test",
#'                              role = "tester",
#'                              username = "test"
#'                            )
#'                          ))
#'
#' }
#'
#' @export
#'
vt_use_validation_config <- function(pkg = ".",
                                     working_dir = "vignettes",
                                     output_dir  = "inst",
                                     report_naming_format = "Validation_Report_{package}_v{version}_{date}",
                                     username_list = list(),
                                     ...,
                                     overwrite = FALSE)
{

  if(file.exists(file.path(pkg,"validation.yml")) & !overwrite){
    abort(
      paste0(
      "Validation config file already exists.\n",
      "To overwrite, set `overwrite` to `TRUE` in `vt_use_validation_config()`"
      ),
      class = "vt.validation_config_exists"
    )
  }

  ## add ".here" ref if not a package
  set_dir_ref(pkg = pkg)

  ## add "validation.yml" to .Rbuildignore if is a package
  if(is_package(pkg = pkg)){
    use_build_ignore2(ignores = "^\\validation.yml$",dir = pkg)
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
    path = pkg,
    working_dir = working_dir,
    output_dir = output_dir,
    report_naming_format = report_naming_format,
    username_list = username_list,
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

#' Add users to validation config file
#'
#' Add user information to the projects validation config file to make for easier documentation
#'
#' @importFrom whoami username
#' @importFrom rlang inform
#'
#' @returns Used for side effect of adding user information to validation config
#'     file. Invisibly returns TRUE on success.
#'
#' @rdname validation_config
#'
#' @examples
#' \dontrun{
#'
#' vt_use_validation_config(pkg = ".")
#'
#' vt_add_user_to_config(
#'     username = "ellis",
#'     name = "Ellis Hughes",
#'     title = "Statistical Programmer",
#'     role = "Programmer")
#'
#' }
#' @export
vt_add_user_to_config <- function(username = whoami::username(), name, title, role, pkg = "."){

  user_info <-
    ask_user_name_title_role(
      username = username,
      name = name,
      title = title,
      role = role)

  validation_config <- read_validation_config(pkg = pkg)

  updating_info <-
    names(user_info) %in% names(validation_config$usernames)

  user_list <-
    c(validation_config$usernames[setdiff(names(validation_config$usernames), names(user_info))],
      user_info)

  write_validation_config(
    path = pkg,
    working_dir = validation_config$working_dir,
    output_directory = validation_config$output_directory,
    report_naming_format = validation_config$report_naming_format,
    username_list = user_list
  )

  inform(paste0(
    "User `",
    username,
    "` ",
    ifelse(!updating_info, "added to", "information updated in the") ,
    " validation config file."
  ),
  class = "vt.validation_config_add_user")

  invisible(TRUE)

}

#' Drop user from validation config file
#'
#' Remove user information from the projects validation config file
#'
#' @importFrom rlang inform warn
#'
#' @returns Used for side effect of removing user information to validation config
#'     file. Invisibly returns TRUE on success.
#'
#' @rdname validation_config
#'
#' @examples
#' \dontrun{
#'
#' vt_use_validation_config(pkg = ".")
#'
#' vt_add_user_to_config(
#'     username = "ellis",
#'     name = "Ellis Hughes",
#'     title = "Statistical Programmer",
#'     role = "Programmer")
#'
#' vt_drop_user_from_config(username = "ellis")
#'
#' }
#' @export
vt_drop_user_from_config <- function(username, pkg = "."){

  validation_config <- read_validation_config(pkg = pkg)

  existing_info <-
    username %in% names(validation_config$usernames)

  if(!existing_info){
    rlang::warn(
      paste0(
        "User `",
        username,
        "` ",
        "does not exist in the " ,
        "validation config file."
      ),
      class = "vt.validation_config_drop_did_not_exist")
  }else{

  user_list <- validation_config$usernames[setdiff(names(validation_config$usernames), username)]

  write_validation_config(
    path = pkg,
    working_dir = validation_config$working_dir,
    output_directory = validation_config$output_directory,
    report_naming_format = validation_config$report_naming_format,
    username_list = user_list
  )

    inform(paste0(
      "User `",
      username,
      "` ",
      "removed from the" ,
      " validation config file."
    ),
    class = "vt.validation_config_drop_user")
  }

  invisible(existing_info)
}



#' Get user information from validation config file
#'
#' Get recorded user information from the validation config file to make for easier documentation
#'
#' @param username computer username associated with the name and role
#' @param type type of information to pull. select at least one: name, title, role
#'
#' @importFrom whoami username
#' @importFrom rlang inform
#'
#' @returns a character vector length of types requested containing the user
#'    information from the validation config file.
#'
#' @rdname validation_config
#'
#' @examples
#' \dontrun{
#'
#' vt_use_validation_config(pkg = ".")
#'
#' vt_add_user_to_config(
#'     username = "ellis",
#'     name = "Ellis Hughes",
#'     title = "Statistical Programmer",
#'     role = "Programmer")
#'
#' vt_get_user_info(username = "ellis", type = c("name","title"))
#'
#' }
#'
#' @export
vt_get_user_info <- function(username, type = c("name","title","role"), pkg = "."){

  type <- match.arg(type,several.ok = TRUE)

  output <- c()

  if("name" %in% type){
    output <- c(name = get_config_user_name(username,pkg = pkg))
  }
  if("title" %in% type){
    output <- c(output, title = get_config_user_title(username,pkg = pkg))
  }
  if("role" %in% type){
    output <- c(output, role = get_config_user_role(username,pkg = pkg))
  }

  return(output)
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
                                    ...) {
  config_contents <- list(
    working_dir = working_dir,
    output_dir = output_dir,
    report_naming_format = report_naming_format,
    usernames = username_list
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
read_validation_config <- function(pkg = "."){

  if(!file.exists(file.path(pkg,"validation.yml"))){
    abort(
      paste0(
        "A validation config file does not exist.\n",
        "Run `valtools::vt_use_validation_config()` to create a validation config file."
      ),
      class = "vt.validation_config_missing"
    )
  }
  read_yaml(file = file.path(pkg,"validation.yml"))

}

#' ask information about user
#' @keywords internal
#' @noRd
#' @param username computer username associated with the name and role
#' @param name name of the user
#' @param role title of the user
#' @importFrom whoami username
ask_user_name_title_role <- function(username = whoami::username(), name, title, role){

  if(!missing(username) & !missing(name) & !missing(title)){
    return(
      vt_user(username = username, name = name, title = title, role)
    )
  }

  message(paste(collapse = "\n",c("",
                "Please supply some information for recording users within the package.",
                "Note, that this information can be updated at any time though `vt_add_user_to_config()`"
  )))

  if(missing(name)){
    cat("\n")
    name <- readline(paste0(" Please provide the name of the person associated with the username `",username,"` and press `Enter`: "))
  }

  if(missing(title)){
    cat("\n")
    title <- readline(paste0(" Please provide the title of the person associated with the username `",username,"` and press `Enter`: "))
  }

  if(missing(role)){
    cat("\n")
    role <- readline(paste0(" Please provide the role of the person for this validation associated with the username `",username,"` and press `Enter`: "))
  }

  cat("\n")

  vt_user(username = username, name = name, title = title, role = role)

}

### Accessor functions for internal use

get_config_working_dir <- function(pkg = "."){
  read_validation_config(pkg = pkg)$working_dir
}

get_config_output_dir <- function(pkg = "."){
  config <- read_validation_config(pkg = pkg)
  if("output_dir" %in% names(config)){
    return(config$output_dir)
  }else{
    return(config$working_dir)
  }
}

get_config_report_naming_format <- function(pkg = "."){
  read_validation_config(pkg = pkg)$report_naming_format
}

#' @importFrom rlang is_interactive inform abort
get_config_user <- function(username, pkg = "."){

  users <- read_validation_config(pkg = pkg)$usernames

  if( !username %in% names(users)){

    if(is_interactive()){


      inform(
        paste0("User `",username,"` does not exist in the config file"),
        class = "vt.validation_config_missing_user_inform"
      )

      message('Add user to config file?')
      decision <- "?"
      while(!tolower(decision) %in% c("y","n")){
        decision <-readline("[Y/n]: ")
      }

      if(tolower(decision) == "y"){
        vt_add_user_to_config(username = username)
        read_validation_config(pkg = pkg)$usernames[[username]]
      }else{
        abort(
          paste0("User `",username,"` does not exist in the config file.\n",
                 "Add `",username,"` to the config file with `vt_add_user_to_config(\"",username,"\")`."),
          class = "vt.validation_config_missing_user_error_interactive"
        )
      }

    }else{
      abort(
        paste0("User `",username,"` does not exist in the config file.\n",
               "Add `",username,"` to the config file with `vt_add_user_to_config(\"",username,"\")`."),
        class = "vt.validation_config_missing_user_error_batch"
      )
    }

  }else{

    return(users[[username]])

  }
}

get_config_user_name <- function(username, pkg = "."){
  get_config_user(username, pkg = pkg)$name
}

get_config_user_title <- function(username, pkg = "."){
  get_config_user(username, pkg = pkg)$title
}

get_config_user_role <- function(username, pkg = "."){
  get_config_user(username, pkg = pkg)$role
}
