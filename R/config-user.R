
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
#' vt_use_config(pkg = ".")
#'
#' vt_add_user_to_config(
#'     username = "ellis",
#'     name = "Ellis Hughes",
#'     title = "Statistical Programmer",
#'     role = "Programmer")
#'
#' }
#' @export
vt_add_user_to_config <- function(username = whoami::username(), name, title, role){

  user_info <-
    ask_user_name_title_role(
      username = username,
      name = name,
      title = title,
      role = role)

  validation_config <- read_validation_config()

  updating_info <-
    names(user_info) %in% names(validation_config$usernames)

  user_list <-
    c(validation_config$usernames[setdiff(names(validation_config$usernames), names(user_info))],
      user_info)

  write_validation_config(
    path = dirname(vt_find_config()),
    working_dir = validation_config$working_dir,
    output_dir = validation_config$output_dir,
    report_naming_format = validation_config$report_naming_format,
    username_list = user_list,
    validation_files = validation_config$validation_files
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
vt_drop_user_from_config <- function(username){

  validation_config <- read_validation_config()

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
      path = dirname(vt_find_config()),
      working_dir = validation_config$working_dir,
      output_dir = validation_config$output_dir,
      report_naming_format = validation_config$report_naming_format,
      username_list = user_list,
      validation_files = validation_config$validation_files

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
vt_get_user_info <- function(username, type = c("name","title","role")){

  type <- match.arg(type,several.ok = TRUE)

  output <- c()

  if("name" %in% type){
    output <- c(name = get_config_user_name(username))
  }
  if("title" %in% type){
    output <- c(output, title = get_config_user_title(username))
  }
  if("role" %in% type){
    output <- c(output, role = get_config_user_role(username))
  }

  return(output)
}

#' Get all users from validation config file without knowing usernames
#' @param pkg Top-level directory of the package to validate
#' @return list of all users in config file
#' @export
vt_get_all_users <- function(){
  return(read_validation_config()$usernames)

}

#' @importFrom rlang is_interactive inform abort
get_config_user <- function(username){

  users <- read_validation_config()$usernames

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

#' @noRd
get_config_user_name <- function(username){
  get_config_user(username)$name
}

get_config_user_title <- function(username){
  get_config_user(username)$title
}

get_config_user_role <- function(username){
  get_config_user(username)$role
}

