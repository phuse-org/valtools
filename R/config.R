
#' use a validation config file
#'
#' validation configuration for directories and username-name-role key
#'
#' @param pkg where to write config file
#' @param val_dir [character] where validation contents are used interactively
#' @param val_dir_o [character] which folder under `inst` should the contents for
#'     validation be copied to on successful validation build.
#' @param username_list named list of named vectors. Name of list is username,
#'     with named list containing entries for username, name, title, and
#'     role to be used for documentation. Formatted as
#'     list(username = list(name = name, title = title, role = role, username = username))
#' @param ... These dots are for future extensions and must be empty.
#' @param overwrite `[boolean]`If a validation file exists, should it be overwritten?
#'    Defaults to FALSE.
#'
#' @importFrom rlang inform abort
#'
#' @returns Used for side effect to create validation config file. Invisibly
#'     returns TRUE on success.
#'
#' @rdname config
#'
#' @export
#'
vt_use_validation_config <- function(pkg = ".",
                                       val_dir = "vignettes/validation",
                                       val_dir_o = "validation",
                                       username_list = list(),
                                       ...,
                                     overwrite = FALSE) {

  if(file.exists(file.path(pkg,".validation")) & !overwrite){
    abort(
      paste0(
      "Validation config file already exists.\n",
      "To overwrite, set `overwrite` to `TRUE` in `vt_use_validation_config()`"
      ),
      class = "vt.validation_config_exists"
    )
  }

  if(length(username_list) > 0 ){
    check_username_list(username_list)
  }

  write_validation_config(
    path = pkg,
    val_dir = val_dir,
    val_dir_o = val_dir_o,
    username_list = username_list,
    ...
  )

  inform(
    "validation config file created. Add user information through `vt_add_user_to_config()`",
    class = "vt.validation_config_created"
  )

  invisible(TRUE)

}

#' Add users to validation config file
#'
#' Add user information to the projects validation config file to make for easier documentation
#'
#' @param username computer username associated with the name, title, and role
#' @param name name of the user
#' @param role role of the user
#' @param title title of the user
#'
#' @importFrom whoami username
#' @importFrom rlang inform
#'
#' @returns Used for side effect of adding user information to validation config
#'     file. Invisibly returns TRUE on success.
#'
#' @rdname config
#'
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
    val_dir = validation_config$validation_directory,
    val_dir_o = validation_config$validation_output_directory,
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
#' @returns Used for side effect of adding user information to validation config
#'     file. Invisibly returns TRUE on success.
#'
#' @rdname config
#'
#' @export
vt_get_user_info <- function(username = whoami::username(), type = c("name","title","role"), pkg = "."){

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
#' @param path where to write config file
#' @param val_dir [character] where validation contents are used interactively
#' @param val_dir_o [character] which folder under `inst` should the contents for
#'     validation be copied to on successful validation build.
#' @param username_list list of lists for username, names, title, and role to be used for
#'     documentation. Formatted as list(username = list(name = name, title = title, role = role, username = username))
#' @param ... These dots are for future extensions and must be empty.
#'
#' @importFrom yaml write_yaml
#' @importFrom rlang abort
write_validation_config <- function(path = ".",
                                    val_dir = "vignettes/validation",
                                    val_dir_o = "validation",
                                    username_list = list(),
                                    ...) {
  config_contents <- list(
    validation_directory = val_dir,
    validation_output_directory = val_dir_o,
    usernames = username_list
  )

  tryCatch({

    write_yaml(x = config_contents,
               file = file.path(path, ".validation"))

  }, error = function(e) {
    abort(paste0(
      c(
        "Error during creation of .validation config file. Error: ",
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

  if(!file.exists(file.path(pkg,".validation"))){
    abort(
      paste0(
        "A validation config file does not exist.\n",
        "Run `valtools::vt_use_validation_config()` to create a validation config file."
      ),
      class = "vt.validation_config_missing"
    )
  }
  read_yaml(file = file.path(pkg,".validation"))

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
      make_userlist_entry(username = username, name = name, title = title, role)
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

  make_userlist_entry(username = username, name = name, title = title, role = role)

}

#' @importFrom stats setNames
#' @noRd
make_userlist_entry <- function(username, name, title, role){
  setNames(list(
    list(name = name, title = title, role = role, username = username)
  ),username)
}

#' @importFrom rlang abort
#' @noRd
check_username_list <- function(username_list){

  all_valid <- all(
    sapply(seq_along(username_list),function(user_info_idx,username_list){

      uname <- names(username_list)[[user_info_idx]]

      user_info <- username_list[[user_info_idx]]

      user_info_is_list <- is.list(user_info)

      if(!user_info_is_list){
        rlang::abort(
          paste0(
            "Entry for username `",uname,"` is not a list."
          ),
          class = 'vt.invalid_username_as_list_entry'
        )
      }

      has_req_fields <- c("name","title","role","username") %in% names(user_info)

      uid_matches <- user_info[["username"]] == uname

      if(any(!has_req_fields)){
        rlang::abort(
          paste0(
            "Entry for username `",uname,"` is missing ",
            "entries for ",paste0("`",c("name","title","role","username")[!has_req_fields],"`", collapse =", ")," in the username list."
            ),
          class = 'vt.invalid_username_list_entry'
        )
      }

      if(!uid_matches){
        rlang::abort(
          paste0(
            "Entry for username `",uname,"` does not match the username ",
            "of the content: `",user_info[["username"]],"`."
          ),
          class = 'vt.mismatched_username_list_entry'
        )
      }

      return(all(c(has_req_fields, uid_matches)))

    },username_list)
  )

  invisible(all_valid)
}


### Accessor functions for internal use

get_config_val_dir <- function(pkg = "."){
  read_validation_config(pkg = pkg)$validation_directory
}

get_config_val_dir_o <- function(pkg = "."){
  read_validation_config(pkg = pkg)$validation_output_directory
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
