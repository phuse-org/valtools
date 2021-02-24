
#' write validation config file
#'
#' validation configuration for directories and username-name-role key
#' @noRd
#'
#' @param path where to write config file
#' @param val_dir [character] where validation contents are used interactively
#' @param val_dir_o [character] which folder under `inst` should the contents for
#'     validation be copied to on successful validation build.
#' @param username_list list of key for username, names, and role to be used for
#'     documentation
#' @param ... These dots are for future extensions and must be empty.
#'
#' @importFrom jsonlite write_json
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
    write_json(x = config_contents,
                         path = file.path(path, ".validation"),
               pretty = TRUE)
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
read_validation_config <- function(path = "."){

  if(!file.exists(file.path(path,".validation"))){
    abort(
      paste0(
        "A validation config file does not exist.",
        "Run `valtools::vt_use_validation_config()` to creae a config file."
        ),
      class = "vt.validation_config_missing"
      )
  }
  jsonlite::read_json(file.path(path,".validation"),simplifyVector = TRUE)
}

#' use a validation config file
#'
#' validation configuration for directories and username-name-role key
#'
#' @param path where to write config file
#' @param val_dir [character] where validation contents are used interactively
#' @param val_dir_o [character] which folder under `inst` should the contents for
#'     validation be copied to on successful validation build.
#' @param username_list named list of named vectors. Name of list is username,
#'     with named character vector containing entries for username, name, and
#'     title to be used for documentation.
#' @param ... These dots are for future extensions and must be empty.
#'
#' @export
#'
vt_use_validation_config <- function(pkg = ".",
                                       val_dir = "vignettes/validation",
                                       val_dir_o = "validation",
                                       username_list = list(),
                                       ...) {
  if (length(username_list) == 0) {
    username_list <- ask_user_name_role()
  }
  check_username_list(username_list)

  write_validation_config(
    path = ".",
    val_dir = val_dir,
    val_dir_o = val_dir_o,
    username_list = username_list,
    ...
  )

}


#' ask information about user
#' @keywords internal
#' @noRd
#' @param username computer username associated with the name and role
#' @param name name of the user
#' @param role title of the user
#' @importFrom whoami username
ask_user_name_role <- function(username = whoami::username(), name, title){

  if(!missing(username) & !missing(name) & !missing(title)){
    return(
      make_userlist_entry(username = username, name = name, title = title)
    )
  }

  message(paste(collapse = "\n",c("",
                "Please supply some information for recording users within the package.",
                "Note, that this information can be updated at any time though `vt_add_user_to_config()`"
  )))

  if(missing(name)){
    cat("\n")
    name <- readline(paste0(" Please provide your name associated with the username `",username,"` and press `Enter`: "))
  }

  if(missing(title)){
    cat("\n")
    title <- readline(paste0(" Please provide your title associated with the username `",username,"` and press `Enter`: "))
  }

  cat("\n")

  make_userlist_entry(username = username, name = name, title = title)

}

make_userlist_entry <- function(username, name, title){
  setNames(list(
    c(name = name, title = title, username = username)
  ),username)
}

#' @importFrom rlang abort
#' @noRd
check_username_list <- function(username_list){

  all_valid <- all(
    sapply(seq_along(username_list),function(user_info_idx,username_list){

      uname <- names(username_list)[[user_info_idx]]

      user_info <- username_list[[user_info_idx]]

      has_req_fields <- c("name","title","username") %in% names(user_info)

      uid_matches <- user_info[["username"]] == uname

      if(any(!has_req_fields)){
        rlang::abort(
          paste0(
            "Entry for username `",uname,"` is missing ",
            "entries for ",paste("`",c("name","title","username")[!has_req_fields],"`", collapse =", ")," in the username list.",
            ),
          class = 'vt.invalid_username_list_entry'
        )
      }

      if(!uid_matches){
        rlang::abort(
          paste0(
            "Entry for username `",uname,"` does not match the username ",
            "of the content: `",user_info[["username"]],"`.",
          ),
          class = 'vt.mismatched_username_list_entry'
        )
      }

    },username_list)
  )

  invisible((all_valid))
}


#' Add users to validation config file
#'
#' Add user information to the projects validation config file to make for easier documentation
#'
#' @param username computer username associated with the name and role
#' @param name name of the user
#' @param role title of the user
#'
#' @importFrom whoami username
#' @importFrom rlang inform
#'
#' @export
vt_add_user_to_config <- function(username = whoami::username(), name, title, pkg = "."){



  user_info <-
    ask_user_name_role(username = username,
                       name = name,
                       title = title)

  validation_config <- read_validation_config(path = pkg)

  updating_info <- names(user_info) %in% names(validation_config$usernames)

  user_list <- c(validation_config$usernames[setdiff(names(validation_config$usernames), names(user_info))],
                 user_info)

  write_validation_config(
    path = pkg,
    val_dir = validation_config$validation_directory,
    val_dir_o = validation_config$validation_output_directory,
    username_list = user_list
  )

  inform(paste0("User `",username,"` ", ifelse(!updating_info, "added to","information updated in the ") ,"validation config file."),
    class = "vt.validation_config_add_user"
  )

  invisible(TRUE)

}
