#' Create a file path to a validation item(requirement, test case, test code file)
#'
#' @param item_name Name of item file
#' @param type "requirements", "test_cases", "test_code"
#'
#' @return Nothing, side-effect to create directories and file
#'
#' @importFrom rlang abort inform
#'
#' @noRd
create_item <- function(type = c("requirements","test_cases","test_code"), item_name){

  type <- match.arg(type)

  validation_directory <- vt_path()

  # Create item folder if this is the first item
  if(!dir.exists(file.path(validation_directory, type))) {
    dir.create(file.path(validation_directory, type))
  }


  # Split out item_name to get any directories and create the structure.
  item_dir <- dirname(item_name)

  # If the item is nested in a folder make sure its made.
  if( !item_dir %in% c("",".")){
    tryCatch({
      dir.create(file.path(validation_directory,type,item_dir), recursive  = TRUE, showWarnings = FALSE)
    },
    error = function(e) {
      abort(paste0("Failed to create validation", type, item_name,
                   sep = " ", collapse = ""),
            class = "vt.itemCreateFail")
    })
  }

  item_file_path <- file.path(validation_directory, type, item_name)

  tryCatch({

    file.create(item_file_path)
    inform(paste0("Item created:", file.path(type, item_name), sep = " ", collapse = ""))
    return(item_file_path)

  }, error = function(e) {
    abort(paste0("Failed to create validation", type, item_name,
                                  sep = " ", collapse = ""),
                           class = "vt.itemCreateFail")
  })


}


#' Get current username
#'
#' Wrapper for whoami::username
#'
#'  @returns `[character]` Username of the person that called the function
#'
#' @importFrom whoami username
#' @export
#' @examples
#' withr::with_tempdir({
#' vt_use_validation(
#'     username_list = list(vt_user(
#'       username = whoami::username(),
#'       name = "test",
#'       title = "title",
#'       role = "role")))
#' vt_username()
#' })
#'
#' @importFrom whoami username
vt_username <- function(){
  user <- username(fallback = "")
  get_config_user_name(username = user)
}


#' Add specific extention to file names
#'
#' @noRd
#' @param filename the filname to add/replace extention
#' @param ext intended extention
#'
#' @returns filename with correct extention
#'
#' @importFrom tools file_ext file_path_sans_ext
vt_set_ext <- function(filename, ext){

    filename_ext <- file_ext(filename)
    filename <- file_path_sans_ext(filename)

    filename_ext <- ifelse(
      identical(tolower(filename_ext), tolower(ext)),
      filename_ext,
      ext
    )

    paste0(filename, ".", filename_ext)
}


## this section heavily cribbing from the usethis:::check_file_name, but
## it is not exported, so implementing our own version here, more or less copied

#' @importFrom tools file_path_sans_ext
valid_file_name <- function (x) {
  grepl("^[a-zA-Z0-9._\\/\\-]+$", x)
}

#' @importFrom usethis ui_stop ui_value
is_valid_name <- function(filename){

  if (!rlang::is_string(filename)) {
    ui_stop("Name must be a single string")
  }

  if(!valid_file_name(filename)){
    ui_stop(c("{ui_value(filename)} is not a valid file name. It should:",
              "* Contain only ASCII letters, numbers, '-','_', or file path delimeters (`\`,'//`)."))
  }
}

#' valtools clone of use_git_ignore to remove here dependency
#' @noRd
#' @importFrom usethis write_union
use_git_ignore2 <- function(ignores, dir = "."){
  write_union(file.path(dir, ".gitignore"), ignores)
}

#' valtools clone of use_build_ignore to remove here dependency
#' @noRd
#' @importFrom usethis write_union
use_build_ignore2 <- function(ignores, dir = "."){
  write_union(file.path(dir, ".Rbuildignore"), ignores)
}

#' @importFrom devtools as.package as.package
is_package <- function(pkg = "."){
  tryCatch({
    isTRUE(devtools::is.package(devtools::as.package(x = pkg)))
  }, error = function(e){
    FALSE
  })
}

set_dir_ref <- function(pkg = "."){
  if(!is_package(pkg) & !file.exists(file.path(pkg, ".here"))){
    file.create(file.path(pkg, ".here"))
  }
}

#' @importFrom desc desc_get_deps desc_set_deps
add_package_to_desc <- function(package, type, pkg = "."){
  if(is_package(pkg = pkg)){

    type <- match.arg(type, c("Depends", "Imports", "Suggests"))
    stopifnot(length(package) == length(type) | length(type) == 1)
    deps <- desc::desc_get_deps(file = pkg)
    if(length(type) == 1){
      type <- rep(type, length(package))
    }

    for(pak_idx in seq_along(package)){
      pak <- package[[pak_idx]]
      pak_type <- type[pak_idx]
      pak_ver <- "*"
      existing <- deps$package == pak
      if(any(existing)){
        dep_type <- deps$type[existing_dep]
        dep_ver <- deps$version[existing_dep]
        ## if new type is "higher" than old, replace
        if(!factor(dep_type, levels = c("Depends", "Imports", "Suggests"),ordered = TRUE) < pak_type ){
          dep_type <- pak_type
        }
        deps[existing,] <- data.frame(
          type = dep_type,
          package = pak,
          version = dep_ver
        )
      }else{
        deps[nrow(deps)+1,] <- data.frame(
          type = pak_type,
          package = pak,
          version = pak_ver
          )
      }
    }
    desc::desc_set_deps(deps, file = pkg)
  }
}

add_valtools_dep <- function(pkg = "."){
  add_package_to_desc("valtools",type = "Suggests",pksg = pkg)
}

#' @importFrom desc desc_get desc_set
#' @importFrom rlang abort
add_field_to_desc <- function(field, value, force = FALSE, pkg = "."){
  if(is_package(pkg = pkg)){
    curr <- desc::desc_get(field, file = pkg)[[1]]
    if(is.na(curr) | force){
      desc::desc_set(field, value, file = pkg)
    }
  }
}


