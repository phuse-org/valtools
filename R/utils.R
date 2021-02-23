#' Create a file path to a validation item(specification, test case, test code file)
#'
#' @param item_name Name of item file
#' @param type "specification", "cases", "code"
#'
#' @return Nothing, side-effect to create directories and file
#'
#' @importFrom rlang abort inform
#'
#' @noRd
create_item <- function(pkg, type = c("specification","test_case","test_code"), item_name){

  type <- match.arg(type)

  validation_directory <- getOption("vt.validation_directory", default = "vignettes/validation")

  # Error out if no validation skeleton
  if(!dir.exists(file.path(pkg, validation_directory))) {
    abort("No validation structure found. Run `valtools::vt_use_validation().`",
          class = "vt.missingStructure")

    # Create item folder if this is the first item
  }

  if(!dir.exists(file.path(pkg, validation_directory, type))) {
    dir.create(file.path(pkg, validation_directory, type))
  }


  # Split out item_name to get any directories and create the structure.
  item_dir <- dirname(item_name)

  # If the item is nested in a folder make sure its made.
  if( !item_dir %in% c("",".")){
    tryCatch({
      dir.create(file.path(pkg,validation_directory,type,item_dir), recursive  = TRUE, showWarnings = FALSE)
    },
    error = function(e) {
      abort(paste0("Failed to create validation", type, item_name,
                   sep = " ", collapse = ""),
            class = "vt.itemCreateFail")
    })
  }

  item_file_path <- file.path(pkg,validation_directory, type, item_name)

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
#' @returns `[character]` Username of the person that called the function
#'
#' @importFrom whoami username
#' @export
#' @examples
#' vt_username()
#'
#' @importFrom whoami username
vt_username <- function(){
  getOption("vt_username",default = whoami::username(fallback = ""))
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
