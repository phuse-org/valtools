#' Create a file path to a validation item(spec, test case, test code file)
#'
#' @param item_name Name/path of item file
#' @param type "spec", "cases", "code"
#'
#' @return Nothing, side-effect to create directories and file
#'
#' @importFrom stringr str_split
#' @importFrom rlang abort
#'
#' @noRd
create_item <- function(pkg, type, item_name){

  if(!dir.exists(paste0(pkg, "inst", "validation", sep = .Platform$file.sep))) {
    abort("No validation structure found. Run `valtools::vt_use_validation().`",
          class = "vt.missingStructure")
  }

  # Split out item_name to get any directories and create the structure.
  item_name_split <- str_split(item_name, .Platform$file.sep)[[1]]
  if(length(item_name_split) > 1){

    val_dir_path <- paste0(c(pkg, "inst", "validation", type,
                             paste0(head(item_name_split,-1)),collapse = ""),
                           sep = .Platform$file.sep, collapse = "")

    tryCatch({
      dir.create(val_dir_path, recursive  = TRUE)

    },
    error = function(e) {
      abort(paste0("Failed to create validation", type, item_name,
                   sep = " ", collapse = ""),
            class = "vt.itemCreateFail")
    })

  }

  item_file_path <- paste0(c(val_dir_path, .Platform$file.sep,tail(item_name_split, 1)),
                           collapse = "")

  file.create(item_file_path)

  inform(paste0("Item created:", type, item_name, sep = " ", collapse = ""))

  item_file_path
}
