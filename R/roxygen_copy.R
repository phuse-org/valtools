#' helper function to copy roxygen blocks from functions into a single folder
#' for parsing in an installed package
#'
#' @param from dir to copy R files from
#' @param to file to compile Roxygen blocks to
#' @param overwrite Should the file in `to` be overwritten?
#'
#' @returns invisibly returns TRUE when copying was completed successfully
#'
#' @noRd
#'
roxygen_copy <- function(from, to, overwrite = FALSE) {

  if( file.exists(to) & !overwrite){
    abort(paste0(
      "Error in copying function roxygen comments:\n",
      "File already exists: `",to,"`.\n",
      "Set overwrite to `TRUE` to overwrite file."),
          class = "vt.function_roxygen_copy_file_exists")
    }

  ## list all files to copy
  list_r_files <- list.files(
    path = from,
    all.files = TRUE,
    pattern = "[.]R$",
    ignore.case = TRUE,
    full.names = TRUE
  )

  tryCatch({

    suppressWarnings({
      roxygen_block_list <-
        do.call('c', lapply(list_r_files, vt_scrape_roxygen))
    })

    write_roxygen_block(
      roxygen_block_list,
      path = to,
      append = FALSE,
      only_with_tags = c("editor", "editDate")
    )
  }, error = function(e) {
    abort(paste0("Error in copying function roxygen comments:\n", e),
          class = "vt.function_roxygen_copy_fail")

  })

  invisible(TRUE)
}
