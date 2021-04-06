#' print files for report generation
#'
#' valtools assists the user in generating the validation report by allowing the
#'    user to define the order in which
#'
#' @param file file to evaluate
#' @param ... These dots are for future extensions and must be empty.
#' @param type method of parse_roxygen to use if other that file extension
#' @param dynamic_referencing Whether to employ dynamic referencing or not.
#'    defaults to FALSE.
#'
#' @returns a list of roxygen blocks found in the file.
#'
#' @noRd

add_file_to_report <- function(file, report, ..., dynamic_referencing = FALSE){

  file_path <- validation_file_path(
    file = file,
    ...
  )

  add_file(file = file_path, report = report, ..., dynamic_referencing = dynamic_referencing)

}

validation_file_path <- function(file, type, ref = vt_path(), ...){

  path <- find_file(file, ref=ref)

  if(missing(type)){
    type <- tools::file_ext(path)
    if("test_code" %in% split_path(path)){
      type <- "r_test_code"
    }
  }

  structure(
    path,
    type = type,
    class = c(tolower(type),"validation_file_path")
  )
}

#' @noRd
#' @keywords internal
#' @importFrom utils getFromNamespace
add_file <- function(file, report, ..., dynamic_referencing = FALSE){

  file_type <- class(file)[[1]]

  func <- tryCatch({
    getFromNamespace(paste0("add_file.",file_type), "valtools")
  }, error = function(e){
    getFromNamespace("add_file.default", "valtools")
  })

  func(file = file, report = report, ..., dynamic_referencing = dynamic_referencing)
}

add_file.default <- function(file, report, ..., dynamic_referencing = FALSE){

  eval_file_text <- c(
    "",
    paste0("```{r ", chunk_name(file),", echo = FALSE, results = 'asis'}"),
    paste0("vt_file(file=vt_path(\"",file,"\")",
           ifelse(dynamic_referencing,", dynamic_referencing = TRUE)",")")),
    "```"
  )

  cat(
    eval_file_text,
    sep = "\n",
    file = report,
    append = TRUE
  )

}

add_file.r_test_code <- function(file, report, ..., dynamic_referencing = FALSE){

  eval_test_code_text <- c(
    "",
    paste0("```{r ", chunk_name(file),", echo = FALSE, results = 'asis'}"),
    paste0("results <- vt_run_test_code_file(file=\"",basename(file),"\")"),
    ifelse(dynamic_referencing,"results <- dynamic_reference_rendering(results)", NA),
    "vt_kable_test_code(results)",
    "```"
  )

  eval_test_code_text <- eval_test_code_text[!is.na(eval_test_code_text)]

  cat(
    eval_test_code_text,
    sep = "\n",
    file = report,
    append = TRUE
  )

}




split_path <- function(path) {
  if (dirname(path) %in% c(".", path)) return(basename(path))
  return(c(split_path(dirname(path)),basename(path)))
}

#' @noRd
#' @importFrom tools file_path_sans_ext
chunk_name <- function(path){
  filename_sans_ex <- file_path_sans_ext(basename(path))
  gsub("_","-",make.names(filename_sans_ex))
}
