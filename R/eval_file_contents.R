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

vt_print_file <- function(file, ..., type = tools::file_ext(file), dynamic_referencing = getOption("vt.dynamic_referencing", FALSE)){

  vt_file_con <- vt_file(
    path = file,
    type = type
  )

  print_file(file = vt_file_con, ..., dynamic_referencing = dynamic_referencing)

}

vt_file <- function(path, type){
  structure(
    path,
    type = type,
    class = tolower(type)
  )
}

#' @noRd
#' @keywords internal
#' @importFrom utils getFromNamespace
print_file <- function(file, ..., dynamic_referencing = FALSE){
  type <- class(text)[[1]]
  func <- trycatch({
    getFromNamespace(paste0("print_file.",type), "valtools")
  }, error = function(e){
    getFromNamespace("print_file.default", "valtools")
  }
  func(file, ..., dynamic_referencing = dynamic_referencing)
}

print_file.default <- function(file, ..., dynamic_referencing = FALSE){

  if(dynamic_referencing){
    text <- dynamic_reference_rendering(file)
  }else{
    text <- readLines(file)
  }

  cat(
    text,
    sep = "\n"
  )
}


print_file.md <- function(file, ..., dynamic_referencing = FALSE){

  if(dynamic_referencing){
    text <- dynamic_reference_rendering(file)
  }else{
    text <- readLines(file)
  }

  ## ID and remove yaml header if exists
  yaml_locs <- grep("^---\\s*$", text)[c(2)]
  if(length(yaml_locs) >= 2 & (yaml_locs[[2]] - yaml_locs[[1]] > 1)){
    text <- text[yaml_locs[[2]] +1 : length(text)]
  }

  ## remove roxygen comments
  text <- text[!grepl("^#'", text)]

  cat(
    text,
    sep = "\n"
  )

}

print_file.rmd <- parse_file.md


#' @noRd
#' @importFrom tools file_path_sans_ext
print_file.r_test_code <- function(file, ..., dynamic_referencing = FALSE){

  eval_test_code_text <- c(
    paste0("```{r ", file_path_sans_ext(basename(file)),", echo = FALSE}"),
    paste0("results <- vt_run_test_code_file(file=\"",basename(file),"\")"),
    ifelse(dynamic_referencing,"results <- dynamic_reference_rendering(results)",NULL),
    "vt_kable_test_code(results)"
    "```"
  )

  cat(
    eval_test_code_text,
    sep = "\n"
  )

}
