#' print files for report generation
#'
#' valtools assists the user in generating the validation report by allowing the
#'    user to define the order in which
#'
#' @param file file to evaluate
#' @param ... These dots are for future extensions and must be empty.
#' @param type method of file_parse to use if other that file extension.
#'   If not a supported file, it will just print the contents of the file.
#'   Currently supported types: md, Rmd
#' @param dynamic_referencing Whether to employ dynamic referencing or not.
#'    defaults to FALSE.
#'
#' @returns a list of roxygen blocks found in the file.
#'
#' @export

vt_file <- function(file, ..., type = tools::file_ext(file), dynamic_referencing = FALSE){

  file_path <- vignette_file_path(
    path = file,
    type = type
  )

  file_parse(file = file_path, ..., dynamic_referencing = dynamic_referencing)

}

vignette_file_path <- function(path, type){
  structure(
    path,
    type = type,
    class = c(tolower(type),"vignette_file_path")
  )
}

#' @noRd
#' @keywords internal
#' @importFrom utils getFromNamespace
file_parse <- function(file, ..., dynamic_referencing = FALSE){

  type <- class(file)[[1]]

  func <- tryCatch({
    getFromNamespace(paste0("file_parse.",type), "valtools")
  }, error = function(e){
    getFromNamespace("file_parse.default", "valtools")
  })

  func(file, ..., dynamic_referencing = dynamic_referencing)
}

#' @importFrom knitr asis_output
file_parse.default <- function(file, ..., dynamic_referencing = FALSE){

  if(dynamic_referencing){
    text <- dynamic_reference_rendering(file, ...)
  }else{
    text <- readLines(file)
  }

  cat(asis_output(
    paste(text,collapse = "\n"),
  ))
}


#' @importFrom knitr knit_child
#' @importFrom withr with_options
file_parse.md <- function(file, ..., reference = NULL, envir = parent.frame(), dynamic_referencing = FALSE){

  if(dynamic_referencing){
    text <- dynamic_reference_rendering(file, reference = reference)
  }else{
    text <- readLines(file)
  }

  ## remove roxygen comments
  text <- text[!grepl("^#'", text)]

  with_options(new = list(knitr.duplicate.label = "allow"), {
    cat(asis_output(knit_child(
      text = text,
      envir = envir,
      ...,
      quiet = TRUE
    )))
  })
}

file_parse.rmd <- file_parse.md
