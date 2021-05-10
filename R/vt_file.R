#' print files for report generation
#'
#' valtools assists the user in generating the validation report by allowing the
#'    user to define the order in which
#'
#' @param file file to evaluate
#' @param ... These dots are for future extensions and must be empty.
#' @param dynamic_referencing Whether to employ dynamic referencing or not.
#'    defaults to FALSE.
#'
#' @returns a list of roxygen blocks found in the file.
#'
#' @export

vt_file <- function(file, ..., dynamic_referencing = FALSE){
  invisible(vt_file_vectorized(file = file, ...,dynamic_referencing = dynamic_referencing))
}

vt_file_serial <- function(file, ..., dynamic_referencing = FALSE){

  file_path <- vignette_file_path(
    file = file,
    ...
  )

  file_parse(file = file_path, ..., dynamic_referencing = dynamic_referencing)

}

vt_file_vectorized <- Vectorize(vt_file_serial)


vignette_file_path <- function(file, type, ...){


  if(missing(type)){
    type <- tools::file_ext(file)
    if("test_code" %in% split_path(file) & tolower(type) == "r"){
      type <- "r_test_code"
    }
  }

  structure(
    file,
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



file_parse.r_test_code <- function(file, ..., reference = NULL, envir = parent.frame(), dynamic_referencing = FALSE){

  text <- c("```{r echo = FALSE}",
            paste0("results <- eval_test_code(path = ",bquote(file),")"),
            ifelse(dynamic_referencing,
                   paste(" results <- dynamic_reference_rendering(results,reference = ",as.character(substitute(reference)),")"),NA),
            paste0("vt_kable_test_code_results(results, format = \"",vt_render_to(),"\")"),
            "```")

  text <- text[!is.na(text)]

  with_options(new = list(knitr.duplicate.label = "allow"), {
    cat(asis_output(knit_child(
      text = text,
      envir = envir,
      ...,
      quiet = TRUE
    )))
  })
}

#' output to render kable to
#'
#' reads the knitr and rmarkdown options to determine which output type is being rendered
#'
#' @importFrom knitr opts_knit current_input
#' @importFrom rmarkdown all_output_formats
#' @export

vt_render_to <- function(){
  output <- opts_knit$get("rmarkdown.pandoc.to")
  if(is.null(output)){
    output <- tryCatch({
      all_output_formats(knitr::current_input())[[1]]
    }, error = function(e){
      NULL
    })
  }
  if(is.null(output)){
    output <- "html"
  }
  return(output)
}
